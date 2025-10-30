;;; carriage-parser.el --- Parsers for patch blocks  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-logging)
(require 'carriage-format-registry)
(require 'carriage-iteration)

;; Forward declaration: buffer-local id of the "last iteration".
;; Defined as buffer-local in carriage-iteration.el.
(defvar carriage--last-iteration-id nil
  "Identifier of the last iteration in the current buffer (if any).")

;; Fallback when Customize not loaded: limit for SRE-batch pairs (see spec/index.org FREEZE)
(defvar carriage-mode-max-batch-pairs 200
  "Fallback maximum number of pairs allowed in sre-batch when Customize not loaded.")

(defun carriage-parse (op header-plist body-text repo-root)
  "Dispatch parse via registry by OP for HEADER-PLIST and BODY-TEXT under REPO-ROOT.

Strict v1 behavior:
- Алиасы :op не поддерживаются (MODE_E_DISPATCH).
- Нет автокоррекции :strip; валидация в парсерах.
- :path не принимается вместо :file (никаких скрытых преобразований).

Если обработчик не зарегистрирован, выполняется ленивый load нужного ops-модуля и повторная попытка."
  (cl-block carriage-parse
    (let* ((op-sym0 (if (symbolp op) op (intern (format "%s" op))))
           (op-sym
            (if (and (boundp 'carriage-mode-allow-op-aliases)
                     carriage-mode-allow-op-aliases)
                (pcase op-sym0
                  ('diff         'patch)
                  ('write        'create)
                  ('create_file  'create)
                  ('delete_file  'delete)
                  ('rename_file  'rename)
                  ('replace      'sre)
                  (_             op-sym0))
              (progn
                (pcase op-sym0
                  ((or 'replace 'diff 'write 'create_file 'delete_file 'rename_file)
                   (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                           (list (format "Alias op not supported in v1: %S" op-sym0))))
                  (_ nil))
                op-sym0)))
           (hdr1 header-plist)
           (rec (and (fboundp 'carriage-format-get) (carriage-format-get op-sym "1")))
           (fn  (and rec (plist-get rec :parse))))
      (unless (functionp fn)
        ;; Лениво грузим ops-модуль и повторяем поиск
        (pcase op-sym
          ((or 'sre 'sre-batch) (load "ops/carriage-op-sre" t t))
          ('patch               (load "ops/carriage-op-patch" t t))
          ((or 'create 'delete 'rename) (load "ops/carriage-op-file" t t))
          (_ nil))
        (setq rec (and (fboundp 'carriage-format-get) (carriage-format-get op-sym "1"))
              fn  (and rec (plist-get rec :parse))))
      (if (functionp fn)
          (funcall fn hdr1 body-text repo-root)
        (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                (list (format "Unknown op or unregistered handler: %S" op-sym)))))))

;;;; Ops-specific parsing moved to ops modules (lisp/ops/*).
;;;; This file now contains only registry dispatch and Org scanning helpers.

;;;; Org buffer helpers

(defun carriage--bounds-of-patch-block-at-point ()
  "Return (BEG . END) bounds of the current #+begin_patch ... #+end_patch block.
Move point is not changed. Return nil if not found."
  (save-excursion
    (let* ((beg (save-excursion
                  (when (re-search-backward "^[ \t]*#\\+begin_patch\\b" nil t)
                    (point))))
           (end (save-excursion
                  (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                    (line-beginning-position)))))
      (when (and beg end (> end beg))
        (cons beg end)))))

(defun carriage--read-patch-header-at (pos)
  "Parse patch header plist at line around POS. Return plist or signal error."
  (save-excursion
    (goto-char pos)
    (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
      (unless (string-match "#\\+begin_patch\\s-+\\((.*)\\)\\s-*$" line)
        (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "Invalid begin_patch header")))
      (car (read-from-string (match-string 1 line))))))

(defun carriage-parse-block-at-point (repo-root)
  "Parse current org patch block at point into a plan item under REPO-ROOT."
  (let* ((bounds (carriage--bounds-of-patch-block-at-point)))
    (unless bounds
      (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "No patch block at point")))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (header-plist (save-excursion
                           (goto-char beg)
                           (carriage--read-patch-header-at beg)))
           (body (save-excursion
                   (goto-char beg)
                   (forward-line 1)
                   (let ((body-beg (point)))
                     (goto-char end)
                     ;; end points to beginning of #+end_patch line
                     (buffer-substring-no-properties body-beg (line-beginning-position))))))
      (carriage-parse (plist-get header-plist :op) header-plist body repo-root))))

;;;; Region/group parsing

(defun carriage-parse-blocks-in-region (beg end repo-root)
  "Parse all #+begin_patch blocks between BEG and END into a PLAN under REPO-ROOT.
Return a list of plan items in buffer order."
  (save-excursion
    (goto-char beg)
    (let* ((plan '()))
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t))
        (let* ((start (match-beginning 0))
               ;; Compute BODY-BEG from START to avoid clobbered match-data in nested calls.
               (body-beg (save-excursion
                           (goto-char start)
                           (forward-line 1)
                           (point)))
               (header-plist (carriage--read-patch-header-at start))
               (block-end (save-excursion
                            (goto-char body-beg)
                            (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
                              (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT)
                                      (list "Unclosed #+begin_patch block")))
                            (line-beginning-position)))
               (body (buffer-substring-no-properties body-beg block-end))
               (op (plist-get header-plist :op)))
          ;; Diagnostics: count open markers in BODY to validate group extraction
          (ignore-errors
            (carriage-log "group-parse: op=%s file=%s opens=%d preview=%s"
                          op
                          (plist-get header-plist :file)
                          (cl-loop for ln in (split-string body "\n" nil nil)
                                   count (string-prefix-p "<<" (string-trim ln)))
                          (let* ((s (substring body 0 (min 200 (length body)))))
                            (replace-regexp-in-string "\n" "\\n" s))))
          (let* ((item (carriage-parse op header-plist body repo-root)))
            (push item plan))
          (goto-char block-end)
          (forward-line 1)))
      (nreverse plan))))

(defun carriage-collect-last-iteration-blocks (&optional repo-root)
  "Collect blocks of the last iteration in current buffer and parse to a PLAN.
If the buffer-local variable `carriage--last-iteration-id' is set, collect only blocks
annotated with that id (text property `carriage-iteration-id' on the #+begin_patch line).
Otherwise, return all patch blocks in the buffer.

If REPO-ROOT is nil, detect via `carriage-project-root' or use `default-directory'."
  (let* ((root (or repo-root (carriage-project-root) default-directory))
         (id   (or (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id)
                   (ignore-errors (carriage-iteration-read-org-id)))))
    (message "Carriage: collect-last-iteration root=%s id=%s"
             (or root "<nil>")
             (and id (substring id 0 (min 8 (length id)))))
    (if (not id)
        (carriage-parse-blocks-in-region (point-min) (point-max) root)
      (save-excursion
        (goto-char (point-min))
        (let* ((plan '()))
          (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
            (let* ((start (match-beginning 0))
                   (prop  (get-text-property start 'carriage-iteration-id))
                   (body-beg (save-excursion
                               (goto-char start)
                               (forward-line 1)
                               (point)))
                   (header-plist (carriage--read-patch-header-at start))
                   (block-end (save-excursion
                                (goto-char body-beg)
                                (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                                  (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT)
                                          (list "Unclosed #+begin_patch block")))
                                (line-beginning-position)))
                   (after (save-excursion
                            (goto-char block-end)
                            (forward-line 1)
                            (point))))
              (carriage-log "iter-collect: begin@%d prop=%s id=%s match=%s"
                            start prop id (if (equal prop id) "yes" "no"))
              (message "Carriage: iter-collect begin@%d prop=%s id=%s match=%s"
                       start prop (and id (substring id 0 (min 8 (length id))))
                       (if (equal prop id) "yes" "no"))
              (when (equal prop id)
                (let* ((body (buffer-substring-no-properties body-beg block-end))
                       (op (plist-get header-plist :op)))
                  (push (carriage-parse op header-plist body root) plan)
                  (carriage-log "iter-collect: pushed op=%s file=%s"
                                op (plist-get header-plist :file))
                  (message "Carriage: iter-collect pushed op=%s file=%s"
                           op (plist-get header-plist :file))))
              ;; Always advance to the first position after this exact block.
              (goto-char after)
              (carriage-log "iter-collect: advanced to %d (after end_patch)" (point))
              (message "Carriage: iter-collect advanced to %d" (point))))
          (setq plan (nreverse plan))
          (message "Carriage: iter-collect done matched=%d" (length plan))
          (if plan
              plan
            ;; Fallback: if id set but no blocks matched, parse all.
            (carriage-parse-blocks-in-region (point-min) (point-max) root)))))))

(provide 'carriage-parser)
;;; carriage-parser.el ends here
