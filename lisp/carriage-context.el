;;; carriage-context.el --- Context collector and formatter  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional arg invisible-ok to-heading))
(declare-function org-up-heading-safe "org" (&optional arg))

(defgroup carriage-context nil
  "Context collection and formatting for Carriage."
  :group 'applications
  :prefix "carriage-")

(defcustom carriage-context-debug nil
  "When non-nil, emit debug logs for context collection/counting."
  :type 'boolean
  :group 'carriage-context)

(defun carriage-context--dbg (fmt &rest args)
  "Internal debug logger for context layer (respects =carriage-context-debug')."
  (when carriage-context-debug
    (condition-case _
        (if (require 'carriage-logging nil t)
            (apply #'carriage-log (concat "Context: " fmt) args)
          (apply #'message (concat "[carriage-context] " fmt) args))
      (error nil))))

(defun carriage-context--project-root ()
  "Return project root directory, or default-directory."
  (or (and (fboundp 'carriage-project-root) (carriage-project-root))
      default-directory))

(defun carriage-context--inside-root-p (truename root)
  "Return non-nil if TRUENAME lies within ROOT."
  (let* ((rt (file-name-as-directory (file-truename root)))
         (pt (file-truename truename)))
    (string-prefix-p rt (file-name-as-directory pt))))

(defun carriage-context--normalize-path (path root)
  "Normalize PATH relative to ROOT; reject unsafe/TRAMP paths.
Return cons (ok . (rel . truename)) or (nil . reason-symbol)."
  (cond
   ((or (null path) (string-empty-p path))
    (cons nil 'empty))
   ((file-remote-p path)
    (cons nil 'remote))
   (t
    (let* ((abs (if (file-name-absolute-p path)
                    path
                  (expand-file-name path root)))
           (true (ignore-errors (file-truename abs))))
      (cond
       ((null true) (cons nil 'unresolvable))
       ((not (carriage-context--inside-root-p true root)) (cons nil 'outside-root))
       (t
        (let* ((rel (file-relative-name true root)))
          (cons t (cons rel true)))))))))

(defun carriage-context--read-file-safe (truename)
  "Read file contents from TRUENAME; return (ok . string-or-reason)."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents truename)
        (let ((s (buffer-substring-no-properties (point-min) (point-max))))
          ;; crude binary check: NUL-byte
          (if (string-match-p "\0" s)
              (cons nil 'binary)
            (cons t s))))
    (error (cons nil (format "read-error:%s" (error-message-string err))))))

(defun carriage-context--find-context-block-in-region (beg end)
  "Return list of path lines found between #+begin_context and #+end_context within BEG..END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((paths '()))
        (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
          (let ((block-beg (line-end-position)))
            (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                (let ((block-end (line-beginning-position)))
                  (save-excursion
                    (goto-char block-beg)
                    (while (< (point) block-end)
                      (let ((ln (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                        (unless (or (string-match-p "^[ \t]*\\(#\\|$\\)" ln))
                          (push (string-trim ln) paths)))
                      (forward-line 1))))
              ;; no end marker, consume till end
              (save-excursion
                (goto-char block-beg)
                (while (not (eobp))
                  (let ((ln (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                    (unless (or (string-match-p "^[ \t]*\\(#\\|$\\)" ln))
                      (push (string-trim ln) paths)))
                  (forward-line 1))))))
        (nreverse (let ((res (delete-dups (delq nil paths))))
                    (carriage-context--dbg "doc-paths: %s (first=%s)" (length res) (car res))
                    res))))))

(defun carriage-context--doc-paths (buffer)
  "Collect paths from #+begin_context block nearest to point in BUFFER.
For org-mode: search current heading; if not found, climb parents.
Fallback: search whole buffer."
  (with-current-buffer buffer
    (save-excursion
      (let ((paths nil))
        (if (derived-mode-p 'org-mode)
            (progn
              (require 'org)
              (ignore-errors (org-back-to-heading t))
              (let* ((level 0)
                     (found nil))
                (while (and (not found) (<= level 50))
                  (let* ((beg (save-excursion
                                (ignore-errors (org-back-to-heading t))
                                (point)))
                         (end (save-excursion
                                (ignore-errors (org-end-of-subtree t t))
                                (point)))
                         (ps (carriage-context--find-context-block-in-region beg end)))
                    (when (and ps (> (length ps) 0))
                      (setq paths ps
                            found t))
                    ;; climb up
                    (setq level (1+ level))
                    (unless (org-up-heading-safe)
                      (setq level 100)))))
              (unless paths
                ;; fallback: whole buffer
                (setq paths (carriage-context--find-context-block-in-region (point-min) (point-max)))))
          ;; not org-mode: scan whole buffer
          (setq paths (carriage-context--find-context-block-in-region (point-min) (point-max))))
        (delete-dups (delq nil paths))))))

(defun carriage-context--maybe-gptel-files ()
  "Collect absolute file paths from gptel context (best-effort).

- Prefer =gptel-context--collect' (when available from gptel-context.el).
- Fallback to =gptel-context' variable when the collector is unavailable.
- For buffer entries include only buffers visiting a file (BUFFER_FILE_NAME).
- Ignore non-existent/TRAMP/unknown sources here; higher-level filters apply."
  (let ((files '()))
    (condition-case _e
        (progn
          ;; Preferred path: use gptel-context--collect if present
          (when (require 'gptel-context nil t)
            (when (fboundp 'gptel-context--collect)
              (dolist (entry (gptel-context--collect))
                (pcase-let* ((`(,src . ,_props) (ensure-list entry)))
                  (cond
                   ((stringp src)
                    (push src files))
                   ((bufferp src)
                    (with-current-buffer src
                      (when buffer-file-name
                        (push buffer-file-name files)))))))))
          ;; Fallback: walk gptel-context alist if nothing was gathered
          (when (and (null files) (boundp 'gptel-context))
            (dolist (entry (symbol-value 'gptel-context))
              (pcase-let* ((`(,src . ,_props) (ensure-list entry)))
                (cond
                 ((stringp src)
                  (push src files))
                 ((bufferp src)
                  (with-current-buffer src
                    (when buffer-file-name
                      (push buffer-file-name files)))))))))
      (error nil))
    (let* ((res (delete-dups (cl-remove-if-not #'file-exists-p files))))
      (carriage-context--dbg "gptel-files: %s %s" (length res) (car res))
      res)))

(defun carriage-context-collect (&optional buffer root)
  "Collect context from sources into a plist:
:files — list of plists (:rel :true :content :reason)
:warnings — list of strings
:omitted — count of omitted files due to limits
:stats — plist (:total-bytes N :included M :skipped K)
This function respects buffer-local toggles:
- carriage-mode-include-gptel-context
- carriage-mode-include-doc-context
and size limits:
- carriage-mode-context-max-files
- carriage-mode-context-max-total-bytes"
  (let* ((buf (or buffer (current-buffer)))
         (root (or root (carriage-context--project-root)))
         ;; Defaults: ON when unbound (per spec); prefer buffer-local when available.
         (include-gptel (with-current-buffer buf
                          (if (boundp 'carriage-mode-include-gptel-context)
                              (buffer-local-value 'carriage-mode-include-gptel-context buf)
                            t)))
         (include-doc (with-current-buffer buf
                        (if (boundp 'carriage-mode-include-doc-context)
                            (buffer-local-value 'carriage-mode-include-doc-context buf)
                          t)))
         (max-files (or (with-current-buffer buf
                          (and (boundp 'carriage-mode-context-max-files)
                               (buffer-local-value 'carriage-mode-context-max-files buf)))
                        100))
         (max-bytes (or (with-current-buffer buf
                          (and (boundp 'carriage-mode-context-max-total-bytes)
                               (buffer-local-value 'carriage-mode-context-max-total-bytes buf)))
                        (* 1024 1024)))
         (warnings '())
         (files '())
         (seen (make-hash-table :test 'equal))
         (total-bytes 0)
         (included 0)
         (skipped 0))
    (carriage-context--dbg "collect: root=%s include{gptel=%s,doc=%s} limits{files=%s,bytes=%s}"
                           root include-gptel include-doc max-files max-bytes)
    ;; Merge sources
    (let* ((doc (when include-doc (carriage-context--doc-paths buf)))
           (gpf (when include-gptel (carriage-context--maybe-gptel-files)))
           (candidates (delete-dups
                        (append (or gpf '())
                                (or doc '())))))
      (carriage-context--dbg "collect: doc-paths=%s gptel-files=%s candidates=%s"
                             (and doc (length doc)) (and gpf (length gpf)) (length candidates))
      (dolist (p candidates)
        ;; Respect max-files against total emitted items (included+skipped),
        ;; not just those with content included.
        (when (< (+ included skipped) max-files)
          (let* ((norm (carriage-context--normalize-path p root)))
            (if (not (car norm))
                (progn
                  (cl-incf skipped)
                  (push (format "skip %s: %s" p (cdr norm)) warnings)
                  (carriage-context--dbg "collect: skip %s → %s" p (cdr norm)))
              (let* ((rel (cadr norm)) (true (cddr norm)))
                (unless (gethash true seen)
                  (puthash true t seen)
                  (let* ((rd (carriage-context--read-file-safe true)))
                    (cond
                     ((not (car rd))
                      (push (list :rel rel :true true :content nil :reason (cdr rd)) files)
                      (cl-incf skipped)
                      (carriage-context--dbg "collect: omit %s reason=%s" rel (cdr rd)))
                     (t
                      (let* ((s (cdr rd))
                             (sb (string-bytes s)))
                        (if (or (> (+ total-bytes sb) max-bytes))
                            (progn
                              (push (format "limit reached, include path only: %s" rel) warnings)
                              (push (list :rel rel :true true :content nil :reason 'size-limit) files)
                              (cl-incf skipped)
                              (carriage-context--dbg "collect: size-limit for %s (sb=%s total=%s)" rel sb total-bytes))
                          (setq total-bytes (+ total-bytes sb))
                          (push (list :rel rel :true true :content s) files)
                          (cl-incf included)
                          (carriage-context--dbg "collect: include %s (bytes=%s total=%s)" rel sb total-bytes)))))))))))))
    (carriage-context--dbg "collect: done files=%s included=%s skipped=%s total-bytes=%s warnings=%s"
                           (length files) included skipped total-bytes (length warnings))
    (list :files (nreverse files)
          :warnings (nreverse warnings)
          :omitted skipped
          :stats (list :total-bytes total-bytes :included included :skipped skipped))))

(defun carriage-context--guess-lang (path)
  "Guess language token for Org src block by PATH extension."
  (let ((ext (downcase (file-name-extension path ""))))
    (pcase ext
      ((or "el" "elisp") "emacs-lisp")
      ((or "md" "markdown") "markdown")
      ("org" "org")
      ((or "js" "mjs" "cjs") "js")
      ((or "ts" "tsx") "ts")
      ((or "py") "python")
      ((or "json") "json")
      ((or "sh" "bash" "zsh") "sh")
      ((or "c" "h") "c")
      ((or "cpp" "hpp" "cc" "hh") "cpp")
      ((or "java") "java")
      ((or "rs") "rust")
      ((or "go") "go")
      ((or "rb") "ruby")
      ((or "yml" "yaml") "yaml")
      (_ "text"))))

(defun carriage-context-format (ctx &key where)
  "Format CTX (plist from carriage-context-collect) into a string for insertion.
WHERE is 'system or 'user (affects only label string)."
  (let* ((files (or (plist-get ctx :files) '()))
         (warnings (or (plist-get ctx :warnings) '()))
         (stats (or (plist-get ctx :stats) '()))
         (hdr (format ";; Context (%s): files=%d included=%s omitted=%s total-bytes=%s\n"
                      (or (and where (symbol-name where)) "system")
                      (length files)
                      (or (plist-get stats :included) 0)
                      (or (plist-get stats :skipped) 0)
                      (or (plist-get stats :total-bytes) 0)))
         (warn-str (mapconcat (lambda (w) (concat ";; " w)) warnings "\n"))
         (sections
          (mapcar
           (lambda (f)
             (let* ((rel (plist-get f :rel))
                    (content (plist-get f :content))
                    (reason (plist-get f :reason))
                    (lang (carriage-context--guess-lang rel)))
               (if (stringp content)
                   (concat (format "In file %s:\n" rel)
                           (format "#+begin_src %s\n" lang)
                           content
                           "\n#+end_src\n")
                 (format "In file %s: [content omitted]%s\n"
                         rel
                         (if reason (format " (%s)" reason) "")))))
           files)))
    (string-join (delq nil (list hdr (and warnings warn-str)
                                 (mapconcat #'identity sections "\n")))
                 "\n")))

;; -----------------------------------------------------------------------------
;; Context counter for modeline/tooltips

(defun carriage-context-count (&optional buffer _point)
  "Вернуть plist со счётчиком элементов контекста для BUFFER.

Формат результата:
  (:count N
   :items  ((:path REL :true TRU :source doc|gptel|both :included t|nil :reason REASON) ...)
   :sources ((doc . ND) (gptel . NG) (both . NB))
   :warnings (STR ...)
   :stats (:total-bytes N :included M :skipped K))"
  (let* ((buf (or buffer (current-buffer)))
         (root (or (and (fboundp 'carriage-project-root)
                        (carriage-project-root))
                   default-directory))
         ;; Поддержка новых и легаси-тумблеров (дефолт ON при отсутствии переменных)
         (inc-gpt
          (with-current-buffer buf
            (let* ((have-new (boundp 'carriage-mode-include-gptel-context))
                   (have-legacy (boundp 'carriage-mode-use-context)))
              (cond
               ((or have-new have-legacy)
                (or (and have-new (buffer-local-value 'carriage-mode-include-gptel-context buf))
                    (and have-legacy (buffer-local-value 'carriage-mode-use-context buf))))
               (t t)))))
         (inc-doc
          (with-current-buffer buf
            (let* ((have-new (boundp 'carriage-mode-include-doc-context))
                   (have-legacy (boundp 'carriage-mode-context-attach-files)))
              (cond
               ((or have-new have-legacy)
                (or (and have-new (buffer-local-value 'carriage-mode-include-doc-context buf))
                    (and have-legacy (buffer-local-value 'carriage-mode-context-attach-files buf))))
               (t t))))))

    (if (not (or inc-gpt inc-doc))
        (progn
          (carriage-context--dbg "count: both sources OFF → 0")
          (list :count 0 :items '() :sources '() :warnings '() :stats '()))
      (let* ((doc-trues
              (let ((acc '()))
                (when inc-doc
                  (dolist (p (ignore-errors (carriage-context--doc-paths buf)))
                    (let* ((norm (carriage-context--normalize-path p root)))
                      (when (car norm) (push (cddr norm) acc)))))
                (delete-dups acc)))
             (gpt-trues
              (let ((acc '()))
                (when inc-gpt
                  (dolist (p (ignore-errors (carriage-context--maybe-gptel-files)))
                    (let* ((norm (carriage-context--normalize-path p root)))
                      (when (car norm) (push (cddr norm) acc)))))
                (delete-dups acc)))
             (doc-map (let ((h (make-hash-table :test 'equal)))
                        (dolist (tru doc-trues) (puthash tru tru h)) h))
             (gpt-map (let ((h (make-hash-table :test 'equal)))
                        (dolist (tru gpt-trues) (puthash tru tru h)) h))
             (col (carriage-context-collect buf root))
             (files (or (plist-get col :files) '()))
             (warnings (or (plist-get col :warnings) '()))
             (stats (or (plist-get col :stats) '()))
             (_ (carriage-context--dbg "count: collect: files=%s included=%s skipped=%s total-bytes=%s warnings=%s"
                                       (length files)
                                       (or (plist-get stats :included) 0)
                                       (or (plist-get stats :skipped) 0)
                                       (or (plist-get stats :total-bytes) 0)
                                       (length warnings)))
             (items
              (mapcar
               (lambda (f)
                 (let* ((rel (plist-get f :rel))
                        (tru (plist-get f :true))
                        (content (plist-get f :content))
                        (reason (plist-get f :reason))
                        (docp (and tru (gethash tru doc-map)))
                        (gptp (and tru (gethash tru gpt-map)))
                        (src (cond
                              ((and docp gptp) 'both)
                              (docp 'doc)
                              (gptp 'gptel)
                              (t nil))))
                   (list :path rel
                         :true tru
                         :source src
                         :included (stringp content)
                         :reason reason)))
               files))
             (srcs
              (let ((d 0) (g 0) (b 0))
                (dolist (it items)
                  (pcase (plist-get it :source)
                    ('doc (setq d (1+ d)))
                    ('gptel (setq g (1+ g)))
                    ('both (setq b (1+ b)))
                    (_ nil)))
                (list (cons 'doc d) (cons 'gptel g) (cons 'both b))))
             (count (length items)))
        (carriage-context--dbg "count: doc-trues=%s gpt-trues=%s files=%s count=%s warns=%s"
                               (length doc-trues) (length gpt-trues) (length files) count (length warnings))
        (list :count count :items items :sources srcs :warnings warnings :stats stats)))))

(provide 'carriage-context)
;;; carriage-context.el ends here
