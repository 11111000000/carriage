;;; carriage-iteration.el --- Mark and collect "last iteration" blocks  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)

(defvar-local carriage--last-iteration-id nil
  "Buffer-local identifier of the last iteration.
When set, only blocks whose text property `carriage-iteration-id' equals this value
are considered by `carriage-collect-last-iteration-blocks'.")

(defun carriage-iteration--generate-id ()
  "Generate a reasonably unique iteration id."
  (md5 (format "%s-%s-%s-%s"
               (float-time) (user-uid) (random most-positive-fixnum) (buffer-file-name))))

(defun carriage-iteration--org-property-line ()
  "Return (BEG . END) for existing CARRIAGE_ITERATION_ID property line, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_ITERATION_ID[ \t]+\\(.+\\)$" nil t)
      (cons (match-beginning 0) (line-end-position)))))

(defun carriage-iteration--write-org-id (id)
  "Best-effort write/update #+PROPERTY: CARRIAGE_ITERATION_ID with ID when in org-mode."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let* ((case-fold-search t)
             (pos-pair (carriage-iteration--org-property-line))
             (line (concat "#+PROPERTY: CARRIAGE_ITERATION_ID " id)))
        (if pos-pair
            (let* ((beg (car pos-pair))
                   (end (cdr pos-pair)))
              (goto-char beg)
              (delete-region beg end)
              (insert line))
          ;; Insert near top after common property lines
          (goto-char (point-min))
          (while (looking-at-p "^[ \t]*#\\+\\(TITLE\\|AUTHOR\\|PROPERTY\\|LANGUAGE\\|OPTIONS\\)\\b")
            (forward-line 1))
          (insert line "\n"))))))

(defun carriage-iteration-read-org-id ()
  "Read Org property CARRIAGE_ITERATION_ID or return nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_ITERATION_ID[ \t]+\\(.+\\)$" nil t)
        (string-trim (match-string 1))))))

;;;###autoload
(defun carriage-mark-last-iteration (beg end)
  "Mark all #+begin_patch blocks between BEG and END as the “last iteration”.

If called interactively without an active region, mark the whole buffer.
Sets (or regenerates) `carriage--last-iteration-id', writes it as text
property `carriage-iteration-id' on begin lines, and syncs Org property
#+PROPERTY: CARRIAGE_ITERATION_ID."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let* ((id (carriage-iteration--generate-id))
         (cnt 0))
    (setq carriage--last-iteration-id id)
    (carriage-log "mark-last-iteration: region %d..%d id=%s"
                  beg end (substring id 0 8))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t))
        (let ((start (match-beginning 0)))
          (save-excursion
            (goto-char start)
            (let* ((line-beg (line-beginning-position))
                   (line-end (line-end-position)))
              (add-text-properties line-beg line-end
                                   (list 'carriage-iteration-id id))
              (setq cnt (1+ cnt)))))
        ;; Skip to end of this block to avoid nested matches
        (when (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
          (forward-line 1))))
    ;; Best-effort sync to Org property
    (ignore-errors (carriage-iteration--write-org-id id))
    (message "Carriage: marked %d block(s) as last iteration (id=%s)" cnt (substring id 0 8))
    id))

;;;###autoload
(defun carriage-current-iteration-id ()
  "Return current buffer's last iteration id, reading Org property if needed."
  (interactive)
  (let* ((id (or carriage--last-iteration-id
                 (ignore-errors (carriage-iteration-read-org-id)))))
    (when id (setq carriage--last-iteration-id id))
    (when (called-interactively-p 'any)
      (message (if id "Last iteration id: %s" "No last iteration id set")
               (or id "")))
    id))

(provide 'carriage-iteration)
;;; carriage-iteration.el ends here
