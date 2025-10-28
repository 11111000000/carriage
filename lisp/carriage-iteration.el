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

;;;###autoload
(defun carriage-mark-last-iteration (beg end)
  "Mark all #+begin_patch blocks between BEG and END as the “last iteration”.

If called interactively without an active region, mark the whole buffer.
Sets (or regenerates) `carriage--last-iteration-id' and writes it
as text property `carriage-iteration-id' on the begin line of each block."
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
            (let ((line-beg (line-beginning-position))
                  (line-end (line-end-position)))
              (add-text-properties line-beg line-end
                                   (list 'carriage-iteration-id id))
              (setq cnt (1+ cnt)))))
        ;; Skip to end of this block to avoid nested matches
        (when (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
          (forward-line 1))))
    (message "Carriage: marked %d block(s) as last iteration (id=%s)" cnt (substring id 0 8))
    id))

;;;###autoload
(defun carriage-current-iteration-id ()
  "Return current buffer's last iteration id, or nil."
  (interactive)
  (if carriage--last-iteration-id
      (progn
        (when (called-interactively-p 'any)
          (message "Last iteration id: %s" carriage--last-iteration-id))
        carriage--last-iteration-id)
    (when (called-interactively-p 'any)
      (message "No last iteration id set"))
    nil))

(provide 'carriage-iteration)
;;; carriage-iteration.el ends here
