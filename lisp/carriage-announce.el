;;; carriage-announce.el --- User-facing success announcements  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defun carriage--announce--report-open (rep &rest _)
  "Announce a concise summary to Messages after successful apply REPORT via carriage-report-open."
  (when (and (not (bound-and-true-p noninteractive))
             (listp rep))
    (let* ((sum (plist-get rep :summary))
           (fail (and (listp sum) (plist-get sum :fail))))
      (when (and sum (numberp fail) (= fail 0))
        (let* ((items (or (plist-get rep :items) '()))
               (oks (cl-remove-if-not (lambda (it)
                                        (eq (plist-get it :status) 'ok))
                                      items))
               (created 0) (deleted 0) (renamed 0) (modified 0)
               (files '()))
          (dolist (it oks)
            (let ((op (plist-get it :op)))
              (pcase op
                ('create (setq created (1+ created)) (push (or (plist-get it :file) (plist-get it :path)) files))
                ('delete (setq deleted (1+ deleted)) (push (or (plist-get it :file) (plist-get it :path)) files))
                ('rename (setq renamed (1+ renamed)) (push (or (plist-get it :file) (plist-get it :path)) files))
                ((or 'patch 'sre 'aibo) (setq modified (1+ modified)) (push (or (plist-get it :file) (plist-get it :path)) files))
                (_ (push (or (plist-get it :file) (plist-get it :path)) files)))))
          (let* ((total (length oks))
                 (files-str (mapconcat #'identity (nreverse (delete-dups (delq nil files))) ", ")))
            (when (> total 0)
              (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                       total created modified deleted renamed files-str))))))))

(with-eval-after-load 'carriage-report
  (advice-add 'carriage-report-open :after #'carriage--announce--report-open))

(provide 'carriage-announce)
;;; carriage-announce.el ends here
