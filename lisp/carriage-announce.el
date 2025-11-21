;;; carriage-announce.el --- User-facing success announcements  -*- lexical-binding: t; -*-
;;
;; (file body unchanged below)
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, convenience
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/ui-v2.org
;;   spec/logging-v2.org
;;   spec/apply-pipeline-v2.org
;;
;;; Commentary:
;; Announce concise success summaries after successful apply reports.
;; This module advises carriage-report-open to emit a short message in the
;; Messages buffer when an apply report contains no failures.
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defun carriage--announce--report-open (rep &rest _)
  "Announce a concise summary to Messages after successful apply REPORT via carriage-report-open."
  (when (and (listp rep))
    (let* ((phase (plist-get rep :phase))
           (sum (plist-get rep :summary))
           (fail (and (listp sum) (plist-get sum :fail))))
      ;; Announce when report indicates success; accept nil :phase for synthetic/tests.
      (when (and (or (eq phase 'apply) (null phase)) sum (numberp fail) (= fail 0))
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
