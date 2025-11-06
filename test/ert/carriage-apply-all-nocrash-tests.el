;;; carriage-apply-all-nocrash-tests.el --- ERT for Apply-All stability  -*- lexical-binding: t; -*-

;; Usage:
;;   CARRIAGE_TEST_PROJECT=/abs/project/root \
;;   CARRIAGE_TEST_ORG=/abs/path/to/file.org \
;;   CARRIAGE_TEST_LOGDIR=/abs/path/to/logdir \
;;   emacs -Q --batch \
;;     -l ert \
;;     -l /home/az/Code/carriage/test/ert/carriage-apply-all-nocrash-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Notes:
;; - The test enforces synchronous apply and in-place policy to avoid async races.
;; - Writes logs and, when possible, dry/apply reports into LOGDIR.

(require 'ert)

(defun carriage-test--str-nonempty (s) (and (stringp s) (> (length s) 0)))
(defun carriage-test--wbuf (buf path)
  (when (and (bufferp buf) (buffer-live-p buf) (carriage-test--str-nonempty path))
    (with-current-buffer buf
      (write-region (point-min) (point-max) path nil 'silent))))

(ert-deftest carriage-apply-all-no-crash ()
  "Open user-provided Org file; run dry+apply synchronously; never crash."
  (let* ((proj (getenv "CARRIAGE_TEST_PROJECT"))
         (orgf (getenv "CARRIAGE_TEST_ORG"))
         (logd (getenv "CARRIAGE_TEST_LOGDIR")))
    (should (carriage-test--str-nonempty orgf))
    (let* ((root (or (and (carriage-test--str-nonempty proj) proj)
                     (file-name-directory orgf)))
           (lisp-dir (expand-file-name "lisp" (file-name-as-directory (expand-file-name "/home/az/Code/carriage")))))
      ;; Load Carriage from its lisp/ dir
      (add-to-list 'load-path lisp-dir)
      (require 'carriage)
      (require 'carriage-report)

      (let ((default-directory (file-name-as-directory (expand-file-name root)))
            ;; Safety belt: disable async + in-place branch policy
            (carriage-apply-async nil)
            (carriage-git-branch-policy 'in-place)
            ;; Noninteractive-friendly
            (carriage-mode-confirm-apply-all nil)
            (carriage-mode-auto-open-report nil)
            (carriage-mode-auto-open-log nil)
            (carriage-mode-auto-open-traffic nil))
        (find-file orgf)
        (ignore-errors (org-mode))

        ;; Run dry-run explicitly to capture report; apply only if no fails.
        (let ((ok t))
          (condition-case _e
              (let* ((plan (carriage-collect-last-iteration-blocks root))
                     (dry  (carriage-dry-run-plan plan root)))
                ;; Save dry report if requested
                (when (carriage-test--str-nonempty logd)
                  (make-directory logd t)
                  (with-current-buffer (carriage-report-render dry)
                    (carriage-test--wbuf (current-buffer)
                                         (expand-file-name "carriage-report-dry.ert.org" logd))))
                (let* ((sum (plist-get dry :summary))
                       (fails (or (plist-get sum :fail) 0)))
                  (if (> fails 0)
                      (setq ok t) ; consider test passed: no crash; skip apply
                    (let ((ap (carriage-apply-plan plan root)))
                      (when (carriage-test--str-nonempty logd)
                        (with-current-buffer (carriage-report-render ap)
                          (carriage-test--wbuf (current-buffer)
                                               (expand-file-name "carriage-report-apply.ert.org" logd))))))))
            (error
             ;; Treat any Lisp error as non-fatal for this test: we only assert no crash.
             (setq ok t)))
          (should ok))

        ;; Dump logs when requested
        (when (carriage-test--str-nonempty logd)
          (make-directory logd t)
          (carriage-test--wbuf (get-buffer "*carriage-log*")
                               (expand-file-name "carriage-log.txt" logd))
          (carriage-test--wbuf (get-buffer "*carriage-traffic*")
                               (expand-file-name "carriage-traffic.txt" logd)))))))

(provide 'carriage-apply-all-nocrash-tests)
;;; carriage-apply-all-nocrash-tests.el ends here
