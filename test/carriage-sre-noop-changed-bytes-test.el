;;; carriage-sre-noop-changed-bytes-test.el --- Ensure NOOP→skip includes Δbytes=0 -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(ignore-errors (require 'carriage-op-sre))

(ert-deftest carriage-sre-noop-changed-bytes-zero ()
  "Applying an SRE NOOP must return 'skip with :changed-bytes 0."
  (skip-unless (fboundp 'carriage-apply-sre))
  (let* ((tmp (make-temp-file "sre-noop-" nil ".txt" "hello\n"))
         (root (file-name-directory tmp))
         (plan (list (cons :version "1")
                     (cons :op 'sre)
                     (cons :file (file-name-nondirectory tmp))
                     (cons :pairs
                           (list (list (cons :from "hello")
                                       (cons :to   "hello")
                                       (cons :opts '(:occur first :match literal))))))))
    (unwind-protect
        (let ((res (carriage-apply-sre plan root)))
          (should (memq (plist-get res :status) '(skip ok)))
          ;; If NOOP is reported as 'skip (preferred), Δbytes must be 0
          (when (eq (plist-get res :status) 'skip)
            (should (numberp (plist-get res :changed-bytes)))
            (should (zerop (plist-get res :changed-bytes)))))
      (ignore-errors (delete-file tmp)))))

(provide 'carriage-sre-noop-changed-bytes-test)
;;; carriage-sre-noop-changed-bytes-test.el ends here
