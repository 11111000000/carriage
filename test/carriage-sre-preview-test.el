;;; carriage-sre-preview-test.el --- SRE preview diff tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-sre-preview-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-sre-dry-run-produces-preview ()
  "Dry-run SRE should include :diff preview with non-empty content."
  (let* ((dir (make-temp-file "carriage-sre-prev-" t)))
    (unwind-protect
        (progn
          (carriage-sre-preview-test--write dir "a.txt" "hello\nhello\nworld\n")
          (let* ((item
                  (list
                   (cons :version "1")
                   (cons :op 'sre)
                   (cons :file "a.txt")
                   (cons :pairs
                         (list
                          (list (cons :from "hello")
                                (cons :to "hi")
                                (cons :opts '(:occur first :match literal)))))))
                 (rep (carriage-dry-run-sre item dir)))
            (should (eq (plist-get rep :status) 'ok))
            (let ((pv (plist-get rep :diff)))
              (should (stringp pv))
              (should (> (length pv) 0))))))
    (ignore-errors (delete-directory dir t)))))

;;; carriage-sre-preview-test.el ends here
