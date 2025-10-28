;;; carriage-sre-context-preview-test.el --- SRE context preview tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-op-sre)

(defun carriage-sre-context-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-sre-dry-run-preview-with-context ()
  "Dry-run SRE should include :diff preview with context lines ±1."
  (let* ((dir (make-temp-file "carriage-sre-ctx-" t))
         (carriage-mode-sre-preview-context-lines 1)) ;; ensure ±1 context
    (unwind-protect
        (progn
          (carriage-sre-context-test--write dir "a.txt" "one\ntwo\nthree\n")
          (let* ((item
                  (list
                   (cons :version "1")
                   (cons :op 'sre)
                   (cons :file "a.txt")
                   (cons :pairs
                         (list
                          (list (cons :from "two")
                                (cons :to "TWO")
                                (cons :opts '(:occur first :match literal)))))))
                 (rep (carriage-dry-run-sre item dir)))
            (should (eq (plist-get rep :status) 'ok))
            (let ((pv (plist-get rep :diff)))
              (should (stringp pv))
              (should (> (length pv) 0))
              ;; Check that preview contains -old/+new and neighbor context lines.
              (should (string-match-p "^-two\\b" pv))
              (should (string-match-p "^\\+TWO\\b" pv))
              (should (string-match-p "^one\\b" pv))
              (should (string-match-p "^three\\b" pv)))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-sre-context-preview-test.el ends here
