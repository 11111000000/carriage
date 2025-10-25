;;; carriage-sre-test.el --- SRE dry-run tests  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-sre-dry-run-occur-all-expect-ok ()
  (let* ((tmpdir (make-temp-file "carriage-sre-" t))
         (file (expand-file-name "a.txt" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "foo bar foo\nbaz foo\n"))
          (let* ((plan-item
                  (list
                   (cons :version "1")
                   (cons :op 'sre-batch)
                   (cons :file "a.txt")
                   (cons :pairs
                         (list
                          (list (cons :from "foo")
                                (cons :to "qux")
                                (cons :opts '(:occur all :expect 3 :match literal)))))))
                 (report (carriage-dry-run-sre plan-item tmpdir)))
            (should (eq (plist-get report :status) 'ok))
            (should (string-match-p "matches:3" (or (plist-get report :details) "")))))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest carriage-sre-dry-run-occur-all-expect-mismatch ()
  (let* ((tmpdir (make-temp-file "carriage-sre-" t))
         (file (expand-file-name "b.txt" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "alpha\nbeta\nalpha\n"))
          (let* ((plan-item
                  (list
                   (cons :version "1")
                   (cons :op 'sre)
                   (cons :file "b.txt")
                   (cons :pairs
                         (list
                          (list (cons :from "alpha")
                                (cons :to "ALPHA")
                                (cons :opts '(:occur all :expect 3 :match literal)))))))
                 (report (carriage-dry-run-sre plan-item tmpdir)))
            (should (eq (plist-get report :status) 'fail))
            (should (string-match-p "Expect mismatch" (or (plist-get report :details) "")))))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory tmpdir t)))))

;;; carriage-sre-test.el ends here
