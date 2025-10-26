;;; carriage-sre-regex-validator-test.el --- Regex validator tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-sre-regex-lookbehind-rejected ()
  "SRE parser should reject PCRE-style lookbehind constructs."
  (let* ((dir (make-temp-file "carriage-sre-rx-" t))
         (default-directory (file-name-as-directory dir))
         (delim "deadbe"))
    (unwind-protect
        (with-temp-buffer
          (insert (format "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"%s\")\n" delim))
          (insert "#+pair (:match regex)\n")
          (insert (format "<<%s\n(?<=foo)bar\n:%s\n" delim delim))
          (insert (format "<<%s\nBAZ\n:%s\n" delim delim))
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-sre-regex-atomic-group-rejected ()
  "SRE parser should reject PCRE atomic group '(?>...)'."
  (let* ((dir (make-temp-file "carriage-sre-rx2-" t))
         (default-directory (file-name-as-directory dir))
         (delim "deadbe"))
    (unwind-protect
        (with-temp-buffer
          (insert (format "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"%s\")\n" delim))
          (insert "#+pair (:match regex)\n")
          (insert (format "<<%s\n(?>foo)bar\n:%s\n" delim delim))
          (insert (format "<<%s\nBAR\n:%s\n" delim delim))
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

(provide 'carriage-sre-regex-validator-test)
;;; carriage-sre-regex-validator-test.el ends here
