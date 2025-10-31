;;; carriage-sre-regex-validator-test.el --- Regex validator tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-sre-regex-lookbehind-rejected ()
  "SRE parser should reject PCRE-style lookbehind constructs."
  (let* ((dir (make-temp-file "carriage-sre-rx-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (with-temp-buffer
          (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\")\n")
          (insert "#+pair (:match regex)\n")
          (insert "#+begin_from\n(?<=foo)bar\n#+end_from\n")
          (insert "#+begin_to\nBAZ\n#+end_to\n")
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-sre-regex-atomic-group-rejected ()
  "SRE parser should reject PCRE atomic group '(?>...)'."
  (let* ((dir (make-temp-file "carriage-sre-rx2-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (with-temp-buffer
          (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\")\n")
          (insert "#+pair (:match regex)\n")
          (insert "#+begin_from\n(?>foo)bar\n#+end_from\n")
          (insert "#+begin_to\nBAR\n#+end_to\n")
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

(provide 'carriage-sre-regex-validator-test)
;;; carriage-sre-regex-validator-test.el ends here
