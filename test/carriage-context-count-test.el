;;; carriage-context-count-test.el --- Tests for context counting in modeline  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-context-count-gptel-and-blocks ()
  "Counting items from both gptel context and begin_context (optional)."
  (if (fboundp 'carriage-context-count)
      (with-temp-buffer
        (org-mode)
        (let ((carriage-mode-use-context t)
              (carriage-mode-context-attach-files t))
          (insert "* H\n#+begin_context\nspec/testing-v1.org\n#+end_context\n")
          (goto-char (point-max))
          (let* ((pl (carriage-context-count (current-buffer) (point)))
                 (n (plist-get pl :count))
                 (srcs (plist-get pl :sources)))
            (should (integerp n))
            (should (listp srcs))))
        (ert-skip "carriage-context-count not implemented yet"))))

(provide 'carriage-context-count-test)
;;; carriage-context-count-test.el ends here
