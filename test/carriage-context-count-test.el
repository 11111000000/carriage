;;; carriage-context-count-test.el --- Tests for context counting in modeline  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-context-count-fn-exists ()
  "Function carriage-context-count should be defined (to be implemented)."
  (if (fboundp 'carriage-context-count)
      (let ((res (ignore-errors (carriage-context-count))))
        (should (or (null res) (listp res))))
    (ert-skip "carriage-context-count not implemented yet")))

(ert-deftest carriage-context-count-begin-context-only ()
  "Counting items from nearest #+begin_context (files only)."
  (if (fboundp 'carriage-context-count)
      (with-temp-buffer
        (org-mode)
        ;; simulate toggles: only files from begin_context
        (let ((carriage-mode-use-context nil)
              (carriage-mode-context-attach-files t))
          (insert "* Heading\n")
          (insert "#+begin_context\n")
          (insert "spec/ui-v1.org\nlisp/carriage-ui.el\n")
          (insert "#+end_context\n")
          (goto-char (point-max))
          (let* ((pl (carriage-context-count (current-buffer) (point)))
                 (n  (plist-get pl :count))
                 (items (plist-get pl :items)))
            (should (integerp n))
            (should (>= n 2))
            (should (consp items)))))
    (ert-skip "carriage-context-count not implemented yet")))

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
