;;; gptel-context-modeline-test.el --- Tests for gptel-context-modeline -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-context-modeline)

(ert-deftest gptel-context-modeline/org-block-items-basic ()
  "Nearest org context block is parsed into non-empty, trimmed lines."
  (with-temp-buffer
    (insert "Some text\n#+begin_context\n a\n\n b \n# comment\n c\n#+end_context\nMore\n")
    (goto-char (point-max))
    (let ((gptel-context-modeline-enable-org-block-context t))
      (should (equal (gptel-context-modeline--org-block-items)
                     '("a" "b" "# comment" "c"))))))

(ert-deftest gptel-context-modeline/org-block-items-nearest-backward ()
  "Prefer block found searching backward from point."
  (with-temp-buffer
    (insert "#+begin_context\nX\n#+end_context\n....\n#+begin_context\nY\n#+end_context\n")
    ;; Point after second block -> should pick second block (nearest backward)
    (goto-char (point-max))
    (should (equal (gptel-context-modeline--org-block-items) '("Y")))))

(ert-deftest gptel-context-modeline/format-and-tooltip ()
  "Modeline string reflects correct count and builds tooltip."
  (with-temp-buffer
    (insert "#+begin_context\na\nb\nc\n#+end_context\n")
    (goto-char (point-max))
    (let* ((gptel-context-modeline-enable-org-block-context true)
           (gptel-context-modeline-enable-gptel-context nil)
           (str (gptel-context-modeline--string)))
      (should (string-match-p "\\[Ctl:3\\]" str))
      (let* ((items (gptel-context-modeline--org-block-items))
             (tooltip (gptel-context-modeline--tooltip items)))
        (should (string-match-p "^a\nb\nc$" tooltip)))))

(ert-deftest gptel-context-modeline/tooltip-limit ()
  "Tooltip respects max items limit and shows summary for the rest."
  (with-temp-buffer
    (insert "#+begin_context\na\nb\nc\nd\n#+end_context\n")
    (goto-char (point-max))
    (let ((gptel-context-modeline-max-tooltip-items 2))
      (let ((tooltip (gptel-context-modeline--tooltip '("a" "b" "c" "d"))))
        (should (string-match-p "^a\nb\n… (\\+2 more)$" tooltip))))))

(ert-deftest gptel-context-modeline/aggregate-with-gptel-provider ()
  "Aggregated count includes items from both providers."
  (with-temp-buffer
    (insert "#+begin_context\na\nb\n#+end_context\n")
    (goto-char (point-max))
    (let* ((gptel-context-modeline-enable-org-block-context t)
           (gptel-context-modeline-enable-gptel-context t)
           (gptel-context-modeline-gptel-items-function (lambda () '("g1" "g2" "g3")))
           (str (gptel-context-modeline--string)))
      (should (string-match-p "\\[Ctl:5\\]" str)))))

(provide 'gptel-context-modeline-test)

;;; gptel-context-modeline-test.el ends here
