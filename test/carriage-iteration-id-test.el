;;; carriage-iteration-id-test.el --- Last iteration mark/collect tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-parser)
(require 'carriage-iteration)

(ert-deftest carriage-last-iteration-mark-and-collect ()
  "Mark two blocks as last iteration
 and ensure only they are collected."
  (let* ((dir (make-temp-file "carriage-iter-id-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (with-temp-buffer
          ;; Three SRE blocks over different files
          (insert (mapconcat #'identity
                             '("#+begin_patch (:version \"1\" :op \"sre\" :file \"a.txt\" :delim \"aaa111\")"
                               "<<aaa111"
                               "old-a"
                               ":aaa111"
                               "<<aaa111"
                               "new-a"
                               ":aaa111"
                               "#+end_patch"
                               ""
                               "#+begin_patch (:version \"1\" :op \"sre\" :file \"b.txt\" :delim \"bbb222\")"
                               "<<bbb222"
                               "old-b"
                               ":bbb222"
                               "<<bbb222"
                               "new-b"
                               ":bbb222"
                               "#+end_patch"
                               ""
                               "#+begin_patch (:version \"1\" :op \"sre\" :file \"c.txt\" :delim \"ccc333\")"
                               "<<ccc333"
                               "old-c"
                               ":ccc333"
                               "<<ccc333"
                               "new-c"
                               ":ccc333"
                               "#+end_patch")
                             "\n"))
          ;; Mark region covering first two blocks
          (goto-char (point-min))
          (let ((beg (point-min)))
            (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (forward-line 1)
            (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (let ((end (line-end-position)))
              (carriage-mark-last-iteration beg end)))
          ;; Collect only marked blocks
          (goto-char (point-min))
          (let ((plan (carriage-collect-last-iteration-blocks default-directory)))
            (should (= (length plan) 2))
            (should (equal (mapcar (lambda (it) (alist-get :file it)) plan)
                           '("a.txt" "b.txt")))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-iteration-id-test.el ends here
