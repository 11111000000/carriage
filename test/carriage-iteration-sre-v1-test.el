;;; carriage-iteration-sre-v1-test.el --- Iteration + plan order (SRE v1) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-apply)

(ert-deftest carriage-group-order-create-then-sre-v1 ()
  "Order: delete→rename→create→patch→sre. Ensure create < sre."
  (let* ((create (list (cons :version "1") (cons :op 'create) (cons :file "a.txt")
                       (cons :content "foo\n")))
         (sre    (list (cons :version "1") (cons :op 'sre) (cons :file "a.txt")
                       (cons :pairs (list (list (cons :from "foo")
                                                (cons :to "bar")
                                                (cons :opts '(:occur first :match literal)))))))
         (sorted (funcall (symbol-function 'carriage--plan-sort) (list sre create))))
    (should (= (length sorted) 2))
    (should (eq (alist-get :op (car sorted)) 'create))
    (should (eq (alist-get :op (cadr sorted)) 'sre))))

(ert-deftest carriage-last-iteration-mark-and-collect-v1 ()
  "Mark two SRE blocks as last iteration and collect them."
  (with-temp-buffer
    (org-mode)
    ;; Insert two SRE v1 blocks
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\")\n"
            "#+begin_from\nfoo\n#+end_from\n#+begin_to\nbar\n#+end_to\n"
            "#+end_patch\n\n")
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"y.txt\")\n"
            "#+begin_from\none\n#+end_from\n#+begin_to\nTWO\n#+end_to\n"
            "#+end_patch\n")
    (goto-char (point-min))
    ;; Mark and collect
    (let ((beg (point-min)) (end (point-max)))
      (carriage-mark-last-iteration beg end))
    (let* ((plan (carriage-collect-last-iteration-blocks default-directory)))
      (should (listp plan))
      (should (= (length plan) 2))
      (should (equal (mapcar (lambda (it) (alist-get :op it)) plan)
                     '(sre sre))))))
(provide 'carriage-iteration-sre-v1-test)
;;; carriage-iteration-sre-v1-test.el ends here
