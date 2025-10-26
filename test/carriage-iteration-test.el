;;; carriage-iteration-test.el --- Group plan order and apply tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-iteration-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-iteration-test--read (dir rel)
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-group-order-create-then-sre ()
  "Plan should sort as create -> sre; dry-run ok; apply modifies file content."
  (let* ((dir (make-temp-file "carriage-iter-" t)))
    (unwind-protect
        (progn
          ;; init repo
          (should (zerop (carriage-iteration-test--git dir "init")))
          (should (zerop (carriage-iteration-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-iteration-test--git dir "config" "user.name" "Tester")))
          (let* ((default-directory (file-name-as-directory dir))
                 (org (mapconcat #'identity
                                 '(
                                   "#+begin_patch (:version \"1\" :op \"create\" :file \"x.txt\" :delim \"cafe01\")"
                                   "<<cafe01"
                                   "hello"
                                   ":cafe01"
                                   "#+end_patch"
                                   ""
                                   "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"deadbe\")"
                                   "<<deadbe"
                                   "hello"
                                   ":deadbe"
                                   "<<deadbe"
                                   "world"
                                   ":deadbe"
                                   "#+end_patch")
                                 "\n")))
            (with-temp-buffer
              (insert org "\n")
              (goto-char (point-min))
              (let* ((plan (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
                (should (= (length plan) 2))
                ;; dry-run
                (let* ((rep (carriage-dry-run-plan plan dir))
                       (items (plist-get rep :items))
                       (ops (mapcar (lambda (it) (plist-get it :op)) items))
                       (sum (plist-get rep :summary)))
                  (should (equal ops '(create sre)))
                  (should (eq (plist-get sum :fail) 0))
                  (should (eq (plist-get sum :ok) 2))
                  ;; SRE item should carry :matches
                  (let* ((sre-it (cadr items)))
                    (should (plist-get sre-it :matches))))
                ;; apply
                (let* ((ap (carriage-apply-plan plan dir)))
                  (should (eq (plist-get (plist-get ap :summary) :fail) 0))
                  (should (string= (carriage-iteration-test--read dir "x.txt") "world\n"))))))))
    (ignore-errors (delete-directory dir t)))))

;;; carriage-iteration-test.el ends here
