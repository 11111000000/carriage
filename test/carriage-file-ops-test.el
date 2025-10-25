;;; carriage-file-ops-test.el --- File ops tests (create/delete/rename)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-file-ops-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-file-ops-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-file-ops-test--read (dir rel)
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-file-ops-create-dry-run-and-apply ()
  "Dry-run create should succeed on non-existing file; then apply and check content."
  (let* ((dir (make-temp-file "carriage-create-" t)))
    (unwind-protect
        (progn
          ;; init repo
          (should (zerop (carriage-file-ops-test--git dir "init")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.name" "Tester")))
          ;; plan
          (let* ((item (list (cons :version "1")
                             (cons :op 'create)
                             (cons :file "docs/intro.md")
                             (cons :content "* Intro\nHello\n")
                             (cons :mkdir t))))
            (let ((dry (carriage-dry-run-create item dir)))
              (should (eq (plist-get dry :status) 'ok)))
            (let ((ap (carriage-apply-create item dir)))
              (should (eq (plist-get ap :status) 'ok))
              (should (string= (carriage-file-ops-test--read dir "docs/intro.md")
                               "* Intro\nHello\n")))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-file-ops-delete-dry-run-and-apply ()
  "Dry-run delete checks existence; apply removes the file via git rm."
  (let* ((dir (make-temp-file "carriage-delete-" t)))
    (unwind-protect
        (progn
          ;; init repo and commit a file
          (should (zerop (carriage-file-ops-test--git dir "init")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.name" "Tester")))
          (carriage-file-ops-test--write dir "tmp/file.txt" "bye\n")
          (should (zerop (carriage-file-ops-test--git dir "add" "--" "tmp/file.txt")))
          (should (zerop (carriage-file-ops-test--git dir "commit" "-m" "add file")))
          ;; plan
          (let ((item (list (cons :version "1") (cons :op 'delete) (cons :file "tmp/file.txt"))))
            (let ((dry (carriage-dry-run-delete item dir)))
              (should (eq (plist-get dry :status) 'ok)))
            (let ((ap (carriage-apply-delete item dir)))
              (should (eq (plist-get ap :status) 'ok))
              (should (not (file-exists-p (expand-file-name "tmp/file.txt" dir)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-file-ops-rename-dry-run-and-apply ()
  "Dry-run rename validates source/target; apply renames via git mv."
  (let* ((dir (make-temp-file "carriage-rename-" t)))
    (unwind-protect
        (progn
          ;; init repo and commit a file
          (should (zerop (carriage-file-ops-test--git dir "init")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-file-ops-test--git dir "config" "user.name" "Tester")))
          (carriage-file-ops-test--write dir "a.txt" "alpha\n")
          (should (zerop (carriage-file-ops-test--git dir "add" "--" "a.txt")))
          (should (zerop (carriage-file-ops-test--git dir "commit" "-m" "add a.txt")))
          ;; plan
          (let ((item (list (cons :version "1") (cons :op 'rename)
                            (cons :from "a.txt") (cons :to "sub/b.txt"))))
            (let ((dry (carriage-dry-run-rename item dir)))
              (should (eq (plist-get dry :status) 'ok)))
            (let ((ap (carriage-apply-rename item dir)))
              (should (eq (plist-get ap :status) 'ok))
              (should (not (file-exists-p (expand-file-name "a.txt" dir))))
              (should (string= (carriage-file-ops-test--read dir "sub/b.txt") "alpha\n")))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-file-ops-test.el ends here
