;;; ert-runner.el --- Run ERT tests in batch  -*- lexical-binding: t; -*-

(require 'ert)

;; Ensure lisp/ is on load-path (flake also adds -L lisp).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" here)))
  (add-to-list 'load-path lisp))

(require 'carriage)

;; Load all *-test.el in this directory.
(let* ((here (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f (directory-files here t "-test\\.el\\'"))
    (load f nil t)))

(ert-run-tests-batch-and-exit)
