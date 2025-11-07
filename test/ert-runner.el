;;; ert-runner.el --- Run ERT tests in batch  -*- lexical-binding: t; -*-

(require 'ert)

;; Ensure lisp/ is on load-path (flake also adds -L lisp).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" here)))
  (add-to-list 'load-path lisp))

(require 'carriage)

;; Load all *-test.el in this directory and subdirs (e.g., engines).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (eng  (expand-file-name "engines" here))
       (dirs (list here (and (file-directory-p eng) eng))))
  ;; Ensure tests directory on load-path so (load "...") works.
  (add-to-list 'load-path here)
  (dolist (dir dirs)
    (when dir
      (dolist (f (directory-files dir t "-test\\.el\\'"))
        (load f nil t)))))

(ert-run-tests-batch-and-exit)
