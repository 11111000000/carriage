;;; carriage-git-wip-test.el --- WIP branch smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'subr-x)

(defun carriage-git-wip-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-git-wip-test--git-out (dir &rest args)
  "Run git ARGS in DIR and return trimmed stdout."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(defun carriage-git-wip-test--write (dir rel content)
  "Write CONTENT to DIR/REL, creating directories as needed."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-wip-checkout-creates-and-switches ()
  "carriage-wip-checkout should create and switch to carriage/WIP."
  (let* ((dir (make-temp-file "carriage-wip-" t)))
    (unwind-protect
        (progn
          (should (zerop (carriage-git-wip-test--git dir "init")))
          (should (zerop (carriage-git-wip-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-git-wip-test--git dir "config" "user.name" "Tester")))
          ;; Create initial commit to keep repo sane
          (carriage-git-wip-test--write dir "README.md" "# tmp\n")
          (should (zerop (carriage-git-wip-test--git dir "add" "README.md")))
          (should (zerop (carriage-git-wip-test--git dir "commit" "-m" "init")))
          ;; Switch to WIP via command
          (let ((default-directory (file-name-as-directory dir)))
            (carriage-wip-checkout))
          (let ((branch (carriage-git-wip-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
            (should (string= branch "carriage/WIP"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-wip-reset-soft-moves-head-back ()
  "carriage-wip-reset-soft should soft reset to HEAD~1 on carriage/WIP."
  (let* ((dir (make-temp-file "carriage-wip-soft-" t)))
    (unwind-protect
        (progn
          (should (zerop (carriage-git-wip-test--git dir "init")))
          (should (zerop (carriage-git-wip-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-git-wip-test--git dir "config" "user.name" "Tester")))
          ;; Initial commit on main
          (carriage-git-wip-test--write dir "a.txt" "1\n")
          (should (zerop (carriage-git-wip-test--git dir "add" "a.txt")))
          (should (zerop (carriage-git-wip-test--git dir "commit" "-m" "add a.txt (1)")))
          ;; Checkout WIP
          (let ((default-directory (file-name-as-directory dir)))
            (carriage-wip-checkout))
          (let ((branch (carriage-git-wip-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
            (should (string= branch "carriage/WIP")))
          ;; Make another commit on WIP
          (carriage-git-wip-test--write dir "a.txt" "2\n")
          (should (zerop (carriage-git-wip-test--git dir "add" "a.txt")))
          (should (zerop (carriage-git-wip-test--git dir "commit" "-m" "update a.txt (2)")))
          (let* ((head (carriage-git-wip-test--git-out dir "rev-parse" "HEAD"))
                 (parent (carriage-git-wip-test--git-out dir "rev-parse" "HEAD~1")))
            (should (not (string= head parent)))
            ;; Soft reset via command
            (let ((default-directory (file-name-as-directory dir)))
              (carriage-wip-reset-soft))
            ;; HEAD should now equal previous commit
            (let ((head2 (carriage-git-wip-test--git-out dir "rev-parse" "HEAD")))
              (should (string= head2 parent)))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-git-wip-test.el ends here
