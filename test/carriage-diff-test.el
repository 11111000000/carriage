;;; carriage-diff-test.el --- Unified diff tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-diff-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-diff-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-diff-test--read (dir rel)
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-diff-dry-run-and-apply ()
  "Dry-run git apply --check should pass; then apply the patch and verify content."
  (let* ((dir (make-temp-file "carriage-diff-" t)))
    ;; init repo
    (should (zerop (carriage-diff-test--git dir "init")))
    (should (zerop (carriage-diff-test--git dir "config" "user.email" "tester@example.com")))
    (should (zerop (carriage-diff-test--git dir "config" "user.name" "Tester")))
    ;; initial file and commit
    (carriage-diff-test--write dir "a.txt" "old\n")
    (should (zerop (carriage-diff-test--git dir "add" "--" "a.txt")))
    (should (zerop (carriage-diff-test--git dir "commit" "-m" "init")))
    ;; make a simple unified diff (old -> new)
    (let* ((diff (concat
                  (mapconcat #'identity
                             '("--- a/a.txt"
                               "+++ b/a.txt"
                               "@@ -1,1 +1,1 @@"
                               "-old"
                               "+new")
                             "\n")
                  "\n"))
           (item `(:version "1" :op 'patch :apply 'git-apply :strip 1
                            :path "a.txt" :diff ,diff)))
      ;; dry-run should be ok
      (let ((rep (carriage-dry-run-diff item dir)))
        (should (eq (plist-get rep :status) 'ok)))
      ;; apply should update file content
      (let ((ap (carriage-apply-diff item dir)))
        (should (eq (plist-get ap :status) 'ok))
        (should (string= (carriage-diff-test--read dir "a.txt") "new\n"))))))


;;; carriage-diff-test.el ends here
