;;; carriage-git.el --- Git integration helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'carriage-utils)
(require 'carriage-logging)
(require 'carriage-errors)

(defun carriage-git--run (root &rest args)
  "Run git ARGS in ROOT. Return plist (:exit :stdout :stderr)."
  (apply #'carriage--call-git root args))

(defun carriage-git-ensure-repo (root)
  "Ensure ROOT looks like a git repo, signal if not."
  (let ((res (carriage-git--run root "rev-parse" "--git-dir")))
    (unless (eq (plist-get res :exit) 0)
      (signal (carriage-error-symbol 'GIT_E_APPLY) (list "Git repo not detected"))))
  t)

(defun carriage-git-apply-check (root diff-str &key strip)
  "Run git apply --check for DIFF-STR in ROOT. Return plist result."
  (carriage-git-ensure-repo root)
  (let* ((patch-file (make-temp-file "carriage-patch-" nil ".diff"))
         (default-directory (file-name-as-directory (expand-file-name root))))
    (unwind-protect
        (progn
          (with-temp-file patch-file (insert diff-str))
          (carriage-git--run root "apply" "--check" "--verbose" "-p" (number-to-string (or strip 1)) patch-file))
      (ignore-errors (delete-file patch-file)))))

(defun carriage-git-apply-index (root diff-str &key strip)
  "Apply DIFF-STR with git apply --index in ROOT."
  (carriage-git-ensure-repo root)
  (let* ((patch-file (make-temp-file "carriage-patch-" nil ".diff"))
         (default-directory (file-name-as-directory (expand-file-name root))))
    (unwind-protect
        (progn
          (with-temp-file patch-file (insert diff-str))
          (carriage-git--run root "apply" "--index" "-p" (number-to-string (or strip 1)) patch-file))
      (ignore-errors (delete-file patch-file)))))

(defun carriage-git-add (root relpath)
  "git add RELPATH in ROOT."
  (carriage-git--run root "add" "--" relpath))

(defun carriage-git-commit (root message)
  "git commit with MESSAGE in ROOT."
  (carriage-git--run root "commit" "-m" message))

(defun carriage-git-mv (root from to)
  "git mv FROM TO in ROOT."
  (carriage-git--run root "mv" "--" from to))

(defun carriage-git-rm (root relpath)
  "git rm RELPATH in ROOT."
  (carriage-git--run root "rm" "-f" "--" relpath))

(defun carriage-git-checkout-wip (root &optional branch)
  "Create/switch to WIP BRANCH in ROOT."
  (let* ((b (or branch carriage-mode-wip-branch))
         (have (carriage-git--run root "rev-parse" "--verify" b)))
    (if (eq (plist-get have :exit) 0)
        (carriage-git--run root "checkout" b)
      (carriage-git--run root "checkout" "-b" b))))

(defun carriage-git-reset-soft (root rev)
  "Soft reset to REV in ROOT."
  (carriage-git--run root "reset" "--soft" rev))

(provide 'carriage-git)
;;; carriage-git.el ends here
