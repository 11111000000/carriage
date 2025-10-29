;;; carriage-git.el --- Git integration helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'carriage-utils)
(require 'carriage-logging)
(require 'carriage-errors)

;; Fallback default for WIP branch name (overridden by defcustom in carriage-mode.el if loaded)
(defvar carriage-mode-wip-branch "carriage/WIP"
  "Default WIP branch name for Carriage.")

(defcustom carriage-git-commit-no-gpg-sign t
  "When non-nil, add --no-gpg-sign to git commit to avoid interactive pinentry."
  :type 'boolean :group 'carriage)

(defcustom carriage-git-commit-skip-verify t
  "When non-nil, add --no-verify to git commit to skip hooks (pre-commit/commit-msg)."
  :type 'boolean :group 'carriage)

(defcustom carriage-git-apply-extra-args '("--reject" "--whitespace=nowarn")
  "Extra arguments appended to =git apply' invocations.
Use to avoid interactive or environment-specific stalls and to relax whitespace checks.
Applies to both index and working-tree apply operations."
  :type '(repeat string) :group 'carriage)

(defun carriage-git--run (root &rest args)
  "Run git ARGS in ROOT. Return plist (:exit :stdout :stderr)."
  (apply #'carriage--call-git root args))

(defun carriage-git-ensure-repo (root)
  "Ensure ROOT looks like a git repo and is not TRAMP, signal if not."
  (when (file-remote-p root)
    (signal (carriage-error-symbol 'IO_E_PATH) (list "TRAMP is not supported in v1")))
  (let* ((res (carriage-git--run root "rev-parse" "--git-dir")))
    (unless (eq (plist-get res :exit) 0)
      (signal (carriage-error-symbol 'GIT_E_APPLY) (list "Git repo not detected"))))
  t)







(defun carriage-git-add (root relpath)
  "git add RELPATH in ROOT."
  (carriage-git--run root "add" "--" relpath))

(defun carriage-git-add-all (root)
  "git add -A in ROOT."
  (carriage-git--run root "add" "-A"))

(defun carriage-git-commit (root message &rest files)
  "git commit with MESSAGE in ROOT. When FILES are provided, commit only those paths."
  (let ((args (append '("commit")
                      (when (boundp 'carriage-git-commit-skip-verify)
                        (and carriage-git-commit-skip-verify '("--no-verify")))
                      (when (boundp 'carriage-git-commit-no-gpg-sign)
                        (and carriage-git-commit-no-gpg-sign '("--no-gpg-sign")))
                      (list "-m" message)
                      (when files (append '("--") files)))))
    (apply #'carriage-git--run root args)))

(defun carriage-git-mv (root from to)
  "git mv FROM TO in ROOT."
  (carriage-git--run root "mv" "--" from to))

(defun carriage-git-rm (root relpath)
  "git rm RELPATH in ROOT."
  (carriage-git--run root "rm" "-f" "--" relpath))

(defun carriage-git-checkout-wip (root &optional branch)
  "Create/switch to WIP BRANCH in ROOT. Ensure HEAD is valid on WIP by creating an initial empty commit when unborn."
  (let* ((b (or branch carriage-mode-wip-branch))
         (have (carriage-git--run root "rev-parse" "--verify" b)))
    (if (eq (plist-get have :exit) 0)
        (carriage-git--run root "checkout" b)
      (carriage-git--run root "checkout" "-b" b)))
  ;; Ensure HEAD is not ambiguous on freshly created WIP branch:
  ;; if repository has no commits yet (unborn HEAD), create an initial empty commit.
  (let* ((head (carriage-git--run root "rev-parse" "--verify" "HEAD")))
    (unless (eq (plist-get head :exit) 0)
      (let ((args (append
                   '("commit")
                   (when (and (boundp 'carriage-git-commit-skip-verify)
                              carriage-git-commit-skip-verify)
                     '("--no-verify"))
                   (when (and (boundp 'carriage-git-commit-no-gpg-sign)
                              carriage-git-commit-no-gpg-sign)
                     '("--no-gpg-sign"))
                   '("--allow-empty" "-m" "carriage: init WIP"))))
        (apply #'carriage-git--run root args)))))

(defun carriage-git-reset-soft (root rev)
  "Soft reset to REV in ROOT."
  (carriage-git--run root "reset" "--soft" rev))

(provide 'carriage-git)
;;; carriage-git.el ends here
