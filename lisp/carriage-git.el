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

;; Async helpers for non-blocking preflight (see spec/async-workflow-v1.org)

(defun carriage-git--run-async (root argv on-done on-fail)
  "Run git ARGV in ROOT asynchronously and return TOKEN plist.
TOKEN keys: :engine 'git, :process, :pid, :timer, :stdout-buf, :stderr-buf, :argv."
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (out (generate-new-buffer " *carriage-git-stdout*"))
         (err (generate-new-buffer " *carriage-git-stderr*"))
         (cmd (cons "git" argv))
         (timeout (or (and (boundp 'carriage-mode-git-timeout-seconds)
                           carriage-mode-git-timeout-seconds)
                      15))
         (token (list :engine 'git :argv argv :stdout-buf out :stderr-buf err))
         (done-called nil))
    (carriage-log "git: exec dir=%s cmd=%s" default-directory
                  (mapconcat #'identity cmd " "))
    (let ((proc
           (make-process
            :name "carriage-git-async"
            :buffer out
            :stderr err
            :command cmd
            :noquery t
            :sentinel
            (lambda (p _e)
              (when (memq (process-status p) '(exit signal))
                (let* ((exit (if (eq (process-status p) 'exit)
                                 (process-exit-status p) 128))
                       (stdout (with-current-buffer out (buffer-string)))
                       (stderr (with-current-buffer err (buffer-string)))
                       (res (list :engine 'git :exit exit
                                  :stdout stdout :stderr stderr
                                  :pid (plist-get token :pid)
                                  :argv argv
                                  :reason (cond
                                           ((plist-get token :aborted) 'aborted)
                                           ((plist-get token :timed-out) 'timeout)
                                           (t nil)))))
                  (unless done-called
                    (setq done-called t)
                    (carriage-log "git: exit=%s stdout-bytes=%d stderr-bytes=%d"
                                  exit (string-bytes stdout) (string-bytes stderr))
                    (when (timerp (plist-get token :timer))
                      (ignore-errors (cancel-timer (plist-get token :timer))))
                    (when (buffer-live-p out) (kill-buffer out))
                    (when (buffer-live-p err) (kill-buffer err))
                    (if (zerop exit)
                        (funcall on-done res)
                      (funcall on-fail res)))))))))
      (plist-put token :process proc)
      (plist-put token :pid (process-id proc))
      (carriage-log "git: pid=%s spawned" (plist-get token :pid))
      ;; Timeout
      (let ((tm (run-at-time
                 timeout nil
                 (lambda ()
                   (when (process-live-p proc)
                     (plist-put token :timed-out t)
                     (carriage-log "git: timeout after %ss; kill pid=%s" timeout (plist-get token :pid))
                     (ignore-errors (interrupt-process proc))
                     (ignore-errors (kill-process proc)))))))
        (plist-put token :timer tm))
      token)))

(defun carriage-git-ensure-repo-async (root on-done on-fail)
  "Async check that ROOT is a git repo."
  (when (file-remote-p root)
    (funcall on-fail (list :exit 128 :stderr "TRAMP is not supported in v1"))
    (cl-return-from carriage-git-ensure-repo-async nil))
  (carriage-git--run-async
   root '("rev-parse" "--git-dir")
   (lambda (res) (funcall on-done res))
   (lambda (res) (funcall on-fail res))))

(defun carriage-git-checkout-wip-async (root &optional branch on-done on-fail)
  "Async ensure WIP BRANCH exists and is checked out; unborn HEAD → empty commit."
  (let* ((b (or branch carriage-mode-wip-branch "carriage/WIP")))
    ;; 1) verify/checkout existing or create new branch
    (carriage-git--run-async
     root (list "rev-parse" "--verify" b)
     (lambda (_ok)
       ;; exists → checkout
       (carriage-git--run-async
        root (list "checkout" b)
        (lambda (_)
          ;; ensure HEAD exists
          (carriage-git--run-async
           root '("rev-parse" "--verify" "HEAD")
           (lambda (r2) (funcall (or on-done #'ignore) r2))
           (lambda (_r2)
             (carriage-git--run-async
              root (append '("commit")
                           (and (boundp 'carriage-git-commit-skip-verify)
                                carriage-git-commit-skip-verify
                                '("--no-verify"))
                           (and (boundp 'carriage-git-commit-no-gpg-sign)
                                carriage-git-commit-no-gpg-sign
                                '("--no-gpg-sign"))
                           '("--allow-empty" "-m" "carriage: init WIP"))
              (lambda (r3) (funcall (or on-done #'ignore) r3))
              (or on-fail #'ignore)))))
        (or on-fail #'ignore)))
     (lambda (_fail)
       ;; not exists → checkout -b
       (carriage-git--run-async
        root (list "checkout" "-b" b)
        (lambda (_)
          (carriage-git--run-async
           root '("rev-parse" "--verify" "HEAD")
           (lambda (r2) (funcall (or on-done #'ignore) r2))
           (lambda (_r2)
             (carriage-git--run-async
              root (append '("commit")
                           (and (boundp 'carriage-git-commit-skip-verify)
                                carriage-git-commit-skip-verify
                                '("--no-verify"))
                           (and (boundp 'carriage-git-commit-no-gpg-sign)
                                carriage-git-commit-no-gpg-sign
                                '("--no-gpg-sign"))
                           '("--allow-empty" "-m" "carriage: init WIP"))
              (lambda (r3) (funcall (or on-done #'ignore) r3))
              (or on-fail #'ignore)))))
        (or on-fail #'ignore))))))


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
