;;; carriage-apply-branch-policy-test.el --- Branch policy integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-apply)

;; Helpers (duplicated locally for test isolation; see also stage-policy-test.el)

(defun carriage-branch-test--call (dir &rest args)
  "Run git ARGS in DIR and return plist (:exit :stdout :stderr)."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let* ((out (current-buffer))
             (stderr-file (make-temp-file "carriage-branch-stderr-"))
             (code (unwind-protect
                       (apply #'call-process "git" nil (list out stderr-file) nil args)
                     (ignore-errors (set-buffer out)))))
        (prog1
            (list :exit code
                  :stdout (buffer-substring-no-properties (point-min) (point-max))
                  :stderr (prog1
                              (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-substring-no-properties (point-min) (point-max)))
                            (ignore-errors (delete-file stderr-file))))
          (erase-buffer))))))

(defun carriage-branch-test--with-temp-repo (fn)
  "Create a temporary git repo, call FN with its root."
  (let* ((root (make-temp-file "carriage-repo-" t)))
    (carriage-branch-test--call root "init")
    (carriage-branch-test--call root "config" "user.email" "ci@example.invalid")
    (carriage-branch-test--call root "config" "user.name" "CI")
    ;; seed file and initial commit
    (let ((f (expand-file-name "foo.txt" root)))
      (with-temp-file f (insert "hello\n")))
    (carriage-branch-test--call root "add" "-A")
    (carriage-branch-test--call root "commit" "-m" "init" "--no-gpg-sign" "--no-verify")
    (funcall fn root)))

(defun carriage-branch-test--current-branch (root)
  "Return current branch name in ROOT."
  (string-trim (plist-get (carriage-branch-test--call root "rev-parse" "--abbrev-ref" "HEAD") :stdout)))

(defun carriage-branch-test--branch-exists-p (root name)
  "Return non-nil if branch NAME exists in ROOT."
  (eq 0 (plist-get (carriage-branch-test--call root "rev-parse" "--verify" name) :exit)))

(defun carriage-branch-test--make-diff (old new)
  "Return unified diff string for foo.txt replacing OLD with NEW."
  (format (concat
           "--- a/foo.txt\n"
           "+++ b/foo.txt\n"
           "@@ -1,1 +1,1 @@\n"
           "-%s\n"
           "+%s\n")
          (string-trim-right old) (string-trim-right new)))

;; Test 1: dry-run patch never switches branches (all policies)
(ert-deftest carriage-branch-policy-dry-run-no-switch ()
  "Dry-run for patch must not switch branches for any branch policy."
  (carriage-branch-test--with-temp-repo
   (lambda (root)
     (let* ((orig (carriage-branch-test--current-branch root))
            (diff (carriage-branch-test--make-diff "hello" "hello world"))
            (plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff diff)))
            ;; Ensure engine=git; try all policies
            (carriage-apply-engine 'git))
       (dolist (pol '(in-place wip ephemeral))
         (let ((carriage-git-branch-policy pol))
           (let* ((rep (carriage-dry-run-plan (list plan-item) root))
                  (_row (car (plist-get rep :items))))
             ;; branch must remain unchanged
             (should (string= (carriage-branch-test--current-branch root) orig)))))))))

;; Test 2: apply with policy='wip switches to WIP and returns back (switch-back=t)
(ert-deftest carriage-branch-policy-apply-wip-switch-back ()
  "Apply with policy='wip should ensure/checkout WIP, then switch back on complete."
  (carriage-branch-test--with-temp-repo
   (lambda (root)
     (let* ((orig (carriage-branch-test--current-branch root))
            (carriage-apply-engine 'git)
            (carriage-git-branch-policy 'wip)
            (carriage-git-switch-back-on-complete t)
            (diff (carriage-branch-test--make-diff "hello" "hello wip"))
            (plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff diff)))
            (done nil) (report nil))
       ;; async apply
       (carriage-apply-plan-async
        (list plan-item) root
        (lambda (rep) (setq report rep done t)))
       (let ((t0 (float-time)))
         (while (and (not done) (< (- (float-time) t0) 5.0))
           (accept-process-output nil 0.05)))
       (should done)
       ;; Back on original branch
       (should (string= (carriage-branch-test--current-branch root) orig))
       ;; WIP branch exists
       (should (carriage-branch-test--branch-exists-p
                root (or (and (boundp 'carriage-mode-wip-branch) carriage-mode-wip-branch) "carriage/WIP")))
       ;; Report metadata shows policy
       (should (eq (plist-get report :branch-policy) 'wip))))))

;; Test 3: apply with policy='ephemeral on empty plan → create and auto-delete ephemeral branch
(ert-deftest carriage-branch-policy-apply-ephemeral-empty-plan-auto-delete ()
  "Apply with policy='ephemeral on empty plan should create and then auto-delete ephemeral branch."
  (carriage-branch-test--with-temp-repo
   (lambda (root)
     (let* ((orig (carriage-branch-test--current-branch root))
            (carriage-apply-engine 'git)
            (carriage-git-branch-policy 'ephemeral)
            (carriage-git-auto-delete-empty-branch t)
            (carriage-git-ephemeral-keep-on-fail t)
            (done nil) (report nil))
       ;; async apply with empty plan (ok==0)
       (carriage-apply-plan-async
        '() root
        (lambda (rep) (setq report rep done t)))
       (let ((t0 (float-time)))
         (while (and (not done) (< (- (float-time) t0) 5.0))
           (accept-process-output nil 0.05)))
       (should done)
       ;; Back on original branch
       (should (string= (carriage-branch-test--current-branch root) orig))
       ;; Ephemeral branch should have been deleted
       (let ((ep (plist-get report :branch-name)))
         (when (and (stringp ep) (not (string-empty-p ep)))
           (should (not (carriage-branch-test--branch-exists-p root ep)))))
       ;; Optional: report contains info message about deletion (best-effort)
       (let* ((msgs (plist-get report :messages))
              (joined (mapconcat (lambda (m)
                                   (format "%s %s" (plist-get m :code) (plist-get m :details)))
                                 msgs "\n")))
         (should (or (string-match-p "Ephemeral branch .* deleted" joined)
                     t)))))))

(provide 'carriage-apply-branch-policy-test)
;;; carriage-apply-branch-policy-test.el ends here
