;;; stage-policy-test.el --- ERT tests for staging policy  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'org)

(defun carriage-test--call (dir &rest args)
  "Run git ARGS in DIR and return plist (:exit :stdout :stderr)."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let* ((out (current-buffer))
             (stderr-file (make-temp-file "carriage-test-stderr-"))
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

(defun carriage-test--with-temp-repo (fn)
  "Create a temporary git repo, call FN with its root."
  (let* ((root (make-temp-file "carriage-repo-" t)))
    (carriage-test--call root "init")
    (carriage-test--call root "config" "user.email" "ci@example.invalid")
    (carriage-test--call root "config" "user.name" "CI")
    ;; seed file and initial commit
    (let ((f (expand-file-name "foo.txt" root)))
      (with-temp-file f (insert "hello\n")))
    (carriage-test--call root "add" "-A")
    (carriage-test--call root "commit" "-m" "init" "--no-gpg-sign" "--no-verify")
    (funcall fn root)))

(defun carriage-test--rev-count (root)
  (string-to-number
   (string-trim (plist-get (carriage-test--call root "rev-list" "--count" "HEAD") :stdout))))

(defun carriage-test--diff-cached-empty-p (root)
  (string-empty-p (string-trim (plist-get (carriage-test--call root "diff" "--cached" "--name-only") :stdout))))

(defun carriage-test--diff-wt-empty-p (root)
  (string-empty-p (string-trim (plist-get (carriage-test--call root "diff" "--name-only") :stdout))))

(defun carriage-test--make-diff (old new)
  "Return unified diff string for foo.txt replacing OLD with NEW."
  (format (concat
           "--- a/foo.txt\n"
           "+++ b/foo.txt\n"
           "@@ -1,1 +1,1 @@\n"
           "-%s\n"
           "+%s\n")
          (string-trim-right old) (string-trim-right new)))

(ert-deftest carriage-stage-policy-none-apply-no-commit ()
  "With policy='none, apply modifies working tree only; commit history unchanged until manual commit."
  (carriage-test--with-temp-repo
   (lambda (root)
     (let* ((diff (carriage-test--make-diff "hello" "hello world"))
            (plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff diff))))
       (let ((carriage-apply-stage-policy 'none))
         ;; dry-run should pass
         (let* ((dr (carriage-dry-run-diff plan-item root)))
           (should (eq (plist-get dr :status) 'ok)))
         (let ((before (carriage-test--rev-count root)))
           (should (= before 1))
           ;; apply
           (let ((ap (carriage-apply-diff plan-item root)))
             (should (eq (plist-get ap :status) 'ok)))
           ;; working tree has changes; index empty; history unchanged
           (should (not (carriage-test--diff-wt-empty-p root)))
           (should (carriage-test--diff-cached-empty-p root))
           (should (= (carriage-test--rev-count root) 1))
           ;; now commit via command
           (with-temp-buffer
             (setq default-directory root)
             (carriage-commit-changes "carriage: test commit"))
           (should (= (carriage-test--rev-count root) 2))
           (should (and (carriage-test--diff-wt-empty-p root)
                        (carriage-test--diff-cached-empty-p root)))))))))

(ert-deftest carriage-stage-policy-index-apply-stages ()
  "With policy='index, apply populates index; commit happens separately."
  (carriage-test--with-temp-repo
   (lambda (root)
     ;; First, make one change and commit to move to rev 2 so we can distinguish later
     (let ((carriage-apply-stage-policy 'none))
       (let* ((diff1 (carriage-test--make-diff "hello" "hello a"))
              (it1 (list (cons :version "1") (cons :op 'patch) (cons :apply 'git-apply) (cons :strip 1)
                         (cons :path "foo.txt") (cons :diff diff1))))
         (carriage-apply-diff it1 root)
         (with-temp-buffer (setq default-directory root)
                           (carriage-commit-changes "seed"))))
     (should (= (carriage-test--rev-count root) 2))
     ;; Now apply with 'index
     (let* ((carriage-apply-stage-policy 'index)
            (diff2 (carriage-test--make-diff "hello a" "hello index"))
            (plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff diff2))))
       (let ((ap (carriage-apply-diff plan-item root)))
         (should (eq (plist-get ap :status) 'ok)))
       ;; Index has changes, working tree should appear clean; history unchanged
       (should (not (carriage-test--diff-cached-empty-p root)))
       (should (= (carriage-test--rev-count root) 2))
       ;; Commit via command (should use index as-is)
       (with-temp-buffer
         (setq default-directory root)
         (let ((carriage-apply-stage-policy 'index))
           (carriage-commit-changes "carriage: index commit")))
       (should (= (carriage-test--rev-count root) 3))
       (should (and (carriage-test--diff-wt-empty-p root)
                    (carriage-test--diff-cached-empty-p root)))))))
