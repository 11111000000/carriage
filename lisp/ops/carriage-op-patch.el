;;; carriage-op-patch.el --- Unified diff patch handlers and prompt fragment  -*- lexical-binding: t; -*-
;;

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

(defun carriage-op-patch-prompt-fragment (_ctx)
  "Return prompt fragment for unified diff (one file)."
  (concat
   "Формат PATCH (unified diff одного файла):\n"
   "#+begin_patch (:version \"1\" :op \"patch\" :apply \"git-apply\" :strip 1)\n"
   "--- a/RELATIVE/PATH\n+++ b/RELATIVE/PATH\n@@ -L1,Len1 +L2,Len2 @@\n-OLD\n+NEW\n"
   "#+end_patch\n"
   "- Ровно один файл на блок, rename/copy запрещены.\n"))

;;;; Internal helpers

(defun carriage--diff-extract-paths (body)
  "Extract --- and +++ paths from BODY. Return (A B)."
  (let ((a nil) (b nil) (count 0))
    (dolist (line (split-string body "\n"))
      (cond
       ((string-match "\\`--- \\(.*\\)\\'" line)
        (setq a (match-string 1 line))
        (setq count (1+ count)))
       ((string-match "\\`\\+\\+\\+ \\(.*\\)\\'" line)
        (setq b (match-string 1 line))
        (setq count (1+ count)))))
    (cond
     ((< count 2)
      (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Missing ---/+++")))
     ((> count 2)
      (signal (carriage-error-symbol 'PATCH_E_MULTI_FILE) (list "Multiple ---/+++ pairs detected"))))
    (list a b)))

(defun carriage--diff-validate-single-file (a b)
  "Validate A and B paths refer to a single file or /dev/null cases."
  (cond
   ((and (string= a "/dev/null")
         (string-match "\\`b/\\(.+\\)\\'" b))
    (cons nil (match-string 1 b)))
   ((and (string-match "\\`a/\\(.+\\)\\'" a)
         (string= b "/dev/null"))
    (cons (match-string 1 a) nil))
   ((and (string-match "\\`a/\\(.+\\)\\'" a)
         (string-match "\\`b/\\(.+\\)\\'" b))
    (let ((ap (match-string 1 a))
          (bp (match-string 1 b)))
      (unless (string= ap bp)
        (signal (carriage-error-symbol 'PATCH_E_PATH_MISMATCH) (list a b)))
      (cons ap bp)))
   (t
    (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Unexpected paths")))))

;;;; Parse

(defun carriage-parse-diff (header body repo-root)
  "Parse unified diff block BODY with HEADER under REPO-ROOT."
  (ignore repo-root)
  (let* ((version (plist-get header :version))
         (op (plist-get header :op))
         (strip (if (plist-member header :strip)
                    (plist-get header :strip)
                  1))
         (apply-key (plist-get header :apply)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'PATCH_E_VERSION) (list version)))
    (unless (string= op "patch")
      (signal (carriage-error-symbol 'PATCH_E_OP) (list op)))
    (when (plist-member header :apply)
      (unless (string= apply-key "git-apply")
        (signal (carriage-error-symbol 'PATCH_E_APPLY)
                (list (format "Unsupported :apply: %S" apply-key)))))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*\\(GIT binary patch\\|Binary files .* differ\\)\\b" nil t)
        (signal (carriage-error-symbol 'PATCH_E_BINARY) (list "Binary diff not supported")))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*\\(rename \\(from\\|to\\)\\|copy \\(from\\|to\\)\\)\\b" nil t)
        (signal (carriage-error-symbol 'PATCH_E_RENAME_COPY) (list "rename/copy not supported"))))
    (let* ((ab (carriage--diff-extract-paths body))
           (a (car ab))
           (b (cadr ab))
           (pair (carriage--diff-validate-single-file a b))
           (rel (or (car pair) (cdr pair))))
      (when (carriage--path-looks-unsafe-p rel)
        (signal (carriage-error-symbol 'PATCH_E_PATH) (list rel)))
      (let ((expected-strip 1))
        (when (and (plist-member header :strip)
                   (not (= strip expected-strip)))
          (signal (carriage-error-symbol 'PATCH_E_STRIP)
                  (list (format "Expected :strip=%d, got %s" expected-strip strip)))))
      (list (cons :version "1")
            (cons :op 'patch)
            (cons :apply 'git-apply)
            (cons :strip strip)
            (cons :path rel)
            (cons :diff body)))))

;;;; Dry-run & Apply

(defun carriage-dry-run-diff (plan-item repo-root)
  "Run git apply --check for unified diff."
  (let* ((diff  (alist-get :diff plan-item))
         (strip (alist-get :strip plan-item))
         (path  (alist-get :path plan-item))
         (res (carriage-git-apply-check repo-root diff :strip strip)))
    (if (and (plist-get res :exit) (zerop (plist-get res :exit)))
        (list :op 'patch :status 'ok :path path :details "git apply --check ok")
      (list :op 'patch :status 'fail :path path :details "git apply --check failed"
            :extra (list :exit (plist-get res :exit)
                         :stderr (plist-get res :stderr)
                         :stdout (plist-get res :stdout))
            :_messages (list (list :code 'PATCH_E_GIT_CHECK
                                   :severity 'error
                                   :file path
                                   :details (or (plist-get res :stderr)
                                                (plist-get res :stdout)
                                                "git apply --check failed")))))))

(defun carriage-apply-diff (plan-item repo-root)
  "Apply unified diff with git apply --index; then git add/commit."
  (let* ((diff  (alist-get :diff plan-item))
         (strip (alist-get :strip plan-item))
         (path  (alist-get :path plan-item))
         (apply-res (carriage-git-apply-index repo-root diff :strip strip)))
    (if (and (plist-get apply-res :exit) (zerop (plist-get apply-res :exit)))
        (progn
          (when path (carriage-git-add repo-root path))
          (carriage-git-commit repo-root (format "carriage: patch %s" (or path "<unknown>")))
          (list :op 'patch :status 'ok :path path :details "git apply --index ok"))
      (list :op 'patch :status 'fail :path path :details "git apply --index failed"
            :extra (list :exit (plist-get apply-res :exit)
                         :stderr (plist-get apply-res :stderr)
                         :stdout (plist-get apply-res :stdout))
            :_messages (list (list :code 'PATCH_E_APPLY
                                   :severity 'error
                                   :file path
                                   :details (or (plist-get apply-res :stderr)
                                                (plist-get apply-res :stdout)
                                                "git apply --index failed")))))))

;;;; Registration

(carriage-format-register 'patch "1"
                          :parse #'carriage-parse-diff
                          :dry-run #'carriage-dry-run-diff
                          :apply #'carriage-apply-diff
                          :prompt-fragment #'carriage-op-patch-prompt-fragment)

(provide 'carriage-op-patch)
;;; carriage-op-patch.el ends here
