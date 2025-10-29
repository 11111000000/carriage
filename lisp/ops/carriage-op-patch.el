;;; carriage-op-patch.el --- Unified diff (patch) handlers v1  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

(defun carriage--patch--reject-rename-copy (text)
  "Signal when TEXT contains rename/copy prelude forbidden in v1."
  (when (or (string-match-p "^rename from " text)
            (string-match-p "^rename to " text)
            (string-match-p "^copy from " text)
            (string-match-p "^copy to " text))
    (signal (carriage-error-symbol 'PATCH_E_RENAME_COPY)
            (list "rename/copy prelude not supported in v1"))))

(defun carriage--patch--reject-binary (text)
  "Signal when TEXT contains binary patch markers forbidden in v1."
  (when (or (string-match-p "^GIT[ \t]+binary[ \t]+patch" text)
            (string-match-p "^Binary files .+ differ" text))
    (signal (carriage-error-symbol 'PATCH_E_BINARY)
            (list "binary patches are not supported in v1"))))

(defun carriage--patch--extract-paths (text)
  "Return plist (:old OLD :new NEW) from TEXT using ---/+++ lines.
OLD/NEW are raw header payloads after \"--- \" and \"+++ \"."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let* ((old nil) (new nil))
      (when (re-search-forward "^---[ \t]+\\(.+\\)$" nil t)
        (setq old (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^\\+\\+\\+[ \t]+\\(.+\\)$" nil t)
        (setq new (string-trim (match-string 1))))
      (list :old old :new new))))

(defun carriage--patch--single-file-p (text)
  "Return non-nil if TEXT contains at most one pair of ---/+++ headers."
  (let ((count 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^---[ \t]+" nil t)
        (setq count (1+ count))))
    (<= count 1)))

(defun carriage--patch--normalize-path (root old new)
  "From OLD/NEW header payloads compute RELPATH for single-file patch.
Supports (/dev/null,b/path) for create and (a/path,/dev/null) for delete.
Returns RELPATH string. Signals PATCH_E_PATH_MISMATCH when a/b paths differ."
  (cond
   ;; Create
   ((and (stringp old) (stringp new)
         (string-prefix-p "/dev/null" old)
         (string-prefix-p "b/" new))
    (let* ((rel (substring new 2)))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root rel)
      rel))
   ;; Delete
   ((and (stringp old) (stringp new)
         (string-prefix-p "a/" old)
         (string-prefix-p "/dev/null" new))
    (let* ((rel (substring old 2)))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root rel)
      rel))
   ;; Modify existing
   ((and (stringp old) (stringp new)
         (string-prefix-p "a/" old)
         (string-prefix-p "b/" new))
    (let* ((r1 (substring old 2))
           (r2 (substring new 2)))
      (unless (string= r1 r2)
        (signal (carriage-error-symbol 'PATCH_E_PATH_MISMATCH)
                (list (format "Paths differ: a/%s vs b/%s" r1 r2))))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root r1)
      r1))
   (t
    (signal (carriage-error-symbol 'PATCH_E_MULTI_FILE)
            (list (format "Unsupported ---/+++ pair: %S / %S" old new))))))

(defun carriage-parse-diff (header body repo-root)
  "Parse unified diff (v1).
Ignores header :apply key entirely; determines :path from ---/+++ lines.
Returns a plan item alist: (:version \"1\" :op 'patch :strip N :path REL :diff BODY)."
  (let* ((version (plist-get header :version))
         (op (plist-get header :op))
         ;; Ignore any :apply key in header (v1 behavior).
         (_ignored-apply (and (plist-member header :apply) (plist-get header :apply)))
         ;; :strip defaults to 1; coerce to 1 when unspecified or invalid.
         (strip (let* ((v (plist-get header :strip)))
                  (if (and (integerp v) (>= v 0)) v 1))))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'PATCH_E_VERSION) (list version)))
    (unless (member (format "%s" op) '("patch" :patch patch))
      (signal (carriage-error-symbol 'PATCH_E_OP) (list op)))
    ;; Hard limits and forbidden preludes
    (when (> (string-bytes body) (* 4 1024 1024))
      (signal (carriage-error-symbol 'PATCH_E_LIMITS) (list "Patch body exceeds 4MiB")))
    (carriage--patch--reject-rename-copy body)
    (carriage--patch--reject-binary body)
    ;; Ensure single-file diff (one ---/+++ pair)
    (unless (carriage--patch--single-file-p body)
      (signal (carriage-error-symbol 'PATCH_E_MULTI_FILE) (list "Multiple files in one patch")))
    (let* ((paths (carriage--patch--extract-paths body))
           (old (plist-get paths :old))
           (new (plist-get paths :new)))
      (unless (and old new)
        (signal (carriage-error-symbol 'PATCH_E_HEADER) (list "Missing ---/+++ headers")))
      ;; Validate :strip against conventional a/ b/ prefixes
      (when (and (string-prefix-p "a/" (or old "")) (string-prefix-p "b/" (or new ""))
                 (not (= strip 1)))
        (signal (carriage-error-symbol 'PATCH_E_STRIP)
                (list (format "strip mismatch for a/b prefixes: %s" strip))))
      (let* ((rel
              (condition-case e
                  (carriage--patch--normalize-path repo-root old new)
                (error
                 (let ((sym (car e)))
                   (cond
                    ;; Map path-related errors to PATCH_E_PATH
                    ((memq sym (list (carriage-error-symbol 'OPS_E_PATH)
                                     (carriage-error-symbol 'IO_E_PATH)))
                     (signal (carriage-error-symbol 'PATCH_E_PATH) (cdr e)))
                    (t (signal sym (cdr e)))))))))
        (list (cons :version "1")
              (cons :op 'patch)
              (cons :strip strip)
              (cons :path rel)
              (cons :diff body))))))

(defun carriage--plan-kv (item key)
  "Return KEY from ITEM that may be a plist or an alist."
  (if (plist-member item key) (plist-get item key) (alist-get key item)))

(defun carriage-dry-run-diff (plan-item repo-root)
  "Run git apply --check for PLAN-ITEM and build a report row."
  (let* ((strip (or (carriage--plan-kv plan-item :strip) 1))
         (path  (or (carriage--plan-kv plan-item :path) "-"))
         (diff  (or (carriage--plan-kv plan-item :diff) ""))
         (res   (carriage-git-apply-check repo-root diff :strip strip))
         (exit  (plist-get res :exit))
         (stderr (string-trim (or (plist-get res :stderr) "")))
         (stdout (string-trim (or (plist-get res :stdout) ""))))
    (if (and (numberp exit) (zerop exit))
        (list :op 'patch :status 'ok :path path :details "git apply --check ok")
      (list :op 'patch :status 'fail :path path
            :details (if (string-empty-p stderr) "git apply --check failed" stderr)
            :_messages (list (list :code 'PATCH_E_GIT_CHECK
                                   :severity 'error
                                   :file path
                                   :details (or (and (not (string-empty-p stderr)) stderr)
                                                (and (not (string-empty-p stdout)) stdout)
                                                "git apply --check failed")))))))

(defun carriage-apply-diff (plan-item repo-root)
  "Apply patch synchronously (v1 sync wrapper).
Delegates to git apply or git apply --index depending on staging policy."
  (let* ((strip (or (carriage--plan-kv plan-item :strip) 1))
         (path  (or (carriage--plan-kv plan-item :path) "-"))
         (diff  (or (carriage--plan-kv plan-item :diff) ""))
         (use-index (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index))
         (res (if use-index
                  (carriage-git-apply-index repo-root diff :strip strip)
                (carriage-git-apply repo-root diff :strip strip)))
         (exit  (plist-get res :exit))
         (stderr (string-trim (or (plist-get res :stderr) "")))
         (stdout (string-trim (or (plist-get res :stdout) ""))))
    (if (and (numberp exit) (zerop exit))
        (list :op 'patch :status 'ok :path path :details (if use-index "Applied (indexed)" "Applied"))
      (list :op 'patch :status 'fail :path path
            :details (if (string-empty-p stderr) "git apply failed" stderr)
            :extra (list :exit exit :stderr stderr :stdout stdout)))))

;;; Prompt fragment
(defun carriage-op-patch-prompt-fragment (_ctx)
  "Prompt fragment for :op patch (unified diff, single file)."
  (concat
   "PATCH (unified diff, один файл):\n"
   "#+begin_patch (:version \"1\" :op \"patch\" :strip 1)\n"
   "--- a/RELATIVE/PATH\n"
   "+++ b/RELATIVE/PATH\n"
   "@@ -N,M +N,M @@\n"
   "-old\n"
   "+new\n"
   "#+end_patch\n"
   "- Требования: один файл (ровно одна пара ---/+++); пути a/ и b/ совпадают; :strip=1 для a/b.\n"
   "- Запрещено: binary patches, rename/copy прелюдии, многофайловые диффы.\n"))

;;; Registration
(carriage-format-register 'patch "1"
                          :parse   #'carriage-parse-diff
                          :dry-run #'carriage-dry-run-diff
                          :apply   #'carriage-apply-diff
                          :prompt-fragment #'carriage-op-patch-prompt-fragment)

(provide 'carriage-op-patch)
;;; carriage-op-patch.el ends here
