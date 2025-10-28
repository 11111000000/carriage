;;; carriage-diff-rename-copy-test.el --- Negative tests for rename/copy in diff -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-parser)
(require 'carriage-op-patch)

(defun carriage--make-header-patch ()
  '(:version "1" :op "patch"))

(ert-deftest carriage-parse-diff-rename-rejected ()
  "Diffs containing rename prefaces must be rejected in v1."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("diff --git a/x.txt b/y.txt"
                            "rename from a/x.txt"
                            "rename to b/y.txt"
                            "--- a/x.txt"
                            "+++ b/y.txt"
                            "@@ -1 +1 @@"
                            "-old"
                            "+new")
                          "\n"))
         (raised nil)
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'PATCH_E_RENAME_COPY)))))

(ert-deftest carriage-parse-diff-copy-rejected ()
  "Diffs containing copy prefaces must be rejected in v1."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("diff --git a/x.txt b/x.txt"
                            "copy from a/x.txt"
                            "copy to b/x.txt"
                            "--- a/x.txt"
                            "+++ b/x.txt"
                            "@@ -1 +1 @@"
                            "-old"
                            "+new")
                          "\n"))
         (raised nil)
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'PATCH_E_RENAME_COPY)))))

;;; carriage-diff-rename-copy-test.el ends here
