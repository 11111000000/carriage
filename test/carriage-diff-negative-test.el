;;; carriage-diff-negative-test.el --- Negative tests for unified diff parser -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-parser)
(require 'carriage-op-patch)

(defun carriage--make-header-patch ()
  '(:version "1" :op "patch"))

(ert-deftest carriage-parse-diff-multifile-rejected ()
  "Multi-file diff must be rejected (only one file per block allowed)."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("diff --git a/a.txt b/a.txt"
                            "--- a/a.txt"
                            "+++ b/a.txt"
                            "@@ -1 +1 @@"
                            "-old"
                            "+new"
                            "diff --git a/b.txt b/b.txt"
                            "--- a/b.txt"
                            "+++ b/b.txt"
                            "@@ -1 +1 @@"
                            "-1"
                            "+2")
                          "\n"))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    ;; Expect multi-file error due to extra ---/+++ pairs
    (should (eq sym (carriage-error-symbol 'PATCH_E_MULTI_FILE)))))


(ert-deftest carriage-parse-diff-binary-rejected ()
  "Binary diffs are rejected (missing ---/+++ or binary markers)."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("diff --git a/bin.dat b/bin.dat"
                            "Binary files a/bin.dat and b/bin.dat differ")
                          "\n"))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'PATCH_E_BINARY)))))

(ert-deftest carriage-parse-diff-path-mismatch-rejected ()
  "Reject diffs where a/ and b/ paths do not match."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("--- a/x.txt"
                            "+++ b/y.txt"
                            "@@ -1 +1 @@"
                            "-old"
                            "+new")
                          "\n"))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'PATCH_E_PATH_MISMATCH)))))

(ert-deftest carriage-parse-diff-unsafe-path-rejected ()
  "Reject diffs with unsafe relative paths (..)."
  (let* ((hdr (carriage--make-header-patch))
         (body (mapconcat #'identity
                          '("--- a/../../passwd"
                            "+++ b/../../passwd"
                            "@@ -1 +1 @@"
                            "-old"
                            "+new")
                          "\n"))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-parse-diff hdr body default-directory)
          (setq raised nil))
      (error (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'PATCH_E_PATH)))))


;;; carriage-diff-negative-test.el ends here
