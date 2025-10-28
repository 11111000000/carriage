;;; carriage-diff-strip-test.el --- :strip consistency tests for unified diff -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-parser)
(require 'carriage-op-patch)

(defun carriage--make-header-patch (&rest kv)
  (apply #'list :version "1" :op "patch" kv))

(ert-deftest carriage-parse-diff-strip-mismatch-rejected ()
  "If diff uses a/.. and b/.. prefixes, :strip must be 1; other values rejected."
  (let* ((hdr (carriage--make-header-patch :strip 0)) ;; explicitly wrong for a/b
         (body (mapconcat #'identity
                          '("--- a/x.txt"
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
    (should (eq sym (carriage-error-symbol 'PATCH_E_STRIP)))))

;;; Optionally, if supporting :strip 0 with plain relative paths in future,
;;; add a positive test here. Current v1 keeps a/b only, hence we skip it.

;;; carriage-diff-strip-test.el ends here
