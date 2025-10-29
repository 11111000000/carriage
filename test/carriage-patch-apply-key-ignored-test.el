;;; carriage-patch-apply-key-ignored-test.el --- Patch parser ignores :apply -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-format-registry)
(require 'carriage-op-patch)

(defun carriage--mk-tmp-dir ()
  (make-temp-file "carriage-parse-diff-" t))

(ert-deftest carriage-patch-parse-ignores-apply-header ()
  "Parser should ignore :apply key in header and produce a valid plan item."
  (let* ((root (carriage--mk-tmp-dir))
         (hdr '(:version "1" :op "patch" :apply "git-apply" :strip 1))
         ;; Create-file diff (/dev/null -> b/new.txt)
         (diff (concat
                "--- /dev/null\n"
                "+++ b/new.txt\n"
                "@@ -0,0 +1,2 @@\n"
                "+hello\n"
                "+world\n"))
         (item (carriage-parse-diff hdr diff root)))
    (unwind-protect
        (progn
          (should (alist-get :op item))
          (should (eq (alist-get :op item) 'patch))
          (should (string= (alist-get :path item) "new.txt"))
          (should (stringp (alist-get :diff item)))
          ;; ensure no :apply leaks into plan item
          (should (null (alist-get :apply item))))
      (ignore-errors (delete-directory root t)))))
