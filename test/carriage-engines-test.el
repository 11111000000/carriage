;;; carriage-engines-test.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'carriage-apply-engine)

(ert-deftest carriage-engines-hide-bare-git ()
  "Engine list must hide bare 'git' and show only git:POLICY combos."
  (let ((lst (carriage-available-apply-engines)))
    (should (cl-some (lambda (s) (string-match-p "\\=git:\\(in-place\\|wip\\|ephemeral\\)" s)) lst))
    (should-not (cl-some (lambda (s) (string-match-p "\\=git\\s-+—" s)) lst))
    (should-not (member "git" lst))))

(ert-deftest carriage-engine-dispatch-patch-guarded-when-emacs ()
  "Dispatching patch with 'emacs engine should report :status 'skip (friendly)."
  (let* ((carriage-apply-engine 'emacs)
         (result nil))
    (carriage-apply-engine-dispatch
     :dry-run 'patch
     '((:op . patch) (:strip . 1) (:path . "x") (:diff . "--- /dev/null\n+++ b/x\n@@ -0,0 +1 @@\n+ok\n"))
     default-directory
     (lambda (row) (setq result row))
     (lambda (err) (setq result (list :status 'fail :error err))))
    (sleep-for 0.05)
    (should (eq (plist-get result :status) 'skip))
    (should (eq (plist-get result :op) 'patch))
    (should (eq (plist-get result :engine) 'emacs))))
