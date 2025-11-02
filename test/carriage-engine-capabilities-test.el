;;; carriage-engine-capabilities-test.el --- Engine capabilities tests -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-engine-git-capabilities-include-aibo ()
  "Git engine capabilities should include 'aibo among supported ops."
  (require 'carriage-apply-engine)
  (require 'carriage-engine-git)
  (let* ((caps (carriage-engine-git-capabilities nil))
         (ops (plist-get caps :ops)))
    (should (listp ops))
    (dolist (op '(patch create delete rename sre aibo))
      (should (member op ops)))))

;; Sidecar engine is optional; if enabled, it must also declare 'aibo.
(ert-deftest carriage-engine-sidecar-capabilities-include-aibo ()
  "Sidecar engine capabilities (when enabled) should include 'aibo."
  (let ((carriage-engine-sidecar-enabled t))
    (require 'carriage-engine-sidecar)
    (let* ((caps (carriage-engine-sidecar-capabilities nil))
           (ops (plist-get caps :ops)))
      (should (listp ops))
      (should (member 'aibo ops)))))

(provide 'carriage-engine-capabilities-test)
;;; carriage-engine-capabilities-test.el ends here
