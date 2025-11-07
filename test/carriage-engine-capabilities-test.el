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


(provide 'carriage-engine-capabilities-test)
;;; carriage-engine-capabilities-test.el ends here
