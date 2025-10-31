;;; carriage-report-messages-sre-v1-test.el --- Report diagnostics (SRE v1) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-op-sre)

(ert-deftest carriage-report-messages-range-clamp-v1 ()
  "Range clamp warning is reported for out-of-bounds :range."
  (let* ((text "alpha\nbeta\ngamma\n")
         (plan (list (cons :version "1")
                     (cons :op 'sre)
                     (cons :file "z.txt")
                     (cons :pairs
                           (list (list (cons :from "beta")
                                       (cons :to "BETA")
                                       (cons :opts '(:occur first :match literal
                                                            :range (:start-line 10 :end-line 20))))))))
         (rep (carriage-sre-dry-run-on-text plan text)))
    (should (eq (plist-get rep :status) 'ok))
    (let ((msgs (plist-get rep :_messages)))
      (should (listp msgs))
      (should (cl-some (lambda (m) (eq (plist-get m :code) 'SRE_W_RANGE_CLAMP)) msgs)))))
(provide 'carriage-report-messages-sre-v1-test)
;;; carriage-report-messages-sre-v1-test.el ends here
