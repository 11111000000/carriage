;;; carriage-announce-success-test.el --- Messages announcement tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage)
(require 'carriage-report)

(ert-deftest carriage-announce-messages-on-success ()
  "carriage-report-open should announce summary in Messages when all items succeeded."
  (let ((msgs '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) msgs)
                 nil)))
      (let ((rep '(:summary (:ok 2 :fail 0)
                            :items ((:op 'create :file "a.txt" :status 'ok)
                                    (:op 'sre    :file "b.txt" :status 'ok)))))
        (carriage-report-open rep)
        (should (cl-some (lambda (s) (string-match-p "Carriage: applied OK" s)) msgs))
        (should (cl-some (lambda (s) (string-match-p "\\ba\\.txt\\b" s)) msgs))
        (should (cl-some (lambda (s) (string-match-p "\\bb\\.txt\\b" s)) msgs))))))

(provide 'carriage-announce-success-test)
;;; carriage-announce-success-test.el ends here
