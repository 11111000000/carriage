;;; carriage-ui-spinner-test.el --- Spinner tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-ui-spinner-starts-and-stops ()
  "Spinner should start on sending and stop on idle; buffer-local and safe."
  (with-temp-buffer
    (let ((noninteractive nil))
      (carriage-mode 1)
      (unwind-protect
          (progn
            (carriage-ui-set-state 'sending)
            (should (and (boundp 'carriage--ui-spinner-timer)
                         (timerp carriage--ui-spinner-timer)))
            (carriage-ui-set-state 'idle)
            (should (or (null carriage--ui-spinner-timer)
                        (not (timerp carriage--ui-spinner-timer)))))
        (carriage-mode -1)
        (should (or (null carriage--ui-spinner-timer)
                    (not (timerp carriage--ui-spinner-timer))))))))

(ert-deftest carriage-ui-spinner-not-in-batch ()
  "In batch (noninteractive=t), spinner must not start."
  (with-temp-buffer
    (let ((noninteractive t))
      (carriage-mode 1)
      (carriage-ui-set-state 'sending)
      (should (or (null (bound-and-true-p carriage--ui-spinner-timer))
                  (not (timerp carriage--ui-spinner-timer))))
      (carriage-mode -1))))

(provide 'carriage-ui-spinner-test)
;;; carriage-ui-spinner-test.el ends here
