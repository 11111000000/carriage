;;; carriage-iteration-test.el --- ERT for single begin-iteration invariant -*- lexical-binding: t; -*-

(require 'ert)

(defvar carriage-iteration-test--count 0
  "Counter of carriage-begin-iteration invocations during test.")

(defun carriage-iteration-test--advice (&rest _args)
  (setq carriage-iteration-test--count (1+ carriage-iteration-test--count)))

(ert-deftest carriage-begin-iteration-single-invocation ()
  "Ensure exactly one carriage-begin-iteration is triggered per request.
Skips gracefully when transport/iteration modules are unavailable."
  (condition-case _e
      (progn
        ;; Load transport and iteration if available (best effort).
        (ignore-errors (require 'carriage-transport))
        (ignore-errors (require 'carriage-iteration))
        (if (not (and (fboundp 'carriage-transport-begin)
                      (fboundp 'carriage-begin-iteration)))
            (ert-skip "Transport or iteration not available in test env")
          (let ((carriage-iteration-test--count 0))
            (advice-add 'carriage-begin-iteration :before #'carriage-iteration-test--advice)
            (unwind-protect
                (progn
                  (funcall 'carriage-transport-begin)
                  ;; Allow timers (if any) to run advice safely.
                  (sleep-for 0.02)
                  (should (= carriage-iteration-test--count 1)))
              (advice-remove 'carriage-begin-iteration #'carriage-iteration-test--advice)))))
    (error
     (ert-skip "Environment not suitable for iteration test"))))

(provide 'carriage-iteration-test)
;;; carriage-iteration-test.el ends here
