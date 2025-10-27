;;; carriage-ui-noglobal-effects-test.el --- No global effects UI tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)
(require 'carriage)

(ert-deftest carriage-ui-buffer-local-only ()
  "Enabling carriage-mode in one buffer must not affect another buffer."
  (let ((buf-a (get-buffer-create "*c-ui-a*"))
        (buf-b (get-buffer-create "*c-ui-b*")))
    (unwind-protect
        (let ((noninteractive nil))
          (with-current-buffer buf-b
            (setq-local header-line-format nil))
          (with-current-buffer buf-a
            (let ((orig-b-hl (with-current-buffer buf-b header-line-format)))
              (carriage-mode 1)
              (unwind-protect
                  (progn
                    (should header-line-format)
                    (should (seq-some (lambda (e)
                                        (and (consp e)
                                             (eq (car e) :eval)
                                             (equal (cadr e) '(carriage-ui--modeline))))
                                      mode-line-format))
                    ;; Switch to B and ensure untouched
                    (with-current-buffer buf-b
                      (should (eq header-line-format orig-b-hl))
                      (should (not (seq-some (lambda (e)
                                               (and (consp e)
                                                    (eq (car e) :eval)
                                                    (equal (cadr e) '(carriage-ui--modeline))))
                                             mode-line-format)))))
                (with-current-buffer buf-a (carriage-mode -1)))))))
    (ignore-errors (kill-buffer buf-a))
    (ignore-errors (kill-buffer buf-b))))

(provide 'carriage-ui-noglobal-effects-test)
;;; carriage-ui-noglobal-effects-test.el ends here
