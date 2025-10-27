;;; carriage-ui-visibility-test.el --- UI visibility tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)
(require 'carriage)

(defun carriage--ml-has-segment-p ()
  "Detect if mode-line-format contains our (:eval (carriage-ui--modeline)) segment."
  (seq-some (lambda (e)
              (and (consp e)
                   (eq (car e) :eval)
                   (equal (cadr e) '(carriage-ui--modeline))))
            mode-line-format))

(ert-deftest carriage-ui-installs-in-interactive ()
  "When noninteractive=nil, enabling carriage-mode installs header-line and modeline segments."
  (with-temp-buffer
    (let ((noninteractive nil)
          (carriage-mode-show-header-line t)
          (carriage-mode-show-mode-line-ui t))
      (let ((orig-hl header-line-format))
        (carriage-mode 1)
        (unwind-protect
            (progn
              (should (local-variable-p 'header-line-format))
              (should header-line-format)
              (should (boundp 'carriage--mode-modeline-construct))
              (should carriage--mode-modeline-construct)
              (should (carriage--ml-has-segment-p)))
          (carriage-mode -1)
          (should (eq header-line-format orig-hl))
          (should (not (carriage--ml-has-segment-p))))))))

(ert-deftest carriage-ui-not-installed-in-batch ()
  "In batch (noninteractive=t), UI must not be initialized."
  (with-temp-buffer
    (let ((carriage-mode-show-header-line t)
          (carriage-mode-show-mode-line-ui t))
      (let ((orig-hl header-line-format)
            (orig-ml mode-line-format))
        (carriage-mode 1)
        (unwind-protect
            (progn
              (should (eq header-line-format orig-hl))
              (should (not (carriage--ml-has-segment-p))))
          (carriage-mode -1)
          (should (eq header-line-format orig-hl))
          (should (equal mode-line-format orig-ml)))))))

(provide 'carriage-ui-visibility-test)
;;; carriage-ui-visibility-test.el ends here
