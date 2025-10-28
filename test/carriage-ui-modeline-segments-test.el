;;; carriage-ui-modeline-segments-test.el --- Mode-line segments presence tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-mode)
(require 'carriage-ui)

(defun carriage--modeline-string ()
  "Build and return the raw string from carriage-ui--modeline for assertions."
  (let ((s (carriage-ui--modeline)))
    (if (stringp s) s (format "%s" s))))

(ert-deftest carriage-ui-modeline-has-core-segments ()
  "Modeline should include core action buttons and toggles."
  (with-temp-buffer
    (let ((noninteractive nil))
      (carriage-mode 1)
      (unwind-protect
          (let ((ml (carriage--modeline-string)))
            ;; Core buttons
            (should (string-match-p "\\[Ask\\]\\|\\[Patch\\]" ml))
            (should (string-match-p "\\[idle\\]\\|\\[sending\\]\\|\\[streaming\\]\\|\\[dry-run\\]\\|\\[apply\\]\\|\\[error\\]" ml))
            (should (string-match-p "\\[Dry\\]" ml))
            (should (string-match-p "\\[Apply\\]" ml))
            (should (string-match-p "\\[All\\]" ml))
            (should (string-match-p "\\[Abort\\]" ml))
            (should (string-match-p "\\[Report\\]" ml))
            (should (string-match-p "\\[Diff\\]" ml))
            (should (string-match-p "\\[Ediff\\]" ml))
            (should (string-match-p "\\[WIP\\]" ml))
            (should (string-match-p "\\[Reset\\]" ml))
            ;; Toggles
            (should (string-match-p "\\[AutoRpt\\]" ml))
            (should (string-match-p "\\[ShowDiffs\\]" ml))
            (should (string-match-p "\\[ConfirmAll\\]" ml))
            (should (string-match-p "\\[Icons\\]" ml)))
        (carriage-mode -1)))))

(ert-deftest carriage-ui-modeline-shows-backend-and-model ()
  "Modeline should include [BACKEND:MODEL] string per defaults."
  (with-temp-buffer
    (let ((noninteractive nil))
      (carriage-mode 1)
      (unwind-protect
          (let* ((ml (carriage--modeline-string))
                 (backend (if (symbolp carriage-mode-backend)
                              (symbol-name carriage-mode-backend)
                            carriage-mode-backend))
                 (model carriage-mode-model)
                 (rx (format "\\[%s:%s\\]" (regexp-quote backend) (regexp-quote model))))
            (should (string-match-p rx ml)))
        (carriage-mode -1)))))

(provide 'carriage-ui-modeline-segments-test)
;;; carriage-ui-modeline-segments-test.el ends here
