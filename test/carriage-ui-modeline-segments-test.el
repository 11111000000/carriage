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
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (let ((ml (carriage--modeline-string)))
            ;; Core buttons
            (should (string-match-p "\\[Ask\\]\\|\\[Code\\]" ml))
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

(ert-deftest carriage-ui-modeline-shows-model ()
  "Modeline should include [MODEL] (basename only); full id is in tooltip."
  (with-temp-buffer
    (let ((noninteractive nil))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (let* ((ml (carriage--modeline-string))
                 (model carriage-mode-model)
                 (rx (format "\\[%s\\]" (regexp-quote model))))
            (should (string-match-p rx ml)))
        (carriage-mode -1)))))

;; New: tooltip for [Engine] should include branch policy for engine='git
(ert-deftest carriage-ui-engine-tooltip-includes-policy ()
  "Engine button help-echo must include branch policy when engine='git."
  (with-temp-buffer
    (let ((noninteractive nil)
          (carriage-mode-use-icons nil)
          (carriage-apply-engine 'git)
          (carriage-git-branch-policy 'ephemeral))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (let* ((seg (carriage-ui--modeline))
                 (found nil)
                 (val nil))
            (should (stringp seg))
            (let ((i 0) (len (length seg)))
              (while (and (< i len) (not found))
                (let ((h (get-text-property i 'help-echo seg)))
                  (when (and h (string-match-p "ephemeral" (format "%s" h)))
                    (setq found t val h)))
                (setq i (1+ i))))
            (should found)
            (should (stringp (format "%s" val))))
        (carriage-mode -1)))))

(provide 'carriage-ui-modeline-segments-test)
;;; carriage-ui-modeline-segments-test.el ends here
