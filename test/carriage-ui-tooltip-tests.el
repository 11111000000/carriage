;;; carriage-ui-tooltip-tests.el --- Tests for [Ctx:N] tooltip profile/limits -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-ui)
(require 'carriage-context)

(ert-deftest carriage-ui/tooltip-shows-profile-p1 ()
  "Tooltip for [Ctx:N] should include P1 profile line."
  (with-temp-buffer
    ;; Simulate toggles (doc/gpt on), P1 by default
    (setq-local carriage-mode-include-doc-context t)
    (setq-local carriage-mode-include-gptel-context t)
    (setq-local carriage-mode-include-visible-context nil)
    (setq-local carriage-mode-include-patched-files nil)
    (setq-local carriage-doc-context-profile 'p1)
    ;; Ensure budgets exist (defaults from context module)
    (when (boundp 'carriage-mode-context-max-files)
      (setq-local carriage-mode-context-max-files 100))
    (when (boundp 'carriage-mode-context-max-total-bytes)
      (setq-local carriage-mode-context-max-total-bytes 1048576))
    ;; Compute badge directly (doc,gpt,vis,patched)
    (cl-destructuring-bind (lbl . hint)
        (carriage-ui--compute-context-badge t t nil nil)
      (should (stringp lbl))
      (should (string-match-p "\\`\\[Ctx:" lbl))
      (should (stringp hint))
      (should (string-match-p "Профиль: P1" hint)))))

(ert-deftest carriage-ui/tooltip-shows-profile-p3-and-warning ()
  "Tooltip for [Ctx:N] should include P3 profile line and warning."
  (with-temp-buffer
    (setq-local carriage-mode-include-doc-context t)
    (setq-local carriage-mode-include-gptel-context t)
    (setq-local carriage-mode-include-visible-context nil)
    (setq-local carriage-mode-include-patched-files nil)
    (setq-local carriage-doc-context-profile 'p3)
    ;; Apply larger budgets typical for P3 profile (if variables exist)
    (when (boundp 'carriage-mode-context-max-files)
      (setq-local carriage-mode-context-max-files 400))
    (when (boundp 'carriage-mode-context-max-total-bytes)
      (setq-local carriage-mode-context-max-total-bytes 4194304))
    (cl-destructuring-bind (_lbl . hint)
        (carriage-ui--compute-context-badge t t nil nil)
      (should (stringp hint))
      (should (string-match-p "Профиль: P3" hint))
      ;; Warning marker presence (russian text added in tooltip for P3)
      (should (or (string-match-p "внимание" (downcase hint))
                  (string-match-p "расширенн" (downcase hint)))))))

(provide 'carriage-ui-tooltip-tests)
;;; carriage-ui-tooltip-tests.el ends here
