;;; carriage-keyspec-bindings-test.el --- Tests for keyspec bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'carriage)
(require 'carriage-mode)
(require 'carriage-keyspec)

(ert-deftest carriage-keyspec-binds-core-keys ()
  "All normative bindings must be available under prefix C-c e in carriage-mode buffers."
  (with-temp-buffer
    (let ((noninteractive nil))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            (cl-labels ((has (key fn)
                          (should (eq (key-binding (kbd key)) fn))))
              ;; Tools/model/context
              (has "C-c e m" #'carriage-select-model)
              (has "C-c e t c" #'carriage-toggle-include-gptel-context)
              (has "C-c e t f" #'carriage-toggle-include-doc-context)
              (has "C-c e e" #'carriage-keys-open-menu)
              ;; Actions
              (has "C-c e d" #'carriage-dry-run-at-point)
              (has "C-c e a" #'carriage-apply-at-point)
              (has "C-c e A" #'carriage-apply-last-iteration)
              (has "C-c e x" #'carriage-abort-current)
              (has "C-c e r" #'carriage-report-open)
              ;; Git / session
              (has "C-c e w" #'carriage-wip-checkout)
              (has "C-c e R" #'carriage-wip-reset-soft)
              (has "C-c e c" #'carriage-commit-changes)
              (has "C-c e i" #'carriage-commit-last-iteration)
              ;; Engine
              (has "C-c e E" #'carriage-select-apply-engine)))
        (carriage-mode -1)))))

(ert-deftest carriage-keyspec-menu-command-present ()
  "Menu command should be present (fallback to completing-read if no transient)."
  (should (fboundp 'carriage-keys-open-menu)))

(provide 'carriage-keyspec-bindings-test)
;;; carriage-keyspec-bindings-test.el ends here
