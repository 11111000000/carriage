;;; tests/carriage-doc-state-save-tests.el --- Before-save normalization tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'carriage-doc-state)

(ert-deftest carriage-doc-state/before-save-normalizes-begin-carriage ()
  "before-save hook should normalize to a single #+begin_carriage block and fold overlays."
  (with-temp-buffer
    (org-mode)
    ;; Simulate file-level properties that should be normalized into begin_carriage
    (insert "#+PROPERTY: CARRIAGE_MODE t\n")
    (insert "#+PROPERTY: CARRIAGE_INTENT Code\n")
    (insert "#+PROPERTY: CARRIAGE_CONTEXT_PROFILE P1\n\n")
    ;; Install the before-save hook buffer-locally and run it
    (carriage-doc-state-install-save-hook)
    (carriage-doc-state--on-before-save)
    (goto-char (point-min))
    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "^[ \t]*#\\+begin_carriage" txt))
      (should (string-match-p "CARRIAGE_MODE[ \t]+t" txt))
      (should (string-match-p "CARRIAGE_INTENT[ \t]+Code" txt))
      (should (string-match-p "CARRIAGE_CONTEXT_PROFILE[ \t]+P1" txt)))))

(provide 'carriage-doc-state-save-tests)
;;; carriage-doc-state-save-tests.el ends here
