;;; carriage-doc-state-tests.el --- Tests for doc-state normalization and begin_carriage -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure before-save normalization creates/updates #+begin_carriage and folds overlays idempotently.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-doc-state)

(ert-deftest carriage-doc-state/before-save-normalizes-begin-carriage ()
  "Installing the before-save hook normalizes storage: writes #+begin_carriage."
  (with-temp-buffer
    (org-mode)
    ;; Seed a minimal buffer with a file-level property to be normalized
    (insert "#+title: Demo\n#+PROPERTY: CARRIAGE_MODE t\n\n* Note\nBody\n")
    ;; Install hook and simulate save
    (carriage-doc-state-install-save-hook)
    (run-hooks 'before-save-hook)
    ;; Expect begin_carriage present with normalized keys
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
      (should (re-search-forward "^CARRIAGE_MODE[ \t]+t\\b" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)))))

(provide 'carriage-doc-state-tests)
;;; carriage-doc-state-tests.el ends here
