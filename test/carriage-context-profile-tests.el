;;; carriage-context-profile-tests.el --- Tests for context profile persistence and toggling -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Verify that context profile switching:
;; - updates buffer-local profile variable,
;; - writes CAR_CONTEXT_PROFILE into #+begin_carriage storage,
;; - can switch back and forth (P1 <-> P3).

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-context)
(require 'carriage-doc-state)

(ert-deftest carriage-context/profile-write-begin-carriage ()
  "Switching profile writes CAR_CONTEXT_PROFILE into #+begin_carriage and toggles correctly."
  (with-temp-buffer
    (org-mode)
    ;; Start with minimal buffer, no begin_carriage yet
    (insert "#+title: Demo\n\n* Note\nBody\n")
    ;; Ensure local default is p1
    (setq-local carriage-doc-context-profile 'p1)

    ;; Switch to P3 and expect begin_carriage to appear with 'CARRIAGE_CONTEXT_PROFILE P3'
    (carriage-context-profile-set 'p3)
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
      (should (re-search-forward "^CARRIAGE_CONTEXT_PROFILE[ \t]+P3\\b" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)))
    (should (eq carriage-doc-context-profile 'p3))

    ;; Switch back to P1 and ensure the profile line updated accordingly
    (carriage-context-profile-set 'p1)
    (save-excursion
      (goto-char (point-min))
      ;; profile line should now be P1 (old P3 should not be the only occurrence)
      (should (re-search-forward "^CARRIAGE_CONTEXT_PROFILE[ \t]+P1\\b" nil t)))
    (should (eq carriage-doc-context-profile 'p1))))

(ert-deftest carriage-context/profile-budgets-apply ()
  "Switching profile applies default budgets for P1/P3 and updates buffer-locals."
  (with-temp-buffer
    (org-mode)
    ;; Prepare local budget vars to be present (carriage-context applies only when bound)
    (defvar-local carriage-mode-context-max-files nil)
    (defvar-local carriage-mode-context-max-total-bytes nil)
    (let* ((p1 (or carriage-context-p1-defaults '(:max-files 100 :max-bytes 1048576)))
           (p3 (or carriage-context-p3-defaults '(:max-files 400 :max-bytes 4194304))))
      ;; Start with P1 defaults
      (carriage-context-profile-set 'p1)
      (should (equal carriage-mode-context-max-files (plist-get p1 :max-files)))
      (should (equal carriage-mode-context-max-total-bytes (plist-get p1 :max-bytes)))
      ;; Switch to P3
      (carriage-context-profile-set 'p3)
      (should (equal carriage-mode-context-max-files (plist-get p3 :max-files)))
      (should (equal carriage-mode-context-max-total-bytes (plist-get p3 :max-bytes)))
      ;; Back to P1
      (carriage-context-profile-set 'p1)
      (should (equal carriage-mode-context-max-files (plist-get p1 :max-files)))
      (should (equal carriage-mode-context-max-total-bytes (plist-get p1 :max-bytes))))))
(provide 'carriage-context-profile-tests)
;;; carriage-context-profile-tests.el ends here
