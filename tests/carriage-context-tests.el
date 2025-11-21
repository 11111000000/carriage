;;; carriage-context-tests.el --- Tests for context profile toggling and basics -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for carriage-context profile toggling:
;; - Toggle P1<->P3 updates buffer-local profile var.
;; - Toggle does not error in a plain Org buffer.
;; - Basic sanity that modeline refresh helpers do not throw (best-effort).

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-context)

(ert-deftest carriage-context/profile-toggle-updates-state ()
  "carriage-context-profile-set should set buffer-local profile to 'p3 and back to 'p1."
  (with-temp-buffer
    (org-mode)
    ;; Defaults: ensure variable is buffer-local and starts as 'p1.
    (setq-local carriage-doc-context-profile 'p1)
    ;; Switch to P3
    (carriage-context-profile-set 'p3)
    (should (eq carriage-doc-context-profile 'p3))
    ;; Switch back to P1
    (carriage-context-profile-set 'p1)
    (should (eq carriage-doc-context-profile 'p1))))

(ert-deftest carriage-context/profile-toggle-does-not-error ()
  "Toggling profiles should not error; message side-effects are acceptable."
  (with-temp-buffer
    (org-mode)
    (setq-local carriage-doc-context-profile 'p1)
    (should (ignore-errors (carriage-context-profile-set 'p3) t))
    (should (ignore-errors (carriage-context-profile-set 'p1) t))))

(provide 'carriage-context-tests)
;;; carriage-context-tests.el ends here
