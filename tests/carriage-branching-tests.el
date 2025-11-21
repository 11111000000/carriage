;;; carriage-branching-tests.el --- Tests for branching provenance and context blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for:
;; - Writing provenance into #+begin_carriage and reading it back via carriage-doc-state.
;; - Inserting begin_context blocks with given paths.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-task)
(require 'carriage-doc-state)

(ert-deftest carriage-branching/provenance-write-and-read ()
  "Write provenance into begin_carriage and read it back via carriage-doc-state-read."
  (with-temp-buffer
    (org-mode)
    ;; Ensure empty buffer; write provenance like carriage-task-create-from-template would do.
    (carriage-task--write-carriage-provenance
     (current-buffer)
     'task/default "1.0" 'p1 (list :begin t :flags t))
    ;; Now read via doc-state; it prefers begin_carriage.
    (let* ((pl (carriage-doc-state-read (current-buffer)))
           (tid (plist-get pl :CAR_TEMPLATE_ID))
           (tver (plist-get pl :CAR_TEMPLATE_VER))
           (prof (plist-get pl :CAR_CONTEXT_PROFILE))
           (inh (plist-get pl :CAR_INHERITED)))
      (should (stringp tid))
      (should (stringp tver))
      (should (stringp prof))
      (should (stringp inh))
      (should (string-match-p "\\`task/default\\'" tid))
      (should (equal tver "1.0"))
      (should (equal prof "P1"))
      (should (member inh '("BOTH" "BEGIN" "FLAGS" "NONE"))))))

(ert-deftest carriage-branching/insert-begin-context-with-paths ()
  "Insert a begin_context block with provided paths at a sensible location."
  (with-temp-buffer
    (org-mode)
    ;; Insert a minimal header and Context section to anchor insertion
    (insert "#+title: Demo\n\n* Context\n\n* Plan\n")
    (let* ((paths '("lisp/carriage-task.el" "spec/document-branching-and-templates-v1.org")))
      (carriage-task--insert-begin-context-in-buffer (current-buffer) paths))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
      (should (re-search-forward "^lisp/carriage-task\\.el$" nil t))
      (should (re-search-forward "^spec/document-branching-and-templates-v1\\.org$" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)))))

(provide 'carriage-branching-tests)
;;; carriage-branching-tests.el ends here
