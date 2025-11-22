;;; carriage-ui-assist-delta-normalization-tests.el --- Tests for UI context-delta normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure carriage-ui--apply-context-delta:
;; - accepts only repo-relative local paths
;; - ignores TRAMP/remote and absolute/out-of-root paths
;; - edits happen within a single undo group

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-ui)
(require 'carriage-utils)
(require 'carriage-context)

(ert-deftest carriage-ui/apply-delta-normalizes-and-ignores-bad-paths ()
  "Repo-relative adds land in begin_context; TRAMP/absolute paths are ignored."
  (with-temp-buffer
    (org-mode)
    ;; Minimal buffer; ensure a context block exists via helper
    (insert "#+title: Demo\n\n")
    (let* ((rg (carriage-ui--ensure-context-block))
           (root (or (and (fboundp 'carriage-project-root) (carriage-project-root))
                     default-directory))
           ;; Construct delta: good (repo-relative), bad absolute, bad TRAMP
           (good (if (file-exists-p (expand-file-name "lisp/carriage-ui.el" root))
                     "lisp/carriage-ui.el"
                   "lisp/carriage-context.el"))
           (bad-abs "/etc/passwd")
           (bad-tramp "/ssh:localhost:/etc/hosts"))
      ;; Apply delta
      (let ((undo-count (length buffer-undo-list)))
        (carriage-ui--apply-context-delta
         (list :add (list good bad-abs bad-tramp)
               :remove '()))
        ;; Check single undo group produced at least one entry
        (should (< undo-count (length buffer-undo-list))))
      ;; Validate begin_context content
      (goto-char (point-min))
      (re-search-forward "^[ \t]*#\\+begin_context\\b")
      (let ((beg (line-end-position)))
        (re-search-forward "^[ \t]*#\\+end_context\\b")
        (let ((end (line-beginning-position)))
          (narrow-to-region (1+ beg) end)
          (goto-char (point-min))
          (let ((lines '()))
            (while (not (eobp))
              (let ((ln (string-trim (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position)))))
                (unless (string-empty-p ln) (push ln lines)))
              (forward-line 1))
            (setq lines (nreverse lines))
            ;; Good path present, bad ones absent
            (should (member good lines))
            (should-not (member bad-abs lines))
            (should-not (member bad-tramp lines))))))))

(provide 'carriage-ui-assist-delta-normalization-tests)
;;; carriage-ui-assist-delta-normalization-tests.el ends here
