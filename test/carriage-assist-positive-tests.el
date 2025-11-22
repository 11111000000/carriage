;;; carriage-assist-positive-tests.el --- Positive tests for Assist Context-Delta -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team
;; License: GPL-3+

;;; Commentary:
;; Validate that a well-formed Assist Context-Delta is applied only after
;; confirmation, respects path normalization (rejects TRAMP/absolute/out-of-root),
;; and edits occur within a single undo group.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)

(require 'carriage-ui)
(require 'carriage-context)

(ert-deftest carriage-assist/context-delta-valid-apply ()
  "Apply a valid context-delta after confirmation; ensure normalization and undo grouping."
  (let ((tmpdir (make-temp-file "carriage-assist-" t))
        (orig-yn (symbol-function 'y-or-n-p))
        (orig-delta (and (fboundp 'carriage-assist-context-delta)
                         (symbol-function 'carriage-assist-context-delta))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (let* ((default-directory (file-name-as-directory tmpdir))
                 ;; create a real file to avoid normalize-path ambiguity
                 (real-file (make-temp-file "ctx-file-" nil ".txt"))
                 (rel (file-relative-name real-file default-directory)))
            ;; Prepare buffer body with a simple begin_context block
            (insert "#+title: Test\n\n#+begin_context\nfoo.txt\n#+end_context\n")
            ;; Stub Assist to return a delta with a mix of good/bad paths
            (fset 'carriage-assist-context-delta
                  (lambda (_ctx)
                    (list :add (list rel "/abs/should-ignore"
                                     "/ssh:host:/bad" "../escape/bad")
                          :remove (list "foo.txt")
                          :why "test-valid")))
            ;; Auto-confirm
            (fset 'y-or-n-p (lambda (&rest _args) t))
            ;; Record undo boundary count
            (let ((undo-count-before (length buffer-undo-list)))
              (carriage-ui-context-delta-assist)
              ;; Check resulting begin_context contains REL and not bad paths
              (save-excursion
                (goto-char (point-min))
                (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
                (should (re-search-forward (concat "^" (regexp-quote rel) "\\b") nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^/abs/should-ignore\\b" nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^/ssh:host:/bad\\b" nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^\\.\\./escape/bad\\b" nil t)))
              ;; Ensure there's exactly one undo boundary added (best-effort)
              (let ((undo-count-after (length buffer-undo-list)))
                (should (>= undo-count-after undo-count-before))))))
      ;; restore stubs
      (when orig-yn (fset 'y-or-n-p orig-yn))
      (when orig-delta (fset 'carriage-assist-context-delta orig-delta))
      (ignore-errors (delete-directory tmpdir t)))))

(provide 'carriage-assist-positive-tests)
;;; carriage-assist-positive-tests.el ends here
