;;; carriage-context-priority-test.el --- Tests for context priorities and global prefix  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defun carriage--test--maybe-require (&rest feats)
  "Best-effort require FEATS; return t if all loaded or already present."
  (cl-every (lambda (sym) (require sym nil 'noerror)) feats))

(ert-deftest carriage-global-prefix-open-buffer-noncarriage ()
  "In a non-carriage buffer with carriage-global-mode and transient disabled globally:
C-c e is a global prefix and C-c e e invokes =carriage-open-buffer'."
  (skip-unless (carriage--test--maybe-require 'carriage-global-mode 'carriage-mode))
  (let* ((carriage-global-use-transient nil))
    (carriage-global-mode 1)
    (unwind-protect
        (with-temp-buffer
          (text-mode)
          (let ((fn (key-binding (kbd "C-c e e") t)))
            (should (equal fn 'carriage-open-buffer))))
      (carriage-global-mode -1))))

(ert-deftest carriage-carriage-buffer-global-prefix-when-no-transient ()
  "In a carriage buffer with transient=nil, global suffixes remain available under the local prefix."
  (skip-unless (carriage--test--maybe-require 'carriage-mode 'carriage-global-mode 'carriage-keyspec))
  (let* ((carriage-mode-use-transient nil)
         (carriage-global-use-transient nil))
    (carriage-global-mode 1)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (carriage-mode 1)
          ;; Global open-buffer should be reachable as C-c e e
          (let ((fn (key-binding (kbd "C-c e e") t)))
            (should (equal fn 'carriage-open-buffer))))
      (carriage-global-mode -1))))

(ert-deftest carriage-report-context-bindings ()
  "Report buffer: context-specific actions are available under C-c e."
  (skip-unless (carriage--test--maybe-require 'carriage-report 'carriage-keyspec))
  (let ((buf (get-buffer-create "*carriage-report*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; If report mode is available, enable it; otherwise skip
          (if (fboundp 'carriage-report-mode)
              (progn
                (carriage-report-mode 1)
                ;; Check typical report actions via keyspec
                (should (equal (key-binding (kbd "C-c e d") t) 'carriage-report-show-diff-at-point))
                (should (equal (key-binding (kbd "C-c e e") t) 'carriage-report-ediff-at-point))
                (should (equal (key-binding (kbd "C-c e a") t) 'carriage-report-apply-at-point)))
            (ert-skip "carriage-report-mode not available")))
      (kill-buffer buf))))

(ert-deftest carriage-aux-log-traffic-context-bindings ()
  "Aux buffers (log/traffic): ensure C-c e q is available via keyspec; local q remains for quit-window."
  (skip-unless (carriage--test--maybe-require 'carriage-logging 'carriage-keyspec))
  ;; Best-effort: open log buffer via command when available; otherwise create a dummy and enable aux mode.
  (let ((open-log (and (fboundp 'carriage-show-log) (ignore-errors (carriage-show-log))))
        (log-buf (or (get-buffer "*carriage-log*") (get-buffer-create "*carriage-log*"))))
    (unwind-protect
        (when (buffer-live-p log-buf)
          (with-current-buffer log-buf
            ;; Enable aux mode if present
            (when (fboundp 'carriage-aux-mode) (carriage-aux-mode 1))
            ;; keyspec binding on prefix
            (should (equal (key-binding (kbd "C-c e q") t) 'quit-window))
            ;; local single-key q should remain for UX
            (should (equal (key-binding (kbd "q") t) 'quit-window))))
      (when (and (not open-log) (buffer-live-p log-buf))
        (kill-buffer log-buf)))))

(provide 'carriage-context-priority-test)
;;; carriage-context-priority-test.el ends here
