;;; carriage-mode.el --- Minor mode, commands, and UI glue  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-parser)
(require 'carriage-apply)
(require 'carriage-report)
(require 'carriage-ui)

(defcustom carriage-mode-default-profile 'Code
  "Default profile for Carriage: 'Ask or 'Code."
  :type '(choice (const Ask) (const Code))
  :group 'carriage)

(defcustom carriage-mode-default-model "gptel-default"
  "Default LLM model name for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-auto-open-report t
  "Open report buffer automatically after dry-run."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-show-diffs t
  "Require showing diffs before apply."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-confirm-apply-all t
  "Ask for confirmation before applying all blocks (C-c !)."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-use-icons nil
  "Use all-the-icons in mode-line if available."
  :type 'boolean :group 'carriage)

(defvar-local carriage-mode-profile carriage-mode-default-profile
  "Current Carriage profile for this buffer: 'Ask or 'Code.")

(defvar-local carriage-mode-model carriage-mode-default-model
  "Current Carriage model string for this buffer.")

;;;###autoload
(define-minor-mode carriage-mode
  "Toggle Carriage minor mode for working with patch blocks in org buffers."
  :lighter (:eval (concat " Carriage" (carriage-ui-state-lighter)))
  :keymap carriage-mode-map
  :group 'carriage
  (if carriage-mode
      (progn
        (setq carriage-mode-profile carriage-mode-default-profile)
        (setq carriage-mode-model carriage-mode-default-model)
        (carriage-log "carriage-mode enabled in %s" (buffer-name)))
    (carriage-log "carriage-mode disabled in %s" (buffer-name))))

;;; Commands (stubs/minimal implementations)

;;;###autoload
(defun carriage-send-buffer ()
  "Send entire buffer to LLM according to current profile."
  (interactive)
  (carriage-ui-set-state 'sending)
  (message "Carriage: send-buffer (profile=%s, model=%s)"
           carriage-mode-profile carriage-mode-model)
  (carriage-ui-set-state 'idle))

;;;###autoload
(defun carriage-send-subtree ()
  "Send current org subtree to LLM according to current profile."
  (interactive)
  (carriage-ui-set-state 'sending)
  (message "Carriage: send-subtree (profile=%s, model=%s)"
           carriage-mode-profile carriage-mode-model)
  (carriage-ui-set-state 'idle))

;;;###autoload
(defun carriage-dry-run-at-point ()
  "Run dry-run for the patch block at point and open report."
  (interactive)
  (carriage-ui-set-state 'dry-run)
  (let* ((root (or (carriage-project-root) default-directory))
         (plan-item (condition-case e
                        (carriage-parse-block-at-point root)
                      (error
                       (carriage-ui-set-state 'error)
                       (user-error "Carriage parse error: %s" (error-message-string e)))))
         (report (carriage-dry-run-plan (list plan-item) root)))
    (when carriage-mode-auto-open-report
      (carriage-report-open report))
    (carriage-ui-set-state 'idle)))

;;;###autoload
(defun carriage-apply-at-point ()
  "Dry-run → confirm → apply for the patch block at point."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (plan-item (condition-case e
                        (carriage-parse-block-at-point root)
                      (error
                       (carriage-ui-set-state 'error)
                       (user-error "Carriage parse error: %s" (error-message-string e)))))
         (dry (progn
                (carriage-ui-set-state 'dry-run)
                (carriage-dry-run-plan (list plan-item) root))))
    (when carriage-mode-auto-open-report
      (carriage-report-open dry))
    (let* ((sum (plist-get dry :summary))
           (fails (or (plist-get sum :fail) 0)))
      (if (> fails 0)
          (progn
            (carriage-ui-set-state 'error)
            (user-error "Dry-run failed; see report for details"))
        (when (or (not carriage-mode-show-diffs)
                  (y-or-n-p "Apply this block? "))
          (carriage-ui-set-state 'apply)
          (let ((ap (carriage-apply-plan (list plan-item) root)))
            (carriage-report-open ap))
          (carriage-ui-set-state 'idle))))))

;;;###autoload
(defun carriage-apply-last-iteration ()
  "Apply all blocks from the last iteration (stub)."
  (interactive)
  (when (and carriage-mode-confirm-apply-all
             (not (y-or-n-p "Apply all blocks from last iteration? ")))
    (user-error "Aborted"))
  (carriage-ui-set-state 'apply)
  (message "Carriage: apply-last-iteration (stub)")
  (carriage-ui-set-state 'idle))

;;;###autoload
(defun carriage-wip-checkout ()
  "Create or switch to the WIP branch in the current repository."
  (interactive)
  (let ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (carriage-git-checkout-wip root)
    (message "Carriage: switched to WIP branch in %s" root)))

;;;###autoload
(defun carriage-wip-reset-soft (&optional rev)
  "Soft reset last commit (default REV is HEAD~1) in WIP."
  (interactive)
  (let ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (carriage-git-reset-soft root (or rev "HEAD~1"))
    (message "Carriage: soft reset to %s" (or rev "HEAD~1"))))

;;;###autoload
(defun carriage-toggle-profile ()
  "Toggle profile between 'Ask and 'Code."
  (interactive)
  (setq carriage-mode-profile (if (eq carriage-mode-profile 'Ask) 'Code 'Ask))
  (message "Carriage profile: %s" carriage-mode-profile))

;;;###autoload
(defun carriage-select-model (model)
  "Select LLM MODEL string for Carriage."
  (interactive "sModel: ")
  (setq carriage-mode-model model)
  (message "Carriage model set to: %s" model))

;;; Navigation placeholders

(defun carriage-next-patch-block ()
  "Jump to next patch block (placeholder)."
  (interactive)
  (message "carriage-next-patch-block: not implemented yet"))

(defun carriage-prev-patch-block ()
  "Jump to previous patch block (placeholder)."
  (interactive)
  (message "carriage-prev-patch-block: not implemented yet"))

(provide 'carriage-mode)
;;; carriage-mode.el ends here
