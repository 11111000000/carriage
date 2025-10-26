;;; carriage-mode.el --- Minor mode, commands, and UI glue  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-parser)
(require 'carriage-apply)
(require 'carriage-report)
(require 'carriage-iteration)
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

(defcustom carriage-mode-wip-branch "carriage/WIP"
  "Default Git WIP branch name used for applying changes."
  :type 'string :group 'carriage)

(defcustom carriage-mode-sre-preview-max 3
  "Maximum number of SRE preview chunks (mini-diffs) to show per pair in dry-run.
For :occur all, at most this many previews are included; the rest are summarized
as a “(+N more)” tail."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-sre-preview-context-lines 1
  "Number of context lines to include above and below each SRE mini-diff preview.
0 means no context (only -old/+new lines)."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-max-batch-pairs 200
  "Maximum number of pairs allowed in an :op 'sre-batch block."
  :type 'integer :group 'carriage)

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
    (when (and carriage-mode-auto-open-report (not noninteractive))
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
    (when (and carriage-mode-auto-open-report (not noninteractive))
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
            (when (not noninteractive)
              (carriage-report-open ap)))
          (carriage-ui-set-state 'idle))))))

;;;###autoload
(defun carriage-apply-last-iteration ()
  "Dry-run → подтверждение → применение всех блоков «последней итерации».
В v1 эта команда берёт все блоки в текущем буфере как «последнюю итерацию»."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (plan (carriage-collect-last-iteration-blocks root)))
    (when (null plan)
      (user-error "Нет patch-блоков в текущем буфере"))
    (when (and carriage-mode-confirm-apply-all
               (not (y-or-n-p (format "Применить все блоки (%d)? " (length plan)))))
      (user-error "Отменено"))
    ;; Dry-run
    (carriage-ui-set-state 'dry-run)
    (let* ((dry (carriage-dry-run-plan plan root)))
      (when (and carriage-mode-auto-open-report (not noninteractive))
        (carriage-report-open dry))
      (let* ((sum (plist-get dry :summary))
             (fails (or (plist-get sum :fail) 0)))
        (if (> fails 0)
            (progn
              (carriage-ui-set-state 'error)
              (user-error "Dry-run провалился для части блоков; смотрите отчёт"))
          ;; Apply
          (when (or (not carriage-mode-show-diffs)
                    (y-or-n-p "Применить группу блоков? "))
            (carriage-ui-set-state 'apply)
            (let ((ap (carriage-apply-plan plan root)))
              (when (not noninteractive)
                (carriage-report-open ap)))
            (carriage-ui-set-state 'idle)))))))

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

(defun carriage--extract-patch-blocks (text)
  "Extract all #+begin_patch ... #+end_patch blocks from TEXT.
Return a single string with blocks concatenated by blank lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((chunks '()))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
        (let ((beg (match-beginning 0)))
          (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (goto-char (point-max)))
          (let ((end (line-end-position)))
            (push (buffer-substring-no-properties beg end) chunks))
          (forward-line 1)))
      (mapconcat #'identity (nreverse chunks) "\n\n"))))

;;;###autoload
(defun carriage-accept-llm-response (&optional input)
  "Accept an LLM response INPUT, keep only begin_patch blocks, insert and dry-run.
Interactively prompts for INPUT. Inserts extracted blocks at point,
marks them as the last iteration, runs dry-run and opens the report.

Returns the dry-run report plist."
  (interactive
   (list (read-string "Paste LLM response (only begin_patch blocks will be kept):\n")))
  (let* ((root (or (carriage-project-root) default-directory))
         (blocks (carriage--extract-patch-blocks (or input ""))))
    (when (or (null blocks) (string-empty-p (string-trim blocks)))
      (user-error "No begin_patch blocks found in input"))
    (carriage-traffic-log 'in "Accepted LLM response (%d chars)" (length input))
    (let ((ins-beg (point)))
      (unless (bolp) (insert "\n"))
      (insert blocks "\n")
      (let ((ins-end (point)))
        (carriage-mark-last-iteration ins-beg ins-end)))
    (let ((plan (carriage-collect-last-iteration-blocks root)))
      (carriage-ui-set-state 'dry-run)
      (let ((rep (carriage-dry-run-plan plan root)))
        (when (and carriage-mode-auto-open-report (not noninteractive))
          (carriage-report-open rep))
        (carriage-ui-set-state 'idle)
        rep))))

(provide 'carriage-mode)
;;; carriage-mode.el ends here
