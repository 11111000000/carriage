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
(require 'carriage-llm-registry)
(require 'carriage-transport)
(require 'carriage-ui)

(defcustom carriage-mode-default-profile 'Code
  "Default profile for Carriage: `Ask' or `Code'."
  :type '(choice (const Ask) (const Code))
  :group 'carriage)

(defcustom carriage-mode-default-model "gptel-default"
  "Default LLM model name for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-default-backend 'gptel
  "Default LLM transport backend for Carriage (e.g., 'gptel)."
  :type '(choice symbol string)
  :group 'carriage)

(defcustom carriage-mode-auto-open-report t
  "Open report buffer automatically after dry-run."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-show-diffs t
  "Require showing diffs before apply."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-auto-open-log t
  "When non-nil, open *carriage-log* automatically on mode enable and when sending."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-auto-open-traffic t
  "When non-nil, open *carriage-traffic* automatically when sending."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-confirm-apply-all t
  "Ask for confirmation before applying all blocks (C-c !)."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-use-icons t
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
  "Maximum number of pairs allowed in an :op =sre-batch' block."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-show-header-line t
  "When non-nil, install a buffer-local header-line segment for Carriage."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-headerline-max-width nil
  "Maximum width of header-line in columns. When nil, use window width."
  :type '(choice (const :tag "Auto" nil) integer)
  :group 'carriage)

(defcustom carriage-mode-show-mode-line-ui t
  "When non-nil, add a buffer-local modeline segment for Carriage."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-spinner-interval 0.15
  "Spinner update interval in seconds for sending/streaming states."
  :type 'number :group 'carriage)

(defcustom carriage-mode-allow-patch-binary nil
  "Allow binary patches in :op \"patch\" blocks.
Note: v1 forbids binary patches; this option remains nil in v1 and is reserved for future versions."
  :type 'boolean :group 'carriage)

(defvar-local carriage-mode-profile carriage-mode-default-profile
  "Current Carriage profile for this buffer: =Ask' or =Code'.")

(defvar-local carriage-mode-model carriage-mode-default-model
  "Current Carriage model string for this buffer.")

(defvar-local carriage-mode-backend carriage-mode-default-backend
  "Current Carriage backend identifier (symbol or string) for this buffer.")

(defvar-local carriage--mode-prev-header-line-format nil
  "Saved previous value of =header-line-format' to restore on mode disable.")

(defvar-local carriage--mode-modeline-construct nil
  "The exact modeline construct object inserted by Carriage for later removal.")

(defvar-local carriage--abort-handler nil
  "Buffer-local abort handler function for the current Carriage activity, or nil.

The handler should be a zero-argument function that cancels the ongoing request or apply.
Set by transports/pipelines when starting an async activity; cleared on completion or when disabling carriage-mode.")


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
        (setq carriage-mode-backend carriage-mode-default-backend)
        ;; Ensure default backend:model is present in registry for completion.
        (when (require 'carriage-llm-registry nil t)
          (let* ((bsym (if (symbolp carriage-mode-backend)
                           carriage-mode-backend
                         (intern (format "%s" carriage-mode-backend))))
                 (backs (carriage-llm-available-backends)))
            (unless (member (symbol-name bsym) backs)
              (carriage-llm-register-backend bsym :models (list carriage-mode-model)))))
        ;; UI (buffer-local, no global effects); respect batch/noninteractive
        (unless (bound-and-true-p noninteractive)
          (when carriage-mode-show-header-line
            (setq carriage--mode-prev-header-line-format header-line-format)
            (setq-local header-line-format '(:eval (carriage-ui--header-line))))
          (when carriage-mode-show-mode-line-ui
            (setq carriage--mode-modeline-construct '(:eval (carriage-ui--modeline)))
            (setq-local mode-line-format
                        (append mode-line-format (list carriage--mode-modeline-construct)))))
        (when (and carriage-mode-auto-open-log (fboundp 'carriage-show-log))
          (ignore-errors (carriage-show-log)))
        (when (and carriage-mode-auto-open-traffic (fboundp 'carriage-show-traffic))
          (ignore-errors (carriage-show-traffic)))
        (carriage-log "carriage-mode enabled in %s" (buffer-name)))
    ;; Disable: restore header-line and remove modeline segment (buffer-local)
    (unless (bound-and-true-p noninteractive)
      (when (local-variable-p 'header-line-format)
        (setq-local header-line-format carriage--mode-prev-header-line-format))
      (setq carriage--mode-prev-header-line-format nil)
      (when (and carriage--mode-modeline-construct
                 (local-variable-p 'mode-line-format))
        (setq-local mode-line-format
                    (delq carriage--mode-modeline-construct mode-line-format)))
      (setq carriage--mode-modeline-construct nil)
      ;; Clear abort handler and stop spinner if running
      (setq carriage--abort-handler nil)
      (when (fboundp 'carriage-ui--spinner-stop)
        (carriage-ui--spinner-stop t))
      (force-mode-line-update t))
    (carriage-log "carriage-mode disabled in %s" (buffer-name))))

;;; Commands (stubs/minimal implementations)

;;;###autoload
(defun carriage-send-buffer ()
  "Send entire buffer to LLM according to current profile."
  (interactive)
  (let* ((backend carriage-mode-backend)
         (model   carriage-mode-model)
         (profile carriage-mode-profile))
    (carriage-log "send-buffer: profile=%s backend=%s model=%s"
                  profile backend model)
    (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-traffic)))
    (let ((unreg (carriage-transport-begin)))
      (carriage-traffic-log 'out "request begin: source=buffer backend=%s model=%s"
                            backend model)
      (condition-case err
          (progn
            ;; Hint UI that streaming started (adapter should call this too).
            (carriage-transport-streaming)
            ;; Dispatch via transport (placeholder will log error if no adapter).
            (carriage-transport-dispatch :source 'buffer
                                         :backend backend
                                         :model model
                                         :buffer (current-buffer))
            t)
        (error
         (carriage-log "send-buffer error: %s" (error-message-string err))
         (carriage-transport-complete t))))))

;;;###autoload
(defun carriage-send-subtree ()
  "Send current org subtree to LLM according to current profile."
  (interactive)
  (let* ((backend carriage-mode-backend)
         (model   carriage-mode-model)
         (profile carriage-mode-profile))
    (carriage-log "send-subtree: profile=%s backend=%s model=%s"
                  profile backend model)
    ;; Best-effort derive a small payload boundary for logs
    (when (derived-mode-p 'org-mode)
      (carriage-log "send-subtree: org-mode detected; using subtree-at-point as payload"))
    (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-traffic)))
    (let ((unreg (carriage-transport-begin)))
      (carriage-traffic-log 'out "request begin: source=subtree backend=%s model=%s"
                            backend model)
      (condition-case err
          (progn
            (carriage-transport-streaming)
            (carriage-transport-dispatch :source 'subtree
                                         :backend backend
                                         :model model
                                         :buffer (current-buffer)
                                         :mode (symbol-name major-mode))
            t)
        (error
         (carriage-log "send-subtree error: %s" (error-message-string err))
         (carriage-transport-complete t))))))

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
  "Toggle profile between `Ask' and `Code'."
  (interactive)
  (setq carriage-mode-profile (if (eq carriage-mode-profile 'Ask) 'Code 'Ask))
  (message "Carriage profile: %s" carriage-mode-profile))

;;;###autoload
(defun carriage-select-model (&optional model)
  "Select LLM MODEL for Carriage, optionally as \"backend:model\".

When registry has entries, offer completion over:
- current backend's models, and
- combined \"backend:model\" candidates.

If the choice is \"backend:model\", both backend and model are updated.
Falls back to plain string prompt when registry is empty."
  (interactive)
  ;; Ensure at least the current backend:model is registered for completion.
  (when (require 'carriage-llm-registry nil t)
    (let* ((bsym (if (symbolp carriage-mode-backend)
                     carriage-mode-backend
                   (intern (format "%s" carriage-mode-backend))))
           (backs (carriage-llm-available-backends)))
      (unless (member (symbol-name bsym) backs)
        (carriage-llm-register-backend bsym :models (list carriage-mode-model)))))
  (let* ((bcur (if (symbolp carriage-mode-backend)
                   (symbol-name carriage-mode-backend)
                 (or carriage-mode-backend "")))
         (models (carriage-llm-available-models (and (stringp bcur) (intern bcur))))
         (pairs  (carriage-llm-candidates))
         ;; Offer both combined and plain model names; keep gptel pairs first.
         (collection (delete-dups (append (or pairs '()) (or models '()))))
         (prompt (if collection
                     (format "Model (or backend:model) [%s]: " bcur)
                   "Model: "))
         (choice (or model
                     (if collection
                         (completing-read prompt collection nil t)
                       (read-string prompt)))))
    (if (and (string-match-p ":" choice)
             (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" choice))

        (let ((b (match-string 1 choice))
              (m (match-string 2 choice)))
          (setq carriage-mode-backend (intern b))
          (setq carriage-mode-model m)
          ;; Keep registry in sync so completion always offers backend:model.
          (when (require 'carriage-llm-registry nil t)
            (let* ((bsym (if (symbolp carriage-mode-backend)
                             carriage-mode-backend
                           (intern (format "%s" carriage-mode-backend))))
                   (existing (or (carriage-llm-available-models bsym) '())))
              (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
          (message "Carriage backend:model set to: %s:%s" b m))
      (setq carriage-mode-model choice)
      ;; Also register model under current backend for future completion.
      (when (require 'carriage-llm-registry nil t)
        (let* ((bsym (if (symbolp carriage-mode-backend)
                         carriage-mode-backend
                       (intern (format "%s" carriage-mode-backend))))
               (existing (or (carriage-llm-available-models bsym) '())))
          (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
      (message "Carriage model set to: %s" choice))))

;;;###autoload
(defun carriage-select-backend (&optional backend)
  "Select LLM transport BACKEND for Carriage.

When registry is available, completion is offered over registered backends.
Otherwise falls back to a free-form prompt. Stores backend as a symbol."
  (interactive)
  ;; Ensure registry exists; preseed with current backend:model if empty.
  (when (require 'carriage-llm-registry nil t)
    (let ((backs (carriage-llm-available-backends)))
      (when (null backs)
        (let* ((bsym (if (symbolp carriage-mode-backend)
                         carriage-mode-backend
                       (intern (format "%s" carriage-mode-backend)))))
          (carriage-llm-register-backend bsym :models (list carriage-mode-model))))))
  (let* ((backs (carriage-llm-available-backends))
         (choice (or backend
                     (if backs
                         (completing-read "Backend: " backs nil t
                                          (if (symbolp carriage-mode-backend)
                                              (symbol-name carriage-mode-backend)
                                            (or carriage-mode-backend "")))
                       (read-string "Backend (symbol or string): ")))))
    (setq carriage-mode-backend
          (cond
           ((symbolp choice) choice)
           ((stringp choice) (intern choice))
           (t (carriage-llm--norm-backend choice))))
    ;; Make sure selected backend is present in registry with current model for backend:model completion.
    (when (require 'carriage-llm-registry nil t)
      (let* ((bsym (if (symbolp carriage-mode-backend)
                       carriage-mode-backend
                     (intern (format "%s" carriage-mode-backend))))
             (existing (or (carriage-llm-available-models bsym) '())))
        (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
    (message "Carriage backend set to: %s"
             (if (symbolp carriage-mode-backend)
                 (symbol-name carriage-mode-backend)
               carriage-mode-backend))))

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
    (carriage-log "accept: extracted blocks bytes=%d"
                  (string-bytes blocks))
    (let ((ins-beg (point)))
      (unless (bolp) (insert "\n"))
      (insert blocks "\n")
      (let ((ins-end (point)))
        (carriage-log "accept: inserted region %d..%d (%d chars)"
                      ins-beg ins-end (- ins-end ins-beg))
        (carriage-mark-last-iteration ins-beg ins-end)))
    (carriage-log "accept: collecting last-iteration blocks…")
    (let ((plan (carriage-collect-last-iteration-blocks root)))
      (carriage-log "accept: plan-size=%d" (length plan))
      (carriage-ui-set-state 'dry-run)
      (let ((rep (carriage-dry-run-plan plan root)))
        (when (and carriage-mode-auto-open-report (not noninteractive))
          (carriage-report-open rep))
        (carriage-ui-set-state 'idle)
        rep))))

;;; Toggles and helpers (UI accessibility)

;;;###autoload
(defun carriage-abort-current ()
  "Abort current Carriage request/apply if one is active.

In v1 this calls a buffer-local handler registered by the transport/pipeline.
If no handler is present, stops UI spinner and reports no active request."
  (interactive)
  (let ((handler carriage--abort-handler))
    (cond
     ((functionp handler)
      ;; Clear handler first to avoid reentrancy; then attempt abort.
      (setq carriage--abort-handler nil)
      (condition-case err
          (funcall handler)
        (error
         (message "Abort handler error: %s" (error-message-string err))))
      (when (fboundp 'carriage-ui--spinner-stop)
        (carriage-ui--spinner-stop t))
      (when (fboundp 'carriage-ui-set-state)
        (carriage-ui-set-state 'idle))
      (message "Carriage: abort requested"))
     (t
      (when (fboundp 'carriage-ui--spinner-stop)
        (carriage-ui--spinner-stop t))
      (when (fboundp 'carriage-ui-set-state)
        (carriage-ui-set-state 'idle))
      (message "Нет активного запроса")))))

(defun carriage-register-abort-handler (fn)
  "Register buffer-local abort handler FN and return an unregister lambda.
FN must be a zero-argument function that cancels the ongoing activity."
  (setq carriage--abort-handler fn)
  (lambda ()
    (when (eq carriage--abort-handler fn)
      (setq carriage--abort-handler nil))))

(defun carriage-clear-abort-handler ()
  "Clear buffer-local abort handler if any."
  (setq carriage--abort-handler nil))

;;;###autoload
(defun carriage-toggle-auto-open-report ()
  "Toggle auto-opening report after dry-run."
  (interactive)
  (setq carriage-mode-auto-open-report (not carriage-mode-auto-open-report))
  (message "Auto-open report: %s" (if carriage-mode-auto-open-report "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-show-diffs ()
  "Toggle requirement to show diffs before apply."
  (interactive)
  (setq carriage-mode-show-diffs (not carriage-mode-show-diffs))
  (message "Show diffs before apply: %s" (if carriage-mode-show-diffs "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-confirm-apply-all ()
  "Toggle confirmation before applying all blocks (C-c !)."
  (interactive)
  (setq carriage-mode-confirm-apply-all (not carriage-mode-confirm-apply-all))
  (message "Confirm apply-all: %s" (if carriage-mode-confirm-apply-all "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-use-icons ()
  "Toggle using icons in the UI (requires all-the-icons)."
  (interactive)
  (setq carriage-mode-use-icons (not carriage-mode-use-icons))
  (message "Use icons: %s" (if carriage-mode-use-icons "on" "off"))
  (force-mode-line-update t))

(provide 'carriage-mode)
;;; carriage-mode.el ends here
