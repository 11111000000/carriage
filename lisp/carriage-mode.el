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
(require 'carriage-ui)
(require 'carriage-suite)
(require 'carriage-sre-core)
;; Defer transport to avoid circular require; call via autoloaded functions.
(declare-function carriage-transport-begin "carriage-transport" (&optional abort-fn))
(declare-function carriage-transport-streaming "carriage-transport" ())
(declare-function carriage-transport-complete "carriage-transport" (&optional errorp))
(declare-function carriage-transport-dispatch "carriage-transport" (&rest args))
(declare-function carriage-select-apply-engine "carriage-apply-engine" (&optional engine))

(defun carriage--ensure-transport ()
  "Load carriage-transport when its functions are not yet defined (no autoloads)."
  (unless (fboundp 'carriage-transport-begin)
    (ignore-errors (require 'carriage-transport))))

(defcustom carriage-mode-default-intent 'Code
  "Default Intent for Carriage: 'Ask | 'Code | 'Hybrid."
  :type '(choice (const Ask) (const Code) (const Hybrid))
  :group 'carriage)

(defcustom carriage-mode-default-suite 'udiff
  "Default Suite: one of 'sre or 'udiff."
  :type '(choice (const sre) (const udiff))
  :group 'carriage)

(defcustom carriage-mode-default-model "gptel-default"
  "Default LLM model name for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-default-backend 'gptel
  "Default LLM transport backend for Carriage (e.g., 'gptel)."
  :type '(choice symbol string)
  :group 'carriage)

(defcustom carriage-mode-state-file ".context/carriage/carriage-state.el"
  "Project-relative path for per-project Carriage state persistence file."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-auto-open-report t
  "Open report buffer automatically after dry-run."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-show-diffs t
  "Require showing diffs before apply."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-auto-open-log nil
  "When non-nil, open *carriage-log* automatically on mode enable and when sending."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-auto-open-traffic nil
  "When non-nil, open *carriage-traffic* automatically when sending."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-confirm-apply-all t
  "Ask for confirmation before applying all blocks (C-c !)."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-use-icons t
  "Use all-the-icons in mode-line if available."
  :type 'boolean :group 'carriage)

;; UI v1.3 — Suite/Engine iconized labels
(defcustom carriage-mode-use-suite-icon t
  "When non-nil, show Suite label as an icon (with [value]) in mode-line."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-use-engine-icon t
  "When non-nil, show Engine label as an icon (with [value]) in mode-line."
  :type 'boolean :group 'carriage)

;; UI v1.3 — Flash and audio notifications
(defcustom carriage-mode-flash-patches t
  "When non-nil, flash last-iteration patch blocks on successful request completion."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-flash-duration 1.0
  "Flash duration (seconds) for highlighting generated patches."
  :type 'number :group 'carriage)

(defcustom carriage-mode-audio-notify nil
  "When non-nil, play a sound on successful request completion."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-audio-sound 'beep
  "Sound to use for notifications: 'beep or a path to a sound file."
  :type '(choice (const :tag "Beep" beep) (string :tag "Sound file path"))
  :group 'carriage)

(defun carriage--audio-notify-success ()
  "Play an audio notification according to Customize settings."
  (when (and (boundp 'carriage-mode-audio-notify) carriage-mode-audio-notify)
    (condition-case _e
        (let ((snd (and (boundp 'carriage-mode-audio-sound) carriage-mode-audio-sound)))
          (cond
           ((and (stringp snd) (file-exists-p snd))
            (play-sound-file snd))
           ((eq snd 'beep)
            (beep))
           (t
            (ignore-errors (beep)))))
      (error nil))))

(defcustom carriage-mode-include-reasoning 'block
  "Policy for including reasoning during streaming:
- 'block — print reasoning inside #+begin_reasoning/#+end_reasoning
- 'ignore — do not insert reasoning into the source buffer (still logged)."
  :type '(choice (const block) (const ignore))
  :group 'carriage)

;; v1.1 — Context toggles and limits
(defcustom carriage-mode-include-gptel-context t
  "When non-nil, include gptel-context (buffers/files) into the request context."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-include-doc-context t
  "When non-nil, include file contents listed in the nearest #+begin_context block."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-context-injection 'system
  "Where to inject collected context: 'system (default) or 'user."
  :type '(choice (const system) (const user))
  :group 'carriage)

(defcustom carriage-mode-context-max-files 100
  "Max number of files to include from context sources."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-context-max-total-bytes 1048576
  "Max total bytes of file contents included from context sources."
  :type 'integer :group 'carriage)

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
  "Maximum number of pairs allowed in an :op 'sre' block."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-sre-noop-on-zero-matches nil
  "When non-nil, treat :occur first with 0 matches as NOOP: report 'skip with a warning.
If nil (default v1 behavior), such cases are considered a failure in dry-run."
  :type 'boolean :group 'carriage)

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

(defcustom carriage-commit-default-message "carriage: apply changes"
  "Default commit message used by Commit commands.
May be a string or a function of zero args returning string."
  :type '(choice string function) :group 'carriage)

(defcustom carriage-mode-use-transient t
  "When non-nil, C-c e invokes Carriage menu (transient or fallback) in carriage-mode buffers.
When nil, C-c e acts purely as a prefix for keyspec sequences (no menu on bare C-c e)."
  :type 'boolean :group 'carriage)

(defcustom carriage-enable-legacy-bindings nil
  "When non-nil, enable legacy bindings (C-c M-RET / C-c RET) in carriage-mode buffers.
These are provided for compatibility and may be removed in a future release."
  :type 'boolean :group 'carriage)

(defvar-local carriage-mode-intent carriage-mode-default-intent
  "Current Carriage Intent for this buffer: 'Ask | 'Code | 'Hybrid.")

(defvar-local carriage-mode-suite carriage-mode-default-suite
  "Current Carriage Suite for this buffer.")

(defvar-local carriage-mode-model carriage-mode-default-model
  "Current Carriage model string for this buffer.")

(defvar-local carriage-mode-backend carriage-mode-default-backend
  "Current Carriage backend identifier (symbol or string) for this buffer.")

(defvar-local carriage-mode-provider nil
  "Current LLM provider slug for the backend (e.g., \"ai-tunnel\" for gptel), or nil.")

(defvar-local carriage--mode-prev-header-line-format nil
  "Saved previous value of =header-line-format' to restore on mode disable.")

(defvar-local carriage--mode-modeline-construct nil
  "The exact modeline construct object inserted by Carriage for later removal.")

(defvar-local carriage--abort-handler nil
  "Buffer-local abort handler function for the current Carriage activity, or nil.

The handler should be a zero-argument function that cancels the ongoing request or apply.
Set by transports/pipelines when starting an async activity; cleared on completion or when disabling carriage-mode.")

(defvar-local carriage--mode-emulation-map nil
  "Per-buffer emulation keymap used to provide Carriage prefix bindings without
populating =carriage-mode-map' with a real prefix (satisfies tests that expect
(bare) C-c e to be unbound in the mode map when transient=nil).")

(defvar-local carriage--emulation-map-alist nil
  "Buffer-local alist for emulation-mode-map-alists, mapping =carriage-mode' to
a per-buffer emulation keymap. This lets us provide prefix sequences in buffers
without turning =carriage-mode-map' into a real prefix.")

(defvar-local carriage--prev-local-function-key-map nil
  "Previous value of local-function-key-map before Carriage tweaks (if any).")

(defvar-local carriage--translation-map nil
  "Local translation keymap we may install when transient=t (reserved; may be nil).")

(defvar carriage-mode-map (make-sparse-keymap)
  "Keymap for carriage-mode.
Do not define bindings here; all key bindings are applied via keyspec and mode setup.")

;; Ensure menu command autoload is available even if keyspec isn't loaded yet.
;; This allows binding C-c e to open the menu immediately in carriage-mode buffers.
(autoload 'carriage-keys-open-menu "carriage-keyspec" "Open Carriage action menu from keyspec." t)

;;; Internal helpers split out of define-minor-mode

(defun carriage-mode--init-state ()
  "Preflight checks and buffer-local state initialization for Carriage."
  (unless (derived-mode-p 'org-mode)
    (if (bound-and-true-p noninteractive)
        (carriage-log "carriage-mode enabled outside org-mode (batch); limited UI")
      (progn
        (setq carriage-mode nil)
        (user-error "carriage-mode работает только в org-mode"))))
  (let ((root (carriage-project-root)))
    (unless root
      (setq carriage-mode nil)
      (user-error "Git repository not detected")))
  (setq carriage-mode-intent carriage-mode-default-intent)
  (setq carriage-mode-suite  carriage-mode-default-suite)
  (setq carriage-mode-model carriage-mode-default-model)
  (setq carriage-mode-backend carriage-mode-default-backend)
  (setq carriage-mode-provider nil)
  ;; Ensure default backend:model is present in registry for completion.
  (when (require 'carriage-llm-registry nil t)
    (let* ((bsym (if (symbolp carriage-mode-backend)
                     carriage-mode-backend
                   (intern (format "%s" carriage-mode-backend))))
           (backs (carriage-llm-available-backends)))
      (unless (member (symbol-name bsym) backs)
        (carriage-llm-register-backend bsym :models (list carriage-mode-model))))))

(defun carriage-mode--init-ui ()
  "Install header/modeline and key bindings; open optional panels."
  ;; UI (buffer-local, no global effects); respect batch/noninteractive
  (unless (bound-and-true-p noninteractive)
    (when carriage-mode-show-header-line
      (setq carriage--mode-prev-header-line-format header-line-format)
      (setq-local header-line-format '(:eval (carriage-ui--header-line)))
      (add-hook 'post-command-hook #'carriage-ui--headerline-post-command nil t)
      (add-hook 'window-scroll-functions #'carriage-ui--headerline-window-scroll nil t))
    (when carriage-mode-show-mode-line-ui
      (setq carriage--mode-modeline-construct '(:eval (carriage-ui--modeline)))
      (setq-local mode-line-format
                  (append mode-line-format (list carriage--mode-modeline-construct)))))
  (when (require 'carriage-keyspec nil t)
    (carriage-keys-apply-known-keymaps)
    (ignore-errors (carriage-keys-which-key-register))
    (let* ((base (string-trim-right (or carriage-keys-prefix "C-c e ")
                                    "[ \t\n\r]+")))
      (if (and (boundp 'carriage-mode-use-transient) carriage-mode-use-transient)
          (progn
            ;; transient=t: ensure bare C-c e opens the menu; do NOT bind longer sequences here
            (define-key carriage-mode-map (kbd base) #'carriage-keys-open-menu))
        ;; transient=nil: keep bare C-c e unbound; provide C-c e RET via emulation-mode-map-alists
        (progn
          (define-key carriage-mode-map (kbd base) nil)
          (setq carriage--mode-emulation-map (make-sparse-keymap))
          (define-key carriage--mode-emulation-map (kbd (concat base " RET")) #'carriage-send-buffer)
          (setq carriage--emulation-map-alist (list (cons 'carriage-mode carriage--mode-emulation-map)))
          (let ((lst (copy-sequence emulation-mode-map-alists)))
            (setq-local emulation-mode-map-alists
                        (if (member carriage--emulation-map-alist lst)
                            lst
                          (cons carriage--emulation-map-alist lst)))))))
    (when (and carriage-mode-auto-open-log (fboundp 'carriage-show-log))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (fboundp 'carriage-show-traffic))
      (ignore-errors (carriage-show-traffic)))
    (carriage-log "carriage-mode enabled in %s" (buffer-name))))

(defun carriage-mode--enable ()
  "Enable Carriage mode in the current buffer (internal)."
  (carriage-mode--init-state)
  (carriage-mode--init-ui))

(defun carriage-mode--disable ()
  "Disable Carriage mode in the current buffer (internal)."
  ;; Disable: restore header-line and remove modeline segment (buffer-local)
  (unless (bound-and-true-p noninteractive)
    (when (local-variable-p 'header-line-format)
      (setq-local header-line-format carriage--mode-prev-header-line-format))
    (setq carriage--mode-prev-header-line-format nil)
    (remove-hook 'post-command-hook #'carriage-ui--headerline-post-command t)
    (remove-hook 'window-scroll-functions #'carriage-ui--headerline-window-scroll t)
    (when (and carriage--mode-modeline-construct
               (local-variable-p 'mode-line-format))
      (setq-local mode-line-format
                  (delq carriage--mode-modeline-construct mode-line-format)))
    (setq carriage--mode-modeline-construct nil)
    ;; Clear abort handler and stop spinner if running
    (setq carriage--abort-handler nil)
    (when (fboundp 'carriage-ui--spinner-stop)
      (carriage-ui--spinner-stop t))
    ;; Remove per-buffer emulation maps we installed for this buffer
    (when (local-variable-p 'emulation-mode-map-alists)
      (let ((lst emulation-mode-map-alists))
        (setq-local emulation-mode-map-alists
                    (delq carriage--emulation-map-alist lst))))
    (setq carriage--emulation-map-alist nil)
    (setq carriage--mode-emulation-map nil)
    (force-mode-line-update t))
  (carriage-log "carriage-mode disabled in %s" (buffer-name)))

;;;###autoload
(define-minor-mode carriage-mode
  "Toggle Carriage minor mode for working with patch blocks in org buffers."
  :lighter (:eval (concat " Carriage" (carriage-ui-state-lighter)))
  :keymap carriage-mode-map
  :group 'carriage
  (if carriage-mode
      (carriage-mode--enable)
    (carriage-mode--disable)))

;; Streaming insertion state and helpers

(defvar-local carriage--stream-beg-marker nil
  "Buffer-local begin marker of the current streaming region.")
(defvar-local carriage--stream-end-marker nil
  "Buffer-local end marker of the current streaming region.")
(defvar-local carriage--stream-origin-marker nil
  "Buffer-local origin marker set at request time; first chunk will use it.")
(defvar-local carriage--reasoning-open nil
  "Non-nil when a #+begin_reasoning block is currently open for streaming.")
(defvar-local carriage--reasoning-tail-marker nil
  "Marker pointing to the end of reasoning content (where #+end_reasoning should be inserted).")
(defvar-local carriage--reasoning-beg-marker nil
  "Marker pointing to the beginning line of the current #+begin_reasoning block.")

(defun carriage-stream-reset (&optional origin-marker)
  "Reset streaming state for current buffer and set ORIGIN-MARKER if provided.
Does not modify buffer text; only clears markers/state so the next chunk opens a region."
  (setq carriage--stream-beg-marker nil)
  (setq carriage--stream-end-marker nil)
  (setq carriage--stream-origin-marker (and (markerp origin-marker) origin-marker))
  (setq carriage--reasoning-open nil)
  (setq carriage--reasoning-tail-marker nil)
  (setq carriage--reasoning-beg-marker nil)
  t)

(defun carriage-stream-region ()
  "Return (BEG . END) of current streaming region in this buffer, or nil."
  (when (and (markerp carriage--stream-beg-marker)
             (markerp carriage--stream-end-marker)
             (eq (marker-buffer carriage--stream-beg-marker) (current-buffer))
             (eq (marker-buffer carriage--stream-end-marker) (current-buffer)))
    (cons (marker-position carriage--stream-beg-marker)
          (marker-position carriage--stream-end-marker))))

(defun carriage--ensure-stream-region ()
  "Ensure streaming region exists. Use origin marker if set; otherwise current point."
  (unless (and (markerp carriage--stream-beg-marker)
               (markerp carriage--stream-end-marker))
    (let* ((pos (cond
                 ((and (markerp carriage--stream-origin-marker)
                       (buffer-live-p (marker-buffer carriage--stream-origin-marker)))
                  (marker-position carriage--stream-origin-marker))
                 (t (point)))))
      (save-excursion
        (goto-char pos)
        (unless (bolp) (insert "\n")))
      (setq carriage--stream-beg-marker (copy-marker pos t))
      (setq carriage--stream-end-marker (copy-marker pos t)))))

(defun carriage-begin-reasoning ()
  "Open a #+begin_reasoning block at the end of streaming region if not open.
Records begin and tail markers so main text can be inserted after reasoning
without auto-closing; the end marker is inserted later at tail."
  (when (eq carriage-mode-include-reasoning 'block)
    (carriage--ensure-stream-region)
    (carriage-log "reasoning: begin-request open=%s" carriage--reasoning-open)
    (unless carriage--reasoning-open
      (let ((inhibit-read-only t)
            (pos (marker-position carriage--stream-end-marker)))
        (save-excursion
          (goto-char pos)
          (insert "#+begin_reasoning\n")
          ;; After inserting, the begin line starts at POS. Record markers:
          (setq carriage--reasoning-beg-marker (copy-marker pos))
          ;; Tail starts where we are now (immediately after begin line)
          (setq carriage--reasoning-tail-marker (copy-marker (point) t))
          ;; Stream end also advances to current point
          (set-marker carriage--stream-end-marker (point) (current-buffer)))
        (setq carriage--reasoning-open t)
        (carriage-log "reasoning: begin inserted beg=%s tail=%s end=%s"
                      (and (markerp carriage--reasoning-beg-marker)
                           (marker-position carriage--reasoning-beg-marker))
                      (and (markerp carriage--reasoning-tail-marker)
                           (marker-position carriage--reasoning-tail-marker))
                      (and (markerp carriage--stream-end-marker)
                           (marker-position carriage--stream-end-marker)))))))

(defun carriage-end-reasoning ()
  "Close the #+begin_reasoning block if it is open. Do not fold here; folding is done on finalize."
  (carriage-log "reasoning: end-request open=%s" carriage--reasoning-open)
  (when carriage--reasoning-open
    (let ((inhibit-read-only t)
          (end-pos nil) (beg-pos nil)
          (skipped 'none)
          (prev-line "")
          (tailpos (or (and (markerp carriage--reasoning-tail-marker)
                            (marker-position carriage--reasoning-tail-marker))
                       (and (markerp carriage--stream-end-marker)
                            (marker-position carriage--stream-end-marker)))))
      (save-excursion
        (when (numberp tailpos) (goto-char tailpos))
        ;; Capture previous line at TAIL for diagnostics
        (setq prev-line (save-excursion
                          (forward-line -1)
                          (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
        ;; Locate matching begin for context and detect existing end marker between beg..end
        (when (re-search-backward "^[ \t]*#\\+begin_reasoning\\b" nil t)
          (setq beg-pos (match-beginning 0))
          (let* ((cur (marker-position carriage--stream-end-marker))
                 (have-end (save-excursion
                             (goto-char (1+ beg-pos))
                             (re-search-forward "^[ \t]*#\\+end_reasoning\\b" cur t))))
            (when have-end
              (setq skipped 'already-present))))
        ;; Quick guard: previous line already end marker
        (when (and (eq skipped 'none)
                   (string-match-p "^[ \t]*#\\+end_reasoning\\b" prev-line))
          (setq skipped 'prev-line))
        ;; Insert end marker at TAIL if not skipped
        (when (and (eq skipped 'none) (numberp tailpos))
          (goto-char tailpos)
          (unless (bolp) (insert "\n"))
          (insert "#+end_reasoning\n")
          (setq end-pos (point))
          ;; If stream-end was equal to tail, advance it; otherwise leave it (text already present beyond).
          (when (and (markerp carriage--stream-end-marker)
                     (= (marker-position carriage--stream-end-marker) tailpos))
            (set-marker carriage--stream-end-marker end-pos (current-buffer)))
          ;; Advance tail as well to the new end
          (when (markerp carriage--reasoning-tail-marker)
            (set-marker carriage--reasoning-tail-marker end-pos (current-buffer)))))
      (carriage-log "reasoning: end %s (end-pos=%s beg-pos=%s prev='%s')"
                    (pcase skipped
                      ('none "inserted")
                      ('prev-line "skipped-duplicate")
                      ('already-present "skipped-already-present")
                      (_ skipped))
                    (or end-pos -1) (or beg-pos -1)
                    (condition-case _ (substring prev-line 0 (min 80 (length prev-line))) (error prev-line)))))
  (setq carriage--reasoning-open nil)
  t)

(defun carriage--stream-insert-at-end (s)
  "Insert string S at the end of the current streaming region."
  (carriage--ensure-stream-region)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position carriage--stream-end-marker))
      (insert s)
      (set-marker carriage--stream-end-marker (point) (current-buffer)))))

;;;###autoload
(defun carriage-insert-stream-chunk (string &optional type)
  "Insert STRING chunk into the current buffer streaming region.
TYPE is either 'text (default) or 'reasoning.
- 'text: append to the region as-is (even if reasoning is open).
- 'reasoning: when carriage-mode-include-reasoning='block, ensure a #+begin_reasoning
  and append to the reasoning tail marker so that main text remains outside the block."
  (let ((s (or string "")))
    (pcase type
      ((or 'reasoning :reasoning)
       (when (eq carriage-mode-include-reasoning 'block)
         (carriage-begin-reasoning)
         (let ((inhibit-read-only t))
           (save-excursion
             (let* ((tail (or carriage--reasoning-tail-marker carriage--stream-end-marker))
                    (tailpos (marker-position tail)))
               (goto-char tailpos)
               (insert s)
               (let ((newpos (point)))
                 ;; Advance tail; if end==tail, advance end as well
                 (when (markerp carriage--reasoning-tail-marker)
                   (set-marker carriage--reasoning-tail-marker newpos (current-buffer)))
                 (when (and (markerp carriage--stream-end-marker)
                            (= (marker-position carriage--stream-end-marker) tailpos))
                   (set-marker carriage--stream-end-marker newpos (current-buffer)))))))))
      (_
       ;; Do not auto-close reasoning here; text is appended after all prior content.
       (carriage--stream-insert-at-end s))))
  (point))

;;;###autoload
(defun carriage-stream-finalize (&optional errorp mark-last-iteration)
  "Finalize the current streaming session.
- Ensure any open reasoning block is closed.
- Fold the reasoning block after the full answer is printed.
- When MARK-LAST-ITERATION and not ERRORP: mark the inserted region as last iteration.
- Trigger UI effects (flash/audio) on success."
  ;; Close reasoning if still open (end marker inserted at tail)
  (ignore-errors (carriage-end-reasoning))
  ;; Fold reasoning now (after the whole response), using recorded begin marker if available.
  (when (and (not errorp)
             (markerp carriage--reasoning-beg-marker))
    (condition-case _e
        (progn
          (require 'org)
          (let ((beg (marker-position carriage--reasoning-beg-marker)))
            (cond
             ((fboundp 'org-fold-hide-drawer-or-block)
              (save-excursion
                (goto-char beg)
                (org-fold-hide-drawer-or-block t)))
             ((fboundp 'org-hide-block-toggle)
              (save-excursion
                (goto-char beg)
                (org-hide-block-toggle t)))
             ((featurep 'org-fold)
              (save-excursion
                (goto-char beg)
                ;; Best effort: fold the block body region
                (let ((body-beg (progn (forward-line 1) (point)))
                      (body-end (save-excursion
                                  (when (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
                                    (forward-line -1))
                                  (line-end-position))))
                  (org-fold-region body-beg body-end t))))
             (t nil))))
      (error nil)))
  (when (and (not errorp) mark-last-iteration)
    (let ((r (carriage-stream-region)))
      (when (and (consp r)
                 (numberp (car r)) (numberp (cdr r))
                 (< (car r) (cdr r)))
        (carriage-mark-last-iteration (car r) (cdr r)))))
  ;; Effects on success
  (when (not errorp)
    (when (and (boundp 'carriage-mode-flash-patches) carriage-mode-flash-patches
               (fboundp 'carriage-ui--flash-last-iteration-patches))
      (carriage-ui--flash-last-iteration-patches (current-buffer)))
    (when (fboundp 'carriage--audio-notify-success)
      (carriage--audio-notify-success)))
  t)

;;; Prompt construction helpers

;; v1.1 — Полный идентификатор модели для tooltip в модлайне.
(defun carriage-llm-full-id (&optional backend provider model)
  "Return normalized full LLM id backend[:provider]:model for current buffer or given args.
Deduplicates segments if MODEL already contains provider/backend."
  (let* ((be (or backend (and (boundp 'carriage-mode-backend) carriage-mode-backend)))
         (pr (or provider (and (boundp 'carriage-mode-provider) carriage-mode-provider)))
         (mo (or model   (and (boundp 'carriage-mode-model)   carriage-mode-model)))
         (be-str (cond
                  ((symbolp be) (symbol-name be))
                  ((stringp be) be)
                  ((null be) "")
                  (t (format "%s" be))))
         (pr-str (cond
                  ((symbolp pr) (symbol-name pr))
                  ((stringp pr) pr)
                  ((null pr) "")
                  (t (format "%s" pr))))
         (mo-str (cond
                  ((symbolp mo) (symbol-name mo))
                  ((stringp mo) mo)
                  ((null mo) "")
                  (t (format "%s" mo)))))
    (let* ((parts (and (stringp mo-str) (not (string-empty-p mo-str))
                       (split-string mo-str ":" t)))
           (n (length parts)))
      (cond
       ;; No model → return backend (or empty)
       ((or (null parts) (zerop n))
        (or be-str ""))
       ;; MODEL already like "backend:...": return as-is
       ((and (not (string-empty-p be-str))
             (string-prefix-p (concat be-str ":") mo-str))
        mo-str)
       ;; MODEL has two parts "provider:model" → prefix backend
       ((= n 2)
        (if (and (not (string-empty-p be-str)))
            (concat be-str ":" mo-str)
          mo-str))
       ;; MODEL has ≥3 parts → assume fully-qualified and return as-is
       ((>= n 3)
        mo-str)
       ;; MODEL is a bare name → compose "backend[:provider]:model" with dedup when pr==backend
       (t
        (if (string-empty-p be-str)
            mo-str
          (concat be-str
                  (if (and (not (string-empty-p pr-str))
                           (not (string= pr-str be-str)))
                      (concat ":" pr-str)
                    "")
                  ":" mo-str)))))))

(defun carriage--modeline-attach-model-tooltip (ret)
  "Attach help-echo with full model id to the model segment inside RET (modeline construct).
RET can be a string or a list; this function is defensive and no-ops if not in carriage-mode."
  (condition-case _e
      (if (and (bound-and-true-p carriage-mode)
               (boundp 'carriage-mode-model) carriage-mode-model)
          (let* ((base carriage-mode-model)
                 (full (carriage-llm-full-id))
                 (_ (require 'carriage-i18n nil t))
                 (templ (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :model-tooltip "%s")
                          "Модель: %s"))
                 (help (format templ full))
                 (prop (lambda (s)
                         (if (and (stringp s) (string-match (regexp-quote base) s))
                             (replace-match (propertize base 'help-echo help) t t s)
                           s))))
            (cond
             ((stringp ret) (funcall prop ret))
             ((consp ret)   (mapcar (lambda (el) (if (stringp el) (funcall prop el) el)) ret))
             (t ret)))
        ret)
    (error ret)))

;; Register advice once (global), but it is effectively active only in carriage-mode buffers.
(when (fboundp 'carriage-ui--modeline)
  (unless (advice-member-p #'carriage--modeline-attach-model-tooltip 'carriage-ui--modeline)
    (advice-add 'carriage-ui--modeline :filter-return #'carriage--modeline-attach-model-tooltip)))

(defun carriage--build-context (source buffer)
  "Return context plist for prompt builder with at least :payload.
SOURCE is 'buffer or 'subtree. BUFFER is the source buffer.
May include :context-text and :context-target per v1.1."
  (with-current-buffer buffer
    (let* ((mode (buffer-local-value 'major-mode buffer))
           (payload
            (pcase source
              ('subtree
               (if (eq mode 'org-mode)
                   (save-excursion
                     (require 'org)
                     (ignore-errors (org-back-to-heading t))
                     (let ((beg (save-excursion (org-back-to-heading t) (point)))
                           (end (save-excursion (org-end-of-subtree t t) (point))))
                       (buffer-substring-no-properties beg end)))
                 (buffer-substring-no-properties (point-min) (point-max))))
              (_ (buffer-substring-no-properties (point-min) (point-max)))))
           (target (if (boundp 'carriage-mode-context-injection)
                       carriage-mode-context-injection
                     'system))
           (ctx-text
            (condition-case _e
                (when (require 'carriage-context nil t)
                  (let ((col (carriage-context-collect buffer (or (carriage-project-root) default-directory))))
                    (when (and col
                               (or (and (boundp 'carriage-mode-include-gptel-context)
                                        carriage-mode-include-gptel-context)
                                   (and (boundp 'carriage-mode-include-doc-context)
                                        carriage-mode-include-doc-context)))
                      ;; Traffic: log context summary and individual elements (paths only)
                      (let* ((files (plist-get col :files))
                             (stats (plist-get col :stats))
                             (inc   (and stats (plist-get stats :included)))
                             (sk    (and stats (plist-get stats :skipped)))
                             (bytes (and stats (plist-get stats :total-bytes))))
                        (carriage-traffic-log 'out "context: target=%s included=%s skipped=%s total-bytes=%s"
                                              target (or inc 0) (or sk 0) (or bytes 0))
                        (dolist (f files)
                          (let ((rel (plist-get f :rel))
                                (included (plist-get f :content))
                                (reason (plist-get f :reason)))
                            (carriage-traffic-log 'out " - %s (%s)"
                                                  rel
                                                  (if (stringp included)
                                                      "included"
                                                    (format "omitted%s"
                                                            (if reason (format ": %s" reason) "")))))))
                      (carriage-context-format col :where target))))
              (error nil))))
      (let ((res (list :payload payload)))
        (when (and (stringp ctx-text) (not (string-empty-p ctx-text)))
          (setq res (append res (list :context-text ctx-text :context-target target))))
        res))))

;;; Commands (stubs/minimal implementations)

;;;###autoload
(defun carriage-send-buffer ()
  "Send entire buffer to LLM according to current Intent/Suite."
  (interactive)
  (let* ((backend carriage-mode-backend)
         (model   carriage-mode-model)
         (intent  carriage-mode-intent)
         (suite   carriage-mode-suite)
         (srcbuf  (current-buffer))
         (origin-marker (copy-marker (point) t))
         (ctx     (carriage--build-context 'buffer srcbuf))
         (built nil) (sys nil) (pr nil))

    (setq built (carriage-build-prompt intent suite ctx)
          sys   (plist-get built :system)
          pr    (plist-get built :prompt))
    (carriage-log "send-buffer: intent=%s suite=%s backend=%s model=%s"
                  intent suite backend model)
    (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-traffic)))
    (carriage--ensure-transport)
    (carriage-stream-reset origin-marker)
    (let* ((unreg (carriage-transport-begin)))
      (carriage-traffic-log 'out "request begin: source=buffer backend=%s model=%s"
                            backend model)
      (condition-case err
          (progn
            ;; Dispatch via transport (placeholder will log error if no adapter).
            (carriage-transport-dispatch :source 'buffer
                                         :backend backend
                                         :model model
                                         :prompt pr
                                         :system sys
                                         :buffer srcbuf
                                         :mode (symbol-name (buffer-local-value 'major-mode srcbuf))
                                         :insert-marker origin-marker)
            t)
        (error
         (carriage-log "send-buffer error: %s" (error-message-string err))
         (carriage-transport-complete t))))))

;;;###autoload
(defun carriage-send-subtree ()
  "Send current org subtree to LLM according to current Intent/Suite."
  (interactive)
  (let* ((backend carriage-mode-backend)
         (model   carriage-mode-model)
         (intent  carriage-mode-intent)
         (suite   carriage-mode-suite)
         (srcbuf  (current-buffer))
         (origin-marker (copy-marker (point) t))
         (ctx     (carriage--build-context 'subtree srcbuf))
         (built nil) (sys nil) (pr nil))

    (setq built (carriage-build-prompt intent suite ctx)
          sys   (plist-get built :system)
          pr    (plist-get built :prompt))
    (carriage-log "send-subtree: intent=%s suite=%s backend=%s model=%s"
                  intent suite backend model)
    ;; Best-effort derive a small payload boundary for logs
    (when (derived-mode-p 'org-mode)
      (carriage-log "send-subtree: org-mode detected; using subtree-at-point as payload"))
    (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-traffic)))
    (carriage--ensure-transport)
    (let* ((unreg (carriage-transport-begin)))
      (carriage-traffic-log 'out "request begin: source=subtree backend=%s model=%s"
                            backend model)
      (condition-case err
          (progn
            (carriage-transport-dispatch :source 'subtree
                                         :backend backend
                                         :model model
                                         :prompt pr
                                         :system sys
                                         :buffer srcbuf
                                         :mode (symbol-name (buffer-local-value 'major-mode srcbuf))
                                         :insert-marker origin-marker)
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
  "Dry-run → confirm → apply for the patch block at point (async by default)."
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
          (if (and (boundp 'carriage-apply-async) carriage-apply-async (not noninteractive))
              (progn
                (carriage-log "apply-at-point: async apply scheduled")
                (carriage-apply-plan-async
                 (list plan-item) root
                 (lambda (rep)
                   (when (not noninteractive)
                     (carriage-report-open rep))
                   (carriage-ui-set-state 'idle))))
            (let ((ap (carriage-apply-plan (list plan-item) root)))
              (when (not noninteractive)
                (carriage-report-open ap))
              (carriage-ui-set-state 'idle))))))))

;;;###autoload
(defun carriage-apply-last-iteration ()
  "Dry-run → подтверждение → применение всех блоков «последней итерации» (async по умолчанию)."
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
            (if (and (boundp 'carriage-apply-async) carriage-apply-async (not noninteractive))
                (progn
                  (carriage-log "apply-all: async apply scheduled (%d items)" (length plan))
                  (carriage-apply-plan-async
                   plan root
                   (lambda (rep)
                     (when (not noninteractive)
                       (carriage-report-open rep))
                     (carriage-ui-set-state 'idle))))
              (let ((ap (carriage-apply-plan plan root)))
                (when (not noninteractive)
                  (carriage-report-open ap))
                (carriage-ui-set-state 'idle)))))))))

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

(defun carriage--commit--default-message ()
  "Return default commit message from `carriage-commit-default-message'."
  (let ((v (and (boundp 'carriage-commit-default-message) carriage-commit-default-message)))
    (cond
     ((functionp v) (ignore-errors (funcall v)))
     ((stringp v) v)
     (t "carriage: apply changes"))))

;;;###autoload
(defun carriage-commit-changes (&optional message)
  "Commit all changes according to staging policy as a single commit.
- If staging policy is 'none, stage everything (git add -A) before commit.
- If staging policy is 'index, commit current index as-is."
  (interactive)
  (let* ((root (carriage-project-root))
         (msg (or message (read-string "Commit message: " (carriage--commit--default-message)))))
    (unless root
      (user-error "Git repository not detected"))
    (when (not (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index))
      (ignore-errors (carriage-git-add-all root)))
    (let* ((res (carriage-git-commit root msg))
           (exit (plist-get res :exit))
           (stderr (string-trim (or (plist-get res :stderr) ""))))
      (if (and exit (zerop exit))
          (message "Carriage: committed changes")
        (user-error "Commit failed: %s" (if (string-empty-p stderr) (format "%S" res) stderr))))))

;;;###autoload
(defun carriage-commit-last-iteration (&optional message)
  "Commit only files changed by the last iteration as a single commit.
Stages as needed depending on staging policy; with 'none, runs git add -A then restricts commit to those paths."
  (interactive)
  (let* ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (let* ((plan (carriage-collect-last-iteration-blocks root))
           (files
            (cl-loop for it in plan
                     for op = (alist-get :op it)
                     append
                     (pcase op
                       ((or 'sre 'create 'delete)
                        (let ((f (alist-get :file it))) (if f (list f) '())))
                       ('patch
                        (let ((p (alist-get :path it))) (if p (list p) '())))
                       ('rename
                        (delq nil (list (alist-get :from it) (alist-get :to it))))
                       (_ '()))))
           (msg (or message (read-string "Commit message: " (carriage--commit--default-message)))))
      (when (null files)
        (user-error "No files in last iteration"))
      ;; Stage as necessary
      (when (not (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index))
        (ignore-errors (carriage-git-add-all root)))
      ;; Commit restricted to files
      (let* ((res (apply #'carriage-git-commit root msg files))
             (exit (plist-get res :exit))
             (stderr (string-trim (or (plist-get res :stderr) ""))))
        (if (and exit (zerop exit))
            (message "Carriage: committed last iteration (%d file(s))" (length files))
          (user-error "Commit failed: %s" (if (string-empty-p stderr) (format "%S" res) stderr)))))))

;;;###autoload
(defun carriage-toggle-intent ()
  "Cycle Intent: Ask → Code → Hybrid → Ask."
  (interactive)
  (setq carriage-mode-intent
        (pcase carriage-mode-intent
          ('Ask 'Code)
          ('Code 'Hybrid)
          (_ 'Ask)))
  (message "Carriage intent: %s" carriage-mode-intent)
  (force-mode-line-update t))

;;;###autoload
(defun carriage-select-suite (&optional suite)
  "Select Suite (sre|aibo|udiff)."
  (interactive)
  (let* ((choices
          (condition-case _e
              (let ((ids (and (fboundp 'carriage-suite-ids) (carriage-suite-ids))))
                (if (and ids (listp ids))
                    (mapcar (lambda (s) (if (symbolp s) (symbol-name s) (format "%s" s))) ids)
                  '("sre" "aibo" "udiff")))
            (error '("sre" "aibo" "udiff"))))
         (default (if (symbolp carriage-mode-suite)
                      (symbol-name carriage-mode-suite)
                    (or carriage-mode-suite "udiff")))
         (sel (or suite (completing-read "Suite: " choices nil t default))))
    (setq carriage-mode-suite (intern sel))
    (message "Carriage suite: %s" carriage-mode-suite)
    (force-mode-line-update t)))

;;;###autoload
(defun carriage-select-model (&optional model)
  "Select LLM MODEL for Carriage, optionally as \"backend:model\".

When registry has entries, offer completion over:
- current backend's models, and
- combined \"backend:model\" candidates.

If the choice is \"backend:model\" (or \"backend:provider:model\"), backend and model are updated.
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
         (def-full (and collection
                        (carriage-llm-default-candidate bcur carriage-mode-model pairs carriage-mode-provider)))
         (prompt (if collection
                     (format "Model (or backend:model) [%s]: " bcur)
                   "Model: "))
         (choice (or model
                     (if collection
                         (let* ((initial (if (and (stringp carriage-mode-model)
                                                  (string= carriage-mode-model "gptel-default"))
                                             ""
                                           def-full))
                                (def (unless (and (stringp carriage-mode-model)
                                                  (string= carriage-mode-model "gptel-default"))
                                       def-full)))
                           (completing-read prompt collection nil t initial nil def))
                       (let ((initial (if (and (stringp carriage-mode-model)
                                               (string= carriage-mode-model "gptel-default"))
                                          ""
                                        def-full)))
                         (read-string prompt initial))))))
    ;; Apply selection:
    ;; When choice contains ':', treat first segment as backend and last segment as model;
    ;; otherwise treat it as plain model (keep backend unchanged).
    (let* ((parts (when (stringp choice) (split-string choice ":" t)))
           (backend  (and parts (car parts)))
           (provider (and (>= (length parts) 3) (nth 1 parts)))
           (model-str (if (and parts (>= (length parts) 2))
                          (car (last parts))
                        choice)))
      (when (and backend (not (string-empty-p backend)))
        (setq carriage-mode-backend (intern backend)))
      (setq carriage-mode-provider (and (stringp provider) (not (string-empty-p provider)) provider))
      (setq carriage-mode-model model-str)
      ;; Ensure registry has backend->model mapping for future completion.
      (when (require 'carriage-llm-registry nil t)
        (let* ((bsym (if (symbolp carriage-mode-backend)
                         carriage-mode-backend
                       (intern (format "%s" carriage-mode-backend))))
               (existing (or (carriage-llm-available-models bsym) '())))
          (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
      (message "Carriage model: %s (backend %s%s)"
               carriage-mode-model
               (if (symbolp carriage-mode-backend)
                   (symbol-name carriage-mode-backend)
                 carriage-mode-backend)
               (if carriage-mode-provider
                   (format " provider %s" carriage-mode-provider)
                 ""))
      (force-mode-line-update t)
      (cons carriage-mode-backend carriage-mode-model))))

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
    (let* ((chunks '()))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
        (let* ((beg (match-beginning 0)))
          (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (goto-char (point-max)))
          (let* ((end (line-end-position)))
            (push (buffer-substring-no-properties beg end) chunks))
          (forward-line 1)))
      (mapconcat #'identity (nreverse chunks) "\n\n"))))

(defun carriage--sre--rewrite-delim-markers (body old new)
  "Rewrite create segment markers in BODY from OLD to NEW token.
Only rewrites full marker lines:
- <<OLD
- :OLD
Returns modified BODY string."
  (let* ((rx-open (concat "^[ \t]*<<\\(" (regexp-quote (or old "")) "\\)[ \t]*$"))
         (rx-close (concat "^[ \t]*:\\(" (regexp-quote (or old "")) "\\)[ \t]*$"))
         (s1 (replace-regexp-in-string rx-open (concat "<<" new) (or body "") t t))
         (s2 (replace-regexp-in-string rx-close (concat ":" new) s1 t t)))
    s2))

(defun carriage--sanitize-llm-response (raw)
  "Return only sanitized #+begin_patch blocks from RAW.

Sanitization rules:
- Keep only begin_patch blocks; drop any text outside blocks.
- For :op create: ensure :delim is a 6-hex token in header.
  If missing/invalid, generate a token and rewrite markers in body (<<TOK and :TOK)."
  (with-temp-buffer
    (insert (or raw ""))
    (goto-char (point-min))
    (let ((acc '()))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
        (let* ((hdr-str (match-string 1))
               (hdr     (car (read-from-string hdr-str)))
               (op      (plist-get hdr :op))
               (opstr   (format "%s" op))
               (body-beg (progn (forward-line 1) (point)))
               (body-end (progn
                           (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                             (goto-char (point-max)))
                           (line-beginning-position)))
               (body (buffer-substring-no-properties body-beg body-end)))
          (when (string-prefix-p ":" (or opstr ""))
            (setq opstr (substring opstr 1)))
          (let* ((needs-delim (member opstr '("create")))
                 (delim (plist-get hdr :delim))
                 (delim-ok (and (stringp delim)
                                (string-match-p "\\`[0-9a-f]\\{6\\}\\'" delim)))
                 (oldtok (cond
                          ((stringp delim) delim)
                          (t (with-temp-buffer
                               (insert body)
                               (goto-char (point-min))
                               (when (re-search-forward "^[ \t]*<<\\([^ \t\n]+\\)[ \t]*$" nil t)
                                 (match-string 1))))))
                 (newtok (if (and needs-delim delim-ok) delim (carriage-generate-delim)))
                 (body1  body)
                 (hdr1   hdr))
            (when needs-delim
              (setq hdr1 (plist-put hdr1 :delim newtok))
              (when (and oldtok (not (string= oldtok newtok)))
                (setq body1 (carriage--sre--rewrite-delim-markers body oldtok newtok))))
            (let* ((hdr-print (prin1-to-string hdr1))
                   (block (concat "#+begin_patch " hdr-print "\n" body1 "\n#+end_patch\n")))
              (push block acc))))
        (forward-line 1))
      (mapconcat #'identity (nreverse acc) "\n"))))

(defun carriage--accept-insertion-target (insert-marker)
  "Return (cons BUFFER . POS) for insertion target based on INSERT-MARKER."
  (if (and (markerp insert-marker)
           (buffer-live-p (marker-buffer insert-marker)))
      (cons (marker-buffer insert-marker) (marker-position insert-marker))
    (cons (current-buffer) nil)))

(defun carriage--insert-blocks-and-mark (buf pos blocks)
  "Insert BLOCKS into BUF at POS (or point if nil), mark last iteration.
Return cons (BEG . END) of inserted region."
  (with-current-buffer buf
    (save-excursion
      (when pos (goto-char pos))
      (let* ((ins-beg (point)))
        (unless (bolp) (insert "\n"))
        (insert blocks "\n")
        (let* ((ins-end (point)))
          (carriage-log "accept: inserted region %d..%d (%d chars)"
                        ins-beg ins-end (- ins-end ins-beg))
          (carriage-mark-last-iteration ins-beg ins-end)
          (cons ins-beg ins-end))))))

(defun carriage--dry-run-last-iteration (root)
  "Collect last-iteration blocks for ROOT, run dry-run, open report if configured.
Return the dry-run report plist."
  (carriage-log "accept: collecting last-iteration blocks…")
  (let* ((plan (carriage-collect-last-iteration-blocks root)))
    (carriage-log "accept: plan-size=%d" (length plan))
    (carriage-ui-set-state 'dry-run)
    (let ((rep (carriage-dry-run-plan plan root)))
      (when (and carriage-mode-auto-open-report (not noninteractive))
        (carriage-report-open rep))
      (carriage-ui-set-state 'idle)
      rep)))

;;;###autoload
(defun carriage-accept-llm-response (&optional input insert-marker)
  "Accept an LLM response INPUT, keep only begin_patch blocks, insert and dry-run.

- Sanitizes begin_patch blocks (enforces :delim only for create).
- Inserts into buffer at INSERT-MARKER or point.
- Marks the region as the last iteration and runs dry-run with report."
  (interactive
   (list (read-string "Paste LLM response (only begin_patch blocks will be kept):\n")))
  (let* ((raw (or input ""))
         (sanitized (carriage--sanitize-llm-response raw))
         (blocks (carriage--extract-patch-blocks sanitized)))
    (when (or (null blocks) (string-empty-p (string-trim blocks)))
      (user-error "No begin_patch blocks found in input"))
    (let* ((target (carriage--accept-insertion-target insert-marker))
           (buf (car target))
           (pos (cdr target)))
      (with-current-buffer buf
        (let* ((root (or (carriage-project-root) default-directory)))
          (carriage-traffic-log 'in "Accepted LLM response (%d chars)" (length raw))
          (carriage-log "accept: extracted blocks bytes=%d" (string-bytes blocks))
          (ignore (carriage--insert-blocks-and-mark buf pos blocks))
          (carriage--dry-run-last-iteration root))))))

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

;;;###autoload
(defun carriage-toggle-include-gptel-context ()
  "Toggle including gptel-context (buffers/files) into the request context."
  (interactive)
  (setq carriage-mode-include-gptel-context (not carriage-mode-include-gptel-context))
  (message "Include gptel-context: %s" (if carriage-mode-include-gptel-context "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-doc-context ()
  "Toggle including file contents from the nearest #+begin_context block in the document."
  (interactive)
  (setq carriage-mode-include-doc-context (not carriage-mode-include-doc-context))
  (message "Include #+begin_context files: %s" (if carriage-mode-include-doc-context "on" "off"))
  (force-mode-line-update t))

;; -------------------------------------------------------------------
;; Project-scoped ephemeral buffer (open-buffer) and exit prompt

(defvar carriage--project-buffers (make-hash-table :test 'equal)
  "Map of project-root (string) → live buffer for Carriage ephemeral project buffers.")

(defvar carriage--ephemeral-exit-hook-installed nil
  "Guard to install kill-emacs query hook only once per session.")

(defvar-local carriage--ephemeral-project-buffer nil
  "Non-nil in buffers created by =carriage-open-buffer' (ephemeral, not visiting a file by default).")

(defun carriage--project-name-from-root (root)
  "Return project name (basename of ROOT directory)."
  (file-name-nondirectory (directory-file-name (or root default-directory))))

(defun carriage--ensure-ephemeral-exit-hook ()
  "Install a kill-emacs query hook to offer saving ephemeral project buffers."
  (unless carriage--ephemeral-exit-hook-installed
    (setq carriage--ephemeral-exit-hook-installed t)
    (add-hook
     'kill-emacs-query-functions
     (lambda ()
       (let* ((bufs (cl-loop for k being the hash-keys of carriage--project-buffers
                             for b = (gethash k carriage--project-buffers)
                             when (and (buffer-live-p b)
                                       (with-current-buffer b
                                         (and carriage--ephemeral-project-buffer
                                              (buffer-modified-p))))
                             collect b))
              (need (and bufs (> (length bufs) 0))))
         (if (not need)
             t
           (when (y-or-n-p "Carriage: save ephemeral project buffers to files before exit? ")
             (dolist (b bufs)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (when (and carriage--ephemeral-project-buffer
                              (buffer-modified-p))
                     ;; Offer a filename interactively
                     (call-interactively #'write-file))))))
           t))))))

;;;###autoload
(defun carriage-open-buffer ()
  "Open or switch to the Carriage ephemeral buffer for the current project.
Creates an org-mode buffer with carriage-mode enabled and default-directory bound to the project root."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (pname (carriage--project-name-from-root root))
         (bname (format "*carriage:%s*" (or (and pname (not (string-empty-p pname)) pname) "-")))
         (existing (gethash root carriage--project-buffers))
         (buf (if (and (buffer-live-p existing)) existing (get-buffer-create bname))))
    (puthash root buf carriage--project-buffers)
    (carriage--ensure-ephemeral-exit-hook)
    (unless (get-buffer-window buf t)
      (pop-to-buffer buf))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory (expand-file-name root)))
      (setq carriage--ephemeral-project-buffer t)
      (unless (derived-mode-p 'org-mode)
        (ignore-errors (org-mode)))
      ;; Ensure carriage-mode is enabled
      (unless (bound-and-true-p carriage-mode)
        (carriage-mode 1)))
    buf))

(provide 'carriage-mode)
;;; carriage-mode.el ends here
