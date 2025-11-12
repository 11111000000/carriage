;;; carriage-ui.el --- Keymap and minimal UI helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-utils)
(require 'carriage-llm-registry)
(require 'carriage-perf nil t)
(declare-function carriage-select-apply-engine "carriage-apply-engine" (&optional engine))

(defgroup carriage-ui nil
  "UI components for Carriage."
  :group 'applications
  :prefix "carriage-")

(defcustom carriage-mode-icon-v-adjust -0.00
  "Vertical offset for all-the-icons in Carriage modeline/header-line.
Negative values move icons up; positive move them down."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-mode-icon-height 0.82
  "Uniform icon height scale for all-the-icons in mode-line/header-line."
  :type 'number
  :group 'carriage-ui)

;; Debug logging controls
(defcustom carriage-ui-debug nil
  "When non-nil, log debug info about icon faces/colors in modeline."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-debug-max 200
  "Max number of debug log entries to emit per session."
  :type 'integer
  :group 'carriage-ui)

(defvar carriage-ui--debug-count 0
  "Counter of emitted debug log entries.")

(defun carriage-ui--dbg (fmt &rest args)
  "Internal logger with throttling for Carriage UI."
  (when (and carriage-ui-debug
             (or (not (numberp carriage-ui-debug-max))
                 (< carriage-ui--debug-count carriage-ui-debug-max)))
    (setq carriage-ui--debug-count (1+ carriage-ui--debug-count))
    (apply #'message (concat "[carriage-ui] " fmt) args)))

(defun carriage-ui--faceprop-foreground (fp)
  "Extract foreground color from face property FP (symbol/list/plist)."
  (cond
   ((null fp) nil)
   ((symbolp fp) (ignore-errors (face-attribute fp :foreground nil 'default)))
   ((and (listp fp) (keywordp (car fp))) (plist-get fp :foreground))
   ((listp fp)
    (let* ((fg nil))
      (dolist (el fp)
        (unless fg
          (setq fg (carriage-ui--faceprop-foreground el))))
      fg))
   (t nil)))

(defun carriage-ui--log-face-prop (tag fp)
  "Log face property FP with TAG."
  (carriage-ui--dbg "%s face=%s fg=%s"
                    tag
                    (prin1-to-string fp)
                    (or (carriage-ui--faceprop-foreground fp) "-")))

(defun carriage-ui--log-face-of-string (tag s)
  "Log face property of string S with TAG."
  (when (and (stringp s) (> (length s) 0))
    (carriage-ui--log-face-prop tag (get-text-property 0 'face s))))

;; Harmonious palette with explicit foregrounds (avoid theme desaturation to gray)
(defface carriage-ui-accent-blue-face
  '((t :inherit nil :foreground "#6fa8dc"))
  "Accent face (blue-ish) for UI icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-green-face
  '((t :inherit nil :foreground "#93c47d"))
  "Accent face (green) for success/apply icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-yellow-face
  '((t :inherit nil :foreground "#f1c232"))
  "Accent face (yellow) for caution/WIP icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-red-face
  '((t :inherit nil :foreground "#e06666"))
  "Accent face (red) for abort/error icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-purple-face
  '((t :inherit nil :foreground "#8e7cc3"))
  "Accent face (purple) for code/report/confirm icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-orange-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Accent face (orange) for diff/dry icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-cyan-face
  '((t :inherit nil :foreground "#76a5af"))
  "Accent face (cyan/teal) for model/reset/icons."
  :group 'carriage-ui)

(defface carriage-ui-muted-face
  '((t :inherit nil :foreground "#9e9e9e"))
  "Muted face for disabled toggle icons."
  :group 'carriage-ui)

;; State faces (UI v1.3)
(defface carriage-ui-state-idle-face
  '((t :inherit nil :foreground "#268bd2"))
  "Mode-line face for idle state (blue)."
  :group 'carriage-ui)

(defface carriage-ui-state-sending-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for sending/streaming states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-error-face
  '((t :inherit nil :foreground "#e06666"))
  "Mode-line face for error state (red)."
  :group 'carriage-ui)

;; New state faces for refined color mapping
(defface carriage-ui-state-success-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for success/idle/done states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-active-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Mode-line face for active reasoning/waiting/streaming/dispatch states (orange)."
  :group 'carriage-ui)

;; Faces for patch block highlighting (spec/ui-v1.org)
(defface carriage-patch-valid-face
  '((t :inherit nil :background "#203a24"))
  "Face for visually marking valid patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-warning-face
  '((t :inherit nil :background "#3a2f20"))
  "Face for visually marking suspicious patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-error-face
  '((t :inherit nil :background "#3a2020"))
  "Face for visually marking erroneous patch blocks."
  :group 'carriage-ui)

;; Faces for report rows (OK/WARN/ERR)
(defface carriage-report-ok-face
  '((t :inherit success))
  "Face for OK rows in report."
  :group 'carriage-ui)

(defface carriage-report-warn-face
  '((t :inherit warning))
  "Face for WARN rows in report."
  :group 'carriage-ui)

(defface carriage-report-err-face
  '((t :inherit error))
  "Face for ERR rows in report."
  :group 'carriage-ui)

;; carriage-mode-map moved to carriage-mode.el (UI must not define keymaps; keys go via keyspec)

(defvar-local carriage--ui-state 'idle
  "Current UI state: one of 'idle 'sending 'streaming 'dry-run 'apply 'error.")

;; Spinner state (buffer-local)
(defvar-local carriage--ui-spinner-timer nil
  "Buffer-local spinner timer for Carriage mode-line.")
(defvar-local carriage--ui-spinner-index 0
  "Current spinner frame index (buffer-local).")
(defconst carriage--ui-spinner-frames-unicode
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Unicode spinner frames.")
(defconst carriage--ui-spinner-frames-ascii
  ["-" "\\" "|" "/"]
  "ASCII spinner frames (TTY fallback).")

(defun carriage-ui--spinner-frames ()
  "Return vector of spinner frames appropriate for current display."
  (if (display-graphic-p)
      carriage--ui-spinner-frames-unicode
    carriage--ui-spinner-frames-ascii))

(defun carriage-ui--spinner-char ()
  "Return current spinner frame as string."
  (let* ((frames (carriage-ui--spinner-frames))
         (n (length frames))
         (i (mod (or carriage--ui-spinner-index 0) (max 1 n))))
    (aref frames i)))

(defun carriage-ui--spinner-tick (buf)
  "Advance spinner in BUF and update mode-line.
Update only when BUF is visible and selected to avoid unnecessary redisplay churn."
  (let ((win (and (buffer-live-p buf) (get-buffer-window buf t))))
    (when (and win (eq (selected-window) win))
      (with-current-buffer buf
        (setq carriage--ui-spinner-index (1+ carriage--ui-spinner-index))
        ;; Local refresh only; avoid (force-mode-line-update t) which repaints all windows.
        (force-mode-line-update)))))

(defun carriage-ui--spinner-start ()
  "Start buffer-local spinner timer if not running."
  (unless (or (bound-and-true-p noninteractive)
              carriage--ui-spinner-timer)
    (setq carriage--ui-spinner-index 0)
    (let* ((buf (current-buffer))
           (interval (or (and (boundp 'carriage-mode-spinner-interval)
                              carriage-mode-spinner-interval)
                         0.15)))
      (setq carriage--ui-spinner-timer
            (run-at-time 0 interval
                         (lambda ()
                           (carriage-ui--spinner-tick buf)))))))

(defun carriage-ui--spinner-stop (&optional reset)
  "Stop buffer-local spinner timer. When RESET, also zero index."
  (when (timerp carriage--ui-spinner-timer)
    (cancel-timer carriage--ui-spinner-timer))
  (setq carriage--ui-spinner-timer nil)
  (when reset
    (setq carriage--ui-spinner-index 0))
  ;; Local refresh only; avoid repainting all windows.
  (force-mode-line-update))

(defun carriage-ui-set-state (state)
  "Set UI STATE symbol for mode-line visuals and manage spinner."
  (setq carriage--ui-state (or state 'idle))
  ;; Invalidate modeline cache on state changes to avoid stale render
  (setq carriage-ui--ml-cache nil
        carriage-ui--ml-cache-key nil)
  (pcase carriage--ui-state
    ;; spinner for active phases (include reasoning/waiting/streaming/dispatch/sending)
    ((or 'sending 'streaming 'dispatch 'waiting 'reasoning)
     (carriage-ui--spinner-start))
    ;; no spinner for preparation/done/others
    ((or 'prompt 'context 'done)
     (carriage-ui--spinner-stop nil))
    (_
     (carriage-ui--spinner-stop t))))

(defun carriage-ui-state-lighter ()
  "Return a short lighter suffix for current =carriage--ui-state'."
  (pcase carriage--ui-state
    ('idle "")
    ('context " prep")
    ('prompt " prep")
    ('dispatch " req")
    ('waiting " wait")
    ('sending " snd")
    ('reasoning " rzn")
    ('streaming " str")
    ('done " done")
    ('dry-run " dry")
    ('apply " apl")
    ('error " ERR")
    (_ "")))

(defun carriage-ui--state-label (state)
  "Return human-readable label for STATE."
  (pcase state
    ('idle "Idle")
    ;; Preparation phases
    ('context "Prepare")
    ('prompt "Prepare")
    ;; Network phases
    ('dispatch "Request")
    ('waiting "Waiting")
    ('sending "Sending")
    ;; Reasoning/stream phases
    ('reasoning "Reasoning")
    ('streaming "Streaming")
    ;; Completion
    ('done "Done")
    ;; Local ops
    ('dry-run "Dry-run")
    ('apply "Apply")
    ('error "Error")
    (_ (capitalize (symbol-name (or state 'idle))))))

(defcustom carriage-ui-context-cache-ttl 2.5
  "Maximum age in seconds for cached context badge computations in the mode-line.
Set to 0 to recompute on every redisplay; set to nil to keep values until other cache
keys change (buffer content, toggle states)."
  :type '(choice (const :tag "Disable caching" 0)
                 (number :tag "TTL (seconds)")
                 (const :tag "Unlimited (until buffer changes)" nil))
  :group 'carriage-ui)

(defconst carriage-ui--modeline-default-blocks
  '(intent
    model
    state
    abort
    apply
    patch
    all
    dry
    diff
    ediff
    toggle-ctx
    toggle-files
    context
    suite
    engine
    branch
    report
    settings)
  "Default order of Carriage modeline blocks.")

(defun carriage-ui--invalidate-icon-cache-all-buffers ()
  "Invalidate icon caches in all live buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (boundp 'carriage-ui--icon-cache-env)
          (setq carriage-ui--icon-cache nil)
          (setq carriage-ui--icon-cache-env nil))
        ;; Drop modeline cache as well (theme/face/icon env changed)
        (when (boundp 'carriage-ui--ml-cache)
          (setq carriage-ui--ml-cache nil)
          (setq carriage-ui--ml-cache-key nil))))))

(defun carriage-ui--set-modeline-blocks (sym val)
  "Setter for `carriage-ui-modeline-blocks' that refreshes modelines everywhere."
  (set-default sym val)
  (carriage-ui--invalidate-icon-cache-all-buffers)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (force-mode-line-update t)))))

(defun carriage-ui-reset-modeline-blocks ()
  "Reset `carriage-ui-modeline-blocks' to defaults and refresh modelines."
  (interactive)
  (customize-set-variable 'carriage-ui-modeline-blocks carriage-ui--modeline-default-blocks))

(defcustom carriage-ui-modeline-blocks carriage-ui--modeline-default-blocks
  "List of symbols describing the Carriage modeline blocks and their order.

Recognized block symbols:
- `suite' — suite selector.
- `engine' — apply engine selector.
- `branch' — current VCS branch badge.
- `model' — model/backend selector.
- `intent' — Ask/Code/Hybrid intent toggle.
- `state' — Carriage state indicator with spinner.
- `context' — context badge.
- `patch' — patch block counter.
- `dry' — Dry-run action button.
- `apply' — Apply action button.
- `all' — Apply last iteration button.
- `abort' — Abort button.
- `report' — Report buffer shortcut.
- `toggle-ctx' — GPT context toggle.
- `toggle-files' — doc context toggle.
- `settings' — settings/menu button.
carriage-ui--set-modeline-blocks
Unknown symbols are ignored."
  :type '(repeat (choice (const :tag "Suite selector" suite)
                         (const :tag "Engine selector" engine)
                         (const :tag "Branch badge" branch)
                         (const :tag "Model selector" model)
                         (const :tag "Intent toggle" intent)
                         (const :tag "State indicator" state)
                         (const :tag "Context badge" context)
                         (const :tag "Patch counter" patch)
                         (const :tag "Dry-run button" dry)
                         (const :tag "Apply button" apply)
                         (const :tag "Apply last iteration button" all)
                         (const :tag "Diff button" diff)
                         (const :tag "Ediff button" ediff)
                         (const :tag "Abort button" abort)
                         (const :tag "Report button" report)
                         (const :tag "GPT context toggle" toggle-ctx)
                         (const :tag "Doc context toggle" toggle-files)
                         (const :tag "Settings button" settings)))
  :set #'carriage-ui--set-modeline-blocks
  :group 'carriage-ui)

(defvar-local carriage-ui--model-block-cache nil
  "Cached (label . help) tuple for the mode-line model segment.")

(defvar-local carriage-ui--model-block-cache-key nil
  "Key signature used to compute `carriage-ui--model-block-cache'.")

(defvar-local carriage-ui--model-block-cache nil
  "Cached (label . help) tuple for the mode-line model segment.")

(defvar-local carriage-ui--model-block-cache-key nil
  "Key signature used to compute `carriage-ui--model-block-cache'.")

(defvar-local carriage-ui--ctx-cache nil
  "Buffer-local cache for context badge computation.
Plist keys: :doc :gpt :tick :time :value.")

(defvar-local carriage-ui--ml-cache nil
  "Cached rendered modeline string for Carriage (buffer-local).")

(defvar-local carriage-ui--ml-cache-key nil
  "Signature (key) of the last rendered modeline for quick short-circuit.")

(defvar-local carriage-ui--button-cache nil
  "Cache of clickable modeline button strings keyed by (label help fn).")

(defvar-local carriage--mode-modeline-string nil
  "Precomputed Carriage modeline string for non-:eval mode-line segment.")

(defun carriage-ui--invalidate-ml-cache ()
  "Invalidate cached modeline/button strings for the current buffer."
  (setq carriage-ui--ml-cache nil
        carriage-ui--ml-cache-key nil)
  (setq carriage-ui--button-cache (make-hash-table :test 'equal)))

(defun carriage-ui--reset-context-cache (&optional buffer)
  "Clear cached context badge for BUFFER or the current buffer."
  (if buffer
      (with-current-buffer buffer
        (setq carriage-ui--ctx-cache nil))
    (setq carriage-ui--ctx-cache nil)))

(defun carriage-ui--compute-context-badge (inc-doc inc-gpt)
  "Compute context badge (LABEL . HINT) for INC-DOC / INC-GPT toggles.
Lightweight: count unique files only; no filesystem size probing."
  (if (not (or inc-doc inc-gpt))
      nil
    (let ((cnt 0))
      (condition-case _e
          (progn
            (require 'carriage-context nil t)
            (let* ((set (make-hash-table :test 'equal))
                   (root (or (carriage-project-root) default-directory)))
              (when (and inc-doc (fboundp 'carriage-context--doc-paths))
                (dolist (p (or (ignore-errors (carriage-context--doc-paths (current-buffer))) '()))
                  (let* ((abs (if (file-name-absolute-p p) p (expand-file-name p root)))
                         (tru (or (gethash abs tru-cache)
                                  (let ((v (ignore-errors (file-truename abs))))
                                    (when v (puthash abs v tru-cache))
                                    v))))
                    (when tru (puthash tru t set)))))
              (when (and inc-gpt (fboundp 'carriage-context--maybe-gptel-files))
                (dolist (p (or (ignore-errors (carriage-context--maybe-gptel-files)) '()))
                  (let* ((abs (if (file-name-absolute-p p) p (expand-file-name p root)))
                         (tru (or (gethash abs tru-cache)
                                  (let ((v (ignore-errors (file-truename abs))))
                                    (when v (puthash abs v tru-cache))
                                    v))))
                    (when tru (puthash tru t set)))))
              (maphash (lambda (_tru _v) (setq cnt (1+ cnt))) set)))
        (error
         (setq cnt 0)))
      (let* ((lbl (format "[Ctx:%d]" cnt))
             (hint (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s"
                           cnt
                           (if inc-doc "on" "off")
                           (if inc-gpt "on" "off"))))
        (cons lbl hint)))))

(defun carriage-ui--context-badge ()
  "Return cached or freshly computed context badge cons cell (LABEL . HINT)."
  (let* ((inc-doc (and (boundp 'carriage-mode-include-doc-context)
                       carriage-mode-include-doc-context))
         (inc-gpt (and (boundp 'carriage-mode-include-gptel-context)
                       carriage-mode-include-gptel-context))
         (tick (buffer-chars-modified-tick))
         (pt (point))
         (now (float-time))
         (ttl carriage-ui-context-cache-ttl)
         (cache carriage-ui--ctx-cache)
         (ttl-ok (or (null ttl)
                     (and (numberp ttl) (> ttl 0)
                          cache
                          (< (- now (or (plist-get cache :time) 0)) ttl)))))
    (if (and cache
             (eq inc-doc (plist-get cache :doc))
             (eq inc-gpt (plist-get cache :gpt))
             (= tick (plist-get cache :tick))
             (= pt (plist-get cache :point))
             ttl-ok)
        (plist-get cache :value)
      (let ((value (carriage-ui--compute-context-badge inc-doc inc-gpt)))
        (setq carriage-ui--ctx-cache
              (list :doc inc-doc
                    :gpt inc-gpt
                    :tick tick
                    :point pt
                    :time now
                    :value value))
        value))))

;; -------------------------------------------------------------------
;; Header-line and Mode-line builders (M3: icons (optional) + outline click)

(defun carriage-ui--truncate-middle (s max)
  "Truncate string S to MAX chars with a middle ellipsis if needed."
  (let ((len (length (or s ""))))
    (if (or (<= max 0) (<= len max))
        (or s "")
      (let* ((keep (max 1 (/ (- max 1) 2)))
             (left (substring s 0 keep))
             (right (substring s (- len keep))))
        (concat left "…" right)))))

(defun carriage-ui--project-name ()
  "Return cached project name (directory name of project root) or \"-\"."
  (or carriage-ui--project-name-cached
      (let* ((root (or (carriage-project-root) default-directory))
             (dir  (file-name-nondirectory (directory-file-name root)))
             (name (or (and dir (not (string-empty-p dir)) dir) "-")))
        (setq carriage-ui--project-name-cached name)
        name)))

(defun carriage-ui--org-outline-path ()
  "Return org outline path (A › B › C) at point, or nil if not available.
Do NOT use Org's outline path cache to avoid stale values while moving."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-get-outline-path))
    ;; Use-cache=nil for instant updates when point moves across headings.
    (let* ((path (ignore-errors (org-get-outline-path t nil))))
      (when (and path (listp path) (> (length path) 0))
        (mapconcat (lambda (s) (if (stringp s) s (format "%s" s))) path " › ")))))

(defun carriage-ui-goto-outline (&optional _event)
  "Go to the current org heading (best-effort) when clicking outline in header-line."
  (interactive "e")
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-find-exact-headline-in-buffer)
             (fboundp 'org-get-outline-path))
    (let* ((path (ignore-errors (org-get-outline-path t t)))
           (title (and path (car (last path))))
           (pos (and title (ignore-errors (org-find-exact-headline-in-buffer title)))))
      (when (number-or-marker-p pos)
        (goto-char pos)
        (recenter 1)))))

(defvar carriage-ui--icons-lib-available (featurep 'all-the-icons)
  "Cached availability of all-the-icons library.")

(defvar-local carriage-ui--icon-cache nil
  "Buffer-local cache of generated icon strings keyed by KEY or (toggle KEY ONP).")

(defvar-local carriage-ui--icon-cache-env nil
  "Environment snapshot for the icon cache to detect invalidation.
List of (gui use-icons height v-adjust themes).")

(defun carriage-ui--icon-cache-env-current ()
  "Return current environment signature for icon rendering."
  (list (display-graphic-p)
        (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons)
        carriage-mode-icon-height
        carriage-mode-icon-v-adjust
        custom-enabled-themes))

(defun carriage-ui--invalidate-icon-cache ()
  "Invalidate icon cache for the current buffer."
  (setq carriage-ui--icon-cache nil)
  (setq carriage-ui--icon-cache-env (carriage-ui--icon-cache-env-current)))

(defun carriage-ui--maybe-refresh-icon-cache-env ()
  "Ensure icon cache environment matches current UI; reset cache if not."
  (let ((cur (carriage-ui--icon-cache-env-current)))
    (unless (equal cur carriage-ui--icon-cache-env)
      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))
      (setq carriage-ui--icon-cache-env cur))))



(defvar carriage-ui--icon-theme-hook-installed nil
  "Internal flag to install theme-change advice once.")

(unless carriage-ui--icon-theme-hook-installed
  (setq carriage-ui--icon-theme-hook-installed t)
  (advice-add 'load-theme :after (lambda (&rest _)
                                   (carriage-ui--invalidate-icon-cache-all-buffers))))

(defun carriage-ui--icons-available-p ()
  "Return non-nil when icons can be used in modeline."
  (let* ((use-flag (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (gui (display-graphic-p)))
    (unless carriage-ui--icons-lib-available
      (setq carriage-ui--icons-lib-available (require 'all-the-icons nil t)))
    (and use-flag gui carriage-ui--icons-lib-available)))

(defun carriage-ui--accent-hex (face)
  "Return final hexadecimal foreground color for FACE."
  (or (ignore-errors (face-foreground face nil 'default))
      (face-attribute face :foreground nil 'default)
      "#aaaaaa"))
(defun carriage-ui--icon (key)
  "Return icon string for KEY using all-the-icons, or nil if unavailable.
Results are cached per-buffer and invalidated when theme or UI parameters change."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (hit (gethash key cache)))
      (if (stringp hit)
          hit
        (let ((res
               (pcase key
                 ;; Intent
                 ('ask  (when (fboundp 'all-the-icons-material)
                          (all-the-icons-material "chat"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
                 ('patch (when (fboundp 'all-the-icons-material)
                           (all-the-icons-material "code"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('hybrid (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "merge_type"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ;; Model/backend (prefer Material; fallback to Octicon CPU)
                 ('model (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "memory"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "cpu"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          (t nil)))
                 ;; Header-line sections
                 ('project (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "repo"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "folder"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            (t nil)))
                 ('file    (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "file-text"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "description"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            (t nil)))
                 ('heading (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "chevron_right"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "bookmark"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            (t nil)))
                 ('suite (cond
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "package"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "category"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          (t nil)))
                 ('engine (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "gear"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "build"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           (t nil)))
                 ;; Actions
                 ('dry    (when (fboundp 'all-the-icons-faicon)
                            (all-the-icons-faicon "flask"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-orange-face)))))
                 ('apply  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "check_circle"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-green-face)))))
                 ('all    (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "rocket"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "play_arrow"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           (t nil)))
                 ('abort  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "stop"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-red-face)))))
                 ('report (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "file-text"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('diff   (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-compare"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-orange-face)))))
                 ('ediff  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "diff"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('wip    (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-branch"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('commit (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-commit"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('reset  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "history"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 (_ nil))))
          (when (stringp res)
            (puthash key res cache))
          res)))))

;; Header-line helpers (split from carriage-ui--header-line)

(defun carriage-ui--hl-sep ()
  "Separator used between header-line segments."
  " › ")

(defun carriage-ui--left-pad (&optional pixels)
  "Return a left padding spacer of PIXELS (default 3) using a display property."
  (let ((px (or pixels 3)))
    (propertize " " 'display (list 'space :width (cons 'pixels px)))))

(defun carriage-ui--hl-build-seg (label icon)
  "Build a segment from LABEL and optional ICON, preserving icon face."
  (if icon (concat icon " " label) label))

(defun carriage-ui--hl-mute-tail (s)
  "Apply muted face to the text portion after the first space. If no space, mute whole string.
Keeps icon color intact when icon is at the head of S."
  (if (not (stringp s)) s
    (let ((idx (string-match " " s)))
      (cond
       ((and idx (< idx (length s)))
        (let ((cp (copy-sequence s))
              (start (1+ idx)))
          ;; Apply muted face only to characters that do not already have a face
          (dotimes (i (- (length cp) start))
            (let ((pos (+ start i)))
              (unless (get-text-property pos 'face cp)
                (put-text-property pos (1+ pos) 'face 'carriage-ui-muted-face cp))))
          cp))
       (t (let ((cp (copy-sequence s)))
            (dotimes (i (length cp))
              (unless (get-text-property i 'face cp)
                (put-text-property i (1+ i) 'face 'carriage-ui-muted-face cp)))
            cp))))))

(defun carriage-ui--hl-clickable-outline (s)
  "Make outline string S clickable to jump to the heading."
  (let* ((omap (let ((m (make-sparse-keymap)))
                 (define-key m [header-line mouse-1] #'carriage-ui-goto-outline)
                 m)))
    (propertize (or s "")
                'mouse-face 'mode-line-highlight
                'help-echo "Перейти к заголовку (mouse-1)"
                'local-map omap)))

(defun carriage-ui--hl-show-outline-p (tty outline avail)
  "Return non-nil when OUTLINE should be shown given TTY flag and AVAIL width."
  (and carriage-mode-headerline-show-outline (not tty) outline (> avail 30)))

(defun carriage-ui--hl-fit (pseg bseg oseg show-outline avail sep)
  "Truncate segments to fit AVAIL width. Returns list (P B O).
Truncation order: outline → buffer → project."
  (let* ((base (concat pseg sep bseg))
         (full (if show-outline (concat base sep oseg) base)))
    (when (> (length full) avail)
      (when show-outline
        (let* ((ol-max (max 10 (- avail (length base) (length sep)))))
          (setq oseg (carriage-ui--truncate-middle oseg ol-max))
          (setq full (concat base sep oseg))))
      (when (> (length full) avail)
        (let* ((buf-max (max 10 (- avail (length pseg) (length sep)))))
          (setq bseg (carriage-ui--truncate-middle bseg buf-max))
          (setq base (concat pseg sep bseg))
          (setq full (if show-outline (concat base sep oseg) base))))
      (when (> (length full) avail)
        (setq pseg (carriage-ui--truncate-middle pseg (max 5 (- avail (length sep) (length bseg)))))
        (setq base (concat pseg sep bseg))
        (setq full (if show-outline (concat base sep oseg) base))))
    (list pseg bseg oseg)))

(defun carriage-ui--header-line ()
  "Build header-line: [icon] project › [icon] buffer › org-outline-path (no icon for heading).
- Graceful degradation in TTY and narrow windows (hide outline).
- Outline segment is clickable in org-mode to jump to the heading.
- Visuals: all text sections except the last are muted gray; icons keep their colors."
  (let* ((project (carriage-ui--project-name))
         (bufname (buffer-name))
         (outline (and carriage-mode-headerline-show-outline (or carriage-ui--last-outline-path-str "")))
         (use-icons (carriage-ui--icons-available-p))
         (p-icon (and use-icons (carriage-ui--icon 'project)))
         (f-icon (and use-icons (carriage-ui--icon 'file)))
         (sep (carriage-ui--hl-sep))
         ;; Window width and policy
         (w (or (ignore-errors (window-total-width)) 80))
         (maxw (or (and (boundp 'carriage-mode-headerline-max-width)
                        carriage-mode-headerline-max-width)
                   w))
         (reserve 10)
         (avail (max 0 (- maxw reserve)))
         (tty (not (display-graphic-p)))
         (show-outline (carriage-ui--hl-show-outline-p tty outline avail))
         (pseg (concat (carriage-ui--left-pad) (carriage-ui--hl-build-seg project p-icon)))
         (bseg (carriage-ui--hl-build-seg bufname f-icon))
         (oseg (or outline "")))
    (cl-destructuring-bind (p1 b1 o1)
        (carriage-ui--hl-fit pseg bseg oseg show-outline avail sep)
      ;; Apply muted face to all but the last section
      (let* ((p2 p1) (b2 b1))
        (if show-outline
            (progn
              (setq p2 (carriage-ui--hl-mute-tail p2))
              (setq b2 (carriage-ui--hl-mute-tail b2)))
          (setq p2 (carriage-ui--hl-mute-tail p2)))
        (let ((base (concat p2 sep b2)))
          (if show-outline
              (concat base sep (carriage-ui--hl-clickable-outline o1))
            base))))))

;; Cache and refresh outline segment on cursor move to ensure timely header updates.
(defvar-local carriage-ui--project-name-cached nil
  "Cached project name for header-line; computed once per buffer.")

(defvar-local carriage-ui--last-outline-path-str nil
  "Cached outline path string for the current buffer's header-line refresh.")
(defvar-local carriage-ui--last-outline-level nil
  "Cached outline level at point for fast header-line refresh.")
(defvar-local carriage-ui--last-outline-title nil
  "Cached outline heading title at point for fast header-line refresh.")

(defvar-local carriage-ui--patch-count-cache nil
  "Cached count of #+begin_patch blocks in the current buffer.")
(defvar-local carriage-ui--patch-count-tick nil
  "Buffer tick corresponding to `carriage-ui--patch-count-cache'.")

;; Branch name cache (to avoid heavy VC/git calls on every modeline render)
(defcustom carriage-ui-branch-cache-ttl 3.0
  "TTL in seconds for cached VCS branch name used in the modeline.
When nil, the cache is considered always valid (until explicitly invalidated)."
  :type '(choice (const :tag "Unlimited (never auto-refresh)" nil) number)
  :group 'carriage-ui)

(defvar-local carriage-ui--branch-cache-string nil
  "Cached branch name for current buffer modeline (or nil).")

(defvar-local carriage-ui--branch-cache-time 0
  "Timestamp of the last branch cache refresh (float seconds).")

(defun carriage-ui--branch-name-cached ()
  "Return VCS branch name for the current buffer using a lightweight cache.
Prefers parsing `vc-mode' when available; falls back to git helper rarely.
Respects `carriage-ui-branch-cache-ttl'."
  (let* ((ttl carriage-ui-branch-cache-ttl)
         (now (float-time))
         (valid (or (null ttl)
                    (< (- now (or carriage-ui--branch-cache-time 0)) (or ttl 0)))))
    (if (and carriage-ui--branch-cache-string valid)
        carriage-ui--branch-cache-string
      (let* ((br
              (or
               ;; Try to parse from vc-mode string to avoid processes
               (let* ((s (and (boundp 'vc-mode) vc-mode)))
                 (when (stringp s)
                   (cond
                    ((string-match "[: -]\\([^: -]+\\)\\'" s) (match-string 1 s))
                    (t nil))))
               ;; Optionally query VC, but guard to avoid expensive calls
               (when (require 'vc-git nil t)
                 (ignore-errors
                   (when (fboundp 'vc-git--symbolic-branch)
                     (vc-git--symbolic-branch default-directory))))
               ;; Fallback to our git helper (single process)
               (condition-case _e
                   (progn
                     (require 'carriage-git)
                     (carriage-git-current-branch default-directory))
                 (error nil)))))
        (setq carriage-ui--branch-cache-string br
              carriage-ui--branch-cache-time now)
        br))))

(defun carriage-ui--patch-count ()
  "Return the number of #+begin_patch blocks in the current buffer."
  (if (not (derived-mode-p 'org-mode))
      0
    (let ((tick (buffer-chars-modified-tick)))
      (if (and carriage-ui--patch-count-cache
               carriage-ui--patch-count-tick
               (= tick carriage-ui--patch-count-tick))
          carriage-ui--patch-count-cache
        (let ((count 0))
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search t))
              (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
                (setq count (1+ count)))))
          (setq carriage-ui--patch-count-cache count
                carriage-ui--patch-count-tick tick)
          count)))))

(defun carriage-ui--point-in-patch-block-p ()
  "Return non-nil when point is inside a #+begin_patch … #+end_patch block."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((pos (point))
            (case-fold-search t))
        (when (re-search-backward "^[ \t]*#\\+begin_patch\\b" nil t)
          (let ((beg (match-beginning 0)))
            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
              (let ((end (match-beginning 0)))
                (and (<= beg pos) (< pos end))))))))))

(defun carriage-ui--region-has-patch-p ()
  "Return non-nil when the active region contains a patch block."
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end))
            (case-fold-search t))
        (goto-char beg)
        (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t)))))

(defun carriage-ui--show-apply-buttons-p ()
  "Return non-nil when modeline should show Dry/Apply actions.

Visible only when point is inside a #+begin_patch … #+end_patch block
or when the active region contains at least one patch block."
  (or (carriage-ui--point-in-patch-block-p)
      (carriage-ui--region-has-patch-p)))

(defvar-local carriage-ui--last-iter-cache-id nil
  "Cached last iteration id used to detect presence of last-iteration blocks.")
(defvar-local carriage-ui--last-iter-cache-tick nil
  "Buffer tick corresponding to `carriage-ui--last-iter-cache-id'.")
(defvar-local carriage-ui--last-iter-cache-result nil
  "Cached boolean result for last-iteration presence detection.")

(defun carriage-ui--last-iteration-present-p ()
  "Return non-nil when there are blocks of the last iteration in the buffer.

Detection strategy:
- If `carriage--last-iteration-id' is non-nil, scan for any #+begin_patch line
  with text property 'carriage-iteration-id equal to that id.
- Results are cached per buffer and invalidated on buffer tick changes
  or when the last-iteration id changes."
  (let* ((id (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id))
         (tick (buffer-chars-modified-tick)))
    (if (and (equal id carriage-ui--last-iter-cache-id)
             (eq tick carriage-ui--last-iter-cache-tick))
        carriage-ui--last-iter-cache-result
      (let ((found
             (and id
                  (save-excursion
                    (goto-char (point-min))
                    (let ((ok nil))
                      (while (and (not ok)
                                  (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t))
                        (let ((lb (line-beginning-position)))
                          (when (equal (get-text-property lb 'carriage-iteration-id) id)
                            (setq ok t))))
                      ok)))))
        (setq carriage-ui--last-iter-cache-id id
              carriage-ui--last-iter-cache-tick tick
              carriage-ui--last-iter-cache-result found)
        found))))

(defcustom carriage-mode-headerline-show-outline t
  "When non-nil, show org outline segment in header-line. Turning it off reduces overhead in large Org files."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-mode-headerline-idle-interval 0.2
  "Idle interval in seconds before refreshing header-line after cursor/scroll changes."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--headerline-idle-timer nil
  "Idle timer used to coalesce frequent header-line refreshes.")

(defun carriage-ui--headerline-queue-refresh ()
  "Schedule a header-line refresh on idle to reduce churn."
  (when (timerp carriage-ui--headerline-idle-timer)
    (cancel-timer carriage-ui--headerline-idle-timer))
  (let* ((buf (current-buffer))
         (interval (or (and (boundp 'carriage-mode-headerline-idle-interval)
                            carriage-mode-headerline-idle-interval)
                       0.12)))
    (setq carriage-ui--headerline-idle-timer
          (run-with-idle-timer interval nil
                               (lambda ()
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (force-mode-line-update t)
                                     (setq carriage-ui--headerline-idle-timer nil)))))))
  t)

(defun carriage-ui--headerline-post-command ()
  "Post-command hook: refresh header-line instantly when Org outline context changes.
Updates on any change of outline path, heading level, or heading title."
  (when (and carriage-mode-headerline-show-outline
             (derived-mode-p 'org-mode)
             (get-buffer-window (current-buffer) t))
    (let* ((win (get-buffer-window (current-buffer) t))
           (w (ignore-errors (and (window-live-p win) (window-total-width win))))
           (wide (or (null w) (>= w 40)))
           (cur-path (and wide (or (carriage-ui--org-outline-path) "")))
           (info (and wide
                      (ignore-errors
                        (save-excursion
                          (org-back-to-heading t)
                          (cons (org-outline-level)
                                (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))))))
           (lvl (and info (car info)))
           (ttl (and info (cdr info))))
      (when (and wide
                 (or (not (equal cur-path carriage-ui--last-outline-path-str))
                     (not (equal lvl carriage-ui--last-outline-level))
                     (not (equal ttl carriage-ui--last-outline-title))))
        (setq carriage-ui--last-outline-path-str cur-path)
        (setq carriage-ui--last-outline-level lvl)
        (setq carriage-ui--last-outline-title ttl)
        (carriage-ui--headerline-queue-refresh)))))

(defun carriage-ui--headerline-window-scroll (_win _start)
  "Refresh header-line on window scroll for instant visual updates."
  (carriage-ui--headerline-queue-refresh))

(defun carriage-ui--ml-button (label fn help)
  "Return a clickable LABEL that invokes FN, preserving LABEL's text properties.
Memoized to avoid per-redisplay keymap/props allocation.

Important: the cache key includes label's text properties to ensure visual updates
(e.g. toggle icons changing color) are not suppressed by memoization."
  (let* ((kprops (and (stringp label) (text-properties-at 0 label)))
         (key (list label help fn kprops))
         (cache (or carriage-ui--button-cache
                    (setq carriage-ui--button-cache (make-hash-table :test 'equal))))
         (hit (and cache (gethash key cache))))
    (if (stringp hit)
        hit
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] fn)
        (let* ((s (copy-sequence (or label "")))
               (len (length s)))
          (when (> len 0)
            (add-text-properties
             0 len
             (list 'mouse-face 'mode-line-highlight
                   'help-echo help
                   'local-map map)
             s))
          (puthash key s cache)
          s)))))

(defun carriage-ui--toggle-icon (key onp)
  "Return colored icon for toggle KEY based on ONP, with caching."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (ckey (list 'toggle key (if onp t nil)))
           (hit (gethash ckey cache)))
      (if (stringp hit)
          hit
        (let* ((face (if onp
                         (pcase key
                           ('auto    'carriage-ui-accent-blue-face)
                           ('diffs   'carriage-ui-accent-orange-face)
                           ('confirm 'carriage-ui-accent-purple-face)
                           ('icons   'carriage-ui-accent-cyan-face)
                           ('ctx     'carriage-ui-accent-blue-face)
                           ('files   'carriage-ui-accent-purple-face)
                           (_        'carriage-ui-accent-blue-face))
                       'carriage-ui-muted-face))
               (fg (carriage-ui--accent-hex face))
               (fplist (list :inherit nil :foreground fg))
               (res
                (pcase key
                  ('auto
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "autorenew"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('diffs
                   (cond
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "diff"
                                            :height carriage-mode-icon-height
                                            :v-adjust carriage-mode-icon-v-adjust
                                            :face fplist))
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "difference"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist))
                    (t nil)))
                  ('confirm
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "done_all"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('icons
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "image"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('ctx
                   (when (fboundp 'all-the-icons-material)
                     (ignore-errors
                       (all-the-icons-material "toc"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
                                               :face fplist))))
                  ('files
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "description"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  (_ nil))))
          (when (stringp res)
            (puthash ckey res cache))
          res)))))

(defun carriage-ui--toggle (label var-sym fn help &optional icon-key)
  "Build a toggle button with LABEL; highlight when VAR-SYM is non-nil.
When ICON-KEY is non-nil and icons are available, show a colored icon that
reflects toggle state (muted when off, bright when on)."
  (let* ((on (and (boundp var-sym) (symbol-value var-sym)))
         (lbl (if (and icon-key (carriage-ui--icons-available-p))
                  (carriage-ui--toggle-icon icon-key on)
                (if on (propertize label 'face 'mode-line-emphasis) label))))
    (carriage-ui--ml-button lbl fn help)))

(defun carriage-ui--maybe-in-report-buffer ()
  "Return non-nil if current buffer is a Carriage report buffer."
  (or (derived-mode-p 'carriage-report-mode)
      (string= (buffer-name) "*carriage-report*")))

(defun carriage-ui--diff-button ()
  "Open Diff for report item if available; otherwise switch to report (no error in batch)."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-show-diff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then press RET or [Diff]"))))
    (error
     (message "Нет доступного отчёта для Diff"))))

(defun carriage-ui--ediff-button ()
  "Open Ediff for report item if available; otherwise switch to report (no error in batch)."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-ediff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then press e or [Ediff]"))))
    (error
     (message "Нет доступного отчёта для Ediff"))))

;; -- Optional: settings "gear" button to open the menu directly.
(defun carriage-ui--settings-btn ()
  "Return a clickable gear settings button for the modeline."
  (let* ((use-icons (carriage-ui--icons-available-p))
         (ic (and use-icons (fboundp 'all-the-icons-octicon)
                  (all-the-icons-octicon "gear" :height carriage-mode-icon-height :v-adjust carriage-mode-icon-v-adjust
                                         :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
         (label (or ic "[Menu]"))
         (btn (carriage-ui--ml-button label #'carriage-keys-open-menu "Открыть меню Carriage (C-c e)")))
    btn))
(defalias 'settings-btn 'carriage-ui--settings-btn)

(defvar carriage--icon-intent-ask "A"
  "Modeline marker for Ask intent.")
(defvar carriage--icon-intent-code "C"
  "Modeline marker for Code intent.")
(defvar carriage--icon-intent-hybrid "H"
  "Modeline marker for Hybrid intent.")

(defun carriage-ui--segment-intent-suite ()
  "Return a short modeline segment for Intent/Suite."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (i (pcase intent
              ('Ask    carriage--icon-intent-ask)
              ('Code   carriage--icon-intent-code)
              ('Hybrid carriage--icon-intent-hybrid)
              (_       carriage--icon-intent-ask)))
         (s (if (symbolp suite) (symbol-name suite) (or suite ""))))
    (format "[%s|%s]" i (if (string-empty-p s) "-" s))))

(defun carriage-ui--flash-last-iteration-patches (&optional buffer)
  "Flash all begin_patch…end_patch blocks of the last streamed region in BUFFER (or current).
If a streamed region is not available, flash all patch blocks in the buffer.
Uses pulse.el when available, otherwise temporary overlays."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dur (or (and (boundp 'carriage-mode-flash-duration)
                         carriage-mode-flash-duration)
                    1.0))
           (use-pulse (require 'pulse nil t))
           (r (and (fboundp 'carriage-stream-region) (carriage-stream-region)))
           (beg (and (consp r) (car r)))
           (end (and (consp r) (cdr r))))
      (save-excursion
        (save-restriction
          (when (and beg end) (narrow-to-region beg end))
          (goto-char (point-min))
          (let ((ranges '()))
            (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
              (let ((pbeg (match-beginning 0)))
                (if (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                    (let ((pend (line-end-position)))
                      (push (cons pbeg pend) ranges))
                  (push (cons pbeg (point-max)) ranges))))
            (dolist (rg ranges)
              (let ((rb (car rg)) (re (cdr rg)))
                (if use-pulse
                    (pulse-momentary-highlight-region rb re 'carriage-patch-valid-face)
                  (let ((ov (make-overlay rb re)))
                    (overlay-put ov 'face 'carriage-patch-valid-face)
                    (run-at-time dur nil
                                 (lambda (o) (when (overlayp o) (delete-overlay o)))
                                 ov)))))))))))
;; -------------------------------------------------------------------
;; Refactor: split carriage-ui--modeline into smaller helpers

(defun carriage-ui--ml-cache-key ()
  "Compute cache key for the modeline string based on current UI environment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (ctx-badge (carriage-ui--context-badge))
         (patch-count (carriage-ui--patch-count))
         (show-patch (carriage-ui--show-apply-buttons-p))
         (has-last (carriage-ui--last-iteration-present-p))
         (blocks (if (and (listp carriage-ui-modeline-blocks)
                          carriage-ui-modeline-blocks)
                     carriage-ui-modeline-blocks
                   carriage-ui--modeline-default-blocks))
         (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
         (spin   (and (memq state '(sending streaming dispatch waiting reasoning))
                      (carriage-ui--spinner-char)))
         (branch (carriage-ui--branch-name-cached))
         (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
    (list uicons
          state spin
          (and ctx-badge (car ctx-badge))
          (and ctx-badge (cdr ctx-badge))
          patch-count show-patch has-last abortp blocks
          (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
          (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
          (and (boundp 'carriage-mode-model)   carriage-mode-model)
          (and (boundp 'carriage-mode-backend) carriage-mode-backend)
          (and (boundp 'carriage-mode-provider) carriage-mode-provider)
          (and (fboundp 'carriage-apply-engine) (carriage-apply-engine))
          branch
          (and (boundp 'carriage-mode-include-gptel-context)
               carriage-mode-include-gptel-context)
          (and (boundp 'carriage-mode-include-doc-context)
               carriage-mode-include-doc-context))))

(defun carriage-ui--ml-seg-intent ()
  "Build Intent segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (label
          (if uicons
              (pcase intent
                ('Ask   (or (carriage-ui--icon 'ask) "[Ask]"))
                ('Code  (or (carriage-ui--icon 'patch) "[Code]"))
                (_      (or (carriage-ui--icon 'hybrid) "[Hybrid]")))
            (format "[%s]" (pcase intent ('Ask "Ask") ('Code "Code") (_ "Hybrid"))))))
    (carriage-ui--ml-button label #'carriage-toggle-intent "Toggle Ask/Code/Hybrid intent")))

(defun carriage-ui--ml-seg-suite ()
  "Build Suite segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (suite-str (cond ((symbolp suite) (symbol-name suite))
                          ((stringp suite) suite) (t "udiff")))
         (icon (and uicons
                    (boundp 'carriage-mode-use-suite-icon)
                    carriage-mode-use-suite-icon
                    (carriage-ui--icon 'suite)))
         (_ (require 'carriage-i18n nil t))
         (label (if icon
                    (format "%s [%s]" icon suite-str)
                  (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                                  (carriage-i18n :suite) "Suite")))
                    (format "%s: [%s]" name suite-str))))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :suite-tooltip)
                 "Select Suite (sre|udiff)")))
    (carriage-ui--ml-button label #'carriage-select-suite help)))

(defun carriage-ui--ml-seg-model ()
  "Build Model segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (model-val (and (boundp 'carriage-mode-model) carriage-mode-model))
         (raw-model (cond
                     ((stringp model-val) model-val)
                     ((symbolp model-val) (symbol-name model-val))
                     ((null model-val) "")
                     (t (format "%s" model-val))))
         (resolved (carriage-llm-resolve-model carriage-mode-backend carriage-mode-provider raw-model))
         (display-source (if (stringp resolved) resolved raw-model))
         (display-name (carriage-llm-display-name (or display-source "")))
         (bm-text (format "[%s]" (if (and (stringp display-name)
                                          (not (string-empty-p display-name)))
                                     display-name
                                   "-")))
         (ic (and uicons (carriage-ui--icon 'model)))
         (full-id
          (let* ((candidate (and (stringp resolved) resolved))
                 (has-colon (and candidate (string-match-p ":" candidate))))
            (cond
             (has-colon candidate)
             (t (or (carriage-llm-make-full-id carriage-mode-backend carriage-mode-provider display-source)
                    display-source
                    raw-model
                    "-")))))
         ;; Make only the text part clickable to ensure mouse highlight and clicks work reliably.
         (btn (carriage-ui--ml-button bm-text
                                      #'carriage-select-model
                                      (format "Модель: %s (клик — выбрать)"
                                              (or full-id "-")))))
    (if ic
        (concat ic " " btn)
      btn)))

(defun carriage-ui--ml-seg-engine ()
  "Build Engine segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (engine-str
          (let ((e (and (boundp 'carriage-apply-engine) carriage-apply-engine)))
            (cond
             ((eq e 'git)
              (let ((pol (and (boundp 'carriage-git-branch-policy)
                              carriage-git-branch-policy)))
                (format "git:%s" (if (symbolp pol) (symbol-name pol) ""))))
             ((symbolp e) (symbol-name e))
             ((stringp e) e)
             (t "git"))))
         (icon (and uicons
                    (boundp 'carriage-mode-use-engine-icon) carriage-mode-use-engine-icon
                    (carriage-ui--icon 'engine)))
         (_ (require 'carriage-i18n nil t))
         (eng (and (boundp 'carriage-apply-engine) carriage-apply-engine))
         (policy (and (eq eng 'git)
                      (boundp 'carriage-git-branch-policy)
                      (symbol-name carriage-git-branch-policy)))
         (help (cond
                ((and (eq eng 'git)
                      (featurep 'carriage-i18n) (fboundp 'carriage-i18n)
                      (stringp policy))
                 (carriage-i18n :engine-tooltip-branch engine-str policy))
                ((and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                 (carriage-i18n :engine-tooltip))
                (t "Select apply engine")))
         (label (if icon
                    (format "%s [%s]" icon engine-str)
                  (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                                  (carriage-i18n :engine-label)
                                "Engine")))
                    (format "%s: [%s]" name engine-str)))))
    (carriage-ui--ml-button label #'carriage-select-apply-engine help)))

(defun carriage-ui--ml-seg-state ()
  "Build State segment with spinner and face."
  (let* ((st (let ((s (and (boundp 'carriage--ui-state) carriage--ui-state)))
               (if (symbolp s) s 'idle)))
         (label (carriage-ui--state-label st))
         (txt (format "[%s%s]"
                      label
                      (if (memq st '(sending streaming dispatch waiting reasoning))
                          (concat " " (carriage-ui--spinner-char))
                        "")))
         (face (pcase st
                 ((or 'idle 'done) 'carriage-ui-state-success-face)
                 ((or 'reasoning 'waiting 'streaming 'dispatch) 'carriage-ui-state-active-face)
                 ('error 'carriage-ui-state-error-face)
                 (_ nil))))
    (if face (propertize txt 'face face) txt)))

(defun carriage-ui--ml-seg-context ()
  "Build Context badge segment."
  (let ((ctx-badge (carriage-ui--context-badge)))
    (when ctx-badge
      (let ((lbl (car ctx-badge))
            (hint (cdr ctx-badge)))
        (if hint (propertize lbl 'help-echo hint) lbl)))))

(defun carriage-ui--ml-seg-branch ()
  "Build Branch segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (br (carriage-ui--branch-name-cached))
         (txt (and (stringp br) (not (string-empty-p br)) (format "[%s]" br))))
    (when txt
      (if (and uicons (fboundp 'all-the-icons-octicon))
          (let ((ic (all-the-icons-octicon "git-branch"
                                           :height carriage-mode-icon-height
                                           :v-adjust carriage-mode-icon-v-adjust
                                           :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
            (concat ic " " txt))
        txt))))

(defun carriage-ui--ml-seg-patch ()
  "Build Patch counter segment."
  (let ((patch-count (carriage-ui--patch-count)))
    (when (and (numberp patch-count) (> patch-count 0))
      (propertize (format "[P:%d]" patch-count)
                  'help-echo "Количество #+begin_patch блоков в буфере"))))

(defun carriage-ui--ml-seg-dry ()
  "Build Dry-run button."
  (when (carriage-ui--show-apply-buttons-p)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'dry)) "[Dry]")))
      (carriage-ui--ml-button label #'carriage-dry-run-at-point "Dry-run at point"))))

(defun carriage-ui--ml-seg-apply ()
  "Build Apply button."
  (when (carriage-ui--show-apply-buttons-p)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'apply)) "[Apply]")))
      (carriage-ui--ml-button label #'carriage-apply-at-point-or-region "Apply at point or region"))))

(defun carriage-ui--ml-seg-all ()
  "Build Apply-all button."
  (when (carriage-ui--last-iteration-present-p)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'all)) "[All]")))
      (carriage-ui--ml-button label #'carriage-apply-last-iteration "Apply last iteration"))))

(defun carriage-ui--ml-seg-diff ()
  "Build Diff button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'diff)) "[Diff]")))
    (carriage-ui--ml-button label #'carriage-ui--diff-button "Открыть Diff для элемента отчёта")))

(defun carriage-ui--ml-seg-ediff ()
  "Build Ediff button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'ediff)) "[Ediff]")))
    (carriage-ui--ml-button label #'carriage-ui--ediff-button "Открыть Ediff для элемента отчёта")))

(defun carriage-ui--ml-seg-abort ()
  "Build Abort button (visible only when an abort handler is registered)."
  (when (and (boundp 'carriage--abort-handler) carriage--abort-handler)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'abort)) "[Abort]")))
      (carriage-ui--ml-button label #'carriage-abort-current "Abort current request"))))

(defun carriage-ui--ml-seg-report ()
  "Build Report button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'report)) "[Report]")))
    (carriage-ui--ml-button label #'carriage-report-open "Open report buffer")))

(defun carriage-ui--ml-seg-toggle-ctx ()
  "Build GPT context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :ctx-tooltip)
                 "Toggle including gptel-context (buffers/files)")))
    (carriage-ui--toggle "[Ctx]" 'carriage-mode-include-gptel-context
                         #'carriage-toggle-include-gptel-context
                         help 'ctx)))

(defun carriage-ui--ml-seg-toggle-files ()
  "Build Files context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :files-tooltip)
                 "Toggle including files from #+begin_context")))
    (carriage-ui--toggle "[Files]" 'carriage-mode-include-doc-context
                         #'carriage-toggle-include-doc-context
                         help 'files)))

(defun carriage-ui--ml-seg-settings ()
  "Build Settings button."
  (carriage-ui--settings-btn))

(defun carriage-ui--ml-render-block (blk)
  "Dispatch builder for a single modeline block BLK symbol."
  (pcase blk
    ('suite         (carriage-ui--ml-seg-suite))
    ('engine        (carriage-ui--ml-seg-engine))
    ('branch        (carriage-ui--ml-seg-branch))
    ('model         (carriage-ui--ml-seg-model))
    ('intent        (carriage-ui--ml-seg-intent))
    ('state         (carriage-ui--ml-seg-state))
    ('context       (carriage-ui--ml-seg-context))
    ('patch         (carriage-ui--ml-seg-patch))
    ('dry           (carriage-ui--ml-seg-dry))
    ('apply         (carriage-ui--ml-seg-apply))
    ('all           (carriage-ui--ml-seg-all))
    ('diff          (carriage-ui--ml-seg-diff))
    ('ediff         (carriage-ui--ml-seg-ediff))
    ('abort         (carriage-ui--ml-seg-abort))
    ('report        (carriage-ui--ml-seg-report))
    ('toggle-ctx    (carriage-ui--ml-seg-toggle-ctx))
    ('toggle-files  (carriage-ui--ml-seg-toggle-files))
    ('settings      (carriage-ui--ml-seg-settings))
    (_ nil)))

(defun carriage-ui--modeline ()
  "Build Carriage modeline segment using `carriage-ui-modeline-blocks' (refactored)."
  (let* ((blocks (if (and (listp carriage-ui-modeline-blocks)
                          carriage-ui-modeline-blocks)
                     carriage-ui-modeline-blocks
                   carriage-ui--modeline-default-blocks))
         (key (carriage-ui--ml-cache-key)))
    (if (and (equal key carriage-ui--ml-cache-key)
             (stringp carriage-ui--ml-cache))
        carriage-ui--ml-cache
      (let* ((segments (cl-loop for blk in blocks
                                for seg = (carriage-ui--ml-render-block blk)
                                if (stringp seg)
                                collect seg))
             (res (if segments (mapconcat #'identity segments " ") "")))
        (setq carriage-ui--ml-cache-key key
              carriage-ui--ml-cache     res)
        res))))

(provide 'carriage-ui)
;;; carriage-ui.el ends here
