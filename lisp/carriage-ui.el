;;; carriage-ui.el --- Keymap and minimal UI helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-utils)

(defgroup carriage-ui nil
  "UI components for Carriage."
  :group 'applications
  :prefix "carriage-")

(defcustom carriage-mode-icon-v-adjust -0.05
  "Vertical offset for all-the-icons in Carriage modeline/header-line.
Negative values move icons up; positive move them down."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-mode-icon-height 1.0
  "Uniform icon height scale for all-the-icons in mode-line/header-line."
  :type 'number
  :group 'carriage-ui)

;; Debug logging controls
(defcustom carriage-ui-debug t
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

(defvar carriage-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-RET") #'carriage-send-buffer)
    (define-key map (kbd "C-c RET")   #'carriage-send-subtree)
    (define-key map (kbd "C-c C-c")   #'carriage-apply-at-point)
    (define-key map (kbd "C-c !")     #'carriage-apply-last-iteration)
    (define-key map (kbd "C-c ?")     #'carriage-dry-run-at-point)
    (define-key map (kbd "C-c b c")   #'carriage-wip-checkout)
    (define-key map (kbd "C-c b l")   #'carriage-show-log)
    (define-key map (kbd "C-c b t")   #'carriage-show-traffic)
    (define-key map (kbd "C-c b L")   #'carriage-show-log-and-traffic)
    (define-key map (kbd "C-c b r")   #'carriage-wip-reset-soft)
    (define-key map (kbd "C-c b m")   #'carriage-commit-changes)
    (define-key map (kbd "C-c b i")   #'carriage-commit-last-iteration)
    ;; Navigation placeholders (optional)
    (define-key map (kbd "M-n")       #'carriage-next-patch-block)
    (define-key map (kbd "M-p")       #'carriage-prev-patch-block)
    map)
  "Keymap for =carriage-mode'.")

(defvar carriage--ui-state 'idle
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
  "Advance spinner in BUF and update mode-line."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq carriage--ui-spinner-index (1+ carriage--ui-spinner-index))
      (force-mode-line-update t))))

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
  (force-mode-line-update t))

(defun carriage-ui-set-state (state)
  "Set UI STATE symbol for mode-line visuals and manage spinner."
  (setq carriage--ui-state state)
  (pcase state
    ((or 'sending 'streaming)
     (carriage-ui--spinner-start))
    (_
     (carriage-ui--spinner-stop t))))

(defun carriage-ui-state-lighter ()
  "Return a short lighter suffix for current =carriage--ui-state'."
  (pcase carriage--ui-state
    ('idle "")
    ('sending " snd")
    ('streaming " str")
    ('dry-run " dry")
    ('apply " apl")
    ('error " ERR")
    (_ "")))

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
  "Return project name (directory name of project root) or \"-\"."
  (let* ((root (or (carriage-project-root) default-directory))
         (dir  (file-name-nondirectory (directory-file-name root))))
    (or (and dir (not (string-empty-p dir)) dir) "-")))

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

(defun carriage-ui--icons-available-p ()
  "Return non-nil when icons can be used in modeline."
  (let* ((use-flag (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (gui (display-graphic-p))
         (ok (and use-flag gui (require 'all-the-icons nil t))))
    (carriage-ui--dbg "icons-available-p: use=%S gui=%S all-the-icons=%S => %S"
                      use-flag gui (featurep 'all-the-icons) ok)
    ok))

(defun carriage-ui--accent-hex (face)
  "Return final hexadecimal foreground color for FACE."
  (or (ignore-errors (face-foreground face nil 'default))
      (face-attribute face :foreground nil 'default)
      "#aaaaaa"))
(defun carriage-ui--icon (key)
  "Return icon string for KEY using all-the-icons, or nil if unavailable.
Emits debug logs with the resulting face property/foreground."
  (when (carriage-ui--icons-available-p)
    (let ((res
           (pcase key
             ;; Intent
             ('ask  (when (fboundp 'all-the-icons-material)
                      (all-the-icons-material "chat"
                                              :height carriage-mode-icon-height
                                              :v-adjust carriage-mode-icon-v-adjust
                                              :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
             ('patch (when (fboundp 'all-the-icons-material)
                       (all-the-icons-material "code"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
                                               :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
             ;; Model/backend (prefer Material; fallback to Octicon CPU)
             ('model (cond
                      ((fboundp 'all-the-icons-material)
                       (all-the-icons-material "memory"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
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
             ;; Actions
             ('dry    (when (fboundp 'all-the-icons-faicon)
                        (all-the-icons-faicon "flask"
                                              :height carriage-mode-icon-height
                                              :v-adjust carriage-mode-icon-v-adjust
                                              :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-orange-face)))))
             ('apply  (when (fboundp 'all-the-icons-material)
                        (all-the-icons-material "check_circle"
                                                :height carriage-mode-icon-height
                                                :v-adjust carriage-mode-icon-v-adjust
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
             ('reset  (when (fboundp 'all-the-icons-octicon)
                        (all-the-icons-octicon "history"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
                                               :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
             (_ nil))))
      (when (and carriage-ui-debug res)
        (carriage-ui--log-face-of-string (format "icon:%s" key) res))
      res)))

(defun carriage-ui--header-line ()
  "Build header-line: [icon] project › [icon] buffer › org-outline-path (no icon for heading).
- Graceful degradation in TTY and narrow windows (hide outline).
- Outline segment is clickable in org-mode to jump to the heading."
  (let* ((project (carriage-ui--project-name))
         (bufname (buffer-name))
         (outline (carriage-ui--org-outline-path))
         (use-icons (carriage-ui--icons-available-p))
         (p-icon (and use-icons (carriage-ui--icon 'project)))
         (f-icon (and use-icons (carriage-ui--icon 'file)))
         (sep " › ")
         ;; Window width and policy
         (w (or (ignore-errors (window-total-width)) 80))
         (maxw (or (and (boundp 'carriage-mode-headerline-max-width)
                        carriage-mode-headerline-max-width)
                   w))
         (reserve 10)
         (avail (max 0 (- maxw reserve)))
         (tty (not (display-graphic-p)))
         (show-outline (and (not tty) outline (> avail 30)))
         (pseg (if p-icon (concat p-icon " " project) project))
         (bseg (if f-icon (concat f-icon " " bufname) bufname))
         (oseg-text (or outline ""))
         (oseg0 oseg-text)
         (base (concat pseg sep bseg))
         (full (if show-outline
                   (concat base sep oseg0)
                 base)))
    ;; If exceeds, start truncating from outline, then buffer, then project
    (when (> (length full) avail)
      (when show-outline
        (let* ((ol-max (max 10 (- avail (length base) (length sep)))))
          (setq oseg0 (carriage-ui--truncate-middle oseg0 ol-max))
          (setq full (concat base sep oseg0))))
      (when (> (length full) avail)
        (let* ((buf-max (max 10 (- avail (length pseg) (length sep)))))
          (setq bseg (carriage-ui--truncate-middle bseg buf-max))
          (setq base (concat pseg sep bseg))
          (setq full (if show-outline (concat base sep oseg0) base))))
      (when (> (length full) avail)
        (setq pseg (carriage-ui--truncate-middle pseg (max 5 (- avail (length sep) (length bseg)))))
        (setq base (concat pseg sep bseg))
        (setq full (if show-outline (concat base sep oseg0) base))))
    ;; Clickable outline (optional)
    (if show-outline
        (let* ((omap (let ((m (make-sparse-keymap)))
                       (define-key m [header-line mouse-1] #'carriage-ui-goto-outline)
                       m))
               (oprop (propertize oseg0
                                  'mouse-face 'mode-line-highlight
                                  'help-echo "Перейти к заголовку (mouse-1)"
                                  'local-map omap)))
          (concat base sep oprop))
      full)))

;; Cache and refresh outline segment on cursor move to ensure timely header updates.
(defvar-local carriage-ui--last-outline-path-str nil
  "Cached outline path string for the current buffer's header-line refresh.")
(defvar-local carriage-ui--last-outline-level nil
  "Cached outline level at point for fast header-line refresh.")
(defvar-local carriage-ui--last-outline-title nil
  "Cached outline heading title at point for fast header-line refresh.")

(defun carriage-ui--headerline-post-command ()
  "Post-command hook: refresh header-line instantly when Org outline context changes.
Updates on any change of outline path, heading level, or heading title."
  (when (derived-mode-p 'org-mode)
    (let* ((cur-path (or (carriage-ui--org-outline-path) ""))
           (info (ignore-errors
                   (save-excursion
                     (org-back-to-heading t)
                     (cons (org-outline-level)
                           (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))))
           (lvl (car info))
           (ttl (cdr info)))
      (when (or (not (equal cur-path carriage-ui--last-outline-path-str))
                (not (equal lvl carriage-ui--last-outline-level))
                (not (equal ttl carriage-ui--last-outline-title)))
        (setq carriage-ui--last-outline-path-str cur-path)
        (setq carriage-ui--last-outline-level lvl)
        (setq carriage-ui--last-outline-title ttl)
        (force-mode-line-update t)))))

(defun carriage-ui--headerline-window-scroll (_win _start)
  "Refresh header-line on window scroll for instant visual updates."
  (force-mode-line-update t))

(defun carriage-ui--ml-button (label fn help)
  "Return a clickable LABEL that invokes FN, preserving LABEL's text properties."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] fn)
    (let* ((s (copy-sequence (or label "")))
           (pre (and (stringp label) (> (length label) 0)
                     (get-text-property 0 'face label))))
      (add-text-properties 0 (length s)
                           (list 'mouse-face 'mode-line-highlight
                                 'help-echo help
                                 'local-map map)
                           s)
      (when carriage-ui-debug
        (carriage-ui--log-face-prop "ml-button:pre" pre)
        (carriage-ui--log-face-of-string "ml-button:post" s))
      s)))

(defun carriage-ui--toggle-icon (key onp)
  "Return colored icon for toggle KEY based on ONP."
  (when (carriage-ui--icons-available-p)
    (let* ((face (if onp
                     (pcase key
                       ('auto    'carriage-ui-accent-blue-face)
                       ('diffs   'carriage-ui-accent-orange-face)
                       ('confirm 'carriage-ui-accent-purple-face)
                       ('icons   'carriage-ui-accent-cyan-face)
                       (_        'carriage-ui-accent-blue-face))
                   'carriage-ui-muted-face))
           (fg (carriage-ui--accent-hex face))
           (fplist (list :inherit nil :foreground fg)))
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
        (_ nil)))))

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
  "Open Diff for report item if available; otherwise switch to report."
  (interactive)
  (if (carriage-ui--maybe-in-report-buffer)
      (call-interactively #'carriage-report-show-diff-at-point)
    (let* ((buf (get-buffer "*carriage-report*")))
      (if buf
          (progn (pop-to-buffer buf)
                 (message "Select a row, then press RET or [Diff]"))
        (user-error "Нет доступного отчёта для Diff")))))

(defun carriage-ui--ediff-button ()
  "Open Ediff for report item if available; otherwise switch to report."
  (interactive)
  (if (carriage-ui--maybe-in-report-buffer)
      (call-interactively #'carriage-report-ediff-at-point)
    (let* ((buf (get-buffer "*carriage-report*")))
      (if buf
          (progn (pop-to-buffer buf)
                 (message "Select a row, then press e or [Ediff]"))
        (user-error "Нет доступного отчёта для Ediff")))))

(defun carriage-ui--modeline ()
  "Build Carriage modeline segment (M3: icons optional + spinner + extended actions)."
  (let* ((use-icons (carriage-ui--icons-available-p))
         ;; Intent and Suite
         (intent-label
          (if use-icons
              (or (if (and (boundp 'carriage-mode-intent)
                           (eq carriage-mode-intent 'Ask))
                      (carriage-ui--icon 'ask)
                    (carriage-ui--icon 'patch))
                  (format "[%s]" (if (eq carriage-mode-intent 'Ask) "Ask" "Patch")))
            (format "[%s]" (if (and (boundp 'carriage-mode-intent)
                                    (eq carriage-mode-intent 'Ask))
                               "Ask" "Patch"))))
         (intent-btn (carriage-ui--ml-button intent-label
                                             #'carriage-toggle-intent
                                             "Toggle Ask/Patch intent"))
         (suite-str (let ((s (and (boundp 'carriage-mode-suite) carriage-mode-suite)))
                      (cond
                       ((symbolp s) (symbol-name s))
                       ((stringp s) s)
                       (t "auto-v1"))))
         (suite-label (format "[Suite:%s]" suite-str))
         (suite-btn (carriage-ui--ml-button suite-label
                                            #'carriage-select-suite
                                            "Select Suite (auto|sre|patch|fileops)"))
         ;; Backend:Model
         (backend-str (let ((b (and (boundp 'carriage-mode-backend) carriage-mode-backend)))
                        (cond
                         ((symbolp b) (symbol-name b))
                         ((stringp b) b)
                         (t "backend"))))
         (model-str (or (and (boundp 'carriage-mode-model) carriage-mode-model) "model"))
         (bm-text (format "[%s:%s]" backend-str model-str))
         (bm-label (if use-icons
                       (let* ((ic (carriage-ui--icon 'model)))
                         (if ic (concat ic " " bm-text) bm-text))
                     bm-text))
         (backend-model-btn (carriage-ui--ml-button bm-label
                                                    #'carriage-select-model
                                                    "Select model (M-x carriage-select-backend to change backend)"))
         ;; State + spinner (textual; spinner already handled)
         (st (let ((s (and (boundp 'carriage--ui-state) carriage--ui-state)))
               (if (symbolp s) s 'idle)))
         (state (format "[%s%s]"
                        (symbol-name st)
                        (if (memq st '(sending streaming))
                            (concat " " (carriage-ui--spinner-char))
                          "")))
         ;; Actions (icon fallback to text)
         (dry-label    (or (and use-icons (carriage-ui--icon 'dry))    "[Dry]"))
         (apply-label  (or (and use-icons (carriage-ui--icon 'apply))  "[Apply]"))
         (all-label    (or (and use-icons (carriage-ui--icon 'all))    "[All]"))
         (abort-label  (or (and use-icons (carriage-ui--icon 'abort))  "[Abort]"))
         (report-label (or (and use-icons (carriage-ui--icon 'report)) "[Report]"))
         (diff-label   (or (and use-icons (carriage-ui--icon 'diff))   "[Diff]"))
         (ediff-label  (or (and use-icons (carriage-ui--icon 'ediff))  "[Ediff]"))
         (wip-label    (or (and use-icons (carriage-ui--icon 'wip))    "[WIP]"))
         (commit-label "[Commit]")
         (reset-label  (or (and use-icons (carriage-ui--icon 'reset))  "[Reset]"))
         (dry    (carriage-ui--ml-button dry-label    #'carriage-dry-run-at-point      "Dry-run at point"))
         (apply  (carriage-ui--ml-button apply-label  #'carriage-apply-at-point        "Apply at point"))
         (all    (carriage-ui--ml-button all-label    #'carriage-apply-last-iteration  "Apply last iteration"))
         (abort  (carriage-ui--ml-button abort-label  #'carriage-abort-current         "Abort current request"))
         (report (carriage-ui--ml-button report-label #'carriage-report-open           "Open report buffer"))
         (diff   (carriage-ui--ml-button diff-label   #'carriage-ui--diff-button       "Show diff (report)"))
         (ediff  (carriage-ui--ml-button ediff-label  #'carriage-ui--ediff-button      "Open Ediff (report)"))
         (wip    (carriage-ui--ml-button wip-label    #'carriage-wip-checkout          "Switch to WIP branch"))
         (commit (carriage-ui--ml-button commit-label #'carriage-commit-changes        "Commit changes"))
         (reset  (carriage-ui--ml-button reset-label  #'carriage-wip-reset-soft        "Soft reset last commit"))
         ;; Toggles (text conveys meaning; active ones emphasized)
         (t-auto  (carriage-ui--toggle "[AutoRpt]"    'carriage-mode-auto-open-report   #'carriage-toggle-auto-open-report   "Toggle auto-open report" 'auto))
         (t-diffs (carriage-ui--toggle "[ShowDiffs]"  'carriage-mode-show-diffs         #'carriage-toggle-show-diffs        "Toggle show diffs before apply" 'diffs))
         (t-all   (carriage-ui--toggle "[ConfirmAll]" 'carriage-mode-confirm-apply-all  #'carriage-toggle-confirm-apply-all "Toggle confirm apply-all" 'confirm))
         (t-icons (carriage-ui--toggle "[Icons]"      'carriage-mode-use-icons          #'carriage-toggle-use-icons         "Toggle icons in UI" 'icons)))
    (mapconcat #'identity
               (list intent-btn suite-btn backend-model-btn state
                     dry apply all abort report diff ediff wip commit reset
                     t-auto t-diffs t-all t-icons)
               " ")))

;; Code style: см. spec/code-style-v1.org (The Tao of Emacs Lisp Programming for LLMs)

(defvar carriage--icon-intent-ask "A"
  "Modeline marker for Ask intent.")
(defvar carriage--icon-intent-patch "P"
  "Modeline marker for Patch intent.")

(defun carriage-ui--segment-intent-suite ()
  "Return a short modeline segment for Intent/Suite."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (i (pcase intent
              ('Patch carriage--icon-intent-patch)
              (_     carriage--icon-intent-ask)))
         (s (if (symbolp suite) (symbol-name suite) (or suite ""))))
    (format "[%s|%s]" i (if (string-empty-p s) "-" s))))

(provide 'carriage-ui)
;;; carriage-ui.el ends here
