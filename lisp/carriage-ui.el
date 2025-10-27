;;; carriage-ui.el --- Keymap and minimal UI helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-utils)

(defvar carriage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-RET") #'carriage-send-buffer)
    (define-key map (kbd "C-c RET")   #'carriage-send-subtree)
    (define-key map (kbd "C-c C-c")   #'carriage-apply-at-point)
    (define-key map (kbd "C-c !")     #'carriage-apply-last-iteration)
    (define-key map (kbd "C-c ?")     #'carriage-dry-run-at-point)
    (define-key map (kbd "C-c b c")   #'carriage-wip-checkout)
    (define-key map (kbd "C-c b l")   #'carriage-show-log)
    (define-key map (kbd "C-c b r")   #'carriage-wip-reset-soft)
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
  "Return org outline path (A › B › C) at point, or nil if not available."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-get-outline-path))
    (let* ((path (ignore-errors (org-get-outline-path t t))))
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
  (and (boundp 'carriage-mode-use-icons)
       carriage-mode-use-icons
       (require 'all-the-icons nil t)))

(defun carriage-ui--icon (key)
  "Return icon string for KEY using all-the-icons, or nil if unavailable."
  (when (carriage-ui--icons-available-p)
    (pcase key
      ;; Profile
      ('ask  (when (fboundp 'all-the-icons-material)
               (all-the-icons-material "chat" :v-adjust -0.1)))
      ('code (when (fboundp 'all-the-icons-material)
               (all-the-icons-material "code" :v-adjust -0.1)))
      ;; Model/backend
      ('model (cond
               ((fboundp 'all-the-icons-octicon) (all-the-icons-octicon "cpu"))
               ((fboundp 'all-the-icons-material) (all-the-icons-material "memory"))
               (t nil)))
      ;; Actions
      ('dry    (when (fboundp 'all-the-icons-faicon)   (all-the-icons-faicon "flask")))
      ('apply  (when (fboundp 'all-the-icons-material) (all-the-icons-material "check_circle")))
      ('all    (cond
                ((fboundp 'all-the-icons-octicon) (all-the-icons-octicon "rocket"))
                ((fboundp 'all-the-icons-octicon) (all-the-icons-octicon "play"))
                (t nil)))
      ('abort  (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "stop")))
      ('report (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "file-text")))
      ('diff   (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "git-compare")))
      ('ediff  (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "diff")))
      ('wip    (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "git-branch")))
      ('reset  (when (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "history")))
      (_ nil))))

(defun carriage-ui--header-line ()
  "Build header-line: project › buffer › org-outline-path with graceful degradation.
Respects =carriage-mode-headerline-max-width' and hides outline on narrow/TTY.
When outline is visible in org-mode, make it clickable to jump to heading."
  (let* ((project (carriage-ui--project-name))
         (bufname (buffer-name))
         (outline (carriage-ui--org-outline-path))
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
         (base (concat project sep bufname))
         (full (if show-outline
                   (concat base (when show-outline sep) (or outline ""))
                 base)))
    ;; If exceeds, start truncating from outline, then buffer, then project
    (when (> (length full) avail)
      (when show-outline
        (let ((ol-max (max 10 (- avail (length base) (length sep)))))
          (setq outline (carriage-ui--truncate-middle outline ol-max))
          (setq full (concat base sep outline))))
      (when (> (length full) avail)
        (let* ((buf-max (max 10 (- avail (length project) (length sep)))))
          (setq bufname (carriage-ui--truncate-middle bufname buf-max))
          (setq base (concat project sep bufname))
          (setq full (if show-outline (concat base sep outline) base))))
      (when (> (length full) avail)
        (setq project (carriage-ui--truncate-middle project (max 5 (- avail (length sep) (length bufname)))))
        (setq base (concat project sep bufname))
        (setq full (if show-outline (concat base sep outline) base))))
    ;; Clickable outline (optional)
    (if show-outline
        (let* ((omap (let ((m (make-sparse-keymap)))
                       (define-key m [header-line mouse-1] #'carriage-ui-goto-outline)
                       m))
               (oprop (propertize (or outline "")
                                  'mouse-face 'mode-line-highlight
                                  'help-echo "Перейти к заголовку (mouse-1)"
                                  'local-map omap)))
          (concat base sep oprop))
      full)))

(defun carriage-ui--ml-button (label fn help)
  "Return a propertized clickable LABEL that invokes FN. HELP shows as tooltip."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] fn)
    (propertize label
                'mouse-face 'mode-line-highlight
                'help-echo help
                'local-map map)))

(defun carriage-ui--toggle (label var-sym fn help)
  "Build a toggle button with LABEL; highlight when VAR-SYM is non-nil."
  (let* ((on (and (boundp var-sym) (symbol-value var-sym)))
         (lbl (if on (propertize label 'face 'mode-line-emphasis) label)))
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
    (let ((buf (get-buffer "*carriage-report*")))
      (if buf
          (progn (pop-to-buffer buf)
                 (message "Select a row, then press RET or [Diff]"))
        (user-error "Нет доступного отчёта для Diff")))))

(defun carriage-ui--ediff-button ()
  "Open Ediff for report item if available; otherwise switch to report."
  (interactive)
  (if (carriage-ui--maybe-in-report-buffer)
      (call-interactively #'carriage-report-ediff-at-point)
    (let ((buf (get-buffer "*carriage-report*")))
      (if buf
          (progn (pop-to-buffer buf)
                 (message "Select a row, then press e or [Ediff]"))
        (user-error "Нет доступного отчёта для Ediff")))))

(defun carriage-ui--modeline ()
  "Build Carriage modeline segment (M3: icons optional + spinner + extended actions)."
  (let* ((use-icons (carriage-ui--icons-available-p))
         ;; Profile
         (profile-label
          (if use-icons
              (or (if (and (boundp 'carriage-mode-profile)
                           (eq carriage-mode-profile 'Ask))
                      (carriage-ui--icon 'ask)
                    (carriage-ui--icon 'code))
                  (format "[%s]" (if (eq carriage-mode-profile 'Ask) "Ask" "Code")))
            (format "[%s]" (if (and (boundp 'carriage-mode-profile)
                                    (eq carriage-mode-profile 'Ask))
                               "Ask" "Code"))))
         (profile-btn (carriage-ui--ml-button profile-label
                                              #'carriage-toggle-profile
                                              "Toggle Ask/Code profile"))
         ;; Backend:Model
         (backend-str (let ((b (and (boundp 'carriage-mode-backend) carriage-mode-backend)))
                        (cond
                         ((symbolp b) (symbol-name b))
                         ((stringp b) b)
                         (t "backend"))))
         (model-str (or (and (boundp 'carriage-mode-model) carriage-mode-model) "model"))
         (bm-text (format "[%s:%s]" backend-str model-str))
         (bm-label (if use-icons
                       (let ((ic (
                                  carriage-ui--icon 'model)))
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
         (reset-label  (or (and use-icons (carriage-ui--icon 'reset))  "[Reset]"))
         (dry    (carriage-ui--ml-button dry-label    #'carriage-dry-run-at-point      "Dry-run at point"))
         (apply  (carriage-ui--ml-button apply-label  #'carriage-apply-at-point        "Apply at point"))
         (all    (carriage-ui--ml-button all-label    #'carriage-apply-last-iteration  "Apply last iteration"))
         (abort  (carriage-ui--ml-button abort-label  #'carriage-abort-current         "Abort current request"))
         (report (carriage-ui--ml-button report-label #'carriage-report-open           "Open report buffer"))
         (diff   (carriage-ui--ml-button diff-label   #'carriage-ui--diff-button       "Show diff (report)"))
         (ediff  (carriage-ui--ml-button ediff-label  #'carriage-ui--ediff-button      "Open Ediff (report)"))
         (wip    (carriage-ui--ml-button wip-label    #'carriage-wip-checkout          "Switch to WIP branch"))
         (reset  (carriage-ui--ml-button reset-label  #'carriage-wip-reset-soft        "Soft reset last commit"))
         ;; Toggles (text conveys meaning; active ones emphasized)
         (t-auto  (carriage-ui--toggle "[AutoRpt]"    'carriage-mode-auto-open-report   #'carriage-toggle-auto-open-report   "Toggle auto-open report"))
         (t-diffs (carriage-ui--toggle "[ShowDiffs]"  'carriage-mode-show-diffs         #'carriage-toggle-show-diffs        "Toggle show diffs before apply"))
         (t-all   (carriage-ui--toggle "[ConfirmAll]" 'carriage-mode-confirm-apply-all  #'carriage-toggle-confirm-apply-all "Toggle confirm apply-all"))
         (t-icons (carriage-ui--toggle "[Icons]"      'carriage-mode-use-icons          #'carriage-toggle-use-icons         "Toggle icons in UI")))
    (mapconcat #'identity
               (list profile-btn backend-model-btn state
                     dry apply all abort report diff ediff wip reset
                     t-auto t-diffs t-all t-icons)
               " ")))

(provide 'carriage-ui)
;;; carriage-ui.el ends here
