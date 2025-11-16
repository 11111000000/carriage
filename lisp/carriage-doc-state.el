;;; carriage-doc-state.el --- Persist/restore document state in Org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: org, persistence, convenience
;;
;; Specifications:
;;   spec/doc-state-v1.org
;;   spec/ui-v1.org
;;   spec/project-overview-v1.org
;;
;;; Commentary:
;; Store and restore Carriage parameters in the Org document itself.
;; A single top-level heading "* Carriage State" with a property drawer
;; is used as the canonical storage. Fallback reader supports #+PROPERTY
;; lines "CARRIAGE_* ...", but writing always updates the drawer.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org nil t)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage-llm-full-id "carriage-mode" (&optional backend provider model))
(declare-function carriage-llm-resolve-model "carriage-mode" (backend provider model))
(declare-function carriage-mode "carriage-mode" (&optional arg))
(declare-function carriage-log "carriage-logging" (fmt &rest args))

(defgroup carriage-doc-state nil
  "Persist and restore Carriage state in Org documents."
  :group 'applications
  :prefix "carriage-doc-state-")

(defcustom carriage-doc-state-save-on-save t
  "When non-nil, write current Carriage state to the document before each save (buffer-local)."
  :type 'boolean
  :group 'carriage-doc-state)
(make-variable-buffer-local 'carriage-doc-state-save-on-save)

(defcustom carriage-doc-state-sync-on-change t
  "When non-nil, update document properties on each state-changing command (buffer-local)."
  :type 'boolean
  :group 'carriage-doc-state)
(make-variable-buffer-local 'carriage-doc-state-sync-on-change)

(defcustom carriage-doc-state-heading "Carriage State"
  "Title of the top-level heading used to store Carriage properties."
  :type 'string
  :group 'carriage-doc-state)

(defun carriage-doc-state--bool->str (v)
  "Normalize boolean V to \"t\" or \"nil\" string."
  (if v "t" "nil"))

(defun carriage-doc-state--str->bool (s)
  "Normalize string S to boolean t/nil. Accepts t/true/1 and nil/false/0."
  (let ((x (downcase (format "%s" s))))
    (cond
     ((member x '("t" "true" "1" "yes" "on")) t)
     ((member x '("nil" "false" "0" "no" "off" "")) nil)
     (t nil))))

(defun carriage-doc-state--goto-or-create-heading ()
  "Move point to the Carriage State heading; create it if needed.
Returns point at the heading line."
  (unless (derived-mode-p 'org-mode)
    (user-error "carriage-doc-state: buffer is not in org-mode"))
  (save-excursion
    (goto-char (point-min))
    (let ((rx (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote carriage-doc-state-heading))))
      (if (re-search-forward rx nil t)
          (match-beginning 0)
        ;; Create heading at top (after file-wide #+ options, if any)
        (goto-char (point-min))
        ;; Skip file keywords (#+...)
        (while (looking-at-p "^[ \t]*#\\+")
          (forward-line 1))
        (let ((pos (point)))
          (insert (format "* %s\n:PROPERTIES:\n:END:\n" carriage-doc-state-heading))
          pos)))))

(defun carriage-doc-state--find-heading ()
  "Return position of Carriage State heading, or nil if absent."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((rx (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote carriage-doc-state-heading))))
        (when (re-search-forward rx nil t)
          (match-beginning 0))))))

(defun carriage-doc-state--within-heading (pos)
  "Narrow to heading at POS; return non-nil if successful."
  (when (and (numberp pos) (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char pos)
      (ignore-errors (org-back-to-heading t))
      (let ((beg (point))
            (end (save-excursion (org-end-of-subtree t t) (point))))
        (save-restriction
          (narrow-to-region beg end)
          t)))))

(defun carriage-doc-state--read-drawer ()
  "Read properties from the Carriage State drawer into an alist of (KEY . VAL)."
  (let ((pos (carriage-doc-state--find-heading)))
    (when pos
      (with-temp-buffer
        (let (alist)
          (save-excursion
            (with-current-buffer (current-buffer)
              ;; Use original buffer to extract properties via org-entry-get
              (with-current-buffer (window-buffer (selected-window))
                (save-excursion
                  (goto-char pos)
                  (ignore-errors (org-back-to-heading t))
                  (let* ((props (ignore-errors (org-entry-properties nil 'standard))))
                    (dolist (cell props)
                      ;; Keep :KEY as presented by org, e.g., "CAR_MODE"
                      (let ((k (car cell))
                            (v (cdr cell)))
                        (when (and (stringp k)
                                   (string-prefix-p "CAR_" k))
                          (push (cons k v) alist)))))))))
          alist)))))

(defun carriage-doc-state--read-properties-lines ()
  "Fallback: read file-level #+PROPERTY: CARRIAGE_* lines into an alist."
  (save-excursion
    (goto-char (point-min))
    (let ((alist '()))
      (while (re-search-forward "^[ \t]*\\(?:#\\+\\)?PROPERTY:[ \t]+\\(CARRIAGE_[A-Z0-9_]+\\)[ \t]+\\(.+\\)$" nil t)
        (let ((k (match-string 1))
              (v (string-trim (match-string 2))))
          (push (cons k v) alist)))
      alist)))

(defun carriage-doc-state--alist->plist (alist)
  "Turn ALIST of (KEY . VAL) into a plist with :KEY VAL (keys normalized to upper-case)."
  (let ((pl '()))
    (dolist (cell alist)
      (let* ((k (car cell))
             (v (cdr cell))
             (key (format ":%s" (upcase (string-remove-prefix ":" (format "%s" k))))))
        (setq pl (append pl (list (intern key) v)))))
    pl))

(defun carriage-doc-state-read (&optional buffer)
  "Read Carriage State from BUFFER (or current) and return a plist with keys:
:CAR_MODE :CAR_INTENT :CAR_SUITE :CAR_MODEL_ID :CAR_ENGINE :CAR_BRANCH_POLICY
:CAR_CTX_GPTEL :CAR_CTX_DOC :CAR_REPORT_POLICY :CAR_STAGE_POLICY
:CAR_ICONS :CAR_FLASH :CAR_AUDIO_NOTIFY

Fallback to #+PROPERTY: CARRIAGE_* when the heading is absent."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dl (carriage-doc-state--read-drawer))
           (pl (carriage-doc-state--alist->plist dl)))
      (if (null pl)
          (carriage-doc-state--alist->plist (carriage-doc-state--read-properties-lines))
        pl))))

(defun carriage-doc-state--set-prop (key val)
  "Set org property KEY to VAL in the Carriage State heading (create if missing)."
  (let ((pos (carriage-doc-state--goto-or-create-heading)))
    (save-excursion
      (goto-char pos)
      (ignore-errors (org-back-to-heading t))
      (let ((inhibit-read-only t))
        (org-set-property key (format "%s" val))))))

(defun carriage-doc-state--collect-current ()
  "Collect current buffer Carriage parameters as an alist of (KEY . VAL)."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (backend (and (boundp 'carriage-mode-backend) carriage-mode-backend))
         (provider (and (boundp 'carriage-mode-provider) carriage-mode-provider))
         (model  (and (boundp 'carriage-mode-model) carriage-mode-model))
         (full-id (ignore-errors (carriage-llm-full-id backend provider model)))
         (engine (and (fboundp 'carriage-apply-engine) (carriage-apply-engine)))
         (branch (and (eq engine 'git)
                      (boundp 'carriage-git-branch-policy)
                      carriage-git-branch-policy))
         (ctx-g (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context))
         (ctx-d (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context))
         (rpt   (and (boundp 'carriage-mode-report-open-policy) carriage-mode-report-open-policy))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy))
         (icons (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (flash (and (boundp 'carriage-mode-flash-patches) carriage-mode-flash-patches))
         (audio (and (boundp 'carriage-mode-audio-notify) carriage-mode-audio-notify)))
    (cl-remove-if-not
     #'identity
     (list
      (cons "CAR_MODE" (carriage-doc-state--bool->str t))
      (and intent (cons "CAR_INTENT" (format "%s" intent)))
      (and suite  (cons "CAR_SUITE"  (format "%s" suite)))
      (and full-id (cons "CAR_MODEL_ID" full-id))
      (and engine (cons "CAR_ENGINE" (format "%s" engine)))
      (and branch (cons "CAR_BRANCH_POLICY" (format "%s" branch)))
      (cons "CAR_CTX_GPTEL" (carriage-doc-state--bool->str ctx-g))
      (cons "CAR_CTX_DOC"   (carriage-doc-state--bool->str ctx-d))
      (and rpt   (cons "CAR_REPORT_POLICY" (format "%s" rpt)))
      (and stage (cons "CAR_STAGE_POLICY" (format "%s" stage)))
      (cons "CAR_ICONS"        (carriage-doc-state--bool->str icons))
      (cons "CAR_FLASH"        (carriage-doc-state--bool->str flash))
      (cons "CAR_AUDIO_NOTIFY" (carriage-doc-state--bool->str audio))))))

(defun carriage-doc-state-write (data &optional buffer)
  "Write DATA into the Carriage State drawer of BUFFER (or current).
DATA may be:
- a plist: (:CAR_INTENT \"Code\" :CAR_SUITE \"udiff\" …), or
- an alist: ((\"CAR_INTENT\" . \"Code\") …).

Returns t on success."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'org-mode)
      (user-error "carriage-doc-state-write: buffer is not in org-mode"))
    (let* ((alist
            (cond
             ;; plist
             ((and (listp data)
                   (keywordp (car data)))
              (let ((acc '()) (pl data))
                (while pl
                  (let ((k (car pl)) (v (cadr pl)))
                    (setq acc (append acc (list (cons (upcase (string-remove-prefix ":" (format "%s" k)))) v))))
                  (setq pl (cddr pl)))
                acc))
             ;; alist
             ((and (listp data) (consp (car data))) data)
             (t (user-error "Unsupported DATA format for carriage-doc-state-write")))))
      (dolist (cell alist)
        (carriage-doc-state--set-prop (car cell) (cdr cell)))
      t)))

(defun carriage-doc-state-write-current (&optional buffer)
  "Write current buffer Carriage parameters into the document and fold the block."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-doc-state-write (carriage-doc-state--collect-current) (current-buffer))
    (ignore-errors (carriage-doc-state-hide (current-buffer)))))

(defun carriage-doc-state-restore (&optional buffer)
  "Restore Carriage parameters from the document for BUFFER (or current).
Gracefully ignores missing keys."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((pl (carriage-doc-state-read (current-buffer))))
      ;; Intent
      (let ((v (plist-get pl :CAR_INTENT)))
        (when v (setq carriage-mode-intent (intern (format "%s" v)))))
      ;; Suite
      (let ((v (plist-get pl :CAR_SUITE)))
        (when v (setq carriage-mode-suite (intern (format "%s" v)))))
      ;; Model/backend/provider
      (let ((id (plist-get pl :CAR_MODEL_ID)))
        (when (and (stringp id) (> (length id) 0))
          (let* ((parts (split-string id ":" t))
                 (n (length parts))
                 (backend (and (>= n 1) (intern (nth 0 parts))))
                 (provider (and (>= n 3) (nth 1 parts)))
                 (model    (cond ((>= n 3) (nth 2 parts))
                                 ((= n 2) (nth 1 parts))
                                 ((= n 1) (nth 0 parts))
                                 (t id))))
            (when backend (setq carriage-mode-backend backend))
            (setq carriage-mode-provider (and provider (not (string-empty-p provider)) provider))
            (when model (setq carriage-mode-model model)))))
      ;; Engine + policy
      (let ((eng (plist-get pl :CAR_ENGINE)))
        (when eng
          (setq carriage-apply-engine (intern (format "%s" eng)))))
      (let ((pol (plist-get pl :CAR_BRANCH_POLICY)))
        (when (and pol (boundp 'carriage-git-branch-policy))
          (setq carriage-git-branch-policy (intern (format "%s" pol)))))
      ;; Context toggles
      (let ((g (plist-get pl :CAR_CTX_GPTEL)))
        (when g (setq-local carriage-mode-include-gptel-context (carriage-doc-state--str->bool g))))
      (let ((d (plist-get pl :CAR_CTX_DOC)))
        (when d (setq-local carriage-mode-include-doc-context (carriage-doc-state--str->bool d))))
      ;; Report policy
      (let ((rp (plist-get pl :CAR_REPORT_POLICY)))
        (when rp (setq-local carriage-mode-report-open-policy (intern (format "%s" rp)))))
      ;; Stage policy
      (let ((st (plist-get pl :CAR_STAGE_POLICY)))
        (when st (setq carriage-apply-stage-policy (intern (format "%s" st)))))
      ;; UI toggles
      (let ((ic (plist-get pl :CAR_ICONS)))
        (when ic (setq-local carriage-mode-use-icons (carriage-doc-state--str->bool ic))))
      (let ((fl (plist-get pl :CAR_FLASH)))
        (when fl (setq-local carriage-mode-flash-patches (carriage-doc-state--str->bool fl))))
      (let ((au (plist-get pl :CAR_AUDIO_NOTIFY)))
        (when au (setq-local carriage-mode-audio-notify (carriage-doc-state--str->bool au))))
      (force-mode-line-update t)
      t)))

(defun carriage-doc-state-hide (&optional buffer)
  "Fold property drawer of the Carriage State heading in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (let ((pos (carriage-doc-state--find-heading)))
        (when pos
          (save-excursion
            (goto-char pos)
            (cond
             ((fboundp 'org-fold-hide-drawer-or-block)
              (org-fold-hide-drawer-or-block t))
             ((fboundp 'org-hide-block-toggle)
              (org-hide-block-toggle t))
             (t nil))))))))

(defun carriage-doc-state-auto-enable ()
  "Find-file hook: auto-enable carriage-mode when CAR_MODE=t in the document.
Requires an Org buffer and a recognizable project root."
  (when (derived-mode-p 'org-mode)
    (condition-case _e
        (let* ((pl (carriage-doc-state-read (current-buffer)))
               (m  (plist-get pl :CAR_MODE))
               (on (carriage-doc-state--str->bool (or m ""))))
          (when on
            (when (and (fboundp 'carriage-project-root)
                       (carriage-project-root))
              (require 'carriage-mode)
              (carriage-mode 1))))
      (error nil))))

;; -------------------------------------------------------------------
;; v1.1+: Prefer file-level #+PROPERTY: CARRIAGE_* storage
;; - Override read/write to use #+PROPERTY lines as canonical storage.
;; - Hide those lines by default (using an invisibility spec).
;; - Keep drawer-based heading readable for backwards compatibility.

(defun carriage-doc-state--prop-key->file (key)
  "Map drawer key string like \"CAR_MODE\" to file-level key \"CARRIAGE_MODE\"."
  (let* ((s (format "%s" key)))
    (cond
     ((string-match-p "\\`CARRIAGE_" s) s)
     ((string-match-p "\\`CAR_" s)
      (concat "CARRIAGE_" (substring s 4)))
     (t (concat "CARRIAGE_" s)))))

(defun carriage-doc-state--file-key->drawer (key)
  "Map file-level key string like \"CARRIAGE_MODE\" to drawer key \"CAR_MODE\"."
  (let ((s (format "%s" key)))
    (if (string-prefix-p "CARRIAGE_" s)
        (concat "CAR_" (substring s 9))
      s)))

(defun carriage-doc-state--set-file-property (key val)
  "Upsert a single #+PROPERTY line for KEY (string) to VAL (string)."
  (save-excursion
    (let* ((inhibit-read-only t)
           (k (upcase (format "%s" key)))
           (line (format "#+PROPERTY: %s %s" k (or val "")))
           (rx (format "^[ \t]*#\\+PROPERTY:[ \t]+%s\\b" (regexp-quote k))))
      (goto-char (point-min))
      (if (re-search-forward rx nil t)
          (progn
            (goto-char (match-beginning 0))
            (delete-region (line-beginning-position) (line-end-position))
            (insert line))
        ;; Insert after existing #+ lines (file keywords)
        (goto-char (point-min))
        (while (looking-at-p "^[ \t]*#\\+")
          (forward-line 1))
        (let ((pos (point)))
          (unless (bolp) (insert "\n"))
          (goto-char pos)
          (insert line "\n"))))
    t))

(defun carriage-doc-state--write-to-file-properties (data &optional buffer)
  "Override writer: write DATA to #+PROPERTY: CARRIAGE_* lines in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((alist
            (cond
             ((and (listp data) (keywordp (car data)))
              (let ((acc '()) (pl data))
                (while pl
                  (let ((k (car pl)) (v (cadr pl)))
                    (setq acc (append acc
                                      (list (cons (upcase (string-remove-prefix ":" (format "%s" k))) v)))))
                  (setq pl (cddr pl)))
                acc))
             ((and (listp data) (consp (car data))) data)
             (t (user-error "Unsupported DATA format for carriage-doc-state-write")))))
      (dolist (cell alist)
        (let* ((k (car cell))
               (v (cdr cell))
               (file-k (carriage-doc-state--prop-key->file k)))
          (carriage-doc-state--set-file-property file-k v)))
      t)))

(defun carriage-doc-state--read-preferring-file (&optional buffer)
  "Read state from BUFFER preferring file-level #+PROPERTY: CARRIAGE_* lines.
Falls back to drawer-based heading when file-level properties are absent."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((props (carriage-doc-state--read-properties-lines)))
      (if (and props (> (length props) 0))
          (let* ((mapped (mapcar (lambda (cell)
                                   (cons (carriage-doc-state--file-key->drawer (car cell))
                                         (cdr cell)))
                                 props)))
            (carriage-doc-state--alist->plist mapped))
        ;; Fallback to drawer
        (carriage-doc-state--alist->plist (carriage-doc-state--read-drawer))))))

(defun carriage-doc-state-install-save-hook ()
  "Install before-save hook to persist current state to #+PROPERTY lines."
  (add-hook 'before-save-hook #'carriage-doc-state-write-current nil t))

(defun carriage-doc-state-remove-save-hook ()
  "Remove before-save hook for persisting document state."
  (remove-hook 'before-save-hook #'carriage-doc-state-write-current t))

(defvar-local carriage-doc-state--props-overlay nil
  "Overlay covering folded file-level Carriage property lines.")

(defun carriage-doc-state--props-range ()
  "Return (BEG END COUNT) for contiguous file-level CARRIAGE_* property lines, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((rx "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_[A-Z0-9_]+\\b.*$")
          (beg nil) (end nil) (count 0))
      (while (re-search-forward rx nil t)
        (unless beg (setq beg (line-beginning-position)))
        (setq end (line-end-position))
        (setq count (1+ count)))
      (when beg (list beg end count)))))

(defun carriage-doc-state--hide-file-properties ()
  "Fold file-level #+PROPERTY: CARRIAGE_* lines into a single overlay.
The overlay is marked invisible via `carriage-doc-state' and shows a one-line
placeholder that can be toggled with `carriage-doc-state-toggle-visibility'."
  (when (derived-mode-p 'org-mode)
    (let ((range (carriage-doc-state--props-range)))
      (when range
        (let ((b (nth 0 range)) (e (nth 1 range)) (n (nth 2 range)))
          (when (overlayp carriage-doc-state--props-overlay)
            (delete-overlay carriage-doc-state--props-overlay))
          (let ((ov (make-overlay b e)))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'invisible 'carriage-doc-state)
            (add-to-invisibility-spec 'carriage-doc-state)
            (overlay-put ov 'before-string
                         (propertize
                          (format "… Carriage State properties (%d) — M-x carriage-doc-state-toggle-visibility …\n" n)
                          'face 'shadow))
            (setq carriage-doc-state--props-overlay ov)
            t))))))

;; Hook into existing API:
;; - Override read/write to use file-level properties.
;; - After hide, also hide file-level property lines.
(ignore-errors
  (advice-add 'carriage-doc-state-read :override #'carriage-doc-state--read-preferring-file)
  (advice-add 'carriage-doc-state-write :override #'carriage-doc-state--write-to-file-properties)
  (advice-add 'carriage-doc-state-hide :after (lambda (&rest _)
                                                (ignore-errors (carriage-doc-state--hide-file-properties)))))

(defun carriage-doc-state--show-file-properties ()
  "Remove invisibility from #+PROPERTY: CARRIAGE_* lines in the current buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((rx "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_[A-Z0-9_]+\\b.*$"))
        (goto-char (point-min))
        (while (re-search-forward rx nil t)
          (let ((b (line-beginning-position))
                (e (line-end-position)))
            (remove-text-properties b e '(invisible nil)))))
      (when (and (listp buffer-invisibility-spec)
                 (member 'carriage-doc-state buffer-invisibility-spec))
        (remove-from-invisibility-spec 'carriage-doc-state))
      t)))

;;;###autoload
(defun carriage-doc-state-toggle-visibility ()
  "Toggle visibility of document-level Carriage properties.

- When hidden, reveal file-level lines \"#+PROPERTY: CARRIAGE_*\".
- When visible, hide them using a buffer-local invisibility spec.

Best-effort: if a \"Carriage State\" property drawer exists,
this command does not force it open/closed; it focuses on file-level
properties visibility."
  (interactive)
  (let* ((hidden (and (listp buffer-invisibility-spec)
                      (member 'carriage-doc-state buffer-invisibility-spec))))
    (if hidden
        (progn
          (carriage-doc-state--show-file-properties)
          (message "Carriage: показаны строки #+PROPERTY: CARRIAGE_*"))
      (progn
        (carriage-doc-state--hide-file-properties)
        (message "Carriage: скрыты строки #+PROPERTY: CARRIAGE_*"))))
  t)

(provide 'carriage-doc-state)
;; -------------------------------------------------------------------
;; v1.2 — Prefer a dedicated, foldable block at the top of the document:
;;   #+begin_carriage
;;   CARRIAGE_MODE t
;;   CARRIAGE_INTENT Code
;;   ...
;;   #+end_carriage
;;
;; Behavior:
;; - Read priority: begin_carriage block → file-level #+PROPERTY: CARRIAGE_* → drawer heading.
;; - Write: upsert/replace begin_carriage block body with current keys.
;; - Hide: fold the begin_carriage block on demand.

(defun carriage-doc-state--carriage-block-range ()
  "Return cons (BODY-BEG . BODY-END) for #+begin_carriage…#+end_carriage block, or nil.
BODY-BEG points to the first char after the begin line newline.
BODY-END points to the beginning of the #+end_carriage line."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              (beg nil) (end nil))
          (when (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
            (setq beg (line-end-position))
            (when (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
              (setq end (line-beginning-position))))
          (when (and beg end (> end beg))
            (cons (1+ beg) end)))))))

(defun carriage-doc-state--parse-carriage-block (beg end)
  "Parse key/value lines between BEG and END of a begin_carriage block.
Accepts lines in the forms:
  KEY: VALUE
  KEY VALUE
Ignores empty lines and lines starting with ; or #.
Returns an alist of (KEY . VAL) where KEY is the drawer-style key (e.g., \"CAR_MODE\")."
  (let ((alist '()))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((ln (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (s (string-trim ln)))
            (unless (or (string-empty-p s)
                        (string-prefix-p ";" s)
                        (string-prefix-p "#" s))
              ;; Accept "KEY: VALUE" or "KEY VALUE" (first token is key, rest is value)
              (let* ((parts (split-string s "[: \t]" t))
                     (kraw (car parts))
                     (rest (string-trim (substring s (length kraw))))
                     (val (string-trim (replace-regexp-in-string "\\`[: \t]+" "" rest))))
                (when (and (stringp kraw) (not (string-empty-p kraw)))
                  ;; Map file-level CARRIAGE_* → drawer CAR_*
                  (let ((k (carriage-doc-state--file-key->drawer kraw)))
                    (push (cons k val) alist))))))
          (forward-line 1))))
    (nreverse alist)))

(defun carriage-doc-state--write-carriage-block (alist)
  "Upsert the begin_carriage block with ALIST of (KEY . VAL) pairs.
Keys may be drawer-style (\"CAR_*\") or file-style (\"CARRIAGE_*\"); they are
normalized to file-style CARRIAGE_* inside the block."
  (let* ((norm
          (mapcar (lambda (cell)
                    (let* ((k (car cell))
                           (v (cdr cell))
                           (fk (carriage-doc-state--prop-key->file k)))
                      (cons fk (format "%s" v))))
                  alist))
         ;; Keep a stable order by sorting keys
         (sorted (sort norm (lambda (a b) (string< (car a) (car b)))))
         (body (mapconcat (lambda (cell)
                            (format "%s %s" (car cell) (cdr cell)))
                          sorted "\n"))
         (block (concat "#+begin_carriage\n" body "\n#+end_carriage\n")))
    (save-excursion
      (save-restriction
        (widen)
        (let ((range (carriage-doc-state--carriage-block-range)))
          (cond
           (range
            (let ((beg (car range))
                  (end (cdr range))
                  (inhibit-read-only t))
              ;; Replace body only
              (delete-region beg end)
              (goto-char beg)
              (insert body "\n")
              t))
           (t
            ;; Insert near the very top: after initial #+ lines, before content
            (goto-char (point-min))
            (while (looking-at-p "^[ \t]*#\\+")
              (forward-line 1))
            (let ((inhibit-read-only t))
              (unless (bolp) (insert "\n"))
              (insert block))
            t)))))))

(defun carriage-doc-state--hide-carriage-block ()
  "Fold the #+begin_carriage block if present (best-effort)."
  (when (derived-mode-p 'org-mode)
    (let ((range (carriage-doc-state--carriage-block-range)))
      (when range
        (save-excursion
          (goto-char (car range))
          (when (re-search-backward "^[ \t]*#\\+begin_carriage\\b" nil t)
            (cond
             ((fboundp 'org-hide-block-toggle)
              (org-hide-block-toggle t))
             ((fboundp 'org-fold-region)
              (let ((a (line-beginning-position))
                    (b (progn
                         (when (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
                           (line-end-position)))))
                (when (and a b) (org-fold-region a b t))))
             (t nil))))))))

;; Override readers/writers to prioritize the carriage block

(defun carriage-doc-state--read-preferring-file (&optional buffer)
  "Read state with priority:
1) #+begin_carriage block,
2) file-level #+PROPERTY: CARRIAGE_*,
3) drawer heading \"Carriage State\"."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((range (carriage-doc-state--carriage-block-range)))
      (if range
          (let* ((alist (carriage-doc-state--parse-carriage-block (car range) (cdr range))))
            (carriage-doc-state--alist->plist alist))
        ;; Fallbacks: file-level properties → drawer
        (let* ((props (carriage-doc-state--read-properties-lines)))
          (if (and props (> (length props) 0))
              (let* ((mapped (mapcar (lambda (cell)
                                       (cons (carriage-doc-state--file-key->drawer (car cell))
                                             (cdr cell)))
                                     props)))
                (carriage-doc-state--alist->plist mapped))
            (carriage-doc-state--alist->plist (carriage-doc-state--read-drawer))))))))

(defun carriage-doc-state--write-to-file-properties (data &optional buffer)
  "Write DATA to the #+begin_carriage block (upsert/replace).
DATA may be a plist (:CAR_* …) or an alist of (\"CAR_*\" . VAL)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((alist
            (cond
             ;; plist → alist
             ((and (listp data) (keywordp (car data)))
              (let ((acc '()) (pl data))
                (while pl
                  (let ((k (car pl)) (v (cadr pl)))
                    (setq acc (append acc (list (cons (upcase (string-remove-prefix ":" (format "%s" k))) v)))))
                  (setq pl (cddr pl)))
                acc))
             ;; alist
             ((and (listp data) (consp (car data))) data)
             (t (user-error "Unsupported DATA format for carriage-doc-state-write")))))
      (carriage-doc-state--write-carriage-block alist))))

;; Also fold the new block on hide
(ignore-errors
  (advice-add 'carriage-doc-state-hide :after (lambda (&rest _)
                                                (ignore-errors (carriage-doc-state--hide-carriage-block)))))

;;; carriage-doc-state.el ends here
