;;; carriage-keyspec.el --- Centralized key binding model (v1.2) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-keyspec nil
  "Centralized key binding model for Carriage."
  :group 'applications
  :prefix "carriage-keys-")

(defcustom carriage-keys-profile 'classic
  "Active keyspec profile (classic|vimish|custom)."
  :type '(choice (const classic) (const vimish) (const custom))
  :group 'carriage-keyspec)

(defcustom carriage-keys-prefix "C-c e "
  "Prefix for all Carriage mode keybindings."
  :type 'string
  :group 'carriage-keyspec)

(defcustom carriage-keys-prefix-alias nil
  "Optional additional prefix key sequence (e.g., \"C-c C-e \") as an alias for `carriage-keys-prefix'.
Used for menu binding etc., not for all suffix bindings."
  :type '(choice (const nil) string)
  :group 'carriage-keyspec)

(defvar carriage-keys--profile-overlays
  '((classic . nil)
    (vimish  . nil)
    (custom  . nil))
  "Optional profile overlays to add/remove keys for actions.
Each value is a plist with :add and/or :remove lists of (:id ID :keys (..)).")

(defvar carriage-keys--spec
  '(
    ;; Tools/model/context
    (:id model-select :cmd carriage-select-model :keys ("m") :contexts (carriage) :section tools :desc-key :model-select)
    (:id toggle-ctx   :cmd carriage-toggle-include-gptel-context :keys ("t c") :contexts (carriage) :section tools :desc-key :toggle-ctx)
    (:id toggle-doc   :cmd carriage-toggle-include-doc-context   :keys ("t f") :contexts (carriage) :section tools :desc-key :toggle-doc)
    ;; Suite/Intent (tools)
    (:id select-suite :cmd carriage-select-suite                 :keys ("s")   :contexts (carriage) :section tools :desc-key :select-suite)
    (:id toggle-intent :cmd carriage-toggle-intent               :keys ("i")   :contexts (carriage) :section tools :desc-key :toggle-intent)
    (:id menu         :cmd carriage-keys-open-menu               :keys ("e")   :contexts (carriage) :section tools :desc-key :menu)
    ;; Actions
    (:id dry-run      :cmd carriage-dry-run-at-point        :keys ("d")     :contexts (carriage) :section act :desc-key :dry-run)
    (:id apply        :cmd carriage-apply-at-point-or-region :keys ("a")     :contexts (carriage) :section act :desc-key :apply)
    (:id apply-all    :cmd carriage-apply-last-iteration    :keys ("A")     :contexts (carriage) :section act :desc-key :apply-all)
    (:id abort        :cmd carriage-abort-current           :keys ("x")     :contexts (carriage) :section act :desc-key :abort)
    (:id send-buffer  :cmd carriage-send-buffer             :keys ("RET")   :contexts (carriage) :section act :desc-key :send-buffer)
    (:id send-subtree :cmd carriage-send-subtree            :keys ("M-RET") :contexts (carriage) :section act :desc-key :send-subtree)
    (:id report       :cmd carriage-report-open             :keys ("r")     :contexts (carriage) :section tools :desc-key :report)
    (:id clean        :cmd carriage-clear-patch-blocks     :keys ("C")     :contexts (carriage) :section act :desc-key :clean)
    ;; Report context actions (available in report buffers under the configured prefix)
    (:id report-diff  :cmd carriage-report-show-diff-at-point :keys ("d")   :contexts (report)   :section act   :desc-key :report-diff)
    (:id report-ediff :cmd carriage-report-ediff-at-point     :keys ("e")   :contexts (report)   :section act   :desc-key :report-ediff)
    (:id report-apply :cmd carriage-report-apply-at-point     :keys ("a")   :contexts (report)   :section act   :desc-key :report-apply)
    ;; Git/WIP
    (:id wip          :cmd carriage-wip-checkout            :keys ("w")  :contexts (carriage) :section session :desc-key :wip)
    (:id reset        :cmd carriage-wip-reset-soft          :keys ("R")  :contexts (carriage) :section session :desc-key :reset)
    (:id commit-all   :cmd carriage-commit-changes          :keys ("c")  :contexts (carriage) :section session :desc-key :commit-all)
    (:id commit-last  :cmd carriage-commit-last-iteration   :keys ("I")     :contexts (carriage) :section session :desc-key :commit-last)
    ;; Global
    (:id show-log     :cmd carriage-show-log                :keys ("L")     :contexts (carriage report global) :section logs :desc-key :show-log)
    (:id show-traffic :cmd carriage-show-traffic            :keys ("T")     :contexts (carriage report global) :section logs :desc-key :show-traffic)
    (:id aux-quit     :cmd quit-window                      :keys ("q")     :contexts (report log traffic)     :section navigate :desc-key :quit)
    (:id open-buffer  :cmd carriage-open-buffer             :keys ("e")     :contexts (global)   :section session :desc-key :open-buffer)
    ;; Engine
    (:id engine       :cmd carriage-select-apply-engine     :keys ("E")  :contexts (carriage) :section tools :desc-key :engine)
    )
  "Keyspec: list of action plists with :id :cmd :keys :contexts :section :desc-key.
All keys are relative to carriage-keys-prefix (default \"C-c e \").")

;;;###autoload
(defun carriage-keys-register-actions (actions)
  "Register or override keyspec actions.

ACTIONS is a list of plists. Each plist should contain at least:
  :id SYMBOL     Unique action identifier.
  :cmd SYMBOL    Interactive command symbol.

Optional keys:
  :keys (LIST OF STR)   Keys relative to `carriage-keys-prefix' (e.g., (\"n\") not full prefix).
  :contexts (LIST)      Contexts like (carriage report global org).
  :section SYMBOL       Grouping for menu (navigate act session tools logs).
  :desc-key SYMBOL      i18n key for labels/tooltips.

If an :id already exists, its entry is replaced. Otherwise, a new entry is appended."
  (dolist (pl actions)
    (let ((id (plist-get pl :id)))
      (when id
        (setq carriage-keys--spec
              (nconc
               (cl-remove-if (lambda (el) (eq (plist-get el :id) id))
                             carriage-keys--spec)
               (list pl))))))
  t)

(defun carriage-keys--ensure-kbd (key)
  "Return a kbd string for KEY under `carriage-keys-prefix'.
KEY may be a single token (\"m\") or a space-separated sequence (\"t c\")."
  (let* ((prefix (or carriage-keys-prefix "C-c e "))
         (ks (string-trim key)))
    (kbd (concat prefix ks))))

(defun carriage-keys--actions-for-context (context)
  "Return actions from keyspec applicable to CONTEXT."
  (cl-remove-if-not
   (lambda (pl)
     (let ((cs (plist-get pl :contexts)))
       (or (null cs) (memq context cs))))
   carriage-keys--spec))

(defun carriage-keys--actions-for-contexts (contexts)
  "Return merged action list for CONTEXTS with left-to-right priority.
Earlier contexts in CONTEXTS take precedence over later ones by :id."
  (let ((seen (make-hash-table :test 'eq))
        (acc '()))
    (dolist (ctx contexts)
      (dolist (pl (carriage-keys--actions-for-context ctx))
        (let ((id (plist-get pl :id)))
          (unless (gethash id seen)
            (puthash id t seen)
            (push pl acc)))))
    (nreverse acc)))

(defun carriage-keys--current-contexts ()
  "Detect active contexts in current buffer with priority order.
- In carriage buffers: (carriage [report|log|traffic?] org global)
- In report/log/traffic buffers: (that-context org global)
- Else: include `org' when in Org buffers; add `global' only if `carriage-global-mode' is on."
  (let* ((in-carriage (and (boundp 'carriage-mode) carriage-mode))
         (is-report  (derived-mode-p 'carriage-report-mode))
         (is-log     (string= (buffer-name) "*carriage-log*"))
         (is-traffic (string= (buffer-name) "*carriage-traffic*"))
         (is-org     (derived-mode-p 'org-mode))
         (ctxs '()))
    (when in-carriage (push 'carriage ctxs))
    (when is-report   (push 'report ctxs))
    (when is-log      (push 'log ctxs))
    (when is-traffic  (push 'traffic ctxs))
    (when is-org      (push 'org ctxs))
    ;; Global is available always inside carriage-mode; outside only if carriage-global-mode is enabled.
    (when (or in-carriage (bound-and-true-p carriage-global-mode))
      (setq ctxs (append ctxs (list 'global))))
    (or ctxs (when (bound-and-true-p carriage-global-mode) '(global)))))

(defun carriage-keys--apply-action (map action)
  "Apply ACTION binding(s) to MAP according to keyspec + profile overlays."
  (let* ((id   (plist-get action :id))
         (cmd  (plist-get action :cmd))
         (keys (copy-sequence (or (plist-get action :keys) '()))))
    (when (symbolp cmd)
      ;; Apply profile overlays: remove/add
      (let* ((ov (alist-get carriage-keys-profile carriage-keys--profile-overlays))
             (rm (plist-get ov :remove))
             (ad (plist-get ov :add))
             (rm-keys (cl-loop for el in rm
                               when (eq (plist-get el :id) id)
                               append (or (plist-get el :keys) '())))
             (ad-keys (cl-loop for el in ad
                               when (eq (plist-get el :id) id)
                               append (or (plist-get el :keys) '()))))
        (dolist (rk rm-keys)
          (setq keys (delete rk keys)))
        (dolist (ak ad-keys)
          (push ak keys)))
      ;; Bind all effective keys
      (dolist (k (delete-dups (delq nil keys)))
        (let* ((seq (carriage-keys--ensure-kbd k))
               (ok t)
               (i 0))
          ;; Skip binding if any prefix of seq is already bound to a non-prefix command in MAP.
          (while (and ok (< i (1- (length seq))))
            (let* ((sub (cl-subseq seq 0 (1+ i)))
                   (binding (lookup-key map sub)))
              (when (and binding (not (keymapp binding)))
                (setq ok nil)))
            (setq i (1+ i)))
          (when ok
            (define-key map seq cmd)))))))

(defun carriage-keys-apply-to (map context)
  "Apply keyspec to MAP for CONTEXT."
  (dolist (act (carriage-keys--actions-for-context context))
    (carriage-keys--apply-action map act))
  map)

(defun carriage-keys-apply-prefix-suffixes (map context)
  "Apply keyspec of CONTEXT to prefix MAP by binding suffix keys relative to MAP.

This is intended for true prefix maps already assigned to a leading prefix
derived from =carriage-keys-prefix'. The keys from keyspec are bound WITHOUT the =carriage-keys-prefix'
added. For example:
- \"t c\" in keyspec becomes (kbd \"t c\") inside MAP,
- \"RET\" becomes (kbd \"RET\") inside MAP.

Profile overlays (:add/:remove) are respected similar to =carriage-keys--apply-action'."
  (dolist (act (carriage-keys--actions-for-context context))
    (let* ((id   (plist-get act :id))
           (cmd  (plist-get act :cmd))
           (keys (copy-sequence (or (plist-get act :keys) '()))))
      (when (and (symbolp cmd) keys)
        (let* ((ov (alist-get carriage-keys-profile carriage-keys--profile-overlays))
               (rm (plist-get ov :remove))
               (ad (plist-get ov :add))
               (rm-keys (cl-loop for el in rm
                                 when (eq (plist-get el :id) id)
                                 append (or (plist-get el :keys) '())))
               (ad-keys (cl-loop for el in ad
                                 when (eq (plist-get el :id) id)
                                 append (or (plist-get el :keys) '()))))
          (dolist (rk rm-keys)
            (setq keys (delete rk keys)))
          (dolist (ak ad-keys)
            (push ak keys)))
        (dolist (k (delete-dups (delq nil keys)))
          (define-key map (kbd (string-trim k)) cmd)))))
  map)

(defun carriage-keys-apply-multi (map contexts)
  "Apply keyspec for CONTEXTS to MAP in order; later contexts override earlier.
Example: (global carriage) → локальные биндинги перекрывают глобальные."
  (dolist (ctx contexts)
    (carriage-keys-apply-to map ctx))
  map)

(defun carriage-keys-apply-known-keymaps ()
  "Apply keyspec to known Carriage keymaps with proper context priority.
- carriage buffers:
  - when `carriage-mode-use-transient' is non-nil, bind the bare prefix (from
    `carriage-keys-prefix') to the menu command and DO NOT bind any suffix
    sequences under this prefix in `carriage-mode-map' to avoid \"non-prefix\"
    errors;
  - when transient is nil, install full prefix sequences for (global carriage).
- report/log/traffic buffers: install (global report|log/traffic) sequences.

This avoids binding conflicts where a bare prefix key is a command (menu)
and therefore cannot also serve as a prefix for longer sequences."
  ;; Carriage buffers
  (when (and (boundp 'carriage-mode-map) (keymapp carriage-mode-map))
    (let ((base (string-trim-right (or carriage-keys-prefix "C-c e ")
                                   "[ \t\n\r]+")))
      (if (and (boundp 'carriage-mode-use-transient) carriage-mode-use-transient)
          (progn
            ;; Do not install any suffixes in carriage-mode-map when transient is ON.
            ;; Only bind the bare prefix to open the menu. Do NOT bind longer sequences
            ;; (like "C-c e e") here since base is a non-prefix command in this map.
            (define-key carriage-mode-map (kbd base) #'carriage-keys-open-menu))
        ;; Transient is OFF → do not install absolute prefix sequences into carriage-mode-map.
        ;; Per-buffer bindings (e.g., C-c e RET) are installed by carriage-mode itself
        ;; using emulation maps; ensure bare prefix is not a command here.
        (define-key carriage-mode-map (kbd base) nil))))

  ;; Child modes (best-effort: apply if the maps are defined)
  ;; Apply report-specific bindings only to report map; apply log/traffic to aux map if present.
  (dolist (mp '(carriage-report-mode-map carriage-aux-mode-map))
    (when (and (boundp mp) (keymapp (symbol-value mp)))
      (carriage-keys-apply-multi
       (symbol-value mp)
       (if (eq mp 'carriage-report-mode-map)
           '(global report)
         '(global log traffic)))))
  ;; Org buffers: install org-only actions (e.g., task-new) under the Carriage prefix
  (when (and (boundp 'org-mode-map) (keymapp org-mode-map))
    (carriage-keys-apply-to org-mode-map 'org))
  ;; Legacy alias: C-c ! applies last iteration (UI v1 legacy)
  (ignore-errors
    (global-set-key (kbd "C-c !") #'carriage-apply-last-iteration))
  t)

(defun carriage-keys-first-key (id)
  "Return the first effective key (kbd string) for action ID in current profile."
  (let* ((act (cl-find id carriage-keys--spec :key (lambda (pl) (plist-get pl :id))))
         (keys (and act (plist-get act :keys))))
    (when (and act keys)
      (carriage-keys--ensure-kbd (car keys)))))

(defun carriage-keys-lint-collisions ()
  "Return a list of (key . (ID1 ID2 ...)) for collisions inside keyspec."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (act carriage-keys--spec)
      (let* ((id (plist-get act :id))
             (keys (plist-get act :keys)))
        (dolist (k keys)
          (let* ((kbd (key-description (carriage-keys--ensure-kbd k)))
                 (cur (gethash kbd table '())))
            (puthash kbd (cons id cur) table)))))
    (cl-loop for k being the hash-keys of table
             for v = (gethash k table)
             when (> (length v) 1)
             collect (cons k (nreverse v)))))

;;;###autoload
(defun carriage-keys-open-menu ()
  "Open Carriage action menu from keyspec.
If transient is available, show multi-column grouped menu with i18n headers.
Fallback: completing-read (group prefix in labels)."
  (interactive)
  (let* ((ctxs (carriage-keys--current-contexts))
         (all-acts (carriage-keys--actions-for-contexts ctxs))
         ;; exclude :menu itself
         (acts (cl-remove-if (lambda (pl) (eq (plist-get pl :id) 'menu)) all-acts))
         (sections '(navigate act session tools logs)))
    (if (and (require 'transient nil t) (fboundp 'transient-define-prefix))
        (let* ((_ (require 'carriage-i18n nil t))
               ;; Build data per section: each element -> (base-key label cmd id section)
               (per-sec
                (cl-loop for sec in sections
                         collect
                         (cl-loop for pl in acts
                                  for s = (plist-get pl :section)
                                  when (eq s sec)
                                  collect
                                  (let* ((cmd (plist-get pl :cmd))
                                         (id  (plist-get pl :id))
                                         (k   (car (plist-get pl :keys)))
                                         (key0 (and (stringp k) (car (last (split-string k " " t)))))
                                         (desc-key (plist-get pl :desc-key))
                                         (lbl (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                                                  (and (symbolp cmd) (symbol-name cmd))
                                                  (format "%s" id))))
                                    (list key0 lbl cmd id sec)))))
               ;; Flatten to resolve unique keys globally
               (flat (apply #'append per-sec))
               (used (make-hash-table :test 'equal))
               (unique
                (cl-loop for it in flat
                         for base = (nth 0 it)
                         for lbl  = (nth 1 it)
                         for cmd  = (nth 2 it)
                         for id   = (nth 3 it)
                         for sec  = (nth 4 it)
                         for idc  = (substring (symbol-name id) 0 1)
                         for cand = (delq nil (list base (and base (upcase base)) idc "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                         for final = (cl-loop for c in cand
                                              when (and (stringp c) (not (gethash c used)))
                                              return c)
                         do (puthash (or final base "x") t used)
                         collect (list (or final base "x") lbl cmd id sec)))
               ;; Build transient spec: a vector per section (column) with i18n title
               (build-col
                (lambda (sec)
                  (let* ((title (pcase sec
                                  ('navigate (if (fboundp 'carriage-i18n) (carriage-i18n :navigate-title) "Navigate"))
                                  ('act      (if (fboundp 'carriage-i18n) (carriage-i18n :act-title) "Actions"))
                                  ('session  (if (fboundp 'carriage-i18n) (carriage-i18n :session-title) "Session/Git"))
                                  ('tools    (if (fboundp 'carriage-i18n) (carriage-i18n :tools-title) "Tools"))
                                  ('logs     (if (fboundp 'carriage-i18n) (carriage-i18n :logs-title) "Logs"))
                                  (_ "Carriage")))
                         (items (cl-loop for it in unique
                                         for ukey = (nth 0 it)
                                         for lbl  = (nth 1 it)
                                         for cmd  = (nth 2 it)
                                         for s    = (nth 4 it)
                                         when (and (eq s sec) (commandp cmd))
                                         collect `(,ukey ,lbl ,cmd))))
                    (vconcat (list title) items))))
               (cols (cl-loop for sec in sections
                              for col = (funcall build-col sec)
                              when (> (length col) 1)
                              collect col))
               (menu-title (if (and (require 'carriage-i18n nil t)
                                    (fboundp 'carriage-i18n))
                               (carriage-i18n :carriage-menu)
                             "Carriage Menu")))
          ;; Redefine transient prefix dynamically
          (when (fboundp 'carriage-keys--menu)
            (fset 'carriage-keys--menu nil))
          ;; Build multi-column layout: top-level vector with title and nested column vectors.
          (let ((layout (apply #'vector (cons menu-title cols))))
            (eval
             `(transient-define-prefix carriage-keys--menu ()
                ,layout)))
          (call-interactively #'carriage-keys--menu))
      ;; Fallback: completing-read with section prefix in label
      (let* ((_ (require 'carriage-i18n nil t))
             (pairs
              (mapcar
               (lambda (pl)
                 (let* ((id  (plist-get pl :id))
                        (cmd (plist-get pl :cmd))
                        (sec (plist-get pl :section))
                        (desc-key (plist-get pl :desc-key))
                        (sec-name (pcase sec
                                    ('navigate (if (fboundp 'carriage-i18n) (carriage-i18n :navigate-title) "Navigate"))
                                    ('act      (if (fboundp 'carriage-i18n) (carriage-i18n :act-title) "Actions"))
                                    ('session  (if (fboundp 'carriage-i18n) (carriage-i18n :session-title) "Session/Git"))
                                    ('tools    (if (fboundp 'carriage-i18n) (carriage-i18n :tools-title) "Tools"))
                                    ('logs     (if (fboundp 'carriage-i18n) (carriage-i18n :logs-title) "Logs"))
                                    (_ "Other")))
                        (lbl (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                                 (and (symbolp cmd) (symbol-name cmd))
                                 (format "%s" id)))
                        (label (format "[%s] %s" sec-name lbl)))
                   (cons label cmd)))
               acts))
             (choice (completing-read "Carriage action: " (mapcar #'car pairs) nil t)))
        (let ((cmd (cdr (assoc choice pairs))))
          (when (commandp cmd)
            (call-interactively cmd)))))))

;;;###autoload
(defun carriage-keys-which-key-register ()
  "Register which-key replacements for Carriage prefix keys (i18n-aware)."
  (interactive)
  (when (require 'which-key nil t)
    (let* ((_ (require 'carriage-i18n nil t))
           (base (string-trim-right (or carriage-keys-prefix "C-c e ") "[ \t\n\r]+"))
           (menu   (if (fboundp 'carriage-i18n) (carriage-i18n :carriage-menu) "Carriage Menu"))
           (toggles (if (fboundp 'carriage-i18n) (carriage-i18n :carriage-toggles) "Carriage Toggles")))
      (which-key-add-key-based-replacements base menu)
      (which-key-add-key-based-replacements (concat base " t") toggles)
      (let ((task (if (fboundp 'carriage-i18n)
                      (or (carriage-i18n :task-new) "Create task doc")
                    "Create task doc")))
        (which-key-add-key-based-replacements (concat base " n") task))
      t)))

;;;###autoload
(defun carriage-keys-which-key-unregister ()
  "Remove which-key replacements for Carriage prefix keys, if present."
  (interactive)
  (when (require 'which-key nil t)
    (let* ((base (string-trim-right (or carriage-keys-prefix "C-c e ") "[ \t\n\r]+")))
      (when (fboundp 'which-key-remove-key-based-replacements)
        (ignore-errors (which-key-remove-key-based-replacements base))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t"))))
      t)))

;; Global prefix is managed by carriage-global-mode (see carriage-global-mode.el).
;; No global prefix is installed from keyspec; only buffer-local bindings are applied here.

(provide 'carriage-keyspec)
;;; carriage-keyspec.el ends here
