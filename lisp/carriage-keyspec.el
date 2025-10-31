;;; carriage-keyspec.el --- Centralized key binding model (v1.1) -*- lexical-binding: t; -*-

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
    (:id menu         :cmd carriage-keys-open-menu               :keys ("e")   :contexts (carriage) :section tools :desc-key :menu)
    ;; Actions
    (:id dry-run      :cmd carriage-dry-run-at-point        :keys ("d")  :contexts (carriage) :section act :desc-key :dry-run)
    (:id apply        :cmd carriage-apply-at-point          :keys ("a")  :contexts (carriage) :section act :desc-key :apply)
    (:id apply-all    :cmd carriage-apply-last-iteration    :keys ("A")  :contexts (carriage) :section act :desc-key :apply-all)
    (:id abort        :cmd carriage-abort-current           :keys ("x")  :contexts (carriage) :section act :desc-key :abort)
    (:id report       :cmd carriage-report-open             :keys ("r")  :contexts (carriage) :section tools :desc-key :report)
    ;; Git/WIP
    (:id wip          :cmd carriage-wip-checkout            :keys ("w")  :contexts (carriage) :section session :desc-key :wip)
    (:id reset        :cmd carriage-wip-reset-soft          :keys ("R")  :contexts (carriage) :section session :desc-key :reset)
    (:id commit-all   :cmd carriage-commit-changes          :keys ("c")  :contexts (carriage) :section session :desc-key :commit-all)
    (:id commit-last  :cmd carriage-commit-last-iteration   :keys ("i")  :contexts (carriage) :section session :desc-key :commit-last)
    ;; Engine
    (:id engine       :cmd carriage-select-apply-engine     :keys ("E")  :contexts (carriage) :section tools :desc-key :engine)
    )
  "Keyspec: list of action plists with :id :cmd :keys :contexts :section :desc-key.
All keys are relative to carriage-keys-prefix (default \"C-c e \").")

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
        (define-key map (carriage-keys--ensure-kbd k) cmd)))))

(defun carriage-keys-apply-to (map context)
  "Apply keyspec to MAP for CONTEXT."
  (dolist (act (carriage-keys--actions-for-context context))
    (carriage-keys--apply-action map act))
  map)

(defun carriage-keys-apply-known-keymaps ()
  "Apply keyspec to known Carriage keymaps."
  (when (and (boundp 'carriage-mode-map) (keymapp carriage-mode-map))
    (carriage-keys-apply-to carriage-mode-map 'carriage))
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
  (let* ((all-acts (carriage-keys--actions-for-context 'carriage))
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
               ;; Note: transient-define-prefix expects body forms (lists/vectors)
               ;; Splice a LIST of column vectors (cols), not a single vector.
               (menu-title (if (and (require 'carriage-i18n nil t)
                                    (fboundp 'carriage-i18n))
                               (carriage-i18n :carriage-menu)
                             "Carriage Menu")))
          ;; Redefine transient prefix dynamically
          (when (fboundp 'carriage-keys--menu)
            (fset 'carriage-keys--menu nil))
          (eval
           `(transient-define-prefix carriage-keys--menu ()
              ,menu-title
              ,@cols))
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
      t)))

(defcustom carriage-keys-global-prefix-install t
  "When non-nil, install a global prefix keymap on \"C-c e\" with a minimal menu entry.
This makes \"C-c e\" a visible prefix even outside carriage-mode buffers and binds
\"C-c e e\" to =carriage-keys-open-menu' globally. All other normative bindings
remain buffer-local and are applied via keyspec in =carriage-mode' buffers."
  :type 'boolean
  :group 'carriage-keyspec)

(defvar carriage-keys--global-prefix-map nil
  "Global prefix keymap installed under \"C-c e\" when =carriage-keys-global-prefix-install' is non-nil.")

(defun carriage-keys--ensure-global-prefix ()
  "Install global binding: \"C-c e\" opens Carriage menu (transient or fallback)."
  (when (and carriage-keys-global-prefix-install (keymapp global-map))
    (define-key global-map (kbd "C-c e") #'carriage-keys-open-menu)
    ;; which-key hints (optional)
    (ignore-errors (carriage-keys-which-key-register))
    t))

;; Install global prefix on load if configured.
(when carriage-keys-global-prefix-install
  (ignore-errors (carriage-keys--ensure-global-prefix)))

(provide 'carriage-keyspec)
;;; carriage-keyspec.el ends here
