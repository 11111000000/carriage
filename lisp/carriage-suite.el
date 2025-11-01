;;; carriage-suite.el --- Suite composition and prompt builder  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-format-registry)
(require 'carriage-intent-registry nil t)

;; Fallback inline intent registry if the external file is not yet available in load-path (e.g., Nix packaging).
(unless (featurep 'carriage-intent-registry)
  (defgroup carriage-intents nil
    "Inline intent fragments registry (fallback)."
    :group 'carriage)

  (defvar carriage-intent-fragment-overrides nil
    "Alist ((INTENT . FRAG) ...) for intent prompt fragment overrides.
FRAG is either a STRING or a function of (CTX) returning STRING).")

  (defvar carriage--intent-registry nil
    "Alist ((INTENT . FRAG) ...) of default intent fragments.")

  (defun carriage-intent-register (intent frag)
    "Register INTENT fragment FRAG (STRING or (lambda (ctx) STRING))."
    (let ((cell (assq intent carriage--intent-registry)))
      (if cell
          (setcdr cell frag)
        (push (cons intent frag) carriage--intent-registry)))
    t)

  (defun carriage-intent-get (intent)
    "Return fragment for INTENT considering overrides."
    (or (and (boundp 'carriage-intent-fragment-overrides)
             (alist-get intent carriage-intent-fragment-overrides))
        (cdr (assq intent carriage--intent-registry))))

  (defun carriage-intent-known ()
    "Return list of known intent symbols."
    (mapcar #'car carriage--intent-registry))

  ;; Defaults (English), can be overridden via carriage-intent-fragment-overrides.
  (carriage-intent-register 'Ask
                            "Ask mode: dialogue only. Do NOT generate any #+begin_patch blocks.")
  (carriage-intent-register 'Code
                            "Code mode: answer ONLY with Org #+begin_patch ... #+end_patch blocks. No text outside blocks.")
  (carriage-intent-register 'Hybrid
                            "Hybrid mode: you MAY include prose, but the tool will apply ONLY the content of #+begin_patch ... #+end_patch blocks.")

  ;; Provide the feature so (require 'carriage-intent-registry) in tests succeeds even without the separate file.
  (provide 'carriage-intent-registry))

(defconst carriage--suite-table
  '((sre   . (:ops-allowed (sre create delete rename)))
    (udiff . (:ops-allowed (patch create delete rename))))
  "Mapping of suite-id to properties. :ops-allowed is a list of ops symbols.")

(defgroup carriage-suite nil
  "Suite composition and prompt builder settings."
  :group 'carriage)

(defcustom carriage-prompt-suite-overlays nil
  "Alist ((SUITE . (:ops-allowed (op ...))) ...) to override suite op whitelists."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type sexp))
  :group 'carriage-suite)

(defcustom carriage-op-fragment-overrides nil
  "Alist ((OP . FRAG) ...) to override op prompt fragments.
FRAG is STRING or function (lambda (ctx) STRING)."
  :type '(alist :key-type symbol :value-type (choice string function))
  :group 'carriage-suite)

(defcustom carriage-suite-missing-op-fragment-policy 'error
  "Policy when an allowed op lacks a prompt fragment.
- 'error: signal an error
- 'skip: skip that fragment"
  :type '(choice (const error) (const skip))
  :group 'carriage-suite)

(defun carriage-suite-ops (suite-id)
  "Return allowed ops list for SUITE-ID, applying overlays when present."
  (let* ((base (let ((cell (assoc suite-id carriage--suite-table)))
                 (or (plist-get (cdr cell) :ops-allowed) '())))
         (ov   (and (boundp 'carriage-prompt-suite-overlays)
                    (alist-get suite-id carriage-prompt-suite-overlays)))
         (over-ops (and (listp ov) (plist-get ov :ops-allowed))))
    (or over-ops base)))

(defun carriage-suite-ids ()
  "Return list of available Suite identifiers."
  (mapcar #'car carriage--suite-table))

(defun carriage-suite-known-p (suite-id)
  "Return non-nil if SUITE-ID is known either in base table or overlays."
  (or (assoc suite-id carriage--suite-table)
      (and (boundp 'carriage-prompt-suite-overlays)
           (alist-get suite-id carriage-prompt-suite-overlays))))

(defun carriage--join-nonempty (parts &optional sep)
  "Join PARTS by SEP ignoring nil/empty strings."
  (let* ((vals (cl-remove-if (lambda (s) (or (null s) (and (stringp s) (string-empty-p s))))
                             parts)))
    (mapconcat #'identity vals (or sep "\n"))))

(defun carriage--suite-guardrails (ops)
  "Return common guardrails string for Code/Hybrid intents, tailored to allowed OPS."
  (let* ((allowed (mapconcat (lambda (o) (format "%s" o)) ops ", ")))
    (concat
     "Carriage tool: respond with Org #+begin_patch ... #+end_patch blocks.\n"
     "- Code mode: NO text outside blocks. Hybrid mode: prose is allowed, tool applies ONLY blocks.\n"
     (format "- Allowed operations: %s.\n" allowed)
     "- Use EXACT operation names. Aliases like write/create_file/delete_file/rename_file are forbidden.\n"
     "- Header must include :version \"1\" and only keys defined by the selected operation.\n"
     "- Paths must be relative to repo root. Use :file (not :path) where applicable.\n"
     "- For create: require :delim (exactly 6 lower hex). Body marker lines: <<DELIM and :DELIM.\n"
     "- For patch: unified diff of EXACTLY ONE file (one ---/+++ pair), a/ and b/ paths MUST match, :strip=1. No binary or rename/copy preludes.\n"
     "- No base64 payloads; the tool will handle fallbacks itself.\n")))

(defun carriage--resolve-op-fragment (op ctx)
  "Resolve prompt fragment for OP using overrides or registry.
Return STRING or nil."
  (let* ((ov (and (boundp 'carriage-op-fragment-overrides)
                  (alist-get op carriage-op-fragment-overrides))))
    (cond
     ((stringp ov) ov)
     ((functionp ov) (ignore-errors (funcall ov ctx)))
     (t
      (let* ((rec (carriage-format-get op "1"))
             (pf (and rec (plist-get rec :prompt-fragment))))
        (cond
         ((stringp pf) pf)
         ((functionp pf) (ignore-errors (funcall pf ctx)))
         (t nil)))))))

(defun carriage--resolve-intent-fragment (intent ctx)
  "Resolve intent fragment for INTENT using overrides or registry.
Return STRING (empty string when not found)."
  (let* ((ov (and (boundp 'carriage-intent-fragment-overrides)
                  (alist-get intent carriage-intent-fragment-overrides))))
    (cond
     ((stringp ov) ov)
     ((functionp ov) (ignore-errors (funcall ov ctx)))
     (t
      (let ((frag (ignore-errors (carriage-intent-get intent))))
        (cond
         ((stringp frag) frag)
         ((functionp frag) (ignore-errors (funcall frag ctx)))
         (t "")))))))

(defun carriage--assert-suite-safety (suite-id system-str)
  "Signal MODE_E_DISPATCH when SYSTEM-STR contains markers forbidden for SUITE-ID."
  (when (and (stringp system-str) (not (string-empty-p system-str)))
    (pcase suite-id
      ('sre
       (when (or (string-match-p "^---\\s-+a/" system-str)
                 (string-match-p "^\\+\\+\\+\\s-+b/" system-str)
                 (string-match-p "^diff --git" system-str)
                 (string-match-p "unified diff" system-str))
         (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                 (list "Suite 'sre' must not include unified diff markers"))))
      ('udiff
       (when (or (string-match-p "#\\+begin_from" system-str)
                 (string-match-p "#\\+begin_to" system-str))
         (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                 (list "Suite 'udiff' must not include SRE begin_from/begin_to markers"))))
      (_ nil)))
  t)

(defun carriage-build-prompt (intent suite-id ctx)
  "Build (:system :prompt) for INTENT ('Ask|'Code|'Hybrid) and SUITE-ID with CTX.
CTX may contain keys like :payload, :context-text, :context-target, :delim, :files, etc.
:payload is user task text. This function is pure (no side effects)."
  (let* ((payload   (or (plist-get ctx :payload) ""))
         (ctx-text  (plist-get ctx :context-text))
         (ctx-target (or (plist-get ctx :context-target) 'system)))
    (pcase intent
      ('Ask
       (let* ((intent-note (carriage--resolve-intent-fragment 'Ask ctx))
              (system (if (and (eq ctx-target 'system)
                               (stringp ctx-text) (not (string-empty-p ctx-text)))
                          (carriage--join-nonempty (list intent-note ctx-text) "\n")
                        intent-note))
              (prompt (if (and (eq ctx-target 'user)
                               (stringp ctx-text) (not (string-empty-p ctx-text)))
                          (concat ctx-text "\n" payload)
                        payload)))
         (list :system (or system "") :prompt prompt)))
      ((or 'Code 'Hybrid)
       (let* ((ops (progn
                     (unless (carriage-suite-known-p suite-id)
                       (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                               (list (format "Unknown suite: %S" suite-id))))
                     (carriage-suite-ops suite-id)))
              ;; Best effort: ensure op modules are loaded so their fragments are registered.
              (_ (dolist (op ops)
                   (unless (carriage-format-get op "1")
                     (pcase op
                       ('sre                 (load "ops/carriage-op-sre" t t))
                       ('patch               (load "ops/carriage-op-patch" t t))
                       ((or 'create 'delete 'rename) (load "ops/carriage-op-file" t t))
                       (_ nil)))))
              (fragments
               (cl-loop for op in ops
                        for s = (carriage--resolve-op-fragment op ctx)
                        if (and (stringp s) (not (string-empty-p s))) collect s
                        else if (eq carriage-suite-missing-op-fragment-policy 'error)
                        do (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                                   (list (format "Missing prompt fragment for op: %s" op)))))
              (intent-note (carriage--resolve-intent-fragment intent ctx))
              (sys-core (carriage--join-nonempty
                         (list (carriage--suite-guardrails ops)
                               (mapconcat #'identity fragments "\n"))
                         "\n"))
              (base (carriage--join-nonempty (list sys-core intent-note) "\n"))
              (system (if (and (eq ctx-target 'system)
                               (stringp ctx-text) (not (string-empty-p ctx-text)))
                          (concat base "\n" ctx-text)
                        base))
              (prompt (if (and (eq ctx-target 'user)
                               (stringp ctx-text) (not (string-empty-p ctx-text)))
                          (concat ctx-text "\n" payload)
                        payload)))
         (carriage--assert-suite-safety suite-id system)
         (list :system system :prompt prompt)))
      (_
       (signal (carriage-error-symbol 'MODE_E_DISPATCH)
               (list (format "Unknown intent: %S" intent)))))))

;; Backward-compatible alias used in some tests/specs
(defun carriage--build-prompt (intent suite-id ctx)
  "Compatibility wrapper for =carriage-build-prompt'."
  (carriage-build-prompt intent suite-id ctx))

(provide 'carriage-suite)
;;; carriage-suite.el ends here
