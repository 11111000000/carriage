;;; carriage-templates.el --- Template registry and rendering engine  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (subr-x "0"))
;; Version: 0.1
;; Keywords: tools, templates, org
;;
;; Specifications:
;;   spec/spec-on-specs.org
;;   spec/document-branching-and-templates-v1.org
;;   spec/ui-v2.org
;;   spec/context-integration-v2.org
;;   spec/security-v2.org
;;
;;; Commentary:
;; Minimal v1 of document branching templates:
;; - Template registry (carriage-templates) with built-in templates.
;; - Deterministic placeholder engine {{name|filter|...}} with a strict whitelist.
;; - Pure rendering enforcement: :render functions run under an I/O guard.
;; - Public API:
;;     carriage-templates-render (TEMPLATE-ID CTX) -> string
;;   CTX is a plist with keys like :title, :today, :project, :origin-file, :origin-heading,
;;   :subtree, :ctx-profile ('p1|'p3), :parent-context (list of strings),
;;   :inherited (plist :begin-context t|nil :car-flags t|nil).
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-utils nil t)  ;; carriage-project-root (optional)

(defgroup carriage-templates nil
  "Template registry and rendering for Carriage."
  :group 'applications
  :prefix "carriage-templates-")

(defcustom carriage-templates nil
  "Registry of templates (list of plist entries).

Each entry is a plist:
  :id        symbol (required)
  :version   string (required, e.g., \"1.0\")
  :label     string (required)
  :desc      string (optional)
  :category  symbol (optional) e.g., analysis|decomposition|testing|debug|design|bug|misc
  :when      function(ctx→bool) (optional) visibility predicate
  :render    function(ctx→string) or string (required). If string, placeholders are applied.
  :after     function(env→nil) (optional) cosmetic hook after insertion

Rendering must be deterministic. I/O/network/TRAMP are forbidden during :render."
  :type '(repeat plist)
  :group 'carriage-templates)

(defcustom carriage-templates-project-file "carriage/templates.el"
  "Project-relative path to an optional templates file.
When present, it SHOULD set a variable `carriage-templates-project'
to a list of template plists. Project templates override user/built-in by :id."
  :type 'string
  :group 'carriage-templates)

(defcustom carriage-templates-user-file (expand-file-name "carriage/templates.el" "~/.config")
  "User-level templates file.
When present, it SHOULD set a variable `carriage-templates-user'
to a list of template plists. User templates override built-in by :id."
  :type 'file
  :group 'carriage-templates)

(defcustom carriage-templates-filters-whitelist '(quote slug trim upper lower)
  "Allowed placeholder filters."
  :type '(repeat symbol)
  :group 'carriage-templates)

;; Internal helpers

(defun carriage-templates--today ()
  "Return today's date in ISO format YYYY-MM-DD."
  (format-time-string "%Y-%m-%d"))

(defun carriage-templates--slug (s)
  "Return a slugified variant of string S."
  (let* ((down (downcase (or s "")))
         (repl (replace-regexp-in-string "[^a-z0-9]+" "-" down)))
    (replace-regexp-in-string "-+" "-" (string-trim repl "-+" "-+"))))

(defun carriage-templates--apply-filter (name val)
  "Apply placeholder filter NAME to VAL."
  (pcase name
    ('quote (format "#+begin_quote\n%s\n#+end_quote" (or val "")))
    ('slug  (carriage-templates--slug val))
    ('trim  (string-trim (or val "")))
    ('upper (upcase (or val "")))
    ('lower (downcase (or val "")))
    (_ (error "TEMPLATE_E_UNSAFE_PLACEHOLDER: unknown filter: %s" name))))

(defun carriage-templates--ctx-get (ctx key)
  "Safely get KEY from CTX plist."
  (plist-get ctx key))

(defun carriage-templates--normalize-ctx (ctx)
  "Ensure CTX has stable keys and derived values."
  (let* ((ctx (copy-sequence (or ctx '())))
         (today (or (plist-get ctx :today) (carriage-templates--today)))
         (pc   (plist-get ctx :parent-context))
         (pcc  (if (listp pc) (length pc) 0)))
    (plist-put ctx :today today)
    (plist-put ctx :parent-context (or pc '()))
    (plist-put ctx :parent-context-count pcc)
    ctx))

(defun carriage-templates--placeholder-value (name ctx)
  "Resolve placeholder variable NAME using CTX."
  (pcase name
    ("title"                 (or (carriage-templates--ctx-get ctx :title) ""))
    ("today"                 (or (carriage-templates--ctx-get ctx :today) (carriage-templates--today)))
    ("project"               (or (carriage-templates--ctx-get ctx :project) ""))
    ("origin_file"           (or (carriage-templates--ctx-get ctx :origin-file) ""))
    ("origin_heading"        (or (carriage-templates--ctx-get ctx :origin-heading) ""))
    ("subtree"               (or (carriage-templates--ctx-get ctx :subtree) ""))
    ("ctx_profile"           (let ((p (carriage-templates--ctx-get ctx :ctx-profile))) (if (symbolp p) (symbol-name p) (format "%s" p))))
    ("parent_context_count"  (number-to-string (or (carriage-templates--ctx-get ctx :parent-context-count) 0)))
    (_ (error "TEMPLATE_E_UNSAFE_PLACEHOLDER: unknown variable: %s" name))))

(defun carriage-templates--apply-placeholders (text ctx)
  "Apply placeholder replacements to TEXT using CTX."
  (let ((rx "{{\\([^}]+\\)}}" )
        (pos 0)
        (out (list)))
    (while (and (< pos (length text))
                (string-match rx text pos))
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (token (match-string 1 text)))
        (push (substring text pos beg) out)
        (let* ((parts (split-string token "|" t "[ \t\n\r]+"))
               (name  (car parts))
               (filters (cdr parts))
               (val   (carriage-templates--placeholder-value name ctx)))
          ;; Enforce whitelist for filters
          (dolist (f filters)
            (let ((fsym (intern f)))
              (unless (memq fsym carriage-templates-filters-whitelist)
                (error "TEMPLATE_E_UNSAFE_PLACEHOLDER: filter not allowed: %s" f))
              (setq val (carriage-templates--apply-filter fsym val))))
          (push (or val "") out))
        (setq pos end)))
    (push (substring text pos) out)
    (apply #'concat (nreverse out))))

(defun carriage-templates--install-io-guards ()
  "Install temporary guards to refuse I/O/network during :render.
Return a list of (SYM . FN) advice pairs suitable for
`carriage-templates--remove-io-guards'."
  (let* ((f-write  (lambda (&rest _) (error "TEMPLATE_E_IO_ATTEMPT: write-region in render forbidden")))
         (f-insert (lambda (&rest _) (error "TEMPLATE_E_IO_ATTEMPT: insert-file-contents in render forbidden")))
         (f-call   (lambda (&rest _) (error "TEMPLATE_E_IO_ATTEMPT: call-process in render forbidden")))
         (f-start  (lambda (&rest _) (error "TEMPLATE_E_IO_ATTEMPT: start-process in render forbidden")))
         (f-net    (lambda (&rest _) (error "TEMPLATE_E_IO_ATTEMPT: network in render forbidden"))))
    (advice-add 'write-region :around f-write)
    (advice-add 'insert-file-contents :around f-insert)
    (advice-add 'call-process :around f-call)
    (advice-add 'start-process :around f-start)
    (advice-add 'make-network-process :around f-net)
    (list (cons 'write-region f-write)
          (cons 'insert-file-contents f-insert)
          (cons 'call-process f-call)
          (cons 'start-process f-start)
          (cons 'make-network-process f-net))))

(defun carriage-templates--remove-io-guards (tokens)
  "Remove IO guards installed by carriage-templates--install-io-guards with TOKENS."
  (dolist (cell tokens)
    (let ((sym (car cell))
          (fn  (cdr cell)))
      (ignore-errors (advice-remove sym fn)))))

(defun carriage-templates--with-io-guard (thunk)
  "Run THUNK under I/O/network guard; remove guards afterwards."
  (let ((guards (carriage-templates--install-io-guards)))
    (unwind-protect
        (funcall thunk)
      (carriage-templates--remove-io-guards guards))))

(defun carriage-templates--find (template-id)
  "Find template entry by TEMPLATE-ID (symbol) in `carriage-templates'."
  (cl-find template-id carriage-templates
           :key (lambda (pl) (plist-get pl :id))
           :test #'eq))

(defun carriage-templates--render-entry (entry ctx)
  "Render template ENTRY with CTX. ENTRY is a registry plist."
  (let* ((ctx* (carriage-templates--normalize-ctx ctx))
         (renderer (plist-get entry :render)))
    (cond
     ((functionp renderer)
      (carriage-templates--with-io-guard (lambda () (funcall renderer ctx*))))
     ((stringp renderer)
      (carriage-templates--apply-placeholders renderer ctx*))
     (t
      (error "TEMPLATE_E_RENDER: unsupported :render type for id=%s"
             (plist-get entry :id))))))

;;; Public API

(defun carriage-templates-render (template-id ctx)
  "Render TEMPLATE-ID using CTX, returning Org string.

- Deterministic: no I/O/network in render path.
- Placeholders: {{title}}, {{today}}, {{project}}, {{origin_file}},
  {{origin_heading}}, {{subtree}}, {{ctx_profile}}, {{parent_context_count}}
  with filters: quote|slug|trim|upper|lower."
  (let ((entry (carriage-templates--find template-id)))
    (unless entry
      (error "TEMPLATE_E_UNKNOWN_ID: %s" template-id))
    (carriage-templates--render-entry entry ctx)))

;;; Built-in templates

(defun carriage-templates--builtin ()
  "Return list of built-in template entries."
  (list
   ;; task/default — Analysis and Plan
   (list :id 'task/default
         :version "1.0"
         :label "Analysis and Plan"
         :desc "Goal, minimal context, 3–5 steps, tests/checks, DoD, artifacts, next step."
         :category 'analysis
         :render
         (concat
          "#+title: {{title|trim}}\n"
          "#+author: \n"
          "#+language: en\n"
          "#+options: toc:2 num:t\n\n"
          "* Goal\n"
          "- What: {{title}}\n"
          "- Why: \n"
          "- Context profile: {{ctx_profile|upper}}\n\n"
          "* Context\n"
          "# Note: begin_context is inserted by the branching command (profile-dependent).\n\n"
          "* Plan (3–5 steps)\n"
          "1. \n2. \n3. \n\n"
          "* Tests / Checks\n"
          "- \n\n"
          "* Definition of Done (DoD)\n"
          "- \n\n"
          "* Artifacts\n"
          "- \n\n"
          "* Next Step\n"
          "- \n\n"
          "* Links\n"
          "- Origin: {{origin_file}} :: {{origin_heading}}\n"
          "- Created: {{today}}\n"
          ))
   ;; task/decomposition
   (list :id 'task/decomposition
         :version "1.0"
         :label "Decomposition"
         :desc "Break a big goal into components, dependencies, risks, next step."
         :category 'decomposition
         :render
         (concat
          "#+title: {{title|trim}} — Decomposition\n\n"
          "* Goal\n- {{title}}\n\n"
          "* Decomposition Criteria\n- \n\n"
          "* Components\n- \n- \n- \n\n"
          "* Dependencies\n- \n\n"
          "* Risks\n- \n\n"
          "* Next Step\n- \n\n"
          ))
   ;; task/implementation-step
   (list :id 'task/implementation-step
         :version "1.0"
         :label "Implementation Step"
         :desc "One micro step: hypothesis, mini-plan, patch, check, result, next step."
         :category 'analysis
         :render
         (concat
          "#+title: {{title|trim}} — Step\n\n"
          "* Hypothesis / Intent\n- \n\n"
          "* Mini-Plan (≤3)\n1. \n2. \n3. \n\n"
          "* Patch area\n- Describe where change will occur\n\n"
          "* Check / Verify\n- \n\n"
          "* Result\n- \n\n"
          "* Next Step\n- \n\n"
          ))
   ;; test/plan
   (list :id 'test/plan
         :version "1.0"
         :label "Test Plan"
         :desc "Scope, risks, test sets, coverage, acceptance."
         :category 'testing
         :render
         (concat
          "#+title: {{title|trim}} — Test Plan\n\n"
          "* Scope\n- \n\n"
          "* Risks / Assumptions\n- \n\n"
          "* Test Sets\n- \n\n"
          "* Coverage / Metrics\n- \n\n"
          "* Acceptance (DoD)\n- \n\n"
          ))
   ;; debug/protocol
   (list :id 'debug/protocol
         :version "1.0"
         :label "Debug Protocol"
         :desc "Symptom, hypotheses, experiments/logs, observations, conclusion, next step."
         :category 'debug
         :render
         (concat
          "#+title: {{title|trim}} — Debug Protocol\n\n"
          "* Symptom\n- \n\n"
          "* Hypotheses\n- \n\n"
          "* Experiments / Logs\n- \n\n"
          "* Observations\n- \n\n"
          "* Conclusion\n- \n\n"
          "* Next Step\n- \n\n"
          ))
   ;; design/adr
   (list :id 'design/adr
         :version "1.0"
         :label "ADR (Architecture Decision)"
         :desc "Context, decision, alternatives, consequences, links."
         :category 'design
         :render
         (concat
          "#+title: {{title|trim}} — ADR\n\n"
          "* Context\n- \n\n"
          "* Decision\n- \n\n"
          "* Alternatives\n- \n\n"
          "* Consequences\n- \n\n"
          "* Links\n- \n\n"
          ))
   ;; bug/incident
   (list :id 'bug/incident
         :version "1.0"
         :label "Incident / Bug"
         :desc "Symptom, evidence, reproduction, cause, fix, tests, post-mortem."
         :category 'bug
         :render
         (concat
          "#+title: {{title|trim}} — Incident/Bug\n\n"
          "* Symptom\n- \n\n"
          "* Evidence / Logs\n- \n\n"
          "* Reproduction\n- \n\n"
          "* Root Cause\n- \n\n"
          "* Fix\n- \n\n"
          "* Tests\n- \n\n"
          "* Post-mortem\n- \n\n"
          ))))

(defun carriage-templates--ensure-builtins ()
  "Ensure built-in templates are present when registry is empty."
  (when (null carriage-templates)
    (setq carriage-templates (carriage-templates--builtin))))

;; Initialize built-ins once
(carriage-templates--ensure-builtins)

;; -----------------------------------------------------------------------------
;; Project/User on-disk overrides and registry refresh (lookup: project > user > built-in)

(defvar carriage-templates-project nil
  "Project-level template registry list (loaded from `carriage-templates-project-file' when present).
Each element is a plist as per the template schema.")

(defvar carriage-templates-user nil
  "User-level template registry list (loaded from `carriage-templates-user-file' when present).
Each element is a plist as per the template schema.")

(defun carriage-templates--plist-template-id (tpl)
  "Return :id from TPL plist or nil."
  (and (listp tpl) (plist-get tpl :id)))

(defun carriage-templates--merge-registry (builtin user project)
  "Merge template registries with precedence PROJECT > USER > BUILTIN by :id."
  (let ((acc (make-hash-table :test 'eq))
        (out '()))
    (dolist (lst (list builtin user project))
      (dolist (tpl (or lst '()))
        (let ((id (carriage-templates--plist-template-id tpl)))
          (when id
            ;; Overwrite lower-precedence entry with higher one
            (puthash id tpl acc)))))
    ;; Preserve a stable order: prefer project ids first (their internal order), then user, then builtin
    (dolist (lst (list project user builtin))
      (dolist (tpl (or lst '()))
        (let ((id (carriage-templates--plist-template-id tpl)))
          (when (and id (eq (gethash id acc) tpl))
            (push tpl out)
            ;; prevent double-push
            (puthash id :emitted acc)))))
    (nreverse out)))

(defun carriage-templates--maybe-load-file (file var-sym)
  "If FILE exists, load it and return value of VAR-SYM (if bound after load), else nil."
  (when (and (stringp file)
             (file-exists-p file))
    (condition-case _e
        (progn
          (load-file file)
          (when (boundp var-sym)
            (symbol-value var-sym)))
      (error nil))))

;;;###autoload
(defun carriage-templates-refresh ()
  "Reload template registry from built-in + user + project with precedence: project > user > built-in.

Sources:
- Built-in: `carriage-templates' as currently loaded (or ensured by built-ins).
- User: either `carriage-templates-user' var if bound, or from `carriage-templates-user-file' if present.
- Project: either `carriage-templates-project' var if bound, or from `carriage-templates-project-file' if present.

Merges registries by :id with project overriding user overriding built-in."
  (interactive)
  (let* ((builtin (copy-sequence (or carriage-templates '())))
         (user-val (or (and (boundp 'carriage-templates-user) carriage-templates-user)
                       (carriage-templates--maybe-load-file carriage-templates-user-file 'carriage-templates-user)))
         (proj-val (or (and (boundp 'carriage-templates-project) carriage-templates-project)
                       (let* ((root (ignore-errors (and (fboundp 'carriage-project-root) (carriage-project-root))))
                              (f (and root (expand-file-name carriage-templates-project-file root))))
                         (and f (carriage-templates--maybe-load-file f 'carriage-templates-project))))))
    (setq carriage-templates
          (carriage-templates--merge-registry builtin user-val proj-val))
    (carriage-templates))

(defun carriage-templates--lint-entry (tpl)
  "Return nil if TPL looks sane; otherwise a string describing the issue."
  (cond
   ((not (listp tpl)) "not-a-plist")
   ((not (symbolp (plist-get tpl :id))) "missing-or-bad :id")
   ((not (stringp (plist-get tpl :version))) "missing-or-bad :version")
   (let ((r (plist-get tpl :render)))
     (cond
      ((or (stringp r) (functionp r)) nil)
      (t ":render must be string or function")))
   (t nil)))

;;;###autoload
(defun carriage-templates-lint (&optional registry)
  "Return a list of problems for REGISTRY (defaults to `carriage-templates').
Each element is (ID . PROBLEM-STRING). Empty list means OK."
  (let ((reg (or registry carriage-templates))
        (issues '()))
    (dolist (tpl (or reg '()))
      (let* ((id (or (carriage-templates--plist-template-id tpl) :unknown))
             (bad (carriage-templates--lint-entry tpl)))
        (when bad (push (cons id bad) issues))))
    (nreverse issues)))

;; Ensure built-ins then apply overrides (user/project) once at load
(ignore-errors (carriage-templates-refresh))

(provide 'carriage-templates)
;;; carriage-templates.el ends here
