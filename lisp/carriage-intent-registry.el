;;; carriage-intent-registry.el --- Intent fragments registry  -*- lexical-binding: t; -*-
;; Specifications:
;;   spec/code-style-v1.org
;;   spec/index.org
;;   spec/errors-v1.org
;;   spec/compliance-checklist-v1.org
;;   spec/prompt-profiles-v1.org
;;   spec/parser-registry-v1.org
;;   spec/project-overview-v1.org

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-prompts nil
  "Prompt composition: intent fragments and suite builder."
  :group 'applications)

(defcustom carriage-intent-fragment-overrides nil
  "Alist of overrides for intent prompt fragments.
Each entry is (INTENT . FRAG), where FRAG is either STRING or FUNCTION (CTX → STRING).
Override takes precedence over the registry defaults."
  :type '(alist :key-type symbol :value-type (choice string function))
  :group 'carriage-prompts)

(defvar carriage--intent-fragments (make-hash-table :test 'eq)
  "Registry of intent fragments: INTENT → FRAG (STRING or FUNCTION (CTX → STRING)).")

(defun carriage-intent-register (intent fragment)
  "Register FRAGMENT for INTENT in the intent registry.
FRAGMENT must be a STRING or a FUNCTION of one argument CTX returning STRING."
  (unless (memq (type-of fragment) '(string cons))
    ;; Allow lambdas/closures as functions; `functionp' covers both.
    (unless (functionp fragment)
      (error "Invalid fragment for intent %S: must be string or function" intent)))
  (puthash intent fragment carriage--intent-fragments)
  t)

(defun carriage-intent-get (intent)
  "Return fragment (STRING or FUNCTION) for INTENT with overrides applied.
The caller is responsible for FUNCALL if the result is a function."
  (let* ((ov (assoc-default intent carriage-intent-fragment-overrides)))
    (cond
     (ov ov)
     ((gethash intent carriage--intent-fragments))
     (t (error "Unknown intent: %S" intent)))))

(defun carriage-intent-known ()
  "Return list of known intent symbols in the registry."
  (let (res)
    (maphash (lambda (k _v) (push k res)) carriage--intent-fragments)
    (nreverse res)))

;; -------------------------------------------------------------------
;; Default fragments (English-only by spec; overrides can replace)

(defun carriage--intent-frag-code (_ctx)
  "Default fragment for Intent=Code."
  "Answer ONLY with Org begin_patch blocks.
Do NOT include any prose outside blocks. No reasoning, no commentary.
- Use exactly one block per operation.
- Paths must be RELATIVE to project root; no absolute paths, no \"..\" segments.
- Allowed operations depend on Suite. Do not mention formats that are not allowed by the Suite.")

(defun carriage--intent-frag-hybrid (_ctx)
  "Default fragment for Intent=Hybrid."
  "You MAY include brief prose, but the tool will extract and apply ONLY Org begin_patch blocks.
Keep prose minimal and place it before or after the blocks. Do not insert text inside blocks.")

(defun carriage--intent-frag-ask (_ctx)
  "Default fragment for Intent=Ask."
  "Do NOT produce any begin_patch blocks. Provide a concise prose answer only.")

;; Register defaults
(carriage-intent-register 'Code   #'carriage--intent-frag-code)
(carriage-intent-register 'Hybrid #'carriage--intent-frag-hybrid)
(carriage-intent-register 'Ask    #'carriage--intent-frag-ask)

(provide 'carriage-intent-registry)
;;; carriage-intent-registry.el ends here
