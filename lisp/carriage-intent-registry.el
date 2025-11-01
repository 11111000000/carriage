;;; carriage-intent-registry.el --- Intent fragments registry and overrides  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-intents nil
  "Carriage intent fragments registry and configuration."
  :group 'carriage)

(defcustom carriage-intent-fragment-overrides nil
  "Alist ((INTENT . FRAG) ...) for intent prompt fragment overrides.
FRAG is either a STRING or a function of (CTX) returning STRING."
  :type '(alist :key-type symbol :value-type (choice string function))
  :group 'carriage-intents)

(defvar carriage--intent-registry nil
  "Alist ((INTENT . FRAG) ...) of default intent fragments.
FRAG is either a STRING or a function (lambda (ctx) STRING).")

(defun carriage-intent-register (intent frag)
  "Register INTENT fragment FRAG (STRING or (lambda (ctx) STRING))."
  (let* ((cell (assq intent carriage--intent-registry)))
    (if cell
        (setcdr cell frag)
      (push (cons intent frag) carriage--intent-registry)))
  t)

(defun carriage-intent-get (intent)
  "Return fragment for INTENT considering overrides.
Result is STRING or FUNCTION (lambda (ctx) STRING), or nil if unknown."
  (let* ((ov (and (boundp 'carriage-intent-fragment-overrides)
                  (alist-get intent carriage-intent-fragment-overrides))))
    (cond
     ;; Override wins
     (ov ov)
     ;; Registry default
     (t (cdr (assq intent carriage--intent-registry))))))

(defun carriage-intent-known ()
  "Return list of known intent symbols."
  (mapcar #'car carriage--intent-registry))

;; Defaults (English, single-language; can be overridden via defcustom above)
;; Keep concise and format-neutral; Suite adds guardrails and op fragments.
(carriage-intent-register 'Ask
                          "Ask mode: dialogue only. Do NOT generate any #+begin_patch blocks.")
(carriage-intent-register 'Code
                          "Code mode: answer ONLY with Org #+begin_patch ... #+end_patch blocks. No text outside blocks.")
(carriage-intent-register 'Hybrid
                          "Hybrid mode: you MAY include prose, but the tool will apply ONLY the content of #+begin_patch ... #+end_patch blocks.")

(provide 'carriage-intent-registry)
;;; carriage-intent-registry.el ends here
