;;; carriage-suite.el --- Suite composition and prompt builder  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-format-registry)

(defconst carriage--suite-table
  '((sre-v1     . (:ops-allowed (sre sre-batch create delete rename)))
    (patch-v1   . (:ops-allowed (patch rename)))
    (file-ops-v1 . (:ops-allowed (create delete rename)))
    (auto-v1    . (:ops-allowed (sre sre-batch patch create delete rename))))
  "Mapping of suite-id to properties. :ops-allowed is a list of ops symbols.")

(defun carriage-suite-ops (suite-id)
  "Return allowed ops list for SUITE-ID."
  (let* ((cell (assoc suite-id carriage--suite-table)))
    (or (plist-get (cdr cell) :ops-allowed) '())))

(defun carriage-suite-ids ()
  "Return list of available Suite identifiers."
  (mapcar #'car carriage--suite-table))

(defun carriage--suite-guardrails ()
  "Return common guardrails string for Patch intent."
  (concat
   "Ты инструмент Carriage-mode. Отвечай строго и только блоками Org:\n"
   "- Разрешены ТОЛЬКО блоки #+begin_patch ... #+end_patch.\n"
   "- Ни одного символа вне блоков (никакого reasoning, пояснений вне блоков).\n"
   "- Пути только относительные. DELIM и пути выбирает инструмент; не придумывай их сам.\n"))

(defun carriage-build-prompt (intent suite-id ctx)
  "Build (:system :prompt) for INTENT ('Ask|'Patch) and SUITE-ID with CTX.
CTX may contain keys like :payload, :delim, :files, etc. Payload is user task text."
  (pcase intent
    ('Ask
     (list :system "Режим диалога (Ask). Не генерируй begin_patch."
           :prompt (or (plist-get ctx :payload) "")))
    ('Patch
     (let* ((ops (carriage-suite-ops suite-id))
            (frags
             (cl-loop for op in ops
                      for rec = (carriage-format-get op "1")
                      for fn = (and rec (plist-get rec :prompt-fragment))
                      when (functionp fn)
                      collect (condition-case e
                                  (funcall fn ctx)
                                (error (format ";; prompt-fragment error for %s: %s\n"
                                               op (error-message-string e)))))))
       (list :system (concat (carriage--suite-guardrails)
                             (mapconcat #'identity (delq nil frags) "\n"))
             :prompt (or (plist-get ctx :payload) ""))))
    (_
     (list :system "Unknown intent; defaulting to Ask." :prompt (or (plist-get ctx :payload) "")))))

(provide 'carriage-suite)
;;; carriage-suite.el ends here
