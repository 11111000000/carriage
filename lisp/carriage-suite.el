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

(defun carriage--suite-guardrails (ops)
  "Return common guardrails string for Patch intent, tailored to allowed OPS."
  (let* ((allowed (mapconcat (lambda (o) (format "%s" o)) ops ", ")))
    (concat
     "Ты инструмент Carriage-mode. Отвечай строго и только блоками Org:\n"
     "- Разрешены ТОЛЬКО блоки #+begin_patch ... #+end_patch.\n"
     "- Ни одного символа вне блоков (никакого reasoning или текста вне блоков).\n"
     (format "- Разрешённые операции: %s.\n" allowed)
     "- ОПЕРАЦИИ УКАЗЫВАТЬ строго как в списке. Запрещены синонимы: write, create_file, delete_file, rename_file.\n"
     "- Заголовок блока: всегда :version \"1\". Ключи только из спецификации для выбранной операции.\n"
     "- Пути только относительные к корню репозитория. Использовать :file (не :path).\n"
     "- Для create/sre/sre-batch обязателен :delim — ровно 6 hex-символов, нижний регистр. Делимитеры в теле: строки <<DELIM и :DELIM отдельными строками.\n"
     "- Для patch: единый unified diff только для одного файла (ровно одна пара ---/+++), пути a/ и b/ совпадают, :strip=1. Без binary/rename/copy прелюдий.\n"
     "- Запрещены base64-вставки; при необходимости инструмент выполнит fallback сам.\n")))

(defun carriage-build-prompt (intent suite-id ctx)
  "Build (:system :prompt) for INTENT ('Ask|'Patch) and SUITE-ID with CTX.
CTX may contain keys like :payload, :delim, :files, etc. Payload is user task text."
  (pcase intent
    ('Ask
     (list :system "Режим диалога (Ask). Не генерируй begin_patch."
           :prompt (or (plist-get ctx :payload) "")))
    ('Patch
     (let* ((ops (carriage-suite-ops suite-id))
            ;; Ensure op modules are loaded so their prompt fragments are registered.
            (_ (dolist (op ops)
                 (unless (carriage-format-get op "1")
                   (pcase op
                     ((or 'sre 'sre-batch) (load "ops/carriage-op-sre" t t))
                     ('patch               (load "ops/carriage-op-patch" t t))
                     ((or 'create 'delete 'rename) (load "ops/carriage-op-file" t t))
                     (_ nil)))))
            (frags
             (cl-loop for op in ops
                      for rec = (carriage-format-get op "1")
                      for fn = (and rec (plist-get rec :prompt-fragment))
                      when (functionp fn)
                      collect (condition-case e
                                  (funcall fn ctx)
                                (error (format ";; prompt-fragment error for %s: %s\n"
                                               op (error-message-string e)))))))
       (list :system (concat (carriage--suite-guardrails ops)
                             (mapconcat #'identity (delq nil frags) "\n"))
             :prompt (or (plist-get ctx :payload) ""))))
    (_
     (list :system "Unknown intent; defaulting to Ask." :prompt (or (plist-get ctx :payload) "")))))

(provide 'carriage-suite)
;;; carriage-suite.el ends here
