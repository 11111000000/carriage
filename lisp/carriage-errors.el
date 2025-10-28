;;; carriage-errors.el --- Error table and helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst carriage--error-table
  '(
    (SRE_E_VERSION          carriage-sre-version          "Неверная версия (SRE_E_VERSION)")
    (SRE_E_OP               carriage-sre-op               "Неверное значение :op (SRE_E_OP)")
    (SRE_E_PATH             carriage-sre-path             "Недопустимый путь (SRE_E_PATH)")
    (SRE_E_SEGMENTS_COUNT   carriage-sre-segments-count   "Неверное число сегментов (SRE_E_SEGMENTS_COUNT)")
    (SRE_E_SEGMENTS_ODD     carriage-sre-segments-odd     "Нечётное количество сегментов (SRE_E_SEGMENTS_ODD)")
    (SRE_E_UNCLOSED_SEGMENT carriage-sre-unclosed-segment "Незакрытый сегмент (SRE_E_UNCLOSED_SEGMENT)")
    (SRE_E_OCCUR_EXPECT     carriage-sre-occur-expect     "Для :occur all требуется :expect (SRE_E_OCCUR_EXPECT)")
    (SRE_E_REGEX_SYNTAX     carriage-sre-regex-syntax     "Неподдерживаемый синтаксис regexp (SRE_E_REGEX_SYNTAX)")
    (SRE_E_NO_MATCHES       carriage-sre-no-matches       "Совпадений не найдено (SRE_E_NO_MATCHES)")
    (SRE_E_EXPECT_MISMATCH  carriage-sre-expect-mismatch  "Число замен не соответствует :expect (SRE_E_EXPECT_MISMATCH)")
    (SRE_E_DELIM            carriage-sre-delim            "Неверный :delim (SRE_E_DELIM)")
    (SRE_E_COLLISION_DELIM  carriage-sre-collision-delim  "Коллизия DELIM (SRE_E_COLLISION_DELIM)")
    (SRE_E_LIMITS           carriage-sre-limits           "Превышены лимиты размера (SRE_E_LIMITS)")

    (PATCH_E_VERSION        carriage-patch-version        "Неверная версия (PATCH_E_VERSION)")
    (PATCH_E_OP             carriage-patch-op             "Неверное значение :op (PATCH_E_OP)")
    (PATCH_E_APPLY          carriage-patch-apply          "Ошибка применения diff (PATCH_E_APPLY)")
    (PATCH_E_MULTI_FILE     carriage-patch-multi-file     "Многофайловый diff запрещён (PATCH_E_MULTI_FILE)")
    (PATCH_E_PATH           carriage-patch-path           "Недопустимый путь (PATCH_E_PATH)")
    (PATCH_E_PATH_MISMATCH  carriage-patch-path-mismatch  "Пути a/ и b/ различаются (PATCH_E_PATH_MISMATCH)")
    (PATCH_E_DIFF_SYNTAX    carriage-patch-diff-syntax    "Синтаксическая ошибка diff (PATCH_E_DIFF_SYNTAX)")
    (PATCH_E_STRIP          carriage-patch-strip          "Несогласован :strip (PATCH_E_STRIP)")
    (PATCH_E_GIT_CHECK      carriage-patch-git-check      "git apply --check отказал (PATCH_E_GIT_CHECK)")
    (PATCH_E_BINARY         carriage-patch-binary         "Бинарные секции запрещены (PATCH_E_BINARY)")
    (PATCH_E_RENAME_COPY    carriage-patch-rename-copy    "rename/copy запрещены в v1 (PATCH_E_RENAME_COPY)")

    (OPS_E_EXISTS           carriage-ops-exists           "Объект уже существует (OPS_E_EXISTS)")
    (OPS_E_NOT_FOUND        carriage-ops-not-found        "Объект не найден (OPS_E_NOT_FOUND)")
    (OPS_E_PATH             carriage-ops-path             "Недопустимый путь (OPS_E_PATH)")
    (OPS_E_PERM             carriage-ops-perm             "Недостаточно прав (OPS_E_PERM)")
    (OPS_E_DELIM            carriage-ops-delim            "Неверный :delim (OPS_E_DELIM)")

    (MODE_E_DISPATCH        carriage-mode-dispatch        "Неизвестные :version или :op (MODE_E_DISPATCH)")
    (GIT_E_APPLY            carriage-git-apply            "Ошибка git apply (GIT_E_APPLY)")
    (IO_E_PATH              carriage-io-path              "Ошибка доступа к пути (IO_E_PATH)")

    ;; LLM transport errors
    (LLM_E_BACKEND          carriage-llm-backend          "Бэкенд транспорта недоступен (LLM_E_BACKEND)")
    (LLM_E_REQUEST          carriage-llm-request          "Ошибка формирования запроса (LLM_E_REQUEST)")
    (LLM_E_STREAM           carriage-llm-stream           "Ошибка потоковой передачи (LLM_E_STREAM)")
    (LLM_E_TOOL_USE         carriage-llm-tool-use         "Ошибка протокола tool-use (LLM_E_TOOL_USE)")
    (LLM_E_SCHEMA           carriage-llm-schema           "Ошибка обработки схемы (LLM_E_SCHEMA)")
    )
  "Mapping of error codes to condition symbols and Russian messages.")

(defun carriage-define-errors ()
  "Register Emacs condition types for Carriage error codes."
  (dolist (row carriage--error-table)
    (pcase-let ((`(,_code ,sym ,msg) row))
      (ignore-errors (define-error sym msg))))
  t)

(defun carriage-error-symbol (code)
  "Return a condition symbol mapped to CODE."
  (cadr (assq code carriage--error-table)))

(defun carriage-error-message (code)
  "Return a Russian message for error CODE."
  (caddr (assq code carriage--error-table)))

(provide 'carriage-errors)
;;; carriage-errors.el ends here
