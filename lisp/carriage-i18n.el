;;; carriage-i18n.el --- Simple i18n layer (ru/en) for UI strings -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-i18n nil
  "Internationalization (i18n) for Carriage UI."
  :group 'applications
  :prefix "carriage-i18n-")

(defcustom carriage-i18n-locale 'ru
  "Current locale for Carriage UI ('ru or 'en)."
  :type '(choice (const ru) (const en))
  :group 'carriage-i18n)

(defconst carriage-i18n--table
  '(
    ;; Menu, groups, which-key
    (:carriage-menu      . ((ru . "Carriage Menu")      (en . "Carriage Menu")))
    (:carriage-toggles   . ((ru . "Carriage Toggles")   (en . "Carriage Toggles")))
    (:navigate-title     . ((ru . "Навигация")          (en . "Navigate")))
    (:act-title          . ((ru . "Действия")           (en . "Actions")))
    (:session-title      . ((ru . "Сессия/Git")         (en . "Session/Git")))
    (:tools-title        . ((ru . "Инструменты")        (en . "Tools")))
    (:logs-title         . ((ru . "Логи/Отчёты")        (en . "Logs/Reports")))

    ;; Model tooltip + common labels
    (:model-tooltip      . ((ru . "Модель: %s")         (en . "Model: %s")))

    ;; Actions (keyspec desc-keys)
    (:model-select       . ((ru . "Выбрать модель")            (en . "Select model")))
    (:toggle-ctx         . ((ru . "Переключить gptel-контекст") (en . "Toggle gptel-context")))
    (:toggle-doc         . ((ru . "Переключить файлы из документа") (en . "Toggle document files")))
    (:dry-run            . ((ru . "Dry-run под точкой")        (en . "Dry-run at point")))
    (:apply              . ((ru . "Применить под точкой")      (en . "Apply at point")))
    (:apply-all          . ((ru . "Применить итерацию")        (en . "Apply last iteration")))
    (:abort              . ((ru . "Отменить")                  (en . "Abort")))
    (:report             . ((ru . "Открыть отчёт")             (en . "Open report")))
    (:wip                . ((ru . "Переключить на WIP")        (en . "Switch to WIP")))
    (:reset              . ((ru . "Soft reset")                (en . "Soft reset")))
    (:commit-all         . ((ru . "Коммит (все изменения)")    (en . "Commit all changes")))
    (:commit-last        . ((ru . "Коммит последней итерации") (en . "Commit last iteration")))
    (:engine             . ((ru . "Выбор движка")              (en . "Select engine")))
    (:menu               . ((ru . "Меню")                      (en . "Menu")))
    )
  "Translation table: KEY → ((ru . STR) (en . STR)).")

(defun carriage-i18n-known-keys ()
  "Return a list of known i18n keys."
  (mapcar #'car carriage-i18n--table))

(defun carriage-i18n (key &rest args)
  "Return localized string for KEY; ARGS are formatted with `format'.
Fallback policy: current locale → 'en → symbol-name of KEY."
  (let* ((cell (assq key carriage-i18n--table))
         (pair (and cell (cdr cell)))
         (lang carriage-i18n-locale)
         (txt (cond
               ((and pair (alist-get lang pair)) (alist-get lang pair))
               ((and pair (alist-get 'en pair))  (alist-get 'en pair))
               (t (symbol-name key)))))
    (if (and args (stringp txt))
        (apply #'format txt args)
      txt)))

(defun carriage-i18n-set-locale (locale)
  "Set LOCALE ('ru or 'en) for Carriage i18n and refresh UI."
  (setq carriage-i18n-locale (if (memq locale '(ru en)) locale 'en))
  (force-mode-line-update t)
  t)

(provide 'carriage-i18n)
;;; carriage-i18n.el ends here
