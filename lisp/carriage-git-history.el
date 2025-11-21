;;; carriage-git-history.el --- Show full Git commit messages history  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: vcs, git, tools
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/logging-v2.org
;;
;;; Commentary:
;; Утилиты для получения полной истории сообщений коммитов Git текущего
;; репозитория. Основная команда формирует буфер с полными текстами описаний
;; всех коммитов (subject + body) в хронологическом порядке (от самых ранних
;; к самым новым), сверху вниз — удобно для дальнейшего анализа и составления
;; рассказа о разработке.
;;
;; Основано на вызове `git log'. Magit не обязателен.
;;
;;; Code:

(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())

(defgroup carriage-git-history nil
  "Показ полной истории сообщений коммитов Git."
  :group 'tools
  :prefix "carriage-git-history-")

(defcustom carriage-git-history-separator
  (concat "\n" (make-string 80 ?-) "\n\n")
  "Строка-разделитель между сообщениями коммитов в итоговом выводе."
  :type 'string
  :group 'carriage-git-history)

(defun carriage-git-history--project-root ()
  "Определить корень проекта (предпочтительно через carriage-project-root)."
  (or (and (fboundp 'carriage-project-root) (carriage-project-root))
      default-directory))

(defun carriage-git-history--repo-root (&optional dir)
  "Вернуть корень Git-репозитория для каталога DIR, либо nil."
  (let* ((default-directory (file-name-as-directory (or dir default-directory))))
    (when (executable-find "git")
      (with-temp-buffer
        (if (zerop (call-process "git" nil (current-buffer) nil
                                 "rev-parse" "--show-toplevel"))
            (string-trim (buffer-string))
          nil)))))

(defun carriage-git-history--collect-messages (repo-root)
  "Собрать полные тексты описаний всех коммитов для REPO-ROOT.

Возвращает строку, где каждый коммит — это полный текст (%B), а между
ними вставлен `carriage-git-history-separator'. Порядок — хронологический
(от старых к новым), то есть сверху вниз «как появлялись»."
  (let* ((default-directory (file-name-as-directory repo-root))
         ;; Разделяем коммиты не встречающимся в тексте символом RS (0x1e),
         ;; затем красиво склеиваем с видимым разделителем.
         (rs (char-to-string 30))
         (args (list "log" "--reverse" (concat "--pretty=format:%B%x1e"))))
    (with-temp-buffer
      (let ((ok (and (executable-find "git")
                     (zerop (apply #'call-process "git" nil t nil args)))))
        (unless ok
          (error "Не удалось получить историю git в %s" default-directory))
        (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
               (parts (split-string raw rs t)))
          (mapconcat (lambda (s) (string-trim-right s))
                     parts
                     (or carriage-git-history-separator "\n\n")))))))

;;;###autoload
(defun carriage-git-show-full-history (&optional copy-to-kill)
  "Показать в отдельном буфере полные тексты описаний всех коммитов Git.

Порядок — хронологический (от самых ранних к самым новым), сверху вниз.
Если задан префикс-аргумент COPY-TO-KILL (C-u), то также скопировать
полученный текст в kill-ring.

Буфер: *carriage git history*."
  (interactive "P")
  (let* ((proj (carriage-git-history--project-root))
         (repo (or (carriage-git-history--repo-root proj)
                   (user-error "Текущая директория не внутри Git-репозитория"))))
    (let ((text (carriage-git-history--collect-messages repo)))
      (when copy-to-kill
        (kill-new text)
        (message "История коммитов скопирована в kill-ring"))
      (let ((buf (get-buffer-create "*carriage git history*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text)
            (goto-char (point-min))
            (view-mode 1)
            (setq-local buffer-read-only t)
            (setq-local truncate-lines nil)
            (setq-local buffer-undo-list t)
            (setq-local default-directory (file-name-as-directory repo))))
        (pop-to-buffer buf)))))

;;;###autoload
(defun carriage-git-collect-full-history ()
  "Вернуть строкой все описания коммитов текущего Git-репозитория.

Полезно для использования в Org src блоках:
  (insert (carriage-git-collect-full-history))"
  (let* ((proj (carriage-git-history--project-root))
         (repo (or (carriage-git-history--repo-root proj)
                   (user-error "Текущая директория не внутри Git-репозитория"))))
    (carriage-git-history--collect-messages repo)))

(provide 'carriage-git-history)
;;; carriage-git-history.el ends here
