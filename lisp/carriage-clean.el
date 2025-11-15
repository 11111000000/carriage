;;; carriage-clean.el --- Utilities to clean patch blocks  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, convenience
;;
;; Specifications:
;;   spec/code-style-v1.org
;;   spec/index.org
;;   spec/errors-v1.org
;;   spec/compliance-checklist-v1.org
;;   spec/parser-registry-v1.org
;;
;;; Commentary:
;; Simple utilities to remove Org #+begin_patch … #+end_patch blocks from the
;; current buffer or region. Used as a developer convenience for cleaning up
;; generated patches.
;;
;;; Code:
;; (file body unchanged below)
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, convenience
;;
;; Specifications:
;;   spec/code-style-v1.org
;;   spec/index.org
;;   spec/errors-v1.org
;;   spec/compliance-checklist-v1.org
;;   spec/parser-registry-v1.org
;;   spec/ui-v1.org
;;
;;; Commentary:
;; Simple utilities to remove Org #+begin_patch … #+end_patch blocks from the
;; current buffer or region. Used as a developer convenience for cleaning up
;; generated patches.
;;
;;; Code:
;; Specifications:
;;   spec/code-style-v1.org
;;   spec/index.org
;;   spec/errors-v1.org
;;   spec/compliance-checklist-v1.org
;;   spec/parser-registry-v1.org
;;   spec/ui-v1.org

(require 'cl-lib)
(require 'subr-x)

;;;###autoload
(defun carriage-clear-patch-blocks ()
  "Удалить из текущего буфера все блоки Org #+begin_patch … #+end_patch.
Если активен регион — выполняется в пределах региона, иначе по всему буферу.
Возвращает число удалённых блоков."
  (interactive)
  (let* ((range-start (if (use-region-p) (region-beginning) (point-min)))
         (range-end   (if (use-region-p) (region-end) (point-max)))
         (count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region range-start range-end)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
          (let ((beg (match-beginning 0)))
            (if (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                (let ((end (line-end-position)))
                  (delete-region beg end)
                  (cl-incf count)
                  ;; Удалить возможные лишние пустые строки после блока
                  (when (and (not (eobp)) (looking-at "\n+"))
                    (replace-match "")))
              ;; Нет закрывающего маркера — удаляем до конца видимой области
              (delete-region beg (point-max))
              (cl-incf count))))))
    (when (called-interactively-p 'interactive)
      (message "Carriage: удалено patch-блоков: %d" count))
    count))

(provide 'carriage-clean)
;;; carriage-clean.el ends here
