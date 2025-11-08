;;; carriage-clean.el --- Utilities to clean patch blocks  -*- lexical-binding: t; -*-

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
