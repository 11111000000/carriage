;;; carriage-task-test.el --- Tests for carriage-task (task docs) -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(require 'carriage-task nil t)
(require 'carriage-keyspec nil t)

(ert-deftest carriage-task--slugify-russian ()
  "Slugify should transliterate RU→EN and normalize to ASCII dashed."
  (should (fboundp 'carriage-task-default-slugify))
  (let* ((title "Задача Пример")
         (slug (carriage-task-default-slugify title)))
    ;; Expect ascii, no spaces, only [a-z0-9-]
    (should (string-match-p "\\`[a-z0-9-]+\\'" slug))
    ;; Basic expected transliteration fragments
    (should (string-match-p "zadacha" slug))
    (should (string-match-p "primer" slug))
    ;; Spaces replaced with single dash
    (should (string-match-p "-" slug))
    ;; No multiple consecutive dashes
    (should-not (string-match-p "--" slug))))

(ert-deftest carriage-task--next-ordinal-scan ()
  "Next ordinal should be max(N)+1 for files N-*.org."
  (let* ((dir (make-temp-file "carriage-task-test-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "1-a.org" dir) (insert ""))
          (with-temp-file (expand-file-name "3-b.org" dir) (insert ""))
          (with-temp-file (expand-file-name "not-a-task.txt" dir) (insert ""))
          (should (= (carriage-task--next-ordinal dir) 4)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-task--org-only-keybinding ()
  "Keyspec should bind C-c e n only for org-mode (context org)."
  (when (and (featurep 'carriage-keyspec)
             (fboundp 'carriage-keys-apply-known-keymaps))
    ;; Ensure org-mode keymap exists and keyspec is applied
    (carriage-keys-apply-known-keymaps)
    (should (keymapp org-mode-map))
    (let ((cmd (lookup-key org-mode-map (kbd "C-c e n"))))
      (should (eq cmd 'carriage-create-task-doc)))))

(ert-deftest carriage-task--create-doc-from-todo-heading ()
  "End-to-end: create org/N-*.org from TODO heading, with backlinks; preserve content and advance TODO."
  (let* ((root (make-temp-file "carriage-task-root-" t))
         (default-directory root)
         (todo (expand-file-name "TODO.org" root))
         (carriage-task-auto-run-analysis nil)) ;; disable streaming for test
    (unwind-protect
        (progn
          ;; Prepare TODO.org with a heading
          (with-temp-file todo
            (insert "#+TODO: TODO THINK DOING DONE\n")
            (insert "* TODO Задача Тест\n\nНекоторый контекст поддерева.\n"))
          ;; Visit TODO and position at heading, with custom TODO workflow
          (let ((buf (find-file-noselect todo)))
            (with-current-buffer buf
              (setq-local org-todo-keywords '((sequence "TODO" "THINK" "DOING" "DONE")))
              (org-mode)
              (goto-char (point-min))
              (search-forward "Задача Тест")
              (org-back-to-heading t)
              ;; Invoke the command
              (call-interactively #'carriage-create-task-doc)))
          ;; Validate org/ directory and created file
          (let* ((org-dir (expand-file-name "org" root))
                 (files (and (file-directory-p org-dir)
                             (directory-files org-dir nil "^[0-9]+-.*\\.org\\'")))
                 (created (and files (expand-file-name (car files) org-dir))))
            (should (file-directory-p org-dir))
            (should files)
            (should (file-exists-p created))
            ;; Check new file contents include a backlink to TODO
            (with-temp-buffer
              (insert-file-contents created)
              (goto-char (point-min))
              (should (re-search-forward (regexp-quote "См. TODO:") nil t))
              (should (re-search-forward (regexp-quote "[[file:../TODO.org::*Задача Тест]") nil t))))
          ;; Check TODO.org got a backlink to org/<N>-<slug>.org, preserved content, and advanced state
          (with-temp-buffer
            (insert-file-contents todo)
            (goto-char (point-min))
            (should (re-search-forward (regexp-quote "Документ: [[file:org/") nil t))
            (goto-char (point-min))
            (should (re-search-forward (regexp-quote "Некоторый контекст поддерева.") nil t))
            ;; Verify TODO state advanced to THINK
            (goto-char (point-min))
            (should (re-search-forward (rx bol "* " "THINK" (+ space) "Задача Тест") nil t))))
      (ignore-errors (delete-directory root t)))))

(provide 'carriage-task-test)
;;; carriage-task-test.el ends here
