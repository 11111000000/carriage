;;; carriage-task.el --- Create task docs and task helpers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Version: 0.1
;; Keywords: task, org
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/tasks-v2.org
;;   spec/testing-v2.org
;;
;;; Commentary:
;; Helpers to create per-heading task documents and links.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'carriage-utils)       ;; carriage-project-root, carriage-write-file-string, etc.
(require 'carriage-logging)
;; Optional: transport for auto analysis
(eval-when-compile (declare-function carriage-transport-dispatch "carriage-transport" (&rest args)))
(eval-when-compile (declare-function carriage-mode "carriage-mode" (&optional arg)))

(defgroup carriage-task nil
  "Create a numbered task document from a TODO.org heading."
  :group 'carriage)

(defcustom carriage-task-docs-dir "org"
  "Directory (relative to project root) where task docs are created."
  :type 'string :group 'carriage-task)

(defcustom carriage-task-filename-format "%s-%s.org"
  "Filename format for new task docs. First %s is the ordinal string (e.g., \"1.0\"), second %s is a slug of the title."
  :type 'string :group 'carriage-task)

(defcustom carriage-task-slugify-fn #'carriage-task-default-slugify
  "Function (TITLE -> SLUG) to build filename slug."
  :type 'function :group 'carriage-task)

(defcustom carriage-task-auto-run-analysis nil
  "When non-nil, automatically start a streaming analysis chat (Intent=Ask) in the created doc."
  :type 'boolean :group 'carriage-task)

(defcustom carriage-task-analysis-backend 'gptel
  "Backend for analysis chat ('gptel or 'echo)."
  :type '(choice (const gptel) (const echo)) :group 'carriage-task)

(defcustom carriage-task-analysis-model "gptel:default"
  "Model id for analysis chat (string or symbol)."
  :type '(choice string symbol) :group 'carriage-task)

(defcustom carriage-task-analysis-prompt
  "Диалектически проанализируй задачу, задай вопросы с целью составить полное ТЗ, а также напиши полный список путей файлов, которые нужны для анализа этой задачи в блоке #+begin_context"
  "Prompt used to start the analysis chat when auto-run is enabled."
  :type 'string :group 'carriage-task)

(defcustom carriage-task-move-subtree-content nil
  "When non-nil, move the subtree content into the new task document, leaving only the heading and backlink in the source."
  :type 'boolean :group 'carriage-task)

(defcustom carriage-task-advance-todo-on-create t
  "When non-nil, advance TODO state of the origin heading after creating a task doc."
  :type 'boolean :group 'carriage-task)

(defcustom carriage-task-next-todo-states '("THINK" "DOING" "NEXT")
  "Preferred TODO states to set after creating task doc; first present in the buffer is used.
If none are available, falls back to advancing to the next state (`org-todo' 'nextset)."
  :type '(repeat string) :group 'carriage-task)

(defun carriage-task--advance-todo-state-at-point ()
  "Advance TODO state at current org heading using `carriage-task-next-todo-states'.
If none of preferred states are available, use `org-todo' with 'nextset."
  (when (derived-mode-p 'org-mode)
    (require 'org)
    (save-excursion
      (org-back-to-heading t)
      (let* ((cands carriage-task-next-todo-states)
             (avail (or (and (boundp 'org-todo-keywords-1) org-todo-keywords-1) '()))
             (chosen (cl-find-if (lambda (s) (member s avail)) cands)))
        (cond
         ((and (stringp chosen) (not (string-empty-p chosen)))
          (ignore-errors (org-todo chosen)))
         (t
          (ignore-errors (org-todo 'nextset))))))))

(defun carriage-task--project-root ()
  "Return project root, falling back to default-directory."
  (or (ignore-errors (carriage-project-root))
      (expand-file-name default-directory)))

(defun carriage-task--docs-abs-dir (root)
  "Absolute path to the docs dir under ROOT."
  (expand-file-name carriage-task-docs-dir (file-name-as-directory root)))

(defun carriage-task--ensure-docs-dir (root)
  "Ensure docs directory exists under ROOT."
  (let ((dir (carriage-task--docs-abs-dir root)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun carriage-task--todo-file-p (file root)
  "Return non-nil when FILE is the primary TODO file under ROOT."
  (and file root
       (string= (expand-file-name file)
                (expand-file-name "TODO.org" (file-name-as-directory root)))))

(defun carriage-task--sanitize-subdir-name (name)
  "Return safe directory NAME using `carriage-task-slugify-fn'."
  (let* ((base (string-trim (or name "")))
         (slug (funcall carriage-task-slugify-fn (if (string-empty-p base) "org" base))))
    (if (string-empty-p slug) "org" slug)))

(defun carriage-task--origin-doc-dir (root origin-file)
  "Return absolute doc directory for ORIGIN-FILE under ROOT.
TODO.org in ROOT uses `carriage-task-docs-dir'. Other files get ROOT/<slug-of-base>/."
  (if (carriage-task--todo-file-p origin-file root)
      (carriage-task--ensure-docs-dir root)
    (let* ((base (and origin-file (file-name-base origin-file)))
           (slug (carriage-task--sanitize-subdir-name base))
           (dir  (expand-file-name slug (file-name-as-directory root))))
      (unless (file-directory-p dir)
        (make-directory dir t))
      dir)))

(defun carriage-task--origin-doc-line (relative-path)
  "Return backlink line for RELATIVE-PATH from origin heading."
  (format "Документ: [[file:%s][%s]]" relative-path relative-path))

(defun carriage-task--source-link (relative-path title)
  "Return link string from new doc back to source heading."
  (if (and relative-path title)
      (format "[[file:%s::*%s][перейти]]" relative-path title)
    ""))

(defun carriage-task-default-slugify (title)
  "Simple slugify for TITLE: transliterate RU→EN (basic), downcase, replace spaces with dashes, strip non [a-z0-9-]."
  (let* ((tbl '((?а . "a") (?б . "b") (?в . "v") (?г . "g") (?д . "d") (?е . "e") (?ё . "e")
                (?ж . "zh") (?з . "z") (?и . "i") (?й . "i") (?к . "k") (?л . "l") (?м . "m")
                (?н . "n") (?о . "o") (?п . "p") (?р . "r") (?с . "s") (?т . "t") (?у . "u")
                (?ф . "f") (?х . "h") (?ц . "c") (?ч . "ch") (?ш . "sh") (?щ . "sch") (?ъ . "")
                (?ы . "y") (?ь . "") (?э . "e") (?ю . "yu") (?я . "ya")))
         (s (downcase (or title "")))
         (out (with-temp-buffer
                (dolist (ch (string-to-list s))
                  (let* ((str (char-to-string ch))
                         (m (or (cdr (assq ch tbl))
                                (cdr (assq (upcase ch) tbl))
                                str)))
                    (insert m)))
                (buffer-string))))
    (setq out (replace-regexp-in-string "[^a-z0-9 _-]" "" out))
    (setq out (replace-regexp-in-string "[ \t]+" "-" out))
    (setq out (replace-regexp-in-string "-+" "-" out))
    (string-trim out "-+" "-+")))

(defun carriage-task--next-ordinal (dir)
  "Return next ordinal string (\"N.0\") for task docs within DIR."
  (let* ((files (ignore-errors (directory-files dir nil "^[0-9]+\\(?:\\.[0-9]+\\)?-.*\\.org\\'")))
         (majors (mapcar (lambda (f)
                           (if (string-match "\\`\\([0-9]+\\)\\(?:\\.[0-9]+\\)?-" f)
                               (string-to-number (match-string 1 f))
                             0))
                         files))
         (maxn (if majors (apply #'max majors) 0)))
    (format "%d.0" (1+ maxn))))

(defun carriage-task--org-heading-at-point ()
  "Return current Org heading text at point, or nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (org-get-heading t t t t))))

(defun carriage-task--org-subtree-text ()
  "Return current Org subtree text (without heading), or empty string."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (let ((beg (save-excursion (org-back-to-heading t) (forward-line 1) (point)))
            (end (save-excursion (org-end-of-subtree t t) (point))))
        (buffer-substring-no-properties beg end)))))

(defun carriage-task--relative (from to)
  "Return TO path relative to FROM directory."
  (file-relative-name (expand-file-name to) (file-name-as-directory (expand-file-name from))))

(defun carriage-task--maybe-insert-backlink-into-todo (todo-file heading new-file-rel)
  "Insert backlink to NEW-FILE-REL under HEADING in TODO-FILE if not present."
  (when (and (stringp todo-file) (file-exists-p todo-file)
             (stringp heading) (not (string-empty-p heading)))
    (with-current-buffer (find-file-noselect todo-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote heading)) nil t)
           (progn
             (org-back-to-heading t)
             (org-show-subtree)
             (let* ((lnk (format "[[file:%s][%s]]" new-file-rel new-file-rel))
                    (line (format "Документ: %s" lnk))
                    (exists nil))
               (save-excursion
                 (let ((hend (save-excursion (org-end-of-subtree t t) (point))))
                   (while (re-search-forward (regexp-quote lnk) hend t)
                     (setq exists t))))
               (unless exists
                 (end-of-line)
                 (insert "\n" line)
                 (save-buffer))))
         (carriage-log "carriage-task: heading not found in TODO: %s" heading))))))

(defun carriage-task--insert-backlink-into-origin (origin-file heading new-file-rel &optional wipe-subtree advance-state)
  "Insert backlink to NEW-FILE-REL under HEADING in ORIGIN-FILE.
When WIPE-SUBTREE non-nil, remove subtree content leaving only HEADING and the backlink.
When ADVANCE-STATE non-nil and `carriage-task-advance-todo-on-create' is t, advance TODO state."
  (when (and origin-file (file-exists-p origin-file) (stringp heading) (not (string-empty-p heading)))
    (with-current-buffer (find-file-noselect origin-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote heading)) nil t)
           (progn
             (org-back-to-heading t)
             (org-show-subtree)
             (let* ((lnk (format "[[file:%s][%s]]" new-file-rel new-file-rel))
                    (line (format "Документ: %s" lnk))
                    (beg (save-excursion (forward-line 1) (point)))
                    (hend (save-excursion (org-end-of-subtree t t) (point)))
                    (exists nil))
               (save-excursion
                 (goto-char beg)
                 (while (re-search-forward (regexp-quote lnk) hend t)
                   (setq exists t)))
               (save-excursion
                 (goto-char beg)
                 (unless exists
                   (insert line "\n")))
               (when wipe-subtree
                 (save-excursion
                   (delete-region beg hend))))
             (when (and advance-state carriage-task-advance-todo-on-create)
               (save-excursion
                 (org-back-to-heading t)
                 (carriage-task--advance-todo-state-at-point)))
             (save-buffer))
         (carriage-log "carriage-task: heading not found in origin: %s" heading))))))


(defun carriage-task--insert-new-file-template (buf title todo-link subtree-text)
  "Insert template into BUF: TITLE, TODO-LINK, and quoted SUBTREE-TEXT."
  (with-current-buffer buf
    (erase-buffer)
    (insert (format "#+title: %s\n\n" title))
    (insert (format "См. TODO: %s\n\n" (or todo-link "")))
    (insert (format "* %s\n\n" title))
    (insert "** Исходный контекст\n")
    (insert "#+begin_quote\n")
    (insert (or (string-trim-right (or subtree-text "")) ""))
    (insert "\n#+end_quote\n\n")
    (insert "** Анализ\n")
    (insert "Диалектически проанализируй эту задачу. Опиши решение. И добавь блок #+begin_context,с полным списком путей файлов - спецификаций, кода и тестов, относящихся к данной задаче.\n")
    (goto-char (point-max))
    (current-buffer)))

(defun carriage-task--start-analysis (buf)
  "Start streaming analysis in BUF using transport adapter."
  (when (and (require 'carriage-transport nil t)
             (with-current-buffer buf (derived-mode-p 'org-mode)))
    (with-current-buffer buf
      (let* ((marker (copy-marker (point) t))
             (backend carriage-task-analysis-backend)
             (model  carriage-task-analysis-model)
             (prompt carriage-task-analysis-prompt))
        (ignore-errors (carriage-mode 1))
        (condition-case e
            (carriage-transport-dispatch
             :backend backend
             :model model
             :buffer (current-buffer)
             :mode major-mode
             :source 'buffer
             :insert-marker marker
             :prompt prompt
             :system nil
             :context nil
             :stream t)
          (error
           (carriage-log "carriage-task: analysis start error: %s" (error-message-string e))
           (message "Analysis start failed: %s" (error-message-string e))))))))

(defun carriage-task--find-todo-in-root (root)
  "Return TODO.org absolute path in ROOT if exists, else nil."
  (let* ((cand (expand-file-name "TODO.org" (file-name-as-directory root))))
    (when (file-exists-p cand) cand)))

;;;###autoload
(defun carriage-create-task-doc (&optional arg)
  "Create a numbered task doc from current Org heading and open it.

Behavior:
- Derive title from current Org heading (or prompt).
- Compute next ordinal N by scanning sibling task docs (N.0 pattern).
- Create N-<slug>.org under:
  - org/ (when origin is ROOT/TODO.org),
  - or ROOT/<slug-of-origin-base>/ for other org files.
- Insert template with backlink, optionally move subtree to the new doc.
- Add backlink into origin heading (and into TODO when applicable).
- Open the new file, enable org-mode and carriage-mode, set Intent=Ask.
- With prefix ARG (C-u), toggle auto analysis for this invocation."
  (interactive "P")
  (let* ((root (carriage-task--project-root))
         (origin-file (and (derived-mode-p 'org-mode) buffer-file-name))
         (origin-buf (current-buffer))
         (todo (carriage-task--find-todo-in-root root))
         (title0 (or (carriage-task--org-heading-at-point)
                     (read-string "Task title: ")))
         (title (string-trim title0))
         ;; Precompute paths and filenames within the same lexical scope
         (docs-dir (carriage-task--origin-doc-dir root origin-file))
         (ordinal (carriage-task--next-ordinal docs-dir))
         (slug (funcall carriage-task-slugify-fn title))
         (filename (format carriage-task-filename-format ordinal slug))
         (abs (expand-file-name filename docs-dir))
         (rel-from-root (carriage-task--relative root abs))
         (rel-to-todo   (carriage-task--relative docs-dir (or todo (expand-file-name "TODO.org" root))))
         (todo-link (format "[[file:%s::*%s][перейти]]" rel-to-todo title))
         (subtree (or (carriage-task--org-subtree-text) ""))
         (auto (if arg (not carriage-task-auto-run-analysis) carriage-task-auto-run-analysis)))
    ;; Confirm when not on a TODO heading
    (when (derived-mode-p 'org-mode)
      (require 'org)
      (save-excursion
        (org-back-to-heading t)
        (let* ((state (org-get-todo-state))
               (done-set (or org-done-keywords '("DONE"))))
          (when (or (null state) (member state done-set))
            (unless (y-or-n-p "Заголовок не в состоянии TODO. Продолжить создание документа? ")
              (user-error "Отменено пользователем"))))))
    ;; Create and populate new file
    (let ((buf (find-file-noselect abs)))
      (carriage-task--insert-new-file-template buf title todo-link subtree)
      (with-current-buffer buf
        (org-mode)
        (ignore-errors (carriage-mode 1))
        (when (boundp 'carriage-mode-intent)
          (setq-local carriage-mode-intent 'Ask))
        (save-buffer)
        (switch-to-buffer buf)))
    ;; Add backlink into TODO (root overview)
    (when todo
      (carriage-task--maybe-insert-backlink-into-todo todo title rel-from-root))
    ;; Add backlink into the origin heading and optionally wipe subtree
    (when (and origin-file (file-exists-p origin-file))
      (let* ((origin-dir (file-name-directory origin-file))
             (rel-from-origin (carriage-task--relative origin-dir abs)))
        (carriage-task--insert-backlink-into-origin
         origin-file title rel-from-origin carriage-task-move-subtree-content t)))
    ;; Save origin buffer if modified
    (when (and (buffer-live-p origin-buf)
               (buffer-modified-p origin-buf))
      (with-current-buffer origin-buf
        (save-buffer)))
    ;; Optional analysis
    (when auto
      (carriage-task--start-analysis (current-buffer)))))

;; Keys integration (via keyspec when available)
(with-eval-after-load 'carriage-keyspec
  (when (fboundp 'carriage-keys-register-actions)
    (ignore-errors
      (carriage-keys-register-actions
       '((:id task-new
              :label "Create task doc"
              :cmd carriage-create-task-doc
              :keys ("n")
              :section tools
              :contexts (carriage org global)
              :desc-key :task-new)))))
  (when (fboundp 'carriage-keys-apply-known-keymaps) (ignore-errors (carriage-keys-apply-known-keymaps))))

(provide 'carriage-task)
;;; carriage-task.el ends here
