;;; carriage-utils.el --- Utilities: project root, paths, delim, IO  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)

;; Silence byte-compiler about project.el functions when not loaded at compile time.
(declare-function project-current "project" (&optional dir))
(declare-function project-root "project" (project))

(defcustom carriage-mode-wip-branch "carriage/WIP"
  "Name of the working WIP branch for Carriage."
  :type 'string
  :group 'carriage)

(defun carriage--call-git (default-dir &rest args)
  "Call git with ARGS in DEFAULT-DIR, return (:exit :stdout :stderr).
Capture stderr via a temporary file to avoid DEST type issues."
  (let ((default-directory (file-name-as-directory (expand-file-name default-dir))))
    (let ((stderr-file (make-temp-file "carriage-git-stderr-")))
      (unwind-protect
          (with-temp-buffer
            (let* ((out (current-buffer))
                   (status (apply #'call-process "git" nil (list out stderr-file) nil args)))
              (list :exit status
                    :stdout (buffer-string)
                    :stderr (with-temp-buffer
                              (insert-file-contents stderr-file)
                              (buffer-string)))))
        (ignore-errors (delete-file stderr-file))))))

(defun carriage-project-root ()
  "Detect project root directory. Return absolute path or nil."
  (let ((root nil))
    (condition-case _e
        (when (require 'project nil t)
          (let* ((proj (project-current nil))
                 (pr (and proj (project-root proj))))
            (when (and pr (file-directory-p pr)) (setq root (expand-file-name pr)))))
      (error nil))
    (or root
        (let* ((res (carriage--call-git default-directory "rev-parse" "--show-toplevel")))
          (when (and (eq (plist-get res :exit) 0)
                     (string-match-p ".+" (plist-get res :stdout)))
            (string-trim (plist-get res :stdout)))))))

(defun carriage--path-looks-unsafe-p (path)
  "Return non-nil if PATH looks unsafe (absolute or has .. or starts with ~)."
  (or (file-name-absolute-p path)
      (string-prefix-p "~" path)
      (seq-some (lambda (seg) (string= seg "..")) (split-string path "/" t))))

(defun carriage-normalize-path (root relpath)
  "Normalize RELPATH under ROOT, ensuring it stays within ROOT. Return truename."
  (when (file-remote-p root)
    (signal (carriage-error-symbol 'IO_E_PATH) (list "TRAMP is not supported in v1")))
  (when (carriage--path-looks-unsafe-p relpath)
    (signal (carriage-error-symbol 'OPS_E_PATH) (list (format "Недопустимый путь: %s" relpath))))
  (let* ((abs (expand-file-name relpath root))
         (tru (file-truename abs))
         (root-tru (file-truename (file-name-as-directory (expand-file-name root)))))
    (unless (string-prefix-p root-tru (file-name-as-directory tru))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list (format "Выход за корень: %s" relpath))))
    tru))

(defun carriage-generate-delim ()
  "Generate a random 6-hex lowercase token."
  (let* ((b1 (random 256))
         (b2 (random 256))
         (b3 (random 256)))
    (format "%02x%02x%02x" b1 b2 b3)))

(defun carriage-read-file-string (path)
  "Read file at PATH and return its contents as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun carriage-write-file-string (path content &optional mkdir)
  "Write CONTENT to PATH. When MKDIR non-nil, create parent dirs."
  (let ((dir (file-name-directory path)))
    (when (and mkdir dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file path
    (insert content))
  t)

(defun carriage--plist-get* (plist &rest keys)
  "Get nested KEYS from PLIST treating it as plist."
  (let ((cur plist))
    (while (and keys cur)
      (setq cur (plist-get cur (car keys)))
      (setq keys (cdr keys)))
    cur))

(defun carriage--alist (plist)
  "Convert PLIST to an alist with symbol keys."
  (let ((res nil) (p plist))
    (while p
      (let ((k (car p)) (v (cadr p)))
        (setq res (cons (cons k v) res)))
      (setq p (cddr p)))
    (nreverse res)))

(provide 'carriage-utils)
;;; carriage-utils.el ends here
