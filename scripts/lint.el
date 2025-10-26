;;; lint.el --- Batch package-lint runner for Carriage  -*- lexical-binding: t; -*-

(defun carriage--collect-elisp-files ()
  "Return absolute paths of all .el files under lisp/."
  (let ((dir (expand-file-name "lisp")))
    (when (file-directory-p dir)
      (directory-files dir t "\\.el\\'"))))

(defun carriage-package-lint-batch ()
  "Install package-lint (if needed) and run it on lisp/*.el.
Exits Emacs with code 0 on success, non-zero on issues."
  (require 'package)
  ;; Minimal MELPA for package-lint
  (let ((melpa '("melpa" . "https://melpa.org/packages/")))
    (unless (assoc (car melpa) package-archives)
      (add-to-list 'package-archives melpa t)))
  (package-initialize)
  (unless (package-installed-p 'package-lint)
    (package-refresh-contents)
    (package-install 'package-lint))
  (require 'package-lint)
  (let ((files (carriage--collect-elisp-files)))
    (unless files
      (message "No lisp/*.el files found")
      (kill-emacs 0))
    ;; package-lint-batch-and-exit accepts a list of files
    (apply #'package-lint-batch-and-exit files)))

(provide 'carriage-lint)
;;; lint.el ends here