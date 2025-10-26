;;; compile-strict.el --- Treat byte-compile warnings as errors  -*- lexical-binding: t; -*-

(defun carriage--collect-elisp-files ()
  "Return absolute paths of all .el files under lisp/."
  (let ((dir (expand-file-name "lisp")))
    (when (file-directory-p dir)
      (directory-files dir t "\\.el\\'"))))

(defun carriage-byte-compile-strict ()
  "Byte-compile lisp/*.el with warnings treated as errors."
  ;; Treat warnings as errors, but ignore docstring formatting warnings.
  (setq byte-compile-error-on-warn t)
  (setq byte-compile-warnings '(not docstrings))
  (let ((files (carriage--collect-elisp-files)))
    (unless files
      (message "No lisp/*.el files found")
      (kill-emacs 0))
    ;; batch-byte-compile reads from `command-line-args-left'
    (setq command-line-args-left files)
    (batch-byte-compile)))

(provide 'carriage-compile-strict)
;;; compile-strict.el ends here
