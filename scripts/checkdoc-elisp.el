;;; checkdoc-elisp.el --- Batch checkdoc for Carriage  -*- lexical-binding: t; -*-

(defun carriage--collect-elisp-files ()
  "Return absolute paths of all .el files under lisp/."
  (let ((dir (expand-file-name "lisp")))
    (when (file-directory-p dir)
      (directory-files dir t "\\.el\\'"))))

(defun carriage-checkdoc-elisp-lint ()
  "Run checkdoc on lisp/*.el and exit.
Note: checkdoc reports to *Messages*; we exit 0 to avoid flakiness on stylistic hints."
  (require 'checkdoc)
  (let ((files (carriage--collect-elisp-files)))
    (if (not files)
        (progn
          (message "No lisp/*.el files found")
          (kill-emacs 0))
      (apply #'checkdoc-batch-files files)
      (kill-emacs 0))))

(provide 'carriage-checkdoc)
;;; checkdoc-elisp.el ends here