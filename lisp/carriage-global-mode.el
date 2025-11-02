;;; carriage-global-mode.el --- Global prefix for Carriage -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-keyspec)

(defgroup carriage-global nil
  "Global integration for Carriage (global prefix/menu)."
  :group 'applications
  :prefix "carriage-global-")

(defcustom carriage-global-use-transient t
  "When non-nil, the global Carriage prefix opens the Carriage menu (transient or fallback).
When nil, the global Carriage prefix acts as a prefix key with suffixes from :contexts (global)."
  :type 'boolean
  :group 'carriage-global)

(defvar carriage-global--prefix-map nil
  "Global prefix keymap installed by carriage-global-mode when
carriage-global-use-transient is nil. Holds suffixes from :contexts (global).")


;;;###autoload
;;;###autoload
(define-minor-mode carriage-global-mode
  "Global minor mode enabling the Carriage prefix/menu for :contexts (global).
When enabled:
- If =carriage-global-use-transient' is non-nil, bind the base prefix (from =carriage-keys-prefix') globally to =carriage-keys-open-menu'.
- If nil, install a global prefix keymap under the base prefix and populate suffixes from :contexts (global) via keyspec.
which-key hints are registered if available."
  :global t
  :group 'carriage-global
  (let* ((base (string-trim-right (or carriage-keys-prefix "C-c e ") "[ \t\n\r]+"))
         (key  (kbd base)))
    ;; Clear any previous binding to avoid stale maps
    (define-key global-map key nil)
    (when (keymapp carriage-global--prefix-map)
      (setq carriage-global--prefix-map nil))
    (if carriage-global-mode
        (progn
          (if carriage-global-use-transient
              (define-key global-map key #'carriage-keys-open-menu)
            ;; Non-transient global prefix: create and populate prefix map from keyspec(:contexts global)
            (setq carriage-global--prefix-map (make-sparse-keymap))
            (define-key global-map key carriage-global--prefix-map)
            (when (require 'carriage-keyspec nil t)
              (if (fboundp 'carriage-keys-apply-prefix-suffixes)
                  (carriage-keys-apply-prefix-suffixes carriage-global--prefix-map 'global)
                (carriage-keys-apply-to carriage-global--prefix-map 'global))))
          (ignore-errors (carriage-keys-which-key-register))
          (message "carriage-global-mode enabled (%s)"
                   (if carriage-global-use-transient "menu" "prefix")))
      ;; Disable: unbind and unregister which-key hints
      (ignore-errors (carriage-keys-which-key-unregister))
      (setq carriage-global--prefix-map nil)
      (message "carriage-global-mode disabled"))))

(provide 'carriage-global-mode)
;;; carriage-global-mode.el ends here
