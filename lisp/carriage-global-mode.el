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
(define-minor-mode carriage-global-mode
  "Global minor mode enabling the Carriage prefix/menu for :contexts (global).
When enabled:
- If =carriage-global-use-transient' is non-nil, bind the base prefix (from =carriage-keys-prefix') globally to =carriage-keys-open-menu'.
- If nil, install a global prefix keymap under the base prefix and populate suffixes from :contexts (global) via keyspec.
which-key hints are registered if available."
  :global t
  :group 'carriage-global
  (let* ((prefixes (carriage-keys-prefixes)))
    (dolist (pref prefixes)
      (define-key global-map (kbd pref) nil))
    (when (keymapp carriage-global--prefix-map)
      (setq carriage-global--prefix-map nil))
    (if carriage-global-mode
        (progn
          (dolist (pref prefixes)
            (define-key global-map (kbd pref) #'carriage-keys-open-menu))
          (ignore-errors (carriage-keys-which-key-register))
          (message "carriage-global-mode enabled (menu)"))
      (ignore-errors (carriage-keys-which-key-unregister))
      (message "carriage-global-mode disabled"))))

(provide 'carriage-global-mode)
;;; carriage-global-mode.el ends here
