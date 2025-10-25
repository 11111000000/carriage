;;; carriage-ui.el --- Keymap and minimal UI helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar carriage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-RET") #'carriage-send-buffer)
    (define-key map (kbd "C-c RET")   #'carriage-send-subtree)
    (define-key map (kbd "C-c C-c")   #'carriage-apply-at-point)
    (define-key map (kbd "C-c !")     #'carriage-apply-last-iteration)
    (define-key map (kbd "C-c ?")     #'carriage-dry-run-at-point)
    (define-key map (kbd "C-c b c")   #'carriage-wip-checkout)
    (define-key map (kbd "C-c b l")   #'carriage-show-log)
    (define-key map (kbd "C-c b r")   #'carriage-wip-reset-soft)
    ;; Navigation placeholders (optional)
    (define-key map (kbd "M-n")       #'carriage-next-patch-block)
    (define-key map (kbd "M-p")       #'carriage-prev-patch-block)
    map)
  "Keymap for =carriage-mode'.")

(defvar carriage--ui-state 'idle
  "Current UI state: one of 'idle 'sending 'streaming 'dry-run 'apply 'error.")

(defun carriage-ui-set-state (state)
  "Set UI STATE symbol for mode-line visuals."
  (setq carriage--ui-state state))

(defun carriage-ui-state-lighter ()
  "Return a short lighter suffix for current =carriage--ui-state'."
  (pcase carriage--ui-state
    ('idle "")
    ('sending " snd")
    ('streaming " str")
    ('dry-run " dry")
    ('apply " apl")
    ('error " ERR")
    (_ "")))

(provide 'carriage-ui)
;;; carriage-ui.el ends here
