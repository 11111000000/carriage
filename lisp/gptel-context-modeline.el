;;; gptel-context-modeline.el --- Modeline context count and tooltip for gptel/org  -*- lexical-binding: t; -*-
;;
;; Author: Project Team
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, ui, tools
;; URL: https://example.invalid
;;
;; This file provides a minor mode that displays the number of context items
;; in the mode line, aggregated from:
;; - A user-provided gptel items provider function (no hard dependency).
;; - The nearest #+begin_context ... #+end_context block in the current buffer (org-like).
;;
;; Usage:
;; (require 'gptel-context-modeline)
;; (global-gptel-context-modeline-mode 1)
;;
;; Optional integration with gptel:
;; (setq gptel-context-modeline-gptel-items-function
;;       (lambda () ... return list of strings ...))

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup gptel-context-modeline nil
  "Modeline indicator for aggregated context items (gptel + org context block)."
  :group 'convenience
  :prefix "gptel-context-modeline-")

(defface gptel-context-modeline-face
  '((t :inherit mode-line))
  "Face used for the modeline indicator."
  :group 'gptel-context-modeline)

(defcustom gptel-context-modeline-enable-gptel-context t
  "When non-nil, include items provided by `gptel-context-modeline-gptel-items-function'."
  :type 'boolean
  :group 'gptel-context-modeline)

(defcustom gptel-context-modeline-enable-org-block-context t
  "When non-nil, include items from the nearest #+begin_context block."
  :type 'boolean
  :group 'gptel-context-modeline)

(defcustom gptel-context-modeline-format " [Ctl:%d]"
  "Format string for the modeline indicator; must contain a single %d placeholder for the count."
  :type 'string
  :group 'gptel-context-modeline)

(defcustom gptel-context-modeline-max-tooltip-items nil
  "Maximum number of items to show in the tooltip. nil means no limit."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'gptel-context-modeline)

(defcustom gptel-context-modeline-gptel-items-function
  (lambda () nil)
  "Function returning a list of strings representing gptel context items.

The function is called with no arguments and should return a list of strings.
By default returns nil (no dependency on gptel)."
  :type 'function
  :group 'gptel-context-modeline)

(defvar gptel-context-modeline--modeline-cookie nil
  "Internal cookie to track insertion into `global-mode-string'.")

(defvar gptel-context-modeline--modeline
  '(:eval (gptel-context-modeline--string))
  "The modeline element inserted into `global-mode-string'.")

(defun gptel-context-modeline--collect-gptel-items ()
  "Collect items from gptel provider."
  (when gptel-context-modeline-enable-gptel-context
    (let* ((fn gptel-context-modeline-gptel-items-function)
           (items (when (functionp fn) (ignore-errors (funcall fn)))))
      (when (listp items) items))))

(defun gptel-context-modeline--nearest-org-context-region ()
  "Return cons (START . END) of the nearest #+begin_context ... #+end_context block.

Prefer a block found by searching backward from point; if none, search forward.
Return nil if no block is found."
  (save-excursion
    (let ((case-fold-search t)
          start end)
      (save-excursion
        (when (re-search-backward "^[ \t]*#\\+begin_context\\b" nil t)
          (setq start (match-end 0))
          (when (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
            (setq end (match-beginning 0)))))
      (unless (and start end)
        (setq start nil end nil)
        (save-excursion
          (when (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
            (setq start (match-end 0))
            (when (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
              (setq end (match-beginning 0))))))
      (when (and start end)
        (cons start end)))))

(defun gptel-context-modeline--org-block-items ()
  "Collect items from the nearest org context block around point."
  (when gptel-context-modeline-enable-org-block-context
    (let ((region (gptel-context-modeline--nearest-org-context-region)))
      (when region
        (let* ((text (buffer-substring-no-properties (car region) (cdr region)))
               (lines (split-string text "\n" t)))
          (cl-remove-if
           #'string-empty-p
           (mapcar (lambda (s) (string-trim s)) lines)))))))

(defun gptel-context-modeline--collect-items ()
  "Collect and return the aggregated list of context items."
  (let ((items nil))
    (when gptel-context-modeline-enable-gptel-context
      (setq items (append items (or (gptel-context-modeline--collect-gptel-items) nil))))
    (when gptel-context-modeline-enable-org-block-context
      (setq items (append items (or (gptel-context-modeline--org-block-items) nil))))
    items))

(defun gptel-context-modeline--tooltip (items)
  "Build tooltip string from ITEMS list, honoring `gptel-context-modeline-max-tooltip-items'."
  (let* ((max gptel-context-modeline-max-tooltip-items)
         (count (length items)))
    (cond
     ((or (null max) (>= max count))
      (string-join items "\n"))
     (t
      (let ((shown (cl-subseq items 0 (max 0 max)))
            (rest (- count max)))
        (concat (string-join shown "\n")
                (format "\n… (+%d more)" rest)))))))

(defun gptel-context-modeline--string ()
  "Compute the modeline string for the current buffer."
  (let* ((items (gptel-context-modeline--collect-items))
         (n (length items)))
    (if (<= n 0)
        ""
      (propertize
       (format gptel-context-modeline-format n)
       'help-echo (gptel-context-modeline--tooltip items)
       'face 'gptel-context-modeline-face
       'mouse-face 'mode-line-highlight))))

;;;###autoload
(define-minor-mode gptel-context-modeline-mode
  "Minor mode to display aggregated context items count in the mode line.

When enabled, shows a segment like \" [Ctl:N]\" with a tooltip containing the
list of context items combined from gptel (if enabled) and the nearest
#+begin_context block (if enabled)."
  :global t
  :group 'gptel-context-modeline
  (if gptel-context-modeline-mode
      (progn
        (unless (member 'gptel-context-modeline--modeline global-mode-string)
          (setq gptel-context-modeline--modeline-cookie t)
          (add-to-list 'global-mode-string 'gptel-context-modeline--modeline t))
        (force-mode-line-update t))
    (when (member 'gptel-context-modeline--modeline global-mode-string)
      (setq global-mode-string (delq 'gptel-context-modeline--modeline global-mode-string)))
    (setq gptel-context-modeline--modeline-cookie nil)
    (force-mode-line-update t)))

;;;###autoload
(define-globalized-minor-mode global-gptel-context-modeline-mode
  gptel-context-modeline-mode
  (lambda () (gptel-context-modeline-mode 1))
  :group 'gptel-context-modeline)

(provide 'gptel-context-modeline)

;;; gptel-context-modeline.el ends here
