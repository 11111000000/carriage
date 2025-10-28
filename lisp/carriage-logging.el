;;; carriage-logging.el --- Logging buffers and helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage nil
  "Carriage-mode customization group."
  :group 'tools)

(defcustom carriage-mode-log-max-lines 2000
  "Maximum number of lines to keep in *carriage-log* and report buffers."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-traffic-max-lines 1000
  "Maximum number of lines in *carriage-traffic* buffer."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-aux-window-side 'right
  "Side for auxiliary buffers (*carriage-log*, *carriage-traffic*)."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'carriage)

(defcustom carriage-mode-aux-window-size 0.33
  "Relative size of the side window for auxiliary buffers.
For left/right sides this is window-width; for top/bottom — window-height."
  :type 'number
  :group 'carriage)

(defcustom carriage-mode-aux-window-reuse t
  "Reuse an existing window that already displays the auxiliary buffer."
  :type 'boolean
  :group 'carriage)

(defconst carriage--log-buffer-name "*carriage-log*")
(defconst carriage--traffic-buffer-name "*carriage-traffic*")

(defun carriage-log-buffer ()
  "Return the log buffer, creating it if necessary."
  (get-buffer-create carriage--log-buffer-name))

(defun carriage-traffic-buffer ()
  "Return the traffic buffer, creating it if necessary."
  (get-buffer-create carriage--traffic-buffer-name))

(defun carriage--buffer-line-count (buffer)
  "Count lines in BUFFER."
  (with-current-buffer buffer
    (count-lines (point-min) (point-max))))

(defun carriage--trim-buffer-lines (buffer max-lines)
  "Trim BUFFER to MAX-LINES from the end."
  (with-current-buffer buffer
    (when (> (carriage--buffer-line-count buffer) max-lines)
      (goto-char (point-max))
      (forward-line (- max-lines))
      (delete-region (point-min) (point)))))

(defun carriage--append-line-capped (buffer string max-lines)
  "Append STRING and newline to BUFFER, cap to MAX-LINES.
STRING may be any object; it will be coerced to a string via `format'."
  (let* ((s (if (stringp string) string (format "%s" string))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert s)
      (unless (or (string-empty-p s)
                  (eq (aref s (1- (length s))) ?\n))
        (insert "\n")))
    (carriage--trim-buffer-lines buffer max-lines)
    buffer))

(defun carriage-log (fmt &rest args)
  "Log a formatted message FMT with ARGS to the general log."
  (let* ((buf (carriage-log-buffer)))
    (carriage--append-line-capped buf (apply #'format fmt args) carriage-mode-log-max-lines)))

(defun carriage-traffic-log (dir fmt &rest args)
  "Log traffic DIR ('in|'out) with FMT and ARGS."
  (let* ((tag (format "[%s] " (if (eq dir 'in) "IN " "OUT")))
         (buf (carriage-traffic-buffer)))
    (carriage--append-line-capped buf (concat tag (apply #'format fmt args)) carriage-mode-traffic-max-lines)))

(defun carriage-clear-logs ()
  "Clear log and traffic buffers."
  (dolist (b (list (carriage-log-buffer) (carriage-traffic-buffer)))
    (with-current-buffer b
      (erase-buffer)))
  (message "Carriage logs cleared.")
  t)

(defun carriage--display-aux-buffer (buffer &optional side size reuse)
  "Display BUFFER in a side window without replacing the current window.
SIDE defaults to =carriage-mode-aux-window-side'. SIZE defaults to
=carriage-mode-aux-window-size'. When REUSE (or
=carriage-mode-aux-window-reuse') is non-nil, reuse an existing window
already showing BUFFER."
  (let* ((side (or side (and (boundp 'carriage-mode-aux-window-side)
                             carriage-mode-aux-window-side)
                   'right))
         (size (or size (and (boundp 'carriage-mode-aux-window-size)
                             carriage-mode-aux-window-size)
                   0.33))
         (reuse (if (boundp 'carriage-mode-aux-window-reuse)
                    carriage-mode-aux-window-reuse
                  t))
         (win (and reuse (get-buffer-window buffer t))))
    (save-selected-window
      (let* ((inhibit-switch-frame t))
        (cond
         (win
          ;; Reuse existing window but do not select it (preserve user focus).
          (set-window-buffer win buffer))
         (t
          ;; Show in a side window and keep main window intact (no focus change).
          (display-buffer buffer
                          `((display-buffer-reuse-window display-buffer-in-side-window)
                            (inhibit-same-window . t)
                            (side . ,side)
                            ,@(if (memq side '(left right))
                                  `((window-width . ,size))
                                `((window-height . ,size)))
                            (slot . -1)
                            (window-parameters . ((no-delete-other-windows . t)
                                                  (no-other-window . t)))))))))))

(defun carriage-show-log ()
  "Display the Carriage log buffer in a side window."
  (interactive)
  (carriage--display-aux-buffer (carriage-log-buffer)
                                carriage-mode-aux-window-side
                                carriage-mode-aux-window-size
                                carriage-mode-aux-window-reuse))

(defun carriage-show-traffic ()
  "Display the Carriage traffic buffer in a side window."
  (interactive)
  (carriage--display-aux-buffer (carriage-traffic-buffer)
                                carriage-mode-aux-window-side
                                carriage-mode-aux-window-size
                                carriage-mode-aux-window-reuse))

;;;###autoload
(defun carriage-show-log-and-traffic ()
  "Open both *carriage-log* and *carriage-traffic* side windows on the right."
  (interactive)
  (let ((reuse (and (boundp 'carriage-mode-aux-window-reuse) carriage-mode-aux-window-reuse))
        (size  (and (boundp 'carriage-mode-aux-window-size) carriage-mode-aux-window-size)))
    (carriage--display-aux-buffer (carriage-log-buffer) 'right size reuse)
    (carriage--display-aux-buffer (carriage-traffic-buffer) 'right size reuse)))

(provide 'carriage-logging)
;;; carriage-logging.el ends here
