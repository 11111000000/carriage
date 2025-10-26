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
  (let ((s (if (stringp string) string (format "%s" string))))
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
  (let ((buf (carriage-log-buffer)))
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

(defun carriage-show-log ()
  "Display the Carriage log buffer."
  (interactive)
  (pop-to-buffer (carriage-log-buffer)))

(provide 'carriage-logging)
;;; carriage-logging.el ends here
