;;; carriage-report.el --- Report buffer and faces  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst carriage--report-buffer-name "*carriage-report*"
  "Name of the Carriage report buffer.")

(defun carriage-report-buffer ()
  "Return the report buffer, creating it if necessary."
  (get-buffer-create carriage--report-buffer-name))

(defface carriage-report-ok-face
  '((t :inherit success))
  "Face for OK items in report."
  :group 'carriage)

(defface carriage-report-warn-face
  '((t :inherit warning))
  "Face for warning items in report."
  :group 'carriage)

(defface carriage-report-err-face
  '((t :inherit error))
  "Face for error items in report."
  :group 'carriage)

(defun carriage--report-insert-line (cols &optional face)
  "Insert a tabular LINE from COLS list. Apply FACE to the whole line."
  (let* ((line (mapconcat (lambda (c) (or (and (stringp c) c)
                                     (format "%s" c)))
                          cols
                          " | ")))
    (insert line "\n")
    (when face
      (add-text-properties (- (point) (1+ (length line))) (point) `(face ,face)))))

(defun carriage-report-render (report)
  "Render REPORT alist into the report buffer.
REPORT shape:
  (:plan PLAN
   :summary (:ok N :fail M :skipped K)
   :items ((:op OP :file PATH :status STATUS :details STR) ...))"
  (let ((buf (carriage-report-buffer)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (let* ((summary (plist-get report :summary))
             (ok (plist-get summary :ok))
             (fail (plist-get summary :fail))
             (skipped (plist-get summary :skipped)))
        (insert (format "Carriage report  ok:%s  fail:%s  skipped:%s\n\n"
                        (or ok 0) (or fail 0) (or skipped 0))))
      (carriage--report-insert-line '("#" "op" "path" "status" "details"))
      (carriage--report-insert-line '("---" "---" "---" "---" "---"))
      (let ((i 0))
        (dolist (it (or (plist-get report :items) '()))
          (setq i (1+ i))
          (let* ((op (plist-get it :op))
                 (file (or (plist-get it :file) (plist-get it :path)))
                 (status (plist-get it :status))
                 (details (or (plist-get it :details) "")))
            (carriage--report-insert-line
             (list i op file status details)
             (pcase status
               ('ok 'carriage-report-ok-face)
               ('fail 'carriage-report-err-face)
               ('warn 'carriage-report-warn-face)
               (_ nil))))))
      (goto-char (point-min))
      (read-only-mode 1))
    buf))

(defun carriage-report-open (&optional report)
  "Open the report buffer and optionally RENDER REPORT alist."
  (interactive)
  (when report
    (carriage-report-render report))
  (pop-to-buffer (carriage-report-buffer)))

(provide 'carriage-report)
;;; carriage-report.el ends here
