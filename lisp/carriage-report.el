;;; carriage-report.el --- Report buffer and faces  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'carriage-utils)
(require 'ediff)

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
   :items ((:op OP :file PATH :status STATUS :details STR :diff PREVIEW :_plan PLAN-ITEM) ...))"
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
      ;; Header with actions column
      (carriage--report-insert-line '("#" "op" "path" "status" "matches" "details" "preview" "actions"))
      (carriage--report-insert-line '("---" "---" "---" "---" "---" "---" "---" "---"))
      (let ((i 0))
        (dolist (it (or (plist-get report :items) '()))
          (setq i (1+ i))
          (let* ((op (plist-get it :op))
                 (file (or (plist-get it :file) (plist-get it :path)))
                 (status (plist-get it :status))
                 (matches (plist-get it :matches))
                 (details (or (plist-get it :details) ""))
                 (preview (or (plist-get it :diff) ""))
                 (has-preview (and (stringp preview) (> (length preview) 0)))
                 (preview-short (if has-preview
                                    (truncate-string-to-width preview 60 nil nil t)
                                  ""))
                 (action (concat (if has-preview "[Diff]" "") " [Ediff]")))
            (let ((row-beg (point)))
              (carriage--report-insert-line
               (list i op file status (or matches "") details preview-short action)
               (pcase status
                 ('ok 'carriage-report-ok-face)
                 ('fail 'carriage-report-err-face)
                 ('warn 'carriage-report-warn-face)
                 (_ nil)))
              (let ((row-end (point)))
                ;; Attach item payload to the whole row for RET activation.
                (add-text-properties row-beg row-end (list 'carriage-report-item it))
                ;; Turn [Diff] into an actual button if present.
                (when has-preview
                  (save-excursion
                    (goto-char row-beg)
                    (when (search-forward "[Diff]" row-end t)
                      (make-text-button (- (point) 6) (point)
                                        'help-echo "Show full diff preview"
                                        'follow-link t
                                        'action (lambda (_btn) (carriage-report-show-diff-at-point))))))
                ;; Add [Ediff] button
                (save-excursion
                  (goto-char row-beg)
                  (when (search-forward "[Ediff]" row-end t)
                    (make-text-button (- (point) 7) (point)
                                      'help-echo "Open Ediff for this item"
                                      'follow-link t
                                      'action (lambda (_btn) (carriage-report-ediff-at-point)))))))))
        ;; Keys
        (let* ((base (or (current-local-map) (make-sparse-keymap)))
               (map (make-sparse-keymap)))
          (set-keymap-parent map base)
          (define-key map (kbd "RET") #'carriage-report-show-diff-at-point)
          (define-key map (kbd "e")   #'carriage-report-ediff-at-point)
          (use-local-map map)))
      (goto-char (point-min))
      (read-only-mode 1))
    buf))

(defun carriage-report-open (&optional report)
  "Open the report buffer and optionally RENDER REPORT alist."
  (interactive)
  (when report
    (carriage-report-render report))
  (pop-to-buffer (carriage-report-buffer)))

(defun carriage-report--item-at-point ()
  "Return report item plist stored at point, or nil."
  (or (get-text-property (point) 'carriage-report-item)
      (get-text-property (line-beginning-position) 'carriage-report-item)))

(defun carriage-report-show-diff-at-point ()
  "Open a buffer with full :diff preview for the report item at point.
If no preview is available, signal a user-visible message."
  (interactive)
  (let* ((it (carriage-report--item-at-point))
         (diff (and it (plist-get it :diff)))
         (path (or (plist-get it :file) (plist-get it :path) "")))
    (if (and (stringp diff) (> (length diff) 0))
        (let* ((buf (get-buffer-create (format "*carriage-diff: %s*" path))))
          (with-current-buffer buf
            (read-only-mode -1)
            (erase-buffer)
            (insert diff)
            (goto-char (point-min))
            (view-mode 1))
          (pop-to-buffer buf))
      (user-error "No diff preview available at point"))))

(defun carriage-report-ediff-at-point ()
  "Open Ediff for the report item at point.
For SRE/SRE-BATCH: build in-memory \"after\" and run ediff-buffers.
For patch: run ediff-patch-file with the unified diff and target file.
In noninteractive (batch) mode, prepare buffers or patch file and return without invoking Ediff."
  (interactive)
  (let* ((it (carriage-report--item-at-point)))
    (unless it
      (user-error "No report item at point"))
    (let* ((plan (plist-get it :_plan))
           (op   (or (plist-get it :op) (and plan (alist-get :op plan))))
           (root (or (plist-get it :_root) (carriage-project-root) default-directory)))
      (pcase op
        ((or 'sre 'sre-batch)
         (let* ((file (or (plist-get it :file) (and plan (alist-get :file plan))))
                (abs  (and file (ignore-errors (carriage-normalize-path root file)))))
           (unless (and abs (file-exists-p abs))
             (user-error "File not found: %s" (or file "<nil>")))
           (let* ((before (carriage-read-file-string abs))
                  (sim    (if plan
                              (carriage-sre-simulate-apply plan root)
                            (list :after before :count 0)))
                  (after  (or (plist-get sim :after) before))
                  (bufA   (get-buffer-create (format "*carriage-ediff A: %s*" file)))
                  (bufB   (get-buffer-create (format "*carriage-ediff B: %s*" file))))
             (with-current-buffer bufA (read-only-mode -1) (erase-buffer) (insert before) (set-buffer-modified-p nil) (read-only-mode 1))
             (with-current-buffer bufB (read-only-mode -1) (erase-buffer) (insert after)  (set-buffer-modified-p nil) (read-only-mode 1))
             (if (bound-and-true-p noninteractive)
                 (message "Prepared SRE Ediff buffers (noninteractive)")
               (ediff-buffers bufA bufB)))))
        ('patch
         (let* ((diff (or (plist-get it :diff) (and plan (alist-get :diff plan))))
                (path (or (plist-get it :path) (and plan (alist-get :path plan))))
                (abs  (and path (ignore-errors (carriage-normalize-path root path)))))
           (unless (and diff abs (file-exists-p abs))
             (user-error "Cannot run Ediff for patch: missing :diff or file"))
           (let ((patch-file (make-temp-file "carriage-ediff-" nil ".diff")))
             (unwind-protect
                 (progn
                   (with-temp-file patch-file (insert diff))
                   (if (bound-and-true-p noninteractive)
                       (message "Prepared patch for Ediff (noninteractive)")
                     (ediff-patch-file patch-file abs)))
               (ignore-errors (delete-file patch-file))))))
        (_
         (user-error "Ediff not supported for op: %S" op))))))

(provide 'carriage-report)
;;; carriage-report.el ends here
