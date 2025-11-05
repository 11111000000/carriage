;;; apply-last-iteration-batch.el --- Batch runner for Carriage apply-all  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -l /home/az/Code/carriage/scripts/apply-last-iteration-batch.el \
;;     --eval '(carriage-batch-apply "/abs/path/to/project" "/abs/path/to/file.org" "/abs/path/to/logdir")'
;;
;; - Project root must be a Git repo (used to resolve paths).
;; - The .org file should contain #+begin_patch blocks; last iteration must be marked;
;;   otherwise error is raised.
;; - Writes logs to LOGDIR/carriage-log.txt and LOGDIR/carriage-traffic.txt.

(defun carriage-batch--write-buffer (buf path)
  (when (and (bufferp buf) (buffer-live-p buf))
    (with-current-buffer buf
      (write-region (point-min) (point-max) path nil 'silent))))

(defun carriage-batch-apply (project-root org-file &optional logdir)
  "Run dry-run+apply for last iteration in batch mode, saving logs and reports to LOGDIR.
Artifacts written (when LOGDIR is provided):
- LOGDIR/carriage-log.txt
- LOGDIR/carriage-traffic.txt
- LOGDIR/carriage-report-dry.org     (always written after dry-run)
- LOGDIR/carriage-report-apply.org   (written on successful apply)"
  (let* ((default-directory (file-name-as-directory (expand-file-name project-root))))
    ;; Load Carriage first, then tweak defcustoms under a dynamic let
    (let* ((lisp-dir (expand-file-name "lisp" default-directory)))
      (add-to-list 'load-path lisp-dir))
    (require 'carriage)
    (require 'carriage-parser)
    (let ((carriage-apply-async nil)
          (carriage-git-branch-policy 'in-place)
          (carriage-mode-confirm-apply-all nil)
          (carriage-mode-show-diffs nil)
          (carriage-mode-auto-open-report nil)
          (carriage-mode-auto-open-log nil)
          (carriage-mode-auto-open-traffic nil))
      (find-file org-file)
      (ignore-errors (org-mode))
      (condition-case err
          (progn
            (ignore-errors (carriage-mode 1))
            (let* ((root default-directory))
              (message "Carriage(batch): apply-all start in %s (file %s)" root org-file)
              ;; Collect plan and run dry-run
              (let* ((plan (carriage-collect-last-iteration-blocks-strict root)))
                (when (or (null plan) (zerop (length plan)))
                  (error "Нет последней итерации (CARRIAGE_ITERATION_ID)"))
                (let* ((dry (carriage-dry-run-plan plan root)))
                  ;; Write dry-run report
                  (when logdir
                    (make-directory logdir t)
                    (with-current-buffer (carriage-report-render dry)
                      (carriage-batch--write-buffer (current-buffer)
                                                    (expand-file-name "carriage-report-dry.org" logdir))))
                  (let* ((sum (plist-get dry :summary))
                         (fails (or (plist-get sum :fail) 0)))
                    (when (> fails 0)
                      (message "Carriage(batch): ERROR: Dry-run провалился для части блоков; смотрите отчёт")
                      ;; Write logs and exit non-zero
                      (let ((log-buf (get-buffer "*carriage-log*"))
                            (traf-buf (get-buffer "*carriage-traffic*")))
                        (when logdir
                          (carriage-batch--write-buffer log-buf (expand-file-name "carriage-log.txt" logdir))
                          (carriage-batch--write-buffer traf-buf (expand-file-name "carriage-traffic.txt" logdir))))
                      (kill-emacs 1))
                    ;; Apply synchronously
                    (let* ((ap (carriage-apply-plan plan root)))
                      (when logdir
                        (with-current-buffer (carriage-report-render ap)
                          (carriage-batch--write-buffer (current-buffer)
                                                        (expand-file-name "carriage-report-apply.org" logdir)))))
                    (message "Carriage(batch): apply-all done OK"))))))
        (error
         (message "Carriage(batch): ERROR: %s" (error-message-string err))
         (let ((log-buf (get-buffer "*carriage-log*"))
               (traf-buf (get-buffer "*carriage-traffic*")))
           (when logdir
             (make-directory logdir t)
             (carriage-batch--write-buffer log-buf (expand-file-name "carriage-log.txt" logdir))
             (carriage-batch--write-buffer traf-buf (expand-file-name "carriage-traffic.txt" logdir))))
         (kill-emacs 1)))
      ;; On success, write logs as well
      (let ((log-buf (get-buffer "*carriage-log*"))
            (traf-buf (get-buffer "*carriage-traffic*")))
        (when logdir
          (make-directory logdir t)
          (carriage-batch--write-buffer log-buf (expand-file-name "carriage-log.txt" logdir))
          (carriage-batch--write-buffer traf-buf (expand-file-name "carriage-traffic.txt" logdir))))))
  (kill-emacs 0))


(provide 'apply-last-iteration-batch)
