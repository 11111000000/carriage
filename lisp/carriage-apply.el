;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

(defcustom carriage-apply-require-wip-branch t
  "If non-nil, ensure and checkout WIP branch before applying a plan."
  :type 'boolean :group 'carriage)

(defcustom carriage-apply-stage-policy 'none
  "Policy for staging changes during apply:
- 'none  — modify working tree only (default), do not stage.
- 'index — stage changes into index (e.g., git apply --index, git add)."
  :type '(choice (const none) (const index))
  :group 'carriage)

(defcustom carriage-apply-async t
  "When non-nil, run apply-plan asynchronously in a Lisp thread to avoid UI blocking.
If threads are unavailable or in batch mode, falls back to synchronous execution."
  :type 'boolean :group 'carriage)

(defun carriage--report-ok (op &rest kv)
  "Build ok report alist with OP and extra KV plist."
  (append (list :op op :status 'ok) kv))

(defun carriage--report-fail (op &rest kv)
  "Build fail report alist with OP and extra KV plist."
  (append (list :op op :status 'fail) kv))

(defun carriage--plan-get (item key)
  "Get KEY from ITEM supporting both plist and alist representations."
  (if (plist-member item key) (plist-get item key) (alist-get key item)))

(defvar carriage-mode-sre-preview-max 3
  "Default maximum number of SRE preview chunks when Customize is not loaded.")

;;; Plan-level pipeline


(defun carriage--op-rank (op)
  "Return rank for OP to sort plan: delete→rename→create→patch→sre."
  (pcase op
    ('delete 1)
    ('rename 2)
    ('create 3)
    ('patch  4)
    (_       5)))

(defun carriage--plan-sort (plan)
  "Return PLAN items sorted by operation type according to v1 order."
  (seq-sort (lambda (a b)
              (< (carriage--op-rank (alist-get :op a))
                 (carriage--op-rank (alist-get :op b))))
            plan))

(defun carriage--dry-run-dispatch (item repo-root)
  "Dispatch dry-run for a single ITEM with REPO-ROOT via format registry."
  (let* ((op  (alist-get :op item))
         (rec (carriage-format-get op "1"))
         (fn  (and rec (plist-get rec :dry-run))))
    (if (functionp fn)
        (funcall fn item repo-root)
      (carriage--report-fail (or op 'unknown) :details "Unknown op"))))

(defun carriage--apply-dispatch (item repo-root)
  "Dispatch apply for a single ITEM with REPO-ROOT via format registry."
  (let* ((op  (alist-get :op item))
         (rec (carriage-format-get op "1"))
         (fn  (and rec (plist-get rec :apply))))
    (if (functionp fn)
        (funcall fn item repo-root)
      (carriage--report-fail (or op 'unknown) :details "Unknown op"))))

(defun carriage-dry-run-plan (plan repo-root)
  "Dry-run PLAN (list of plan items) under REPO-ROOT.
Return report alist:
  (:plan PLAN
   :summary (:ok N :fail M :skipped K)
   :items ...
   :messages LIST) where :messages aggregates per-item diagnostics."
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0)
         (virt '())  ; virtual created files: (\"path\" . content)
         (msgs '()))
    (dolist (it sorted)
      (let* ((op (alist-get :op it))
             (file (alist-get :file it))
             (res
              (cond
               ;; If SRE/SRE-BATCH targets a file that will be created in this plan, simulate on that content.
               ((and (memq op '(sre sre-batch))
                     file
                     (not (let* ((abs (ignore-errors (carriage-normalize-path repo-root file))))
                            (and abs (file-exists-p abs))))
                     (assoc-string file virt t))
                (if (fboundp 'carriage-sre-dry-run-on-text)
                    (carriage-sre-dry-run-on-text it (cdr (assoc-string file virt t)))
                  (carriage--report-fail op :file file :details "SRE simulation not available")))
               (t
                (carriage--dry-run-dispatch it repo-root)))))
        ;; Stash original plan item and repo root into report item for UI actions (e.g., Ediff).
        (let* ((res (append res (list :_plan it :_root repo-root)))
               (status (plist-get res :status)))
          (push res items)
          ;; Aggregate per-item diagnostics into top-level :messages if present.
          (let ((im (plist-get res :_messages)))
            (when im
              (dolist (d im)
                (push d msgs))))
          (pcase status
            ('ok   (setq ok (1+ ok)))
            ('fail (setq fail (1+ fail)))
            (_     (setq skip (1+ skip)))))
        ;; Update virtual FS for subsequent SRE checks
        (when (and (eq op 'create) file)
          (let ((content (or (alist-get :content it) "")))
            (setq virt (cons (cons file content) (assq-delete-all file virt)))))))
    (list :plan plan
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items)
          :messages (nreverse msgs))))

(defun carriage-apply-plan (plan repo-root)
  "Apply PLAN (list of plan items) under REPO-ROOT sequentially.
Stops on first failure. Returns report alist as in carriage-dry-run-plan."
  ;; Ensure WIP branch if policy enabled
  (when carriage-apply-require-wip-branch
    (carriage-git-ensure-repo repo-root)
    (carriage-git-checkout-wip repo-root))
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0)
         (stop nil)
         (msgs '()))
    (dolist (it sorted)
      (unless stop
        (let* ((res0 (carriage--apply-dispatch it repo-root))
               ;; Store original plan item and root on the row (parity with dry-run report)
               (res (append res0 (list :_plan it :_root repo-root)))
               (status (plist-get res :status))
               (im (plist-get res :_messages)))
          ;; Aggregate per-item diagnostics into top-level :messages
          (when im
            (dolist (d im)
              (push d msgs)))
          (push res items)
          (pcase status
            ('ok   (setq ok (1+ ok)))
            ('fail (setq fail (1+ fail))
                   (setq stop t))
            (_     (setq skip (1+ skip)))))))
    (list :plan plan
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items)
          :messages (nreverse msgs))))

(defun carriage-apply-plan-async (plan repo-root &optional callback)
  "Run (carriage-apply-plan PLAN REPO-ROOT) asynchronously when possible.
If CALLBACK is non-nil, schedule it on the main thread with the report plist.
In batch mode or when threads are unavailable, runs synchronously and returns report."
  (if (and carriage-apply-async
           (fboundp 'make-thread)
           (not (bound-and-true-p noninteractive)))
      (let ((p plan) (root repo-root))
        (carriage-log "async-apply: spawn thread items=%d" (length p))
        (make-thread
         (lambda ()
           (let* ((rep (carriage-apply-plan p root))
                  (sum (plist-get rep :summary)))
             (run-at-time 0 nil
                          (lambda ()
                            (carriage-log "async-apply: done ok=%s fail=%s"
                                          (or (plist-get sum :ok) 0) (or (plist-get sum :fail) 0))
                            (when (functionp callback)
                              (condition-case e
                                  (funcall callback rep)
                                (error
                                 (carriage-log "async-apply: callback error: %s" (error-message-string e))))))))))
        "carriage-apply-thread"))
  ;; Fallback sync
  (carriage-apply-plan plan repo-root))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
