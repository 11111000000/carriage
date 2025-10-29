;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

;; Register abort handler provided by async apply pipeline (declared in carriage-mode).
(declare-function carriage-register-abort-handler "carriage-mode" (fn))

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
          :engine (carriage-apply-engine)
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
          :engine (carriage-apply-engine)
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items)
          :messages (nreverse msgs))))

(defun carriage--make-apply-state (queue repo-root)
  "Create initial async apply STATE plist."
  (list :queue queue
        :root repo-root
        :ok 0 :fail 0 :skipped 0
        :items '()
        :messages '()
        :current nil
        :aborted nil
        :fs-timer nil))

(defun carriage--apply-summary (state)
  "Build summary plist from STATE."
  (list :ok (plist-get state :ok)
        :fail (plist-get state :fail)
        :skipped (plist-get state :skipped)))

(defun carriage--apply-finish (plan state callback)
  "Finish async apply: build REPORT from PLAN and STATE, invoke CALLBACK if any."
  (let ((report (list :plan plan
                      :engine (carriage-apply-engine)
                      :summary (carriage--apply-summary state)
                      :items (nreverse (plist-get state :items))
                      :messages (nreverse (plist-get state :messages)))))
    (when (functionp callback)
      (run-at-time 0 nil (lambda () (funcall callback report))))
    report))

(defun carriage--apply-acc-row (state row)
  "Accumulate ROW in STATE."
  (plist-put state :items (cons row (plist-get state :items))))

(defun carriage--apply-acc-msg (state msg)
  "Accumulate diagnostic MSG in STATE."
  (plist-put state :messages (cons msg (plist-get state :messages))))

(defun carriage--apply-bump (state status)
  "Bump counters in STATE per STATUS."
  (pcase status
    ('ok   (plist-put state :ok (1+ (plist-get state :ok))))
    ('fail (plist-put state :fail (1+ (plist-get state :fail))))
    (_     (plist-put state :skipped (1+ (plist-get state :skipped))))))

(defun carriage--apply-update-abort (state token)
  "Update TOKEN :abort-fn to cancel current async operation based on STATE.
Registers the handler with the mode when available."
  (let* ((cur (plist-get state :current)))
    (setf (plist-get token :abort-fn)
          (lambda ()
            (plist-put state :aborted t)
            ;; Cancel pending FS timer if any
            (let ((tm (plist-get state :fs-timer)))
              (when (timerp tm)
                (ignore-errors (cancel-timer tm))
                (plist-put state :fs-timer nil)))
            ;; Engine-specific abort (e.g., git)
            (cond
             ((and (plist-get cur :engine)
                   (eq (plist-get cur :engine) 'git)
                   (fboundp 'carriage-engine-git-abort))
              (ignore-errors (carriage-engine-git-abort cur))
              t)
             (t t)))))
  (when (fboundp 'carriage-register-abort-handler)
    (carriage-register-abort-handler (plist-get token :abort-fn))))

(defun carriage--engine-row (op res t0 ok-details fail-details &optional path-key)
  "Normalize engine RES into a report row for OP started at T0."
  (let* ((exit (plist-get res :exit))
         (pid  (plist-get res :pid))
         (pth  (or (plist-get res (or path-key :path))
                   (alist-get :file res) "-"))
         (stderr (string-trim (or (plist-get res :stderr) "")))
         (elapsed (truncate (* 1000 (max 0.0 (- (float-time) t0))))))
    (if (and (numberp exit) (zerop exit))
        (list :op op :status 'ok
              (if (eq op 'patch) :path :file) pth
              :details ok-details
              :pid pid :elapsed-ms elapsed :engine 'git)
      (list :op op :status 'fail
            (if (eq op 'patch) :path :file) pth
            :details (if (string-empty-p stderr) fail-details stderr)
            :pid pid :elapsed-ms elapsed :engine 'git))))

(defun carriage--apply-done-patch (state t0 res plan repo-root callback token)
  "Handle completion of a patch step."
  (let ((row (carriage--engine-row 'patch res t0 "Applied" "git apply failed" :path)))
    (carriage--apply-acc-row state row)
    (carriage--apply-bump state (plist-get row :status))
    (if (eq (plist-get row :status) 'ok)
        (carriage--apply-next state plan repo-root callback token)
      (carriage--apply-finish plan state callback))))

(defun carriage--apply-run-engine (state kind op item repo-root on-ok on-fail token)
  "Dispatch KIND/OP ITEM via engine and update STATE/TOKEN."
  (let* ((t0 (float-time))
         (eng-token (carriage-apply-engine-dispatch
                     kind op item repo-root
                     (lambda (res) (funcall on-ok t0 res))
                     (lambda (res) (funcall on-fail t0 res)))))
    (plist-put state :current eng-token)
    (carriage--apply-update-abort state token)))

(defun carriage--apply-fs-async (state thunk token plan)
  "Run THUNK asynchronously for filesystem ops; on error, mark STATE aborted and finish."
  (let ((tm (run-at-time
             0 nil
             (lambda ()
               (condition-case e
                   (funcall thunk)
                 (error
                  (carriage--apply-acc-msg
                   state (list :code 'MODE_E_DISPATCH :severity 'error
                               :details (error-message-string e)))
                  (plist-put state :aborted t)))))))
    (plist-put state :fs-timer tm)
    (carriage--apply-update-abort state token)))

(defun carriage--apply-run-item (state item repo-root plan callback token)
  "Run one ITEM according to its :op, updating STATE and continuing or finishing."
  (let ((op (alist-get :op item)))
    (pcase op
      ('patch
       (carriage--apply-run-engine
        state :apply 'patch item repo-root
        (lambda (t0 res) (carriage--apply-done-patch state t0 res plan repo-root callback token))
        (lambda (t0 res) (carriage--apply-done-patch state t0 res plan repo-root callback token))
        token))
      ('delete
       (if (eq carriage-apply-stage-policy 'index)
           (carriage--apply-run-engine
            state :apply 'delete item repo-root
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'delete res t0 "Deleted (staged)" "git rm failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (if (eq (plist-get row :status) 'ok)
                    (carriage--apply-next state plan repo-root callback token)
                  (carriage--apply-finish plan state callback))))
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'delete res t0 "Deleted (staged)" "git rm failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (carriage--apply-finish plan state callback)))
            token)
         (carriage--apply-fs-async
          state
          (lambda ()
            (let ((row (carriage-apply-delete item repo-root)))
              (carriage--apply-acc-row state row)
              (carriage--apply-bump state (plist-get row :status))
              (carriage--apply-next state plan repo-root callback token)))
          token plan)))
      ('rename
       (if (eq carriage-apply-stage-policy 'index)
           (carriage--apply-run-engine
            state :apply 'rename item repo-root
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'rename res t0 "Renamed (staged)" "git mv failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (if (eq (plist-get row :status) 'ok)
                    (carriage--apply-next state plan repo-root callback token)
                  (carriage--apply-finish plan state callback))))
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'rename res t0 "Renamed (staged)" "git mv failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (carriage--apply-finish plan state callback)))
            token)
         (carriage--apply-fs-async
          state
          (lambda ()
            (let ((row (carriage-apply-rename item repo-root)))
              (carriage--apply-acc-row state row)
              (carriage--apply-bump state (plist-get row :status))
              (carriage--apply-next state plan repo-root callback token)))
          token plan)))
      ('create
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row0 (carriage-apply-create item repo-root)))
            (if (eq carriage-apply-stage-policy 'index)
                (let* ((file (alist-get :file item)))
                  (carriage--apply-run-engine
                   state :apply 'create (list (cons :file file)) repo-root
                   (lambda (t0 res)
                     (let ((r (carriage--engine-row 'create res t0 "Created (staged)" "git add failed" :path)))
                       (carriage--apply-acc-row state (plist-put (copy-sequence r) :file file))
                       (carriage--apply-bump state (plist-get r :status))
                       (if (eq (plist-get r :status) 'ok)
                           (carriage--apply-next state plan repo-root callback token)
                         (carriage--apply-finish plan state callback))))
                   (lambda (t0 res)
                     (let ((r (carriage--engine-row 'create res t0 "Created (staged)" "git add failed" :path)))
                       (carriage--apply-acc-row state (plist-put (copy-sequence r) :file file))
                       (carriage--apply-bump state (plist-get r :status))
                       (carriage--apply-finish plan state callback)))
                   token))
              (carriage--apply-acc-row state row0)
              (carriage--apply-bump state (plist-get row0 :status))
              (carriage--apply-next state plan repo-root callback token))))
        token plan))
      ((or 'sre 'sre-batch)
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row (carriage-apply-sre item repo-root)))
            (carriage--apply-acc-row state row)
            (carriage--apply-bump state (plist-get row :status))
            (carriage--apply-next state plan repo-root callback token)))
        token plan))
      (_
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row (carriage--report-fail (or op 'unknown) :details "Unknown op")))
            (carriage--apply-acc-row state row)
            (carriage--apply-bump state 'fail)))
        token plan)
       (carriage--apply-finish plan state callback)))))

(defun carriage--apply-next (state plan repo-root callback token)
  "Advance to the next item or finish."
  (if (plist-get state :aborted)
      (carriage--apply-finish plan state callback)
    (let ((q (plist-get state :queue)))
      (if (null q)
          (carriage--apply-finish plan state callback)
        (let ((item (car q)))
          (plist-put state :queue (cdr q))
          (carriage--apply-run-item state item repo-root plan callback token))))))

(defun carriage-apply-plan-async (plan repo-root &optional callback)
  "Apply PLAN under REPO-ROOT asynchronously (event-driven FSM).
Returns a TOKEN plist with :abort-fn that cancels the current step (engine kill) or pending timers.
CALLBACK, when non-nil, is invoked with the final REPORT on the main thread."
  ;; Ensure WIP branch if policy enabled (fast ops, non-blocking)
  (when carriage-apply-require-wip-branch
    (carriage-git-ensure-repo repo-root)
    (carriage-git-checkout-wip repo-root))
  (let* ((queue (carriage--plan-sort plan))
         (state (carriage--make-apply-state queue repo-root))
         (token (list :abort-fn nil)))
    ;; Kick off the chain
    (run-at-time 0 nil (lambda () (carriage--apply-next state plan repo-root callback token)))
    ;; Ensure Abort is registered initially
    (carriage--apply-update-abort state token)
    token))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
