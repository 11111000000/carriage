;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

;; Engines: try registry first in a fail-safe way; if unavailable (packaging omits engines/*),
;; install a minimal fallback that dispatches to sync git helpers asynchronously.
(eval-and-compile
  (ignore-errors (require 'carriage-apply-engine))
  (unless (featurep 'carriage-apply-engine)
    (defvar carriage-apply-engine 'git
      "Active apply engine symbol. Fallback default is 'git when engines/* are not packaged.")
    (defun carriage-apply-engine ()
      "Return the active apply engine symbol (fallback shim)."
      'git)
    (defun carriage-apply-engine-dispatch (kind op plan-item repo-root on-done on-fail)
      "Fallback shim: emulate async engine by calling git helpers and scheduling callbacks.
This avoids hard dependency on engines/ during packaging."
      (let* ((start (float-time)))
        (pcase kind
          ((or 'dry-run :dry-run)
           (pcase op
             ('patch
              (run-at-time 0 nil
                           (lambda ()
                             (condition-case e
                                 (let* ((strip (or (alist-get :strip plan-item) 1))
                                        (diff  (or (alist-get :diff plan-item) ""))
                                        (res   (carriage-git-apply-check repo-root diff :strip strip)))
                                   (funcall on-done (append res (list :engine 'git))))
                               (error
                                (when (functionp on-fail)
                                  (funcall on-fail (list :exit 128 :stderr (error-message-string e)))))))))
             (_
              (run-at-time 0 nil
                           (lambda ()
                             (when (functionp on-done)
                               (funcall on-done (list :engine 'git :exit 0 :status 'noop))))))))
          ((or 'apply :apply)
           (pcase op
             ('patch
              (run-at-time 0 nil
                           (lambda ()
                             (condition-case e
                                 (let* ((strip (or (alist-get :strip plan-item) 1))
                                        (diff  (or (alist-get :diff plan-item) ""))
                                        (use-index (eq (and (boundp 'carriage-apply-stage-policy)
                                                            carriage-apply-stage-policy) 'index))
                                        (res (if use-index
                                                 (carriage-git-apply-index repo-root diff :strip strip)
                                               (carriage-git-apply repo-root diff :strip strip))))
                                   (when (functionp on-done)
                                     (funcall on-done (append res (list :engine 'git)))))
                               (error
                                (when (functionp on-fail)
                                  (funcall on-fail (list :exit 128 :stderr (error-message-string e)))))))))
             ('create
              (if (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index)
                  (let ((file (or (alist-get :file plan-item) "-")))
                    (run-at-time 0 nil
                                 (lambda ()
                                   (condition-case e
                                       (let ((res (carriage-git-add repo-root file)))
                                         (when (functionp on-done)
                                           (funcall on-done (append res (list :engine 'git)))))
                                     (error
                                      (when (functionp on-fail)
                                        (funcall on-fail (list :exit 128 :stderr (error-message-string e)))))))))
                (run-at-time 0 nil
                             (lambda ()
                               (when (functionp on-done)
                                 (funcall on-done (list :engine 'git :exit 0 :status 'noop)))))))
             ('delete
              (if (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index)
                  (let ((file (or (alist-get :file plan-item) "-")))
                    (run-at-time 0 nil
                                 (lambda ()
                                   (condition-case e
                                       (let ((res (carriage-git-rm repo-root file)))
                                         (when (functionp on-done)
                                           (funcall on-done (append res (list :engine 'git)))))
                                     (error
                                      (when (functionp on-fail)
                                        (funcall on-fail (list :exit 128 :stderr (error-message-string e)))))))))
                (run-at-time 0 nil
                             (lambda ()
                               (when (functionp on-done)
                                 (funcall on-done (list :engine 'git :exit 0 :status 'noop)))))))
             ('rename
              (if (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index)
                  (let ((from (alist-get :from plan-item))
                        (to   (alist-get :to plan-item)))
                    (run-at-time 0 nil
                                 (lambda ()
                                   (condition-case e
                                       (let ((res (carriage-git-mv repo-root from to)))
                                         (when (functionp on-done)
                                           (funcall on-done (append res (list :engine 'git)))))
                                     (error
                                      (when (functionp on-fail)
                                        (funcall on-fail (list :exit 128 :stderr (error-message-string e)))))))))
                (run-at-time 0 nil
                             (lambda ()
                               (when (functionp on-done)
                                 (funcall on-done (list :engine 'git :exit 0 :status 'noop)))))))
             (_
              (run-at-time 0 nil
                           (lambda ()
                             (when (functionp on-done)
                               (funcall on-done (list :engine 'git :exit 0 :status 'noop))))))))
          (_
           (run-at-time 0 nil
                        (lambda ()
                          (when (functionp on-fail)
                            (funcall on-fail (list :error 'bad-kind)))))))))))

;; Optional: load async git engine when available (no hard fail if missing).
(ignore-errors (require 'carriage-engine-git))

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
  "Apply PLAN under REPO-ROOT as an event-driven async chain.
Returns a TOKEN plist with at least:
  :queue   remaining items
  :current current engine token (for engine-driven ops)
  :abort-fn zero-arg function to abort the current step (kill process if any)

CALLBACK, when non-nil, is invoked on the main thread with the final REPORT."
  ;; Ensure WIP branch if policy enabled (non-blocking precondition; fast commands)
  (when carriage-apply-require-wip-branch
    (carriage-git-ensure-repo repo-root)
    (carriage-git-checkout-wip repo-root))
  (let* ((engine (carriage-apply-engine))
         (queue (carriage--plan-sort plan))
         (state (list :engine engine
                      :repo repo-root
                      :queue queue
                      :items '()
                      :messages '()
                      :ok 0 :fail 0 :skipped 0
                      :current nil
                      :aborting nil
                      :callback callback)))
    (cl-labels
        ((finalize ()
           (let* ((items (nreverse (plist-get state :items)))
                  (ok (plist-get state :ok))
                  (fail (plist-get state :fail))
                  (skipped (plist-get state :skipped))
                  (msgs (nreverse (plist-get state :messages)))
                  (report (list :plan plan
                                :engine engine
                                :summary (list :ok ok :fail fail :skipped skipped)
                                :items items
                                :messages msgs)))
             ;; Clear abort handler for UI if available
             (when (fboundp 'carriage-clear-abort-handler)
               (carriage-clear-abort-handler))
             (let ((cb (plist-get state :callback)))
               (when (functionp cb)
                 (run-at-time 0 nil
                              (lambda ()
                                (condition-case e
                                    (funcall cb report)
                                  (error
                                   (carriage-log "apply-chain: callback error: %s" (error-message-string e))))))))
             report))
         (push-messages (im)
           (when (and im (listp im))
             (dolist (d im)
               (push d (plist-get state :messages)))))
         (accumulate (row plan-item)
           (let* ((row1 (append row (list :_plan plan-item :_root repo-root)))
                  (status (plist-get row1 :status))
                  (im (plist-get row1 :_messages)))
             (push-messages im)
             (push row1 (plist-get state :items))
             (pcase status
               ('ok   (plist-put state :ok (1+ (plist-get state :ok))))
               ('fail (plist-put state :fail (1+ (plist-get state :fail))))
               (_     (plist-put state :skipped (1+ (plist-get state :skipped)))))))
         (engine->row-patch (item res elapsed-ms)
           (let* ((path (or (alist-get :path item) "-"))
                  (exit (plist-get res :exit))
                  (pid  (plist-get res :pid))
                  (stderr (or (plist-get res :stderr) ""))
                  (stdout (or (plist-get res :stdout) "")))
             (if (and (numberp exit) (zerop exit))
                 (list :op 'patch :status 'ok :path path :details "git apply ok"
                       :engine engine
                       :pid pid
                       :elapsed-ms (and (numberp elapsed-ms) (floor (* 1000 elapsed-ms))))
               (list :op 'patch :status 'fail :path path :details "git apply failed"
                     :engine engine
                     :pid pid
                     :elapsed-ms (and (numberp elapsed-ms) (floor (* 1000 elapsed-ms)))
                     :extra (list :exit exit :stderr stderr :stdout stdout)
                     :_messages (list (list :code 'PATCH_E_APPLY
                                            :severity 'error
                                            :file path
                                            :details (or (string-trim stderr) (string-trim stdout) "git apply failed")))))))
         (engine->row-fileop (op item res elapsed-ms)
           (let* ((exit (plist-get res :exit))
                  (pid  (plist-get res :pid))
                  (okp  (and (numberp exit) (zerop exit)))
                  (file (pcase op
                          ('rename (format "%s -> %s"
                                           (or (alist-get :from item) "-")
                                           (or (alist-get :to item) "-")))
                          (_ (or (alist-get :file item)
                                 (alist-get :path item) "-"))))
                  (details (pcase op
                             ('create (if okp "Created (staged)" "git add failed"))
                             ('delete (if okp "Deleted (staged)" "git rm failed"))
                             ('rename (if okp "Renamed (staged)" "git mv failed"))
                             (_       (if okp "ok" "failed")))))
             (append (list :op op :status (if okp 'ok 'fail)
                           :file file
                           :details details
                           :engine engine
                           :pid pid
                           :elapsed-ms (and (numberp elapsed-ms) (floor (* 1000 elapsed-ms))))
                     (unless okp
                       (list :extra (list :exit exit
                                          :stderr (or (plist-get res :stderr) "")
                                          :stdout (or (plist-get res :stdout) "")))))))
         (next ()
           (let* ((q (plist-get state :queue)))
             (cond
              ((or (plist-get state :aborting) (null q))
               (finalize))
              (t
               (let* ((item (car q))
                      (_ (plist-put state :queue (cdr q))))
                 (run-item item))))))
         (run-item (item)
           (let* ((op (alist-get :op item))
                  (start (float-time)))
             (pcase op
               ('patch
                ;; Engine-driven async (returns engine token)
                (let* ((eng-token
                        (carriage-apply-engine-dispatch
                         :apply 'patch item repo-root
                         (lambda (res)
                           (run-at-time 0 nil
                                        (lambda ()
                                          (let* ((elapsed (- (float-time) start))
                                                 (row (engine->row-patch item res elapsed)))
                                            (accumulate row item)
                                            (plist-put state :current nil)
                                            (next)))))
                         (lambda (err)
                           (run-at-time 0 nil
                                        (lambda ()
                                          (let* ((elapsed (- (float-time) start))
                                                 (row (engine->row-patch item (if (listp err) err (list :exit 128 :stderr (format "%s" err))) elapsed)))
                                            (accumulate row item)
                                            (plist-put state :current nil)
                                            ;; Stop on first failure (group semantics)
                                            (finalize))))))))
                  (plist-put state :current eng-token)
                  ;; Register abort handler for UI
                  (when (fboundp 'carriage-register-abort-handler)
                    (carriage-register-abort-handler
                     (lambda ()
                       (plist-put state :aborting t)
                       (let ((tok (plist-get state :current)))
                         (cond
                          ((and (processp (plist-get tok :process)) (process-live-p (plist-get tok :process)))
                           (ignore-errors (interrupt-process (plist-get tok :process)))
                           (ignore-errors (kill-process (plist-get tok :process))))
                          ((functionp (plist-get tok :abort-fn))
                           (condition-case _ (funcall (plist-get tok :abort-fn)) (error nil)))
                          (t
                           (carriage-log "apply-chain: no abort handle in engine token"))))))))))
             ((or 'delete 'rename)
              (if (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index)
                  ;; Engine-driven for index policy
                  (let* ((eng-token
                          (carriage-apply-engine-dispatch
                           :apply op item repo-root
                           (lambda (res)
                             (run-at-time 0 nil
                                          (lambda ()
                                            (let* ((elapsed (- (float-time) start))
                                                   (row (engine->row-fileop op item res elapsed)))
                                              (accumulate row item)
                                              (plist-put state :current nil)
                                              (next)))))
                           (lambda (err)
                             (run-at-time 0 nil
                                          (lambda ()
                                            (let* ((elapsed (- (float-time) start))
                                                   (res (if (listp err) err (list :exit 128 :stderr (format "%s" err))))
                                                   (row (engine->row-fileop op item res elapsed)))
                                              (accumulate row item)
                                              (plist-put state :current nil)
                                              (finalize))))))))
                    (plist-put state :current eng-token)
                    (when (fboundp 'carriage-register-abort-handler)
                      (carriage-register-abort-handler
                       (lambda ()
                         (plist-put state :aborting t)
                         (let ((tok (plist-get state :current)))
                           (cond
                            ((and (processp (plist-get tok :process)) (process-live-p (plist-get tok :process)))
                             (ignore-errors (interrupt-process (plist-get tok :process)))
                             (ignore-errors (kill-process (plist-get tok :process))))
                            ((functionp (plist-get tok :abort-fn))
                             (condition-case _ (funcall (plist-get tok :abort-fn)) (error nil)))
                            (t
                             (carriage-log "apply-chain: no abort handle in engine token")))))))))
              ;; Non-index: run ops-layer (FS) asynchronously
              (run-at-time 0 nil
                           (lambda ()
                             (condition-case e
                                 (let* ((res (carriage--apply-dispatch item repo-root))
                                        (elapsed (- (float-time) start))
                                        (row (append res (list :engine engine
                                                               :elapsed-ms (floor (* 1000 (max 0 elapsed)))))))
                                   (accumulate row item)
                                   (next))
                               (error
                                (let ((row (list :op op :status 'fail :details (error-message-string e) :engine engine)))
                                  (accumulate row item)
                                  (finalize))))))))
           ('create
            ;; First perform FS write via ops; then, if index policy, stage via engine git add.
            (run-at-time 0 nil
                         (lambda ()
                           (condition-case e
                               (let* ((fs-res (carriage--apply-dispatch item repo-root))
                                      (status (plist-get fs-res :status)))
                                 (if (eq status 'fail)
                                     (let* ((elapsed (- (float-time) start))
                                            (row (append fs-res (list :engine engine
                                                                      :elapsed-ms (floor (* 1000 (max 0 elapsed)))))))
                                       (accumulate row item)
                                       (finalize))
                                   (if (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index)
                                       ;; Stage with engine (git add)
                                       (let* ((eng-token
                                               (carriage-apply-engine-dispatch
                                                :apply 'create item repo-root
                                                (lambda (res)
                                                  (run-at-time 0 nil
                                                               (lambda ()
                                                                 (let* ((elapsed (- (float-time) start))
                                                                        (row (engine->row-fileop 'create item res elapsed)))
                                                                   (accumulate row item)
                                                                   (plist-put state :current nil)
                                                                   (next)))))
                                                (lambda (err)
                                                  (run-at-time 0 nil
                                                               (lambda ()
                                                                 (let* ((elapsed (- (float-time) start))
                                                                        (res (if (listp err) err (list :exit 128 :stderr (format "%s" err))))
                                                                        (row (engine->row-fileop 'create item res elapsed)))
                                                                   (accumulate row item)
                                                                   (plist-put state :current nil)
                                                                   (finalize))))))))
                                         (plist-put state :current eng-token)
                                         (when (fboundp 'carriage-register-abort-handler)
                                           (carriage-register-abort-handler
                                            (lambda ()
                                              (plist-put state :aborting t)
                                              (let ((tok (plist-get state :current)))
                                                (cond
                                                 ((and (processp (plist-get tok :process)) (process-live-p (plist-get tok :process)))
                                                  (ignore-errors (interrupt-process (plist-get tok :process)))
                                                  (ignore-errors (kill-process (plist-get tok :process))))
                                                 ((functionp (plist-get tok :abort-fn))
                                                  (condition-case _ (funcall (plist-get tok :abort-fn)) (error nil)))
                                                 (t
                                                  (carriage-log "apply-chain: no abort handle in engine token"))))))))
                                     ;; Policy 'none: just report FS result
                                     (let* ((elapsed (- (float-time) start))
                                            (row (append fs-res (list :engine engine
                                                                      :elapsed-ms (floor (* 1000 (max 0 elapsed)))))))
                                       (accumulate row item)
                                       (next))))))
                           (error
                            (let ((row (list :op 'create :status 'fail :details (error-message-string e) :engine engine)))
                              (accumulate row item)
                              (finalize)))))))
         (_
           ;; Other non-engine ops (e.g., sre): run via ops layer but schedule via timer to keep UI non-blocking
           (run-at-time 0 nil
                        (lambda ()
                          (condition-case e
                              (let* ((res (carriage--apply-dispatch item repo-root))
                                     (elapsed (- (float-time) start))
                                     ;; Augment with engine and elapsed metrics for consistency
                                     (row (append res (list :engine engine
                                                            :elapsed-ms (floor (* 1000 (max 0 elapsed)))))))
                                (accumulate row item)
                                (next))
                            (error
                             (let* ((row (list :op (alist-get :op item)
                                               :status 'fail
                                               :details (error-message-string e)
                                               :engine engine)))
                               (accumulate row item)
                               (finalize)))))))))))
;; Kick off
(run-at-time 0 nil (lambda () (next)))
;; Return token to caller (abort-fn also registered in UI handler)
(plist-put state :abort-fn
           (lambda ()
             (plist-put state :aborting t)
             (let ((tok (plist-get state :current)))
               (when tok
                 (cond
                  ((and (processp (plist-get tok :process)) (process-live-p (plist-get tok :process)))
                   (ignore-errors (interrupt-process (plist-get tok :process)))
                   (ignore-errors (kill-process (plist-get tok :process))))
                  ((functionp (plist-get tok :abort-fn))
                   (condition-case _ (funcall (plist-get tok :abort-fn)) (error nil))))))))
state))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
