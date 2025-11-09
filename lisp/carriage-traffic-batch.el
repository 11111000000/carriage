;;; carriage-traffic-batch.el --- Batch traffic logging to reduce redisplay churn -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-traffic-batch nil
  "Batcher for Carriage traffic logging to reduce frequent redisplay."
  :group 'carriage-traffic)

(defcustom carriage-traffic-batch-enabled t
  "When non-nil, batch carriage-traffic-log and carriage-traffic-log-local calls.
Entries are enqueued and flushed on a short timer, reducing redisplay frequency."
  :type 'boolean :group 'carriage-traffic-batch)

(defcustom carriage-traffic-batch-interval 0.05
  "Interval (seconds) between batched flushes of traffic logs."
  :type 'number :group 'carriage-traffic-batch)

(defcustom carriage-traffic-batch-bytes-threshold 4096
  "Flush the queue early when the total size of enqueued payloads reaches this threshold (bytes).
Set to nil to disable early flush based on size."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'carriage-traffic-batch)

;; Internal queue and timer
(defvar carriage-traffic-batch--queue nil
  "Queue of pending traffic log entries.
Each entry is a plist: (:kind 'global|'local :fn ORIG :args LIST :bytes N).")

(defvar carriage-traffic-batch--timer nil
  "Timer used for scheduled flushes of traffic logs.")

(defvar carriage-traffic-batch--queued-bytes 0
  "Approximate total bytes accumulated in the traffic queue (for threshold flush).")

(defvar carriage-traffic-batch--flushing nil
  "Reentrancy guard for the flush routine.")

(defvar carriage-traffic-batch--flush-count 0
  "Number of flushes performed (diagnostic; used in tests).")

(defun carriage-traffic-batch--schedule ()
  "Ensure a flush is scheduled according to `carriage-traffic-batch-interval'."
  (when (and carriage-traffic-batch-enabled
             (null carriage-traffic-batch--timer))
    (let ((interval (max 0.01 (or carriage-traffic-batch-interval 0.05))))
      (setq carriage-traffic-batch--timer
            (run-at-time interval nil #'carriage-traffic-batch--flush)))))

(defun carriage-traffic-batch--enqueue (kind orig-fn &rest args)
  "Enqueue a logging entry of KIND ('global or 'local) with ORIG-FN and ARGS."
  (let* ((bytes (or (ignore-errors
                      (let ((fmt (and args (cadr args)))
                            (rest (cddr args)))
                        (cond
                         ;; For carriage-traffic-log: (dir fmt &rest)
                         ((eq kind 'global)
                          (string-bytes (apply #'format (or fmt "%s") rest)))
                         ;; For carriage-traffic-log-local: (origin type fmt &rest)
                         ((eq kind 'local)
                          (let ((fmt2 (nth 2 args))
                                (rest2 (nthcdr 3 args)))
                            (string-bytes (apply #'format (or fmt2 "%s") rest2))))
                         (t 0))))
                    0)))
    (push (list :kind kind :fn orig-fn :args args :bytes bytes)
          carriage-traffic-batch--queue)
    (cl-incf carriage-traffic-batch--queued-bytes bytes)
    (when (and carriage-traffic-batch-bytes-threshold
               (numberp carriage-traffic-batch-bytes-threshold)
               (>= carriage-traffic-batch--queued-bytes carriage-traffic-batch-bytes-threshold))
      ;; Flush early on size threshold
      (carriage-traffic-batch--flush))
    (carriage-traffic-batch--schedule)))

(defun carriage-traffic-batch--flush ()
  "Flush pending traffic log entries."
  (when (and carriage-traffic-batch-enabled (not carriage-traffic-batch--flushing))
    (let ((carriage-traffic-batch--flushing t)
          (q (nreverse carriage-traffic-batch--queue)))
      (setq carriage-traffic-batch--queue nil)
      (setq carriage-traffic-batch--queued-bytes 0)
      (when (timerp carriage-traffic-batch--timer)
        (cancel-timer carriage-traffic-batch--timer))
      (setq carriage-traffic-batch--timer nil)
      (cl-incf carriage-traffic-batch--flush-count)
      (dolist (it q)
        (let ((fn (plist-get it :fn))
              (args (plist-get it :args)))
          (condition-case _e
              (apply fn args)
            (error nil)))))))

;; Advices

(defun carriage-traffic-batch--around-global (orig dir fmt &rest args)
  "Around-advice for `carriage-traffic-log' to batch calls."
  (if (not carriage-traffic-batch-enabled)
      (apply orig dir fmt args)
    (carriage-traffic-batch--enqueue 'global orig dir fmt args)
    nil))

(defun carriage-traffic-batch--around-local (orig origin-buffer type fmt &rest args)
  "Around-advice for `carriage-traffic-log-local' to batch calls."
  (if (not carriage-traffic-batch-enabled)
      (apply orig origin-buffer type fmt args)
    (carriage-traffic-batch--enqueue 'local orig origin-buffer type fmt args)
    nil))

(defun carriage-traffic-batch--install ()
  "Install advices for traffic batching (idempotent)."
  (ignore-errors
    (when (fboundp 'carriage-traffic-log)
      (advice-add 'carriage-traffic-log :around #'carriage-traffic-batch--around-global)))
  (ignore-errors
    (when (fboundp 'carriage-traffic-log-local)
      (advice-add 'carriage-traffic-log-local :around #'carriage-traffic-batch--around-local))))

(defun carriage-traffic-batch--remove ()
  "Remove advices for traffic batching."
  (ignore-errors
    (advice-remove 'carriage-traffic-log #'carriage-traffic-batch--around-global))
  (ignore-errors
    (advice-remove 'carriage-traffic-log-local #'carriage-traffic-batch--around-local)))

(defun carriage-traffic-batch-refresh ()
  "Refresh batching: install or remove advices based on current setting."
  (if carriage-traffic-batch-enabled
      (carriage-traffic-batch--install)
    (carriage-traffic-batch--remove)))

;; Activate on load according to current setting
(carriage-traffic-batch-refresh)

;; Load performance helpers (safe if feature not found).
(require 'carriage-perf nil t)

(provide 'carriage-traffic-batch)
;;; carriage-traffic-batch.el ends here
