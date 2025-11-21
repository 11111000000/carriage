;;; carriage-traffic-batch.el --- Traffic / batch transport helpers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: transport, traffic
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/logging-v2.org
;;   spec/llm-transport-v2.org
;;
;;; Commentary:
;; Helpers used by transports for batching and traffic recording.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-traffic-batch nil
  "Batcher for Carriage traffic logging to reduce frequent redisplay."
  :group 'carriage-traffic)

(defcustom carriage-traffic-batch-enabled t
  "When non-nil, batch carriage-traffic-log and carriage-traffic-log-local calls.
Entries are enqueued and flushed on a short timer, reducing redisplay frequency."
  :type 'boolean :group 'carriage-traffic-batch)

(defcustom carriage-traffic-batch-interval 0.1
  "Interval (seconds) between batched flushes of traffic logs."
  :type 'number :group 'carriage-traffic-batch)

(defcustom carriage-traffic-batch-bytes-threshold 4096
  "Flush the queue early when the total size of enqueued payloads reaches this threshold (bytes).
Set to nil to disable early flush based on size."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'carriage-traffic-batch)

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
  (let* ((bytes (condition-case nil
                    (cond
                     ;; carriage-traffic-log signature: (dir fmt &rest args)
                     ((eq kind 'global)
                      (let ((fmt (nth 1 args))
                            (rest (nthcdr 2 args)))
                        (string-bytes (apply #'format (or fmt "%s") rest))))
                     ;; carriage-traffic-log-local signature: (origin type fmt &rest args)
                     ((eq kind 'local)
                      (let ((fmt (nth 2 args))
                            (rest (nthcdr 3 args)))
                        (string-bytes (apply #'format (or fmt "%s") rest))))
                     (t 0))
                  (error 0))))
    (push (list :kind kind :fn orig-fn :args args :bytes bytes)
          carriage-traffic-batch--queue)
    (setq carriage-traffic-batch--queued-bytes (+ carriage-traffic-batch--queued-bytes (or bytes 0)))
    (when (and carriage-traffic-batch-bytes-threshold
               (numberp carriage-traffic-batch-bytes-threshold)
               (>= carriage-traffic-batch--queued-bytes carriage-traffic-batch-bytes-threshold))
      (carriage-traffic-batch--flush))
    (carriage-traffic-batch--schedule)))

(defun carriage-traffic-batch--flush ()
  "Flush pending traffic log entries."
  (when (and carriage-traffic-batch-enabled (not carriage-traffic-batch--flushing))
    (let ((carriage-traffic-batch--flushing t)
          (q (nreverse carriage-traffic-batch--queue)))
      (setq carriage-traffic-batch--queue nil)
      (setq carriage-traffic-batch--queued-bytes 0)
      (setq carriage-traffic-batch--timer nil)
      (cl-incf carriage-traffic-batch--flush-count)
      ;; Call original functions captured at enqueue-time to avoid recursion.
      (dolist (it q)
        (condition-case _e
            (apply (plist-get it :fn) (plist-get it :args))
          (error nil))))))

(defun carriage-traffic-batch--around-global (orig-fn dir fmt &rest args)
  "Around advice for `carriage-traffic-log'."
  (if carriage-traffic-batch-enabled
      (apply #'carriage-traffic-batch--enqueue 'global orig-fn dir fmt args)
    (apply orig-fn dir fmt args)))

(defun carriage-traffic-batch--around-local (orig-fn origin type fmt &rest args)
  "Around advice for `carriage-traffic-log-local'."
  (if carriage-traffic-batch-enabled
      (apply #'carriage-traffic-batch--enqueue 'local orig-fn origin type fmt args)
    (apply orig-fn origin type fmt args)))

(with-eval-after-load 'carriage-logging
  (advice-add 'carriage-traffic-log :around #'carriage-traffic-batch--around-global)
  (advice-add 'carriage-traffic-log-local :around #'carriage-traffic-batch--around-local))

(provide 'carriage-traffic-batch)
;;; carriage-traffic-batch.el ends here
