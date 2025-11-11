;;; carriage-performance.el --- Performance optimizations for Carriage -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools to reduce redisplay pressure and interpreter overhead in Carriage.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function byte-compile "bytecomp" (function))

(defgroup carriage-performance nil
  "Performance tunables for the Carriage assistant."
  :group 'carriage
  :prefix "carriage-performance-")

(defcustom carriage-performance-mode-line-min-interval 0.25
  "Minimum time in seconds between forced mode-line refreshes."
  :type 'number
  :group 'carriage-performance)

(defcustom carriage-performance-mode-line-idle-delay 0.2
  "Idle delay in seconds before a deferred mode-line update is flushed."
  :type 'number
  :group 'carriage-performance)

(defcustom carriage-performance-llm-cache-size 64
  "Maximum number of cached Carriage LLM results per cache."
  :type 'integer
  :group 'carriage-performance)

(defvar carriage-performance--last-mode-line-update 0.0)
(defvar carriage-performance--pending-mode-line-update nil)
(defvar carriage-performance--mode-line-timer nil)

(defvar carriage-performance--modeline-last-key nil)
(defvar carriage-performance--modeline-last-value nil)

(defconst carriage-performance--cache-miss (make-symbol "carriage-performance--cache-miss"))

(defvar carriage-performance--llm-candidates-cache (make-hash-table :test #'equal))
(defvar carriage-performance--llm-candidates-order nil)

(defvar carriage-performance--llm-default-cache (make-hash-table :test #'equal))
(defvar carriage-performance--llm-default-order nil)

(defun carriage-performance--autoload-p (fn)
  (and (consp fn) (eq (car fn) 'autoload)))

(defun carriage-performance--cache-last (lst)
  (car (last lst)))

(defun carriage-performance--cache-get (table order-sym key)
  (let ((value (gethash key table carriage-performance--cache-miss)))
    (unless (eq value carriage-performance--cache-miss)
      (set order-sym (cons key (delq key (symbol-value order-sym)))))
    (unless (eq value carriage-performance--cache-miss)
      value)))

(defun carriage-performance--cache-put (table order-sym max-size key value)
  (puthash key value table)
  (set order-sym (cons key (delq key (symbol-value order-sym))))
  (while (> (hash-table-count table) max-size)
    (let* ((order (symbol-value order-sym))
           (victim (carriage-performance--cache-last order)))
      (setq order (delq victim order))
      (remhash victim table)
      (set order-sym order)))
  value)

(defun carriage-performance--modeline-cache-key (args)
  (list args
        (buffer-name (current-buffer))
        (buffer-modified-tick)
        (point)
        (when (boundp 'carriage-session-id)
          (symbol-value 'carriage-session-id))
        (when (boundp 'carriage-active-agent)
          (symbol-value 'carriage-active-agent))))

(defun carriage-performance--memoize-modeline (orig-fn &rest args)
  (let ((key (carriage-performance--modeline-cache-key args)))
    (if (equal key carriage-performance--modeline-last-key)
        carriage-performance--modeline-last-value
      (let ((result (apply orig-fn args)))
        (setq carriage-performance--modeline-last-key key
              carriage-performance--modeline-last-value result)
        result))))

(defun carriage-performance--llm-cache-key (fn args)
  (list fn
        (buffer-name (current-buffer))
        (buffer-modified-tick)
        (point)
        args
        (when (boundp 'carriage-session-id)
          (symbol-value 'carriage-session-id))
        (when (boundp 'carriage-active-agent)
          (symbol-value 'carriage-active-agent))))

(defun carriage-performance--memoize-llm-candidates (orig-fn &rest args)
  (let* ((key (carriage-performance--llm-cache-key 'carriage-llm-candidates args))
         (cached (carriage-performance--cache-get carriage-performance--llm-candidates-cache
                                                  'carriage-performance--llm-candidates-order
                                                  key)))
    (if cached
        cached
      (let ((result (apply orig-fn args)))
        (carriage-performance--cache-put carriage-performance--llm-candidates-cache
                                         'carriage-performance--llm-candidates-order
                                         carriage-performance-llm-cache-size
                                         key result)))))

(defun carriage-performance--memoize-llm-default (orig-fn &rest args)
  (let* ((key (carriage-performance--llm-cache-key 'carriage-llm-default-candidate args))
         (cached (carriage-performance--cache-get carriage-performance--llm-default-cache
                                                  'carriage-performance--llm-default-order
                                                  key)))
    (if cached
        cached
      (let ((result (apply orig-fn args)))
        (carriage-performance--cache-put carriage-performance--llm-default-cache
                                         'carriage-performance--llm-default-order
                                         carriage-performance-llm-cache-size
                                         key result)))))

(defun carriage-performance--reset-modeline-cache ()
  (setq carriage-performance--modeline-last-key nil
        carriage-performance--modeline-last-value nil))

(defun carriage-performance--reset-llm-caches ()
  (clrhash carriage-performance--llm-candidates-cache)
  (clrhash carriage-performance--llm-default-cache)
  (setq carriage-performance--llm-candidates-order nil
        carriage-performance--llm-default-order nil))

(defun carriage-performance--flush-mode-line-update ()
  (let ((args carriage-performance--pending-mode-line-update))
    (setq carriage-performance--mode-line-timer nil
          carriage-performance--pending-mode-line-update nil
          carriage-performance--last-mode-line-update (float-time))
    (when args
      (apply #'force-mode-line-update args))))

(defun carriage-performance--throttle-force-mode-line-update (orig-fn &rest args)
  (let* ((now (float-time))
         (elapsed (- now carriage-performance--last-mode-line-update)))
    (if (>= elapsed carriage-performance-mode-line-min-interval)
        (prog1 (apply orig-fn args)
          (setq carriage-performance--last-mode-line-update now))
      (setq carriage-performance--pending-mode-line-update args)
      (unless (timerp carriage-performance--mode-line-timer)
        (setq carriage-performance--mode-line-timer
              (run-with-idle-timer carriage-performance-mode-line-idle-delay nil
                                   #'carriage-performance--flush-mode-line-update)))
      nil)))

(defun carriage-performance--cancel-timers ()
  (when (timerp carriage-performance--mode-line-timer)
    (cancel-timer carriage-performance--mode-line-timer))
  (setq carriage-performance--mode-line-timer nil
        carriage-performance--pending-mode-line-update nil))

(defun carriage-performance--maybe-byte-compile (sym)
  (when (fboundp sym)
    (let ((fn (symbol-function sym)))
      (unless (or (subrp fn)
                  (byte-code-function-p fn)
                  (carriage-performance--autoload-p fn))
        (setf (symbol-function sym) (byte-compile fn))))))

(defun carriage-performance--maybe-attach-carriage-ui ()
  (when (fboundp 'carriage-ui--modeline)
    (unless (advice-member-p #'carriage-performance--memoize-modeline 'carriage-ui--modeline)
      (advice-add 'carriage-ui--modeline :around #'carriage-performance--memoize-modeline))
    (carriage-performance--maybe-byte-compile 'carriage-ui--modeline)))

(defun carriage-performance--maybe-detach-carriage-ui ()
  (when (and (fboundp 'carriage-ui--modeline)
             (advice-member-p #'carriage-performance--memoize-modeline 'carriage-ui--modeline))
    (advice-remove 'carriage-ui--modeline #'carriage-performance--memoize-modeline)))

(defun carriage-performance--maybe-attach-carriage-llm ()
  (when (fboundp 'carriage-llm-candidates)
    (unless (advice-member-p #'carriage-performance--memoize-llm-candidates 'carriage-llm-candidates)
      (advice-add 'carriage-llm-candidates :around #'carriage-performance--memoize-llm-candidates))
    (carriage-performance--maybe-byte-compile 'carriage-llm-candidates))
  (when (fboundp 'carriage-llm-default-candidate)
    (unless (advice-member-p #'carriage-performance--memoize-llm-default 'carriage-llm-default-candidate)
      (advice-add 'carriage-llm-default-candidate :around #'carriage-performance--memoize-llm-default))
    (carriage-performance--maybe-byte-compile 'carriage-llm-default-candidate)))

(defun carriage-performance--maybe-detach-carriage-llm ()
  (when (and (fboundp 'carriage-llm-candidates)
             (advice-member-p #'carriage-performance--memoize-llm-candidates 'carriage-llm-candidates))
    (advice-remove 'carriage-llm-candidates #'carriage-performance--memoize-llm-candidates))
  (when (and (fboundp 'carriage-llm-default-candidate)
             (advice-member-p #'carriage-performance--memoize-llm-default 'carriage-llm-default-candidate))
    (advice-remove 'carriage-llm-default-candidate #'carriage-performance--memoize-llm-default)))

(defun carriage-performance--enable ()
  (setq carriage-performance--last-mode-line-update 0.0)
  (carriage-performance--reset-modeline-cache)
  (carriage-performance--reset-llm-caches)
  (unless (advice-member-p #'carriage-performance--throttle-force-mode-line-update 'force-mode-line-update)
    (advice-add 'force-mode-line-update :around #'carriage-performance--throttle-force-mode-line-update))
  (carriage-performance--maybe-attach-carriage-ui)
  (carriage-performance--maybe-attach-carriage-llm))

(defun carriage-performance--disable ()
  (carriage-performance--cancel-timers)
  (when (advice-member-p #'carriage-performance--throttle-force-mode-line-update 'force-mode-line-update)
    (advice-remove 'force-mode-line-update #'carriage-performance--throttle-force-mode-line-update))
  (carriage-performance--maybe-detach-carriage-ui)
  (carriage-performance--maybe-detach-carriage-llm)
  (carriage-performance--reset-modeline-cache)
  (carriage-performance--reset-llm-caches))

;;;###autoload
(define-minor-mode carriage-performance-mode
  "Toggle Carriage performance optimizations."
  :global t
  :group 'carriage-performance
  (if carriage-performance-mode
      (carriage-performance--enable)
    (carriage-performance--disable)))

(with-eval-after-load 'carriage-ui
  (when carriage-performance-mode
    (carriage-performance--maybe-attach-carriage-ui)))

(with-eval-after-load 'carriage-llm
  (when carriage-performance-mode
    (carriage-performance--maybe-attach-carriage-llm)))

(provide 'carriage-performance)

;;; carriage-performance.el ends here
