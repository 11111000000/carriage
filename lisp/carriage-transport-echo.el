;;; carriage-transport-echo.el --- Echo transport (dev fallback) -*- lexical-binding: t; -*-

;; A tiny reference adapter that simulates streaming without external deps.
;; - Streams chunks of a derived prompt to *carriage-traffic*.
;; - Drives UI states via carriage-transport-begin/streaming/complete.
;; - Registers an abort handler to cancel the stream timer.
;; - Does NOT call carriage-accept-llm-response (no edits), purely for UX/debug.

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-transport)
;; Avoid hard dependency on carriage-mode to break cycles at load time.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))

(defgroup carriage-transport-echo nil
  "Echo transport adapter (development fallback)."
  :group 'carriage)

(defcustom carriage-transport-echo-chunk-ms 120
  "Interval between streamed chunks for echo transport, in ms."
  :type 'integer :group 'carriage-transport-echo)

(defcustom carriage-transport-echo-chunk-size 48
  "Size of each streamed chunk (characters)."
  :type 'integer :group 'carriage-transport-echo)

(defun carriage--echo--prompt (source buffer mode)
  "Build dev prompt string from BUFFER given SOURCE and MODE."
  (with-current-buffer buffer
    (pcase source
      ('subtree
       (if (eq mode 'org-mode)
           (save-excursion
             (require 'org)
             (ignore-errors (org-back-to-heading t))
             (let ((beg (save-excursion (org-back-to-heading t) (point)))
                   (end (save-excursion (org-end-of-subtree t t) (point))))
               (buffer-substring-no-properties beg end)))
         (buffer-substring-no-properties (point-min) (point-max))))
      (_ (buffer-substring-no-properties (point-min) (point-max))))))

(defun carriage--echo--chunk-string (s n)
  "Return a list of chunks splitting S into pieces of at most N chars."
  (let* ((len (length s))
         (i 0)
         (acc '()))
    (while (< i len)
      (let* ((j (min len (+ i (max 1 n))))
             (chunk (substring s i j)))
        (push chunk acc)
        (setq i j)))
    (nreverse acc)))

(defun carriage-transport-echo-dispatch (&rest args)
  "Dispatch Carriage request via echo backend when :backend is 'echo.

ARGS is a plist with keys like :backend, :model, :source, :buffer, :mode.

Simulates streaming by emitting chunks to *carriage-traffic* on a timer.
Does not modify the Org buffer and does not call =carriage-transport-begin'."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (or (plist-get args :source) 'buffer))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (mode    (or (plist-get args :mode)
                      (buffer-local-value 'major-mode buffer))))
    (unless (eq (if (symbolp backend) backend (intern (format "%s" backend))) 'echo)
      (carriage-log "Transport[echo]: backend mismatch (%s), dropping" backend)
      (carriage-transport-complete t)
      (user-error "No transport adapter for backend: %s" backend))
    (with-current-buffer buffer
      (let* ((raw (carriage--echo--prompt source buffer (intern (format "%s" mode))))
             (trimmed (string-trim raw))
             (payload (if (string-empty-p trimmed)
                          (format "Echo backend active. Set backend to gptel for real LLM. model=%s" model)
                        (format "ECHO: %s" trimmed)))
             (chunks (carriage--echo--chunk-string payload carriage-transport-echo-chunk-size))
             (first t)
             (timer nil))
        ;; Install abort handler that cancels the streaming timer
        (carriage-register-abort-handler
         (lambda ()
           (when (and timer (timerp timer))
             (cancel-timer timer))
           (setq timer nil)
           (carriage-traffic-log 'in "echo: aborted")
           (carriage-transport-complete t)))
        ;; Log request
        (carriage-traffic-log 'out "echo request: source=%s model=%s bytes=%d"
                              source model (length payload))
        ;; Drive streaming via timer
        (let* ((interval (/ (max 1 carriage-transport-echo-chunk-ms) 1000.0))
               (rest chunks))
          (setq timer
                (run-at-time
                 0 interval
                 (lambda ()
                   (condition-case e
                       (progn
                         (if (null rest)
                             (progn
                               (carriage-traffic-log 'in "echo: done")
                               (carriage-transport-complete nil)
                               (when (timerp timer)
                                 (cancel-timer timer))
                               (setq timer nil))
                           (progn
                             (when first
                               (setq first nil)
                               (carriage-transport-streaming))
                             (let ((chunk (pop rest)))
                               (carriage-traffic-log 'in "%s" chunk)))))
                     (error
                      (carriage-log "Echo stream error: %s" (error-message-string e))
                      (carriage-transport-complete t)
                      (when (timerp timer) (cancel-timer timer))
                      (setq timer nil))))))))))
  (provide 'carriage-transport-echo)
;;; carriage-transport-echo.el ends here
