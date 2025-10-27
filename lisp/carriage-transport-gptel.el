;;; carriage-transport-gptel.el --- GPTel adapter for Carriage transport -*- lexical-binding: t; -*-

;; Minimal reference adapter that maps Carriage transport events to gptel-request.
;; - Streams chunks to *carriage-traffic* and accumulates response.
;; - On completion feeds accumulated text to carriage-accept-llm-response.
;; - Registers an abort handler that stops the underlying gptel request.

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)              ;; provided by the gptel package
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-mode)
(require 'carriage-transport)

(defgroup carriage-transport-gptel nil
  "GPTel transport adapter for Carriage."
  :group 'carriage)

(defun carriage--gptel--normalize-model (model)
  "Return a symbol for GPTel's `gptel-model' from MODEL string/symbol.

Accepts forms:
- \"gptel:PROVIDER:MODEL\" → MODEL
- \"PROVIDER:MODEL\"       → MODEL
- \"MODEL\"                → MODEL
- symbol → as-is

If MODEL cannot be interned meaningfully, return it unchanged."
  (cond
   ((symbolp model) model)
   ((stringp model)
    (let* ((last (car (last (split-string model ":" t)))))
      (if (and last (not (string-empty-p last)))
          (intern last)
        (intern model))))
   (t model)))

(defun carriage--gptel--prompt (source buffer mode)
  "Build prompt string for GPTel from SOURCE and BUFFER in MODE."
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

(defun carriage--gptel--maybe-open-logs ()
  "Open log/traffic buffers if user prefs demand and we are interactive."
  (when (and (boundp 'carriage-mode-auto-open-log)
             carriage-mode-auto-open-log
             (not (bound-and-true-p noninteractive)))
    (ignore-errors (carriage-show-log)))
  (when (and (boundp 'carriage-mode-auto-open-traffic)
             carriage-mode-auto-open-traffic
             (not (bound-and-true-p noninteractive)))
    (ignore-errors (carriage-show-traffic))))

(defun carriage-transport-dispatch (&rest args)
  "Dispatch Carriage request via GPTel when backend is 'gptel.

ARGS is a plist with at least :backend, :model, :source, :buffer, :mode.
Streams chunks to *carriage-traffic*, accumulates text, and on completion
invokes `carriage-accept-llm-response' in the originating buffer.

On backend mismatch, logs and completes with error."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (plist-get args :source))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (mode    (or (plist-get args :mode)
                      (buffer-local-value 'major-mode buffer))))
    (unless (memq (if (symbolp backend) backend (intern (format "%s" backend)))
                  '(gptel))
      (carriage-log "Transport[gptel]: backend mismatch (%s), dropping" backend)
      (carriage-transport-complete t)
      (user-error "No transport adapter for backend: %s" backend))
    ;; Prepare environment for gptel
    (let* ((acc "")
           (first-chunk t)
           (prompt (carriage--gptel--prompt source buffer (intern (format "%s" mode))))
           ;; Let-bind gptel variables locally to this request buffer
           (gptel-buffer buffer)
           (abort-installed nil)
           ;; Normalize model to a symbol; gptel will sanitize/fallback if needed
           (gptel-model (carriage--gptel--normalize-model model)))
      (with-current-buffer gptel-buffer
        (carriage--gptel--maybe-open-logs)
        ;; Install abort via Carriage; `gptel-abort' expects a buffer.
        (carriage-register-abort-handler
         (lambda ()
           (condition-case e
               (gptel-abort gptel-buffer)
             (error (carriage-log "Transport[gptel]: abort error: %s"
                                  (error-message-string e))))))
        (setq abort-installed t)
        ;; Kick off request
        (carriage-traffic-log 'out "gptel request: model=%s source=%s bytes=%d"
                              gptel-model source (length prompt))
        (gptel-request prompt
          :callback
          (lambda (response info)
            (cond
             ;; Streaming text chunk
             ((stringp response)
              (when first-chunk
                (setq first-chunk nil)
                (carriage-transport-streaming))
              (setq acc (concat acc response))
              (carriage-traffic-log 'in "%s" response))
             ;; Reasoning block: log (do not accumulate)
             ((and (consp response) (eq (car response) 'reasoning))
              (let ((chunk (cdr response)))
                (when (stringp chunk)
                  (carriage-traffic-log 'in "[reasoning] %s" chunk))))
             ;; Tool events: note and continue
             ((and (consp response) (memq (car response) '(tool-call tool-result)))
              (carriage-traffic-log 'in "[%s] ..." (car response)))
             ;; Completed successfully (t)
             ((eq response t)
              (carriage-traffic-log 'in "gptel: done")
              (carriage-transport-complete nil)
              (when (and (stringp acc) (not (string-empty-p (string-trim acc))))
                (with-current-buffer gptel-buffer
                  (condition-case e
                      (carriage-accept-llm-response acc)
                    (error
                     (carriage-log "accept error: %s" (error-message-string e))
                     (carriage-ui-set-state 'error))))))
             ;; Aborted or error (nil or 'abort)
             ((or (null response) (eq response 'abort))
              (let ((msg (or (plist-get info :status) "error/abort")))
                (carriage-traffic-log 'in "gptel: %s" msg))
              (carriage-transport-complete t)))))))))

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
