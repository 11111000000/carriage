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
(require 'carriage-transport)
(require 'carriage-errors)
;; Avoid hard dependency on carriage-mode to break cycles at load time.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-insert-stream-chunk "carriage-mode" (string &optional type))
(declare-function carriage-begin-reasoning "carriage-mode" ())
(declare-function carriage-end-reasoning "carriage-mode" ())
(declare-function carriage-stream-finalize "carriage-mode" (&optional errorp mark-last-iteration))

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

(defun carriage-transport-gptel-dispatch (&rest args)
  "Dispatch Carriage request via GPTel when backend is 'gptel.

ARGS is a plist with at least :backend, :model, :source, :buffer, :mode.
Optionally accepts :prompt and :system to override prompt construction.
When :insert-marker is a live marker, insert accepted blocks at its position.
Streams chunks to *carriage-traffic*, accumulates text, and on completion
invokes =carriage-accept-llm-response' in the originating buffer/marker.

On backend mismatch, logs and completes with error."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (plist-get args :source))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (mode    (or (plist-get args :mode)
                      (buffer-local-value 'major-mode buffer)))
         (ins-marker (plist-get args :insert-marker)))
    (unless (memq (if (symbolp backend) backend (intern (format "%s" backend)))
                  '(gptel))
      (carriage-log "Transport[gptel]: backend mismatch (%s), dropping" backend)
      (with-current-buffer buffer
        ;; Сигнализируем LLM_E_BACKEND (см. spec/errors-v1.org), затем завершаем транспорт.
        (condition-case _
            (signal (carriage-error-symbol 'LLM_E_BACKEND)
                    (list (format "Unknown transport backend: %s" backend)))
          (error nil))
        (carriage-transport-complete t))
      (user-error "No transport adapter for backend: %s" backend))
    ;; Prepare environment for gptel
    (let* ((first-chunk t)
           (any-text-seen nil)
           (prompt (or (plist-get args :prompt)
                       (carriage--gptel--prompt source buffer (intern (format "%s" mode)))))
           (system (plist-get args :system))
           ;; Let-bind gptel variables locally to this request buffer
           (gptel-buffer buffer)
           (abort-installed nil)
           ;; Normalize model to a symbol; gptel will sanitize/fallback if needed
           (gptel-model (carriage--gptel--normalize-model model)))
      (with-current-buffer gptel-buffer
        (carriage--gptel--maybe-open-logs)
        ;; Install abort via Carriage; =gptel-abort' expects a buffer.
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
          :system system
          :stream t
          :callback
          (lambda (response info)
            (cond
             ;; Streaming text chunk
             ((stringp response)
              (when first-chunk
                (setq first-chunk nil)
                (with-current-buffer gptel-buffer
                  (carriage-transport-streaming)))
              (with-current-buffer gptel-buffer
                (carriage-insert-stream-chunk response 'text))
              (setq any-text-seen t)
              (carriage-traffic-log 'in "%s" response))
             ;; Reasoning block: log (do not accumulate)
             ((and (consp response) (eq (car response) 'reasoning))
              (let ((chunk (cdr response)))
                (cond
                 ;; Explicit end-of-reasoning marker
                 ((eq chunk t)
                  (with-current-buffer gptel-buffer
                    (ignore-errors (carriage-end-reasoning)))
                  (carriage-traffic-log 'in "[reasoning] end"))
                 ;; Reasoning text
                 ((stringp chunk)
                  (if any-text-seen
                      ;; After content started: keep in traffic only (avoid noisy reinsertion)
                      (carriage-traffic-log 'in "[reasoning] %s" chunk)
                    ;; Before content: insert into reasoning block
                    (with-current-buffer gptel-buffer
                      (carriage-insert-stream-chunk chunk 'reasoning))
                    (carriage-traffic-log 'in "[reasoning] %s" chunk))))))

             ;; Tool events: note and continue
             ((and (consp response) (memq (car response) '(tool-call tool-result)))
              (carriage-traffic-log 'in "[%s] ..." (car response)))
             ;; Completed successfully (t)
             ((eq response t)
              (carriage-traffic-log 'in "gptel: done")
              (with-current-buffer gptel-buffer
                (carriage-stream-finalize nil t)
                (carriage-transport-complete nil)))
             ;; Aborted or error (nil or 'abort)
             ((or (null response) (eq response 'abort))
              (let ((msg (or (plist-get info :status) "error/abort")))
                (carriage-traffic-log 'in "gptel: %s" msg))
              (with-current-buffer gptel-buffer
                (carriage-stream-finalize t nil)
                (carriage-transport-complete t))))))))))


;; Entry-point: carriage-transport-gptel-dispatch

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
