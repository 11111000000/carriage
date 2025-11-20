;;; carriage-transport-gptel.el --- gptel transport adapter  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (gptel "0.1"))
;; Version: 0.1
;; Keywords: transport, gptel
;;
;; Specifications:
;;   spec/code-style-v1.org
;;   spec/index.org
;;   spec/errors-v1.org
;;   spec/compliance-checklist-v1.org
;;   spec/llm-transport-v1.org
;;   spec/logging-v1.org
;;
;;; Commentary:
;; Adapter for streaming gptel backends.
;;
;;; Code:

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
  "Build prompt string for GPTel from SOURCE and BUFFER in MODE.
Strips any #+begin_carriage … #+end_carriage blocks from the outgoing text."
  (with-current-buffer buffer
    (let* ((raw
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
      ;; Filter out carriage state blocks to avoid leaking configuration into prompts
      (with-temp-buffer
        (insert (or raw ""))
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
            (let ((beg (match-beginning 0)))
              (if (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
                  (let ((end (line-end-position)))
                    (delete-region beg end)
                    (when (looking-at "\n") (delete-char 1)))
                (delete-region beg (point-max))))))
        (buffer-substring-no-properties (point-min) (point-max))))))

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

(defun carriage--gptel--classify (response)
  "Normalize GPTel RESPONSE into a plist with :kind and payload.
Kinds: 'text 'reasoning 'both 'reasoning-end 'tool 'done 'abort 'unknown.
When both reasoning and text are present, returns :kind 'both with keys :thinking and :text."
  (cond
   ;; Plain text piece
   ((stringp response)
    (list :kind 'text :text response))
   ;; End
   ((eq response t)
    (list :kind 'done))
   ;; Abort/error
   ((or (null response) (eq response 'abort))
    (list :kind 'abort))
   ;; Reasoning stream (classic pair: (reasoning . CHUNK|t))
   ((and (consp response) (eq (car response) 'reasoning))
    (let ((chunk (cdr response)))
      (cond
       ((eq chunk t) (list :kind 'reasoning-end))
       ((stringp chunk) (list :kind 'reasoning :text chunk))
       (t (list :kind 'unknown)))))
   ;; Tool events
   ((and (consp response) (memq (car response) '(tool-call tool-result)))
    (list :kind 'tool))
   ;; Plist/alist with :thinking and/or :content/:delta
   ((listp response)
    (let* ((th  (or (plist-get response :thinking)
                    (and (fboundp 'alist-get) (alist-get :thinking response))))
           (txt (or (plist-get response :content)
                    (plist-get response :delta)
                    (and (fboundp 'alist-get) (alist-get :content response))
                    (and (fboundp 'alist-get) (alist-get :delta response)))))
      (cond
       ;; End of reasoning
       ((eq th t) (list :kind 'reasoning-end))
       ;; Both thinking and text present
       ((and (stringp th) (stringp txt))
        (list :kind 'both :thinking th :text txt))
       ;; Only thinking
       ((stringp th)
        (list :kind 'reasoning :text th))
       ;; Only text
       ((stringp txt)
        (list :kind 'text :text txt))
       (t (list :kind 'unknown)))))
   (t (list :kind 'unknown))))

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
        (condition-case _
            (signal (carriage-error-symbol 'LLM_E_BACKEND)
                    (list (format "Unknown transport backend: %s" backend)))
          (error nil))
        (carriage-transport-complete t))
      (user-error "No transport adapter for backend: %s" backend))
    ;; Prepare environment for gptel
    (let* ((first-event t)         ;; first reasoning OR text event
           (any-text-seen nil)
           (prompt (or (plist-get args :prompt)
                       (carriage--gptel--prompt source buffer (intern (format "%s" mode)))))
           (system (plist-get args :system))
           (gptel-buffer buffer)
           (gptel-model (carriage--gptel--normalize-model model))
           ;; Response accumulation for summary (head/tail)
           (carriage--resp-head "") (carriage--resp-tail "")
           (carriage--resp-bytes 0)
           (carriage--resp-head-limit (or (and (boundp 'carriage-traffic-summary-head-bytes)
                                               carriage-traffic-summary-head-bytes) 4096))
           (carriage--resp-tail-limit (or (and (boundp 'carriage-traffic-summary-tail-bytes)
                                               carriage-traffic-summary-tail-bytes) 4096)))
      (with-current-buffer gptel-buffer
        (carriage--gptel--maybe-open-logs)
        (carriage-register-abort-handler
         (lambda ()
           (condition-case e
               (gptel-abort gptel-buffer)
             (error (carriage-log "Transport[gptel]: abort error: %s"
                                  (error-message-string e))))))
        ;; Structured request logging (per-buffer traffic buffer)
        (carriage-traffic-log-request gptel-buffer
                                      :backend 'gptel
                                      :model gptel-model
                                      :system system
                                      :prompt prompt
                                      :context (plist-get args :context))
        (carriage-traffic-log 'out "gptel request: model=%s source=%s bytes=%d"
                              gptel-model source (length prompt))
        ;; Explicit waiting phase until the first chunk arrives
        (with-current-buffer gptel-buffer
          (carriage-ui-set-state 'waiting)
          ;; Initialize progress meta for tooltip (model/provider/start-time)
          (ignore-errors
            (carriage-ui-note-stream-progress
             (list :model (format "%s" gptel-model)
                   :provider (or (plist-get args :provider) nil)
                   :time-start (float-time)
                   :time-last (float-time))))))
      (condition-case err
          (gptel-request prompt
            :system system
            :stream t
            :callback
            (lambda (response info)
              (condition-case qerr
                  (let* ((cls (carriage--gptel--classify response))
                         (kind (plist-get cls :kind))
                         (text (plist-get cls :text))
                         (thinking (plist-get cls :thinking)))
                    (pcase kind
              ;; Reasoning stream (before main text)
              ('reasoning
               (when first-event
                 (setq first-event nil)
                 (with-current-buffer gptel-buffer
                   (carriage-ui-set-state 'reasoning)))
               (if any-text-seen
                   (carriage-traffic-log 'in "[reasoning] %s" (or text ""))
                 (with-current-buffer gptel-buffer
                   (carriage-insert-stream-chunk (or text "") 'reasoning)
                   (ignore-errors (carriage-ui-note-reasoning-chunk (or text ""))))))
              ;; Mixed: thinking + text in one event
              ('both
               (when first-event
                 (setq first-event nil)
                 (with-current-buffer gptel-buffer
                   (carriage-ui-set-state 'reasoning)))
               ;; Print thinking only until we see first text
               (when (and (stringp thinking) (not any-text-seen))
                 (with-current-buffer gptel-buffer
                   (carriage-insert-stream-chunk thinking 'reasoning))
                 (carriage-traffic-log 'in "[reasoning] %s" thinking))
               ;; Then print main text (auto-closes reasoning if needed)
               (when (stringp text)
                 ;; transition to streaming on first text
                 (when (not any-text-seen)
                   (with-current-buffer gptel-buffer
                     (carriage-transport-streaming)))
                 (with-current-buffer gptel-buffer
                   (carriage-insert-stream-chunk text 'text)
                   (ignore-errors (carriage-ui-note-stream-progress (list :inc-chunk t :time-last (float-time))))))
               ;; Accumulate response summary head/tail
               (setq carriage--resp-bytes (+ carriage--resp-bytes (string-bytes text)))
               (when (< (length carriage--resp-head) carriage--resp-head-limit)
                 (let* ((need (max 0 (- carriage--resp-head-limit (length carriage--resp-head))))
                        (take (min need (length text))))
                   (setq carriage--resp-head
                         (concat carriage--resp-head (substring text 0 take)))
                   (setq text (substring text take))))
               (when (> (length text) 0)
                 (let* ((concatd (concat carriage--resp-tail text))
                        (len (length concatd)))
                   (setq carriage--resp-tail
                         (if (<= len carriage--resp-tail-limit)
                             concatd
                           (substring concatd (- len carriage--resp-tail-limit) len)))))
               (setq any-text-seen t)
               (carriage-traffic-log 'in "%s" text))
              ;; Reasoning end marker
              ('reasoning-end
               (with-current-buffer gptel-buffer
                 (ignore-errors (carriage-end-reasoning)))
               (carriage-traffic-log 'in "[reasoning] end"))
              ;; Main text tokens
              ('text
               (when first-event
                 (setq first-event nil)
                 (with-current-buffer gptel-buffer
                   (carriage-transport-streaming)))
               (when (stringp text)
                 (with-current-buffer gptel-buffer
                   ;; This will auto-close an open reasoning block
                   (carriage-insert-stream-chunk text 'text)
                   (ignore-errors (carriage-ui-note-stream-progress (list :inc-chunk t :time-last (float-time))))))
               ;; Accumulate response summary head/tail
               (setq carriage--resp-bytes (+ carriage--resp-bytes (string-bytes text)))
               (when (< (length carriage--resp-head) carriage--resp-head-limit)
                 (let* ((need (max 0 (- carriage--resp-head-limit (length carriage--resp-head))))
                        (take (min need (length text))))
                   (setq carriage--resp-head
                         (concat carriage--resp-head (substring text 0 take)))
                   (setq text (substring text take))))
               (when (> (length text) 0)
                 (let* ((concatd (concat carriage--resp-tail text))
                        (len (length concatd)))
                   (setq carriage--resp-tail
                         (if (<= len carriage--resp-tail-limit)
                             concatd
                           (substring concatd (- len carriage--resp-tail-limit) len)))))
               (setq any-text-seen t)
               (when (stringp text)
                 (carriage-traffic-log 'in "%s" text)))
              ;; Tool telemetry
              ('tool
               (carriage-traffic-log 'in "[tool] ..."))
              ;; Done
              ('done
               (carriage-traffic-log 'in "gptel: done")
               (with-current-buffer gptel-buffer
                 (ignore-errors (carriage-ui-note-stream-progress (list :time-last (float-time)))))
               ;; Structured response summary
               (carriage-traffic-log-response-summary
                gptel-buffer
                (concat carriage--resp-head
                        (when (> carriage--resp-bytes
                                 (+ (string-bytes carriage--resp-head)
                                    (string-bytes carriage--resp-tail)))
                          "…")
                        carriage--resp-tail))
               (with-current-buffer gptel-buffer
                 (carriage-stream-finalize nil t)
                 (carriage-transport-complete nil)))
              ;; Abort or error
              ('abort
               (let ((msg (or (plist-get info :status) "error/abort")))
                 (carriage-traffic-log 'in "gptel: %s" msg)
                 (with-current-buffer gptel-buffer
                   (ignore-errors
                     (carriage-ui-note-error
                      (list :code 'LLM_ABORT :message msg :source 'transport)))))
               ;; Emit summary for what we've got so far
               (carriage-traffic-log-response-summary
                gptel-buffer
                (concat carriage--resp-head
                        (when (> carriage--resp-bytes
                                 (+ (string-bytes carriage--resp-head)
                                    (string-bytes carriage--resp-tail)))
                          "…")
                        carriage--resp-tail))
               (with-current-buffer gptel-buffer
                 (carriage-stream-finalize t nil)
                 (carriage-transport-complete t)))
              ;; Unknown chunk kinds
              (_
               (carriage-traffic-log 'in "[unknown] %S" response)))))))))

;; Entry-point: carriage-transport-gptel-dispatch

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
