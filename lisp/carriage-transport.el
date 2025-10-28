;;; carriage-transport.el --- Transport integration helpers (M4)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-ui)
(require 'carriage-logging)
(require 'carriage-errors)
;; Break circular dependency with carriage-mode: call its fns via declare-function.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-clear-abort-handler "carriage-mode" ())

(defvar carriage--transport-loading-adapter nil
  "Guard to prevent recursive/layered adapter loading in transport dispatcher.")

;;;###autoload
(defun carriage-transport-begin (&optional abort-fn)
  "Signal beginning of an async request: set UI to 'sending and install ABORT-FN.
ABORT-FN should be a zero-arg function that cancels the ongoing request.
Returns an unregister lambda that clears the handler when called."
  (when (functionp abort-fn)
    (carriage-register-abort-handler abort-fn))
  (carriage-log "Transport: begin (abort=%s)" (if (functionp abort-fn) "installed" "none"))
  (carriage-ui-set-state 'sending)
  ;; Return unregister lambda
  (lambda ()
    (carriage-clear-abort-handler)
    (carriage-log "Transport: unregister abort handler")))

;;;###autoload
(defun carriage-transport-streaming ()
  "Signal that transport has progressed to streaming: update UI state."
  (carriage-log "Transport: streaming")
  (carriage-ui-set-state 'streaming))

;;;###autoload
(defun carriage-transport-complete (&optional errorp)
  "Signal completion of an async request: clear abort handler and set UI state.
If ERRORP non-nil, set state to 'error; otherwise set 'idle."
  (carriage-clear-abort-handler)
  (carriage-ui-set-state (if errorp 'error 'idle))
  (carriage-log "Transport: complete (status=%s)" (if errorp "error" "ok"))
  t)

;;;###autoload
(defun carriage-transport-dispatch (&rest args)
  "Dispatch request ARGS to transport adapter with safe lazy loading.

Contract:
- Prefer direct entry-point call (carriage-transport-<backend>-dispatch) when fboundp.
- If missing, attempt one-shot lazy load of adapter (guarded), then call entry-point.
- No recursion, no reliance on function cell replacement."
  (carriage-traffic-log 'out "dispatch request: %S" args)
  (let* ((backend (plist-get args :backend))
         (bsym (cond
                ((symbolp backend) backend)
                ((stringp backend) (intern backend))
                (t (ignore-errors (intern (format "%s" backend)))))))
    (pcase bsym
      ;; GPTel backend
      ('gptel
       (cond
        ;; Fast path: entry-point present
        ((fboundp 'carriage-transport-gptel-dispatch)
         (apply #'carriage-transport-gptel-dispatch args))
        ;; One-shot lazy load guarded
        ((not carriage--transport-loading-adapter)
         (let* ((carriage--transport-loading-adapter t))
           (when (and (require 'gptel nil t)
                      (require 'carriage-transport-gptel nil t))
             (carriage-log "Transport: gptel adapter loaded on demand"))
           (if (fboundp 'carriage-transport-gptel-dispatch)
               (apply #'carriage-transport-gptel-dispatch args)
             (carriage-log "Transport: no gptel entry-point; request dropped")
             (carriage-transport-complete t)
             (user-error "No transport adapter installed (gptel)"))))
        (t
         (carriage-log "Transport: adapter loading already in progress; dropping")
         (carriage-transport-complete t)
         (user-error "No transport adapter installed (gptel)"))))
      ;; Echo backend (dev)
      ('echo
       (cond
        ((fboundp 'carriage-transport-echo-dispatch)
         (apply #'carriage-transport-echo-dispatch args))
        ((not carriage--transport-loading-adapter)
         (let* ((carriage--transport-loading-adapter t))
           (when (require 'carriage-transport-echo nil t)
             (carriage-log "Transport: echo adapter loaded on demand"))
           (if (fboundp 'carriage-transport-echo-dispatch)
               (apply #'carriage-transport-echo-dispatch args)
             (carriage-log "Transport: no echo entry-point; request dropped")
             (carriage-transport-complete t)
             (user-error "No transport adapter installed (echo)"))))
        (t
         (carriage-log "Transport: adapter loading already in progress; dropping")
         (carriage-transport-complete t)
         (user-error "No transport adapter installed (echo)"))))
      ;; Unknown backend
      (_
       (carriage-log "Transport: unknown backend=%s" bsym)
       (signal (carriage-error-symbol 'LLM_E_BACKEND) (list (format "Unknown transport backend: %s" bsym)))
       (carriage-transport-complete t)
       (user-error "Unknown transport backend: %s" bsym)))))

(provide 'carriage-transport)
;;; carriage-transport.el ends here
