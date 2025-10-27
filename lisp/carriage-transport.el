;;; carriage-transport.el --- Transport integration helpers (M4)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-ui)
(require 'carriage-mode)
(require 'carriage-logging)

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

(defun carriage-transport-dispatch (&rest args)
  "Dispatch a request to the current backend adapter (placeholder).
ARGS is a plist, may include :source, :backend, :model, :payload, :buffer, etc.

In v1, when no adapter is installed, we log and complete with error.
Backends (e.g., gptel) should advise/override this function in their adapter."
  (carriage-traffic-log 'out "dispatch request: %S" args)
  (carriage-log "Transport: no adapter installed; request dropped")
  (carriage-transport-complete t)
  (user-error "No transport adapter installed (gptel or other)"))

(provide 'carriage-transport)
;;; carriage-transport.el ends here
