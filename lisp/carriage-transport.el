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
  (carriage-ui-set-state 'sending)
  ;; Return unregister lambda
  (lambda ()
    (carriage-clear-abort-handler)))

;;;###autoload
(defun carriage-transport-streaming ()
  "Signal that transport has progressed to streaming: update UI state."
  (carriage-ui-set-state 'streaming))

;;;###autoload
(defun carriage-transport-complete (&optional errorp)
  "Signal completion of an async request: clear abort handler and set UI state.
If ERRORP non-nil, set state to 'error; otherwise set 'idle."
  (carriage-clear-abort-handler)
  (carriage-ui-set-state (if errorp 'error 'idle))
  (when errorp
    (carriage-log "Transport complete with error"))
  t)

(provide 'carriage-transport)
;;; carriage-transport.el ends here
