;;; carriage-engine-sidecar.el --- Sidecar apply engine (skeleton)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-apply-engine)

(defgroup carriage-engine-sidecar nil
  "Sidecar-based apply engine (external process; skeleton)."
  :group 'carriage-engines
  :prefix "carriage-engine-sidecar-")

;; Note: Do not define a defcustom here to avoid clashes with lexical let-binding in tests.
;; The sidecar engine is a skeleton and can be registered unconditionally.

(defun carriage-engine-sidecar--noop (op item repo on-done on-fail)
  "Skeleton action: immediately fail with not-implemented."
  (ignore op item repo)
  (run-at-time 0 nil
               (lambda ()
                 (if (functionp on-fail)
                     (funcall on-fail (list :engine 'sidecar
                                            :exit 125
                                            :stderr "sidecar engine not implemented"))
                   (carriage-log "sidecar: on-fail not a function")))))

(defun carriage-engine-sidecar-dry-run (op item repo on-done on-fail)
  "Entrypoint :dry-run. Currently a stub."
  (carriage-engine-sidecar--noop op item repo on-done on-fail))

(defun carriage-engine-sidecar-apply (op item repo on-done on-fail)
  "Entrypoint :apply. Currently a stub."
  (carriage-engine-sidecar--noop op item repo on-done on-fail))

(defun carriage-engine-sidecar-capabilities (_op)
  "Capabilities for sidecar engine (skeleton)."
  (list :name "Sidecar (skeleton)"
        :ops '(patch create delete rename sre aibo)
        :async t
        :timeout t))

;; Register only if explicitly enabled to avoid confusing users
(carriage-register-apply-engine
 'sidecar "Sidecar (skeleton)"
 :dry-run #'carriage-engine-sidecar-dry-run
 :apply   #'carriage-engine-sidecar-apply
 :capabilities #'carriage-engine-sidecar-capabilities)

(provide 'carriage-engine-sidecar)
;;; carriage-engine-sidecar.el ends here
