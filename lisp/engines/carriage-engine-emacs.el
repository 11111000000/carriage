;;; carriage-engine-emacs.el --- Emacs apply engine (local ops) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-apply-engine)

(defgroup carriage-engine-emacs nil
  "Emacs-based apply engine (local FS ops; no git; no unified diff)."
  :group 'carriage-engines
  :prefix "carriage-engine-emacs-")

(defun carriage-engine-emacs--fail-dispatch (op _item _repo on-fail)
  "Fail with MODE_E_DISPATCH for unsupported OP."
  (run-at-time 0 nil
               (lambda ()
                 (when (functionp on-fail)
                   (funcall on-fail
                            (list :engine 'emacs
                                  :exit 125
                                  :stderr (format "Unsupported op for emacs engine: %s" op)
                                  :code 'MODE_E_DISPATCH))))))

(defun carriage-engine-emacs--ok-noop (op item _repo on-done)
  "Succeed as NOOP for supported non-patch ops (async stub)."
  (run-at-time 0 nil
               (lambda ()
                 (when (functionp on-done)
                   (funcall on-done
                            (list :engine 'emacs
                                  :exit 0
                                  :op op
                                  :path (or (alist-get :file item)
                                            (alist-get :path item) "-")))))))

(defun carriage-engine-emacs-dry-run (op item repo on-done on-fail)
  "Entrypoint :dry-run for Emacs engine.
Unsupported: 'patch → MODE_E_DISPATCH. Others → async OK/NOOP."
  (if (eq op 'patch)
      (carriage-engine-emacs--fail-dispatch op item repo on-fail)
    (carriage-engine-emacs--ok-noop op item repo on-done)))

(defun carriage-engine-emacs-apply (op item repo on-done on-fail)
  "Entrypoint :apply for Emacs engine.
Unsupported: 'patch → MODE_E_DISPATCH. Others → async OK/NOOP.

Note: real SRE/AIBO/file-ops are executed in ops layer in v1; engine stub remains
for registry/UI consistency and forward evolution."
  (if (eq op 'patch)
      (carriage-engine-emacs--fail-dispatch op item repo on-fail)
    (carriage-engine-emacs--ok-noop op item repo on-done)))

(defun carriage-engine-emacs-capabilities (_op)
  "Capabilities for Emacs engine."
  (list :name "Emacs apply engine"
        :ops '(sre aibo create delete rename) ; patch is intentionally unsupported
        :async t
        :timeout nil))

;; Register engine
(carriage-register-apply-engine
 'emacs "Emacs apply engine"
 :dry-run #'carriage-engine-emacs-dry-run
 :apply   #'carriage-engine-emacs-apply
 :capabilities #'carriage-engine-emacs-capabilities)

(provide 'carriage-engine-emacs)
;;; carriage-engine-emacs.el ends here
