;;; carriage.el --- Carriage entry point  -*- lexical-binding: t; -*-

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
;; Registries and suite builder (ops modules register themselves)
(require 'carriage-format-registry)
(require 'carriage-intent-registry)
(require 'carriage-suite)
(require 'carriage-global-mode)

;; Ensure 'ops' and 'engines' directories are on load-path for requiring modules
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (ops-dir (and this-dir (expand-file-name "ops" this-dir)))
       (transports-dir (and this-dir (expand-file-name "transports" this-dir)))
       (engines-dir (and this-dir (expand-file-name "engines" this-dir))))
  (when (and ops-dir (file-directory-p ops-dir))
    (add-to-list 'load-path ops-dir))
  (when (and ops-dir (file-directory-p transports-dir))
    (add-to-list 'load-path transports-dir))
  (when (and engines-dir (file-directory-p engines-dir))
    (add-to-list 'load-path engines-dir)))
;; Ops modules are lazy-loaded by suite/parser when needed (no eager require here)

(require 'carriage-parser)
(require 'carriage-apply)

(require 'carriage-apply-engine)
(require 'carriage-engine-git)
(require 'carriage-engine-emacs)
(require 'carriage-mode)
(require 'carriage-transport)
;; Transports are loaded lazily by carriage-transport-dispatch per spec.
;; Do not require adapters by default here.

(provide 'carriage)
;;; carriage.el ends here
