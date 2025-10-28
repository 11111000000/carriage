;;; carriage.el --- Carriage entry point  -*- lexical-binding: t; -*-

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
;; Registries and suite builder (ops modules register themselves)
(require 'carriage-format-registry)
(require 'carriage-suite)

(require 'carriage-parser)
(require 'carriage-apply)

;; Load ops modules (registration references functions from parser/apply)
(require 'carriage-op-sre)
(require 'carriage-op-patch)
(require 'carriage-op-file)

(require 'carriage-mode)
(require 'carriage-transport)
;; Optional: enable gptel adapter when available (fallback to echo)
(if (require 'gptel nil t)
    (require 'carriage-transport-gptel)
  (require 'carriage-transport-echo))

(provide 'carriage)
;;; carriage.el ends here
