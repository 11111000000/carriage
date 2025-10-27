;;; carriage.el --- Carriage entry point  -*- lexical-binding: t; -*-

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-parser)
(require 'carriage-apply)
(require 'carriage-mode)
(require 'carriage-transport)
;; Optional: enable gptel adapter when available (fallback to echo)
(if (require 'gptel nil t)
    (require 'carriage-transport-gptel)
  (require 'carriage-transport-echo))

(provide 'carriage)
;;; carriage.el ends here
