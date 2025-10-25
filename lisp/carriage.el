;;; carriage.el --- Carriage entry point  -*- lexical-binding: t; -*-

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-parser)
(require 'carriage-apply)
(require 'carriage-mode)

(provide 'carriage)
;;; carriage.el ends here
