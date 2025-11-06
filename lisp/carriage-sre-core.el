;;; carriage-sre-core.el --- Core SRE helpers (shared)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; DELIM marker rewriter is unified in lisp/carriage-sre-delim.el (carriage-sre-rewrite-delim-markers).
;; This file no longer provides an alternate implementation.
(require 'carriage-sre-delim)

(provide 'carriage-sre-core)
;;; carriage-sre-core.el ends here
