;;; carriage-sre-delim.el --- Unified DELIM markers rewriter for :op "create" -*- lexical-binding: t; -*-

(require 'subr-x)

;; Public API
;; (carriage-sre-rewrite-delim-markers TEXT OLD NEW)
;; Rewrites ONLY exact delimiter lines in TEXT:
;;   "<<OLD" -> "<<NEW"
;;   ":OLD"  -> ":NEW"
;; where OLD/NEW are exactly 6 lowercase hex ([0-9a-f]{6}).
;; Lines with spaces or extra characters (e.g., " <<OLD", "<< OLD", ": DELIM", "::DELIM")
;; are NOT rewritten.

(defun carriage-sre-rewrite-delim-markers (text old new)
  "Rewrite exact DELIM markers in TEXT from OLD to NEW.

Only lines that are exactly \"<<OLD\" or \":OLD\" (no leading/trailing whitespace)
are rewritten. Lines like \" <<OLD\", \"<< OLD\", \": DELIM\", \"::DELIM\" remain unchanged.

Signals an error if OLD/NEW don't match \\`[0-9a-f]\\{6\\}\\'."
  (let ((rx "\\`[0-9a-f]\\{6\\}\\'"))
    (unless (and (stringp old) (string-match-p rx old)
                 (stringp new) (string-match-p rx new))
      (condition-case _
          (if (fboundp 'carriage-error-symbol)
              (signal (carriage-error-symbol 'OPS_E_DELIM) (list "Invalid :delim token(s)"))
            (signal 'error (list "Invalid :delim token(s)")))
        (error (signal 'error (list "Invalid :delim token(s)"))))))
  (let* ((open-old (concat "<<" old))
         (close-old (concat ":" old))
         (open-new (concat "<<" new))
         (close-new (concat ":" new))
         ;; Keep empty lines to preserve final newline semantics
         (lines (split-string (or text "") "\n" nil)))
    (mapconcat
     (lambda (ln)
       (cond
        ((string= ln open-old) open-new)
        ((string= ln close-old) close-new)
        (t ln)))
     lines
     "\n")))

(provide 'carriage-sre-delim)
;;; carriage-sre-delim.el ends here
