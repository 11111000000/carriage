;;; carriage-op-aibo.el --- AIBO v1 (literal-only search/replace) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-format-registry)
(require 'carriage-op-sre)  ;; delegate parse/dry-run/apply core

(defun carriage-op-aibo-prompt-fragment (_ctx)
  "Return prompt fragment for AIBO v1 (literal-only begin_from/begin_to)."
  (concat
   "AIBO (literal-only, one file):\n"
   "#+begin_patch (:version \"1\" :op \"aibo\" :file \"RELATIVE/PATH\")\n"
   "#+pair (:occur first) ; optional, applies to the NEXT pair\n"
   "#+begin_from\nFROM text (literal)\n#+end_from\n"
   "#+begin_to\nTO text (literal)\n#+end_to\n"
   "#+end_patch\n"
   "- No regex: :match key is forbidden; all matches are literal.\n"
   "- Allowed options: :occur 'first|'all (for 'all require :expect ≥ 0), :expect, :range.\n"))

(defun carriage--aibo--sanitize-pairs (pairs)
  "Force literal matching, validate :occur, and reject empty segments."
  (mapcar
   (lambda (p)
     (let* ((from (alist-get :from p))
            (to   (alist-get :to p))
            (opts (copy-sequence (alist-get :opts p))))
       ;; Empty segment check
       (when (or (null from) (null to) (string-empty-p from) (string-empty-p to))
         (signal (carriage-error-symbol 'SRE_E_EMPTY_SEGMENT) (list "Empty FROM/TO in AIBO")))
       ;; Disallow :match key entirely
       (when (plist-member opts :match)
         (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX) (list ":match is not allowed in AIBO")))
       ;; Force :match literal
       (setq opts (plist-put opts :match 'literal))
       ;; Normalize and validate :occur
       (let* ((occur (plist-get opts :occur))
              (oc (cond
                   ((symbolp occur) occur)
                   ((stringp occur) (intern (downcase occur)))
                   (t occur))))
         (unless (memq oc '(first all))
           (signal (carriage-error-symbol 'SRE_E_OCCUR_VALUE)
                   (list (format "Invalid :occur for AIBO: %S" occur))))
         (setq opts (plist-put opts :occur oc)))
       (list (cons :from from) (cons :to to) (cons :opts opts))))
   pairs))

(defun carriage-parse-aibo (header body repo-root)
  "Parse AIBO v1 via SRE parser, then sanitize pairs for literal-only semantics."
  (let* ((version (plist-get header :version))
         (op      (plist-get header :op))
         (file    (plist-get header :file)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'SRE_E_VERSION) (list version)))
    (unless (member op '(aibo 'aibo "aibo"))
      (signal (carriage-error-symbol 'SRE_E_OP) (list op)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'SRE_E_PATH) (list file))))
  ;; Delegate to SRE parse with adjusted header
  (let* ((hdr (copy-sequence header)))
    (plist-put hdr :op "sre")
    (let* ((sre-plan (carriage-parse-sre hdr body repo-root))
           (pairs    (carriage--aibo--sanitize-pairs (alist-get :pairs sre-plan)))
           (norm     (carriage-normalize-path repo-root (plist-get header :file))))
      (list (cons :version "1")
            (cons :op 'aibo)
            (cons :file (file-relative-name norm repo-root))
            (cons :pairs pairs)))))

(defun carriage-dry-run-aibo (plan-item repo-root)
  "Dry-run for AIBO: reuse SRE dry-run and map :op for reporting."
  (let* ((tmp (append (list (cons :op 'sre))
                      (assq-delete-all :op (copy-sequence plan-item)))))
    (let ((row (carriage-dry-run-sre tmp repo-root)))
      (plist-put row :op 'aibo)
      row)))

(defun carriage-apply-aibo (plan-item repo-root)
  "Apply for AIBO: reuse SRE apply and map :op for reporting."
  (let* ((tmp (append (list (cons :op 'sre))
                      (assq-delete-all :op (copy-sequence plan-item)))))
    (let ((row (carriage-apply-sre tmp repo-root)))
      (plist-put row :op 'aibo)
      row)))

;; Registration
(carriage-format-register 'aibo "1"
                          :parse #'carriage-parse-aibo
                          :dry-run #'carriage-dry-run-aibo
                          :apply #'carriage-apply-aibo
                          :prompt-fragment #'carriage-op-aibo-prompt-fragment)

(provide 'carriage-op-aibo)
;;; carriage-op-aibo.el ends here
