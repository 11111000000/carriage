;;; carriage-llm-registry.el --- LLM backend/model registry  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar carriage-llm--registry nil
  "Alist registry of LLM backends and their models.
Shape: ((BACKEND . (:models (\"m1\" \"m2\") :models-fn FN)) ...).
BACKEND is a symbol (preferred) or string. :models-fn, when non-nil,
should be a zero-arg function returning a list of model strings.")

(defun carriage-llm--norm-backend (backend)
  "Normalize BACKEND to a symbol."
  (cond
   ((symbolp backend) backend)
   ((stringp backend) (intern backend))
   (t (intern (format "%s" backend)))))

(defun carriage-llm-register-backend (backend &rest kvs)
  "Register BACKEND with optional keys:
:models — list of model strings; :models-fn — function returning list of models.
Returns an unregister zero-arg lambda."
  (let* ((b (carriage-llm--norm-backend backend))
         (entry (list :models (plist-get kvs :models)
                      :models-fn (plist-get kvs :models-fn)))
         (cell (assoc b carriage-llm--registry)))
    (if cell
        (setcdr cell entry)
      (push (cons b entry) carriage-llm--registry))
    (lambda ()
      (setq carriage-llm--registry (assq-delete-all b carriage-llm--registry)))))

(defun carriage-llm-available-backends ()
  "Return list of backend names (strings) from the registry."
  (mapcar (lambda (cell) (symbol-name (car cell))) carriage-llm--registry))

(defun carriage-llm-available-models (&optional backend)
  "Return list of model strings for BACKEND (defaults to first registered)."
  (let* ((b (and backend (carriage-llm--norm-backend backend)))
         (cell (or (and b (assoc b carriage-llm--registry))
                   (car carriage-llm--registry))))
    (when cell
      (let* ((pl (cdr cell))
             (static (plist-get pl :models))
             (fn (plist-get pl :models-fn))
             (dyn (when (functionp fn)
                    (condition-case _ (funcall fn) (error nil)))))
        (or dyn static)))))

(defun carriage-llm--slug (s)
  "Return a lowercase slug for string S with non-alnum replaced by dashes."
  (let* ((s (format "%s" s))
         (s (downcase s)))
    (replace-regexp-in-string "[^a-z0-9]+" "-" s)))

(defun carriage-llm-candidates ()
  "Return combined candidates for model selection.

Preference order:
- If gptel is available, enumerate models from gptel’s registered backends
  and return BOTH:
    - \"gptel:PROVIDER:MODEL\" (provider is a slug from backend name)
    - \"gptel:MODEL\"          (back-compat)
- Otherwise, fall back to the internal registry and return \"backend:model\"."
  (cond
   ;; Prefer gptel as the source of truth when available
   ((and (boundp 'gptel--known-backends) gptel--known-backends)
    (let ((acc '()))
      (dolist (cell gptel--known-backends)
        (let* ((provider-name (car cell))          ;string key in the gptel registry
               (backend (cdr cell))
               (prov (carriage-llm--slug (or provider-name
                                             (ignore-errors (gptel-backend-name backend))
                                             "default")))
               (models (ignore-errors (gptel-backend-models backend))))
          (dolist (m (or models '()))
            (let ((mstr (if (symbolp m) (symbol-name m) (format "%s" m))))
              ;; New triple and old double forms
              (push (format "gptel:%s:%s" prov mstr) acc)
              (push (format "gptel:%s" mstr) acc)))))
      (delete-dups (nreverse acc))))
   (t
    ;; Fallback to internal simple registry
    (cl-loop for (b . pl) in carriage-llm--registry
             for bname = (symbol-name b)
             for models = (or (carriage-llm-available-models b) (plist-get pl :models))
             append (mapcar (lambda (m) (format "%s:%s" bname m))
                            (or models '()))))))

(provide 'carriage-llm-registry)
;;; carriage-llm-registry.el ends here
