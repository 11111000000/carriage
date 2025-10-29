;;; carriage-apply-engine.el --- Apply Engine registry and selector  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)

(defgroup carriage-engines nil
  "Apply engines for Carriage (registry and selection)."
  :group 'carriage)

(defcustom carriage-apply-engine 'git
  "Active apply engine symbol. Default is 'git."
  :type '(choice symbol)
  :group 'carriage-engines)

(defcustom carriage-apply-engine-extra-args nil
  "Extra arguments for apply engines (per-kind list).
For 'git engine, this is a plist/alist with optional keys:
  :apply (list of strings)  ; appended to git apply invocation
  :check (list of strings)  ; appended to git apply --check
Example: '(:apply (\"--reject\" \"--whitespace=nowarn\") :check (\"--verbose\"))."
  :type '(repeat sexp)
  :group 'carriage-engines)

(defcustom carriage-apply-timeout-seconds nil
  "Per-step timeout in seconds for apply engines.
When non-nil, takes precedence over engine-specific timeouts."
  :type '(choice (const :tag "Engine default" nil) integer)
  :group 'carriage-engines)

(defvar carriage-apply-engine--registry nil
  "Alist registry of apply engines.
Shape: ((SYM . (:name STRING :dry-run FN :apply FN :capabilities FN?)) ...).")

(defun carriage-apply-engine--norm (sym)
  "Normalize engine SYM to a symbol."
  (cond
   ((symbolp sym) sym)
   ((stringp sym) (intern sym))
   (t (intern (format "%s" sym)))))

(defun carriage-register-apply-engine (sym name &rest kvs)
  "Register apply engine SYM with human NAME and callbacks in KVS.

KVS keys:
  :dry-run (op item repo on-done on-fail)
  :apply   (op item repo on-done on-fail)
  :capabilities (optional FN taking OP, returns plist)

Returns an unregister lambda."
  (let* ((s (carriage-apply-engine--norm sym))
         (entry (list :name (or name (symbol-name s))
                      :dry-run (plist-get kvs :dry-run)
                      :apply   (plist-get kvs :apply)
                      :capabilities (plist-get kvs :capabilities)))
         (cell (assoc s carriage-apply-engine--registry)))
    (if cell
        (setcdr cell entry)
      (push (cons s entry) carriage-apply-engine--registry))
    (carriage-log "Engine registered: %s (%s)" s (plist-get entry :name))
    (lambda ()
      (setq carriage-apply-engine--registry
            (assq-delete-all s carriage-apply-engine--registry))
      (carriage-log "Engine unregistered: %s" s))))

(defun carriage-apply-engine ()
  "Return currently selected engine symbol (fallback to first registered)."
  (let* ((cur (carriage-apply-engine--norm carriage-apply-engine)))
    (cond
     ((assoc cur carriage-apply-engine--registry) cur)
     ((carriage-apply-engine--first) (carriage-apply-engine--first))
     (t cur))))

(defun carriage-apply-engine--first ()
  "Return first registered engine symbol or nil."
  (caar carriage-apply-engine--registry))

(defun carriage-apply-engine--get (sym)
  "Return registry entry plist for engine SYM."
  (cdr (assoc (carriage-apply-engine--norm sym) carriage-apply-engine--registry)))

(defun carriage-available-apply-engines ()
  "Return list of available engines as strings for completion."
  (mapcar (lambda (cell)
            (let* ((s (car cell))
                   (pl (cdr cell))
                   (nm (or (plist-get pl :name) (symbol-name s))))
              (format "%s%s" (symbol-name s)
                      (if (and nm (not (string= nm (symbol-name s))))
                          (format " — %s" nm) ""))))
          (reverse carriage-apply-engine--registry)))

(defun carriage-apply-engine--parse-choice (choice)
  "Extract engine symbol from CHOICE string produced by carriage-available-apply-engines."
  (when (and (stringp choice)
             (string-match "\\=\\([^  —]+\\)" choice))
    (intern (match-string 1 choice))))

;;;###autoload
(defun carriage-select-apply-engine (&optional engine)
  "Interactively select active apply ENGINE from the registry."
  (interactive)
  (let* ((choices (carriage-available-apply-engines))
         (current (symbol-name (carriage-apply-engine)))
         (sel-s (cond
                 ((and engine (symbolp engine)) (symbol-name engine))
                 ((and engine (stringp engine)) engine)
                 ((and choices (completing-read
                                (format "Apply engine [%s]: " current)
                                choices nil t nil nil current)))
                 (t current)))
         (sym (or (and sel-s (carriage-apply-engine--parse-choice sel-s))
                  (carriage-apply-engine--norm sel-s))))
    (setq carriage-apply-engine sym)
    (carriage-log "Apply engine selected: %s" sym)
    (message "Apply engine: %s" sym)
    (force-mode-line-update t)
    sym))

(defun carriage-apply-engine-dispatch (kind op plan-item repo-root on-done on-fail)
  "Dispatch KIND ('dry-run|'apply) for OP via active engine with callbacks.
Callbacks are invoked on the main thread via run-at-time 0. Returns the engine token
(if the engine callback returns one), or nil otherwise."
  (let* ((eng (carriage-apply-engine))
         (rec (carriage-apply-engine--get eng))
         (cb  (pcase kind
                ((or 'dry-run :dry-run) (plist-get rec :dry-run))
                ((or 'apply   :apply)   (plist-get rec :apply))
                (_ nil))))
    (if (functionp cb)
        (condition-case e
            (let ((ret
                   (funcall cb op plan-item repo-root
                            (lambda (result)
                              (run-at-time 0 nil
                                           (lambda ()
                                             (when (functionp on-done)
                                               (funcall on-done result)))))
                            (lambda (err)
                              (run-at-time 0 nil
                                           (lambda ()
                                             (when (functionp on-fail)
                                               (funcall on-fail err))))))))
              ret)
          (error
           (carriage-log "Engine dispatch error (%s): %s" eng (error-message-string e))
           (when (functionp on-fail)
             (run-at-time 0 nil (lambda () (funcall on-fail e))))
           nil))
      (progn
        (carriage-log "No %s callback for engine %s (op=%s)" kind eng op)
        (when (functionp on-fail)
          (run-at-time 0 nil
                       (lambda ()
                         (funcall on-fail (list :error 'no-callback :engine eng :kind kind)))))
        nil))))

(provide 'carriage-apply-engine)
;;; carriage-apply-engine.el ends here
