;;; carriage-engine-git.el --- Async Git apply engine  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-apply-engine)

(defgroup carriage-engine-git nil
  "Git-based apply engine for Carriage."
  :group 'carriage-engines
  :prefix "carriage-engine-git-")

(defcustom carriage-engine-git-timeout-seconds 30
  "Per-process timeout in seconds for git engine operations."
  :type 'integer :group 'carriage-engine-git)

(defun carriage-engine-git--policy-index-p ()
  "Return non-nil when staging policy is 'index."
  (and (boundp 'carriage-apply-stage-policy)
       (eq carriage-apply-stage-policy 'index)))

(defun carriage-engine-git--join-cmd (argv)
  "Return a readable single-line shell-ish string from ARGV."
  (mapconcat (lambda (s)
               (cond
                ((string-match-p "[ \t\"'\\\\]" s) (format "%S" s))
                (t s)))
             argv " "))

(defun carriage-engine-git--cleanup-token (token)
  "Cleanup buffers/files/timers associated with TOKEN."
  (let ((out (plist-get token :stdout-buf))
        (err (plist-get token :stderr-buf))
        (tmp (plist-get token :patch-file))
        (tmr (plist-get token :timer)))
    (when (and (bufferp out) (buffer-live-p out))
      (ignore-errors (kill-buffer out)))
    (when (and (bufferp err) (buffer-live-p err))
      (ignore-errors (kill-buffer err)))
    (when (and (stringp tmp) (file-exists-p tmp))
      (ignore-errors (delete-file tmp)))
    (when (timerp tmr)
      (ignore-errors (cancel-timer tmr)))))

(defun carriage-engine-git-abort (token)
  "Attempt to abort the running process associated with TOKEN."
  (let ((proc (plist-get token :process)))
    (when (process-live-p proc)
      (plist-put token :aborted t)
      (ignore-errors (interrupt-process proc))
      (ignore-errors (kill-process proc))
      t)))

(defun carriage-engine-git--start (root argv token on-done on-fail)
  "Start process git ARGV in ROOT, wire TOKEN state, set sentinel and timeout."
  (when (file-remote-p (or root ""))
    (funcall on-fail (list :error 'remote-not-supported :details "TRAMP is disabled in v1"))
    (cl-return-from carriage-engine-git--start nil))
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (name (format "carriage-git:%s" (or (plist-get token :op) "op")))
         (out (generate-new-buffer " *carriage-git-stdout*"))
         (err (generate-new-buffer " *carriage-git-stderr*"))
         (cmd (cons "git" argv))
         (cmd-str (carriage-engine-git--join-cmd cmd))
         (done-called nil)
         (finalize
          (lambda (okp exit-code reason)
            (unless done-called
              (setq done-called t)
              (let* ((stdout (with-current-buffer out (buffer-string)))
                     (stderr (with-current-buffer err (buffer-string)))
                     (res (list :engine 'git
                                :exit exit-code
                                :stdout stdout
                                :stderr stderr
                                :stdout-bytes (string-bytes stdout)
                                :stderr-bytes (string-bytes stderr)
                                :pid (plist-get token :pid)
                                :op (plist-get token :op)
                                :path (plist-get token :path)
                                :reason reason)))
                (carriage-log "engine[git] exit: code=%s stdout=%dB stderr=%dB reason=%s"
                              exit-code
                              (plist-get res :stdout-bytes)
                              (plist-get res :stderr-bytes)
                              (or reason 'ok))
                (carriage-engine-git--cleanup-token token)
                (if okp
                    (funcall on-done res)
                  (funcall on-fail res)))))))
    (carriage-log "engine[git] exec: dir=%s cmd=%s" default-directory cmd-str)
    (let* ((proc
            (make-process
             :name name
             :buffer out
             :command cmd
             :stderr err
             :noquery t
             :sentinel
             (lambda (p ev)
               (ignore ev)
               (condition-case e
                   (let ((stat (process-status p)))
                     (when (memq stat '(exit signal))
                       (let* ((exit (if (eq stat 'exit)
                                        (process-exit-status p)
                                      128))
                              (reason (cond
                                       ((plist-get token :aborted) 'aborted)
                                       ((plist-get token :timed-out) 'timeout)
                                       (t nil))))
                         (funcall finalize (and (numberp exit) (zerop exit)) exit reason))))
                 (error
                  (carriage-log "engine[git] sentinel error: %s" (error-message-string e))
                  (funcall finalize nil 128 'sentinel-error))))))
           (pid (process-id proc))
           (timeout (or (and (numberp carriage-engine-git-timeout-seconds)
                             carriage-engine-git-timeout-seconds)
                        30))
           (timer
            (run-at-time timeout nil
                         (lambda ()
                           (when (process-live-p proc)
                             (plist-put token :timed-out t)
                             (carriage-log "engine[git] timeout: pid=%s after %ss" pid timeout)
                             (ignore-errors (interrupt-process proc))
                             (ignore-errors (kill-process proc)))))))
      (plist-put token :process proc)
      (plist-put token :stdout-buf out)
      (plist-put token :stderr-buf err)
      (plist-put token :pid pid)
      (plist-put token :timer timer)
      (carriage-log "engine[git] pid=%s" pid)
      token)))

(defun carriage-engine-git--write-patch (diff-str)
  "Write DIFF-STR to a temporary file and return its filename."
  (let ((f (make-temp-file "carriage-patch-" nil ".diff")))
    (with-temp-file f
      (insert (or diff-str "")))
    f))

(defun carriage-engine-git--args-apply-check (strip patch-file)
  "Build argv for git apply --check with STRIP and PATCH-FILE."
  (append '("apply" "--check" "--verbose")
          (when (and (integerp strip) (>= strip 0))
            (list "-p" (number-to-string strip)))
          (list patch-file)))

(defun carriage-engine-git--args-apply (strip patch-file)
  "Build argv for git apply (maybe --index) with STRIP and PATCH-FILE."
  (append '("apply")
          (when (carriage-engine-git--policy-index-p) '("--index"))
          (when (boundp 'carriage-git-apply-extra-args)
            (and (listp carriage-git-apply-extra-args) carriage-git-apply-extra-args))
          (when (and (integerp strip) (>= strip 0))
            (list "-p" (number-to-string strip)))
          (list patch-file)))

(defun carriage-engine-git--log-begin (kind item)
  "Log begin message for KIND (:dry-run/:apply) and ITEM."
  (let ((op  (alist-get :op item))
        (pth (or (alist-get :path item) (alist-get :file item)))
        (strip (alist-get :strip item))
        (diff (alist-get :diff item)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s bytes=%s strip=%s"
                  kind op (or pth "-")
                  (if (stringp diff) (string-bytes diff) 0)
                  (or strip 1))))

(defun carriage-engine-git--dry-run-patch (item root on-done on-fail)
  "Dry-run patch via git apply --check."
  (let* ((diff (alist-get :diff item))
         (strip (or (alist-get :strip item) 1))
         (patch-file (carriage-engine-git--write-patch diff))
         (argv (carriage-engine-git--args-apply-check strip patch-file))
         (token (list :engine 'git :op 'patch
                      :path (or (alist-get :path item) "-")
                      :patch-file patch-file)))
    (carriage-engine-git--log-begin :dry-run item)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-patch (item root on-done on-fail)
  "Apply patch via git apply (maybe --index)."
  (let* ((diff (alist-get :diff item))
         (strip (or (alist-get :strip item) 1))
         (patch-file (carriage-engine-git--write-patch diff))
         (argv (carriage-engine-git--args-apply strip patch-file))
         (token (list :engine 'git :op 'patch
                      :path (or (alist-get :path item) "-")
                      :patch-file patch-file)))
    (carriage-engine-git--log-begin :apply item)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--noop (op item root on-done _on-fail)
  "NOOP async task for ops not implemented in engine."
  (ignore root)
  (carriage-log "engine[git] noop: op=%s path=%s" op (or (alist-get :file item) (alist-get :path item) "-"))
  (run-at-time 0 nil
               (lambda ()
                 (funcall on-done (list :engine 'git :exit 0 :op op :status 'noop)))))

(defun carriage-engine-git--ensure-parent-dir (root relpath)
  "Ensure parent directory exists for RELPATH under ROOT."
  (let* ((abs (expand-file-name relpath (file-name-as-directory (expand-file-name root))))
         (dir (file-name-directory abs)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))))

(defun carriage-engine-git--apply-add (item root on-done on-fail)
  "Stage file via git add."
  (let* ((file (or (alist-get :file item) (alist-get :path item) "-"))
         (argv (list "add" "--" file))
         (token (list :engine 'git :op 'create :path file)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'create file)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-rm (item root on-done on-fail)
  "Remove file via git rm -f."
  (let* ((file (or (alist-get :file item) (alist-get :path item) "-"))
         (argv (list "rm" "-f" "--" file))
         (token (list :engine 'git :op 'delete :path file)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'delete file)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-mv (item root on-done on-fail)
  "Rename/move via git mv, ensuring destination directory exists."
  (let* ((from (or (alist-get :from item) "-"))
         (to   (or (alist-get :to item) "-")))
    (ignore-errors (carriage-engine-git--ensure-parent-dir root to))
    (let* ((argv (list "mv" "--" from to))
           (token (list :engine 'git :op 'rename :path (format "%s -> %s" from to))))
      (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'rename (format "%s -> %s" from to))
      (carriage-engine-git--start root argv token on-done on-fail))))

(defun carriage-engine-git-dry-run (op item repo on-done on-fail)
  "Engine entrypoint: :dry-run dispatcher."
  (pcase op
    ('patch  (carriage-engine-git--dry-run-patch item repo on-done on-fail))
    (_       (carriage-engine-git--noop op item repo on-done on-fail))))

(defun carriage-engine-git-apply (op item repo on-done on-fail)
  "Engine entrypoint: :apply dispatcher."
  (pcase op
    ('patch  (carriage-engine-git--apply-patch item repo on-done on-fail))
    ('delete
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-rm item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    ('rename
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-mv item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    ('create
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-add item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    (_ (carriage-engine-git--noop op item repo on-done on-fail))))

(defun carriage-engine-git-capabilities (_op)
  "Return engine capability plist (static for v1)."
  (list :name "Git apply engine"
        :ops '(patch create delete rename sre)
        :async t
        :timeout t))

(carriage-register-apply-engine
 'git "Git apply engine"
 :dry-run #'carriage-engine-git-dry-run
 :apply   #'carriage-engine-git-apply
 :capabilities #'carriage-engine-git-capabilities)

(provide 'carriage-engine-git)
;;; carriage-engine-git.el ends here
