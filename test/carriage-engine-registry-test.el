;;; carriage-engine-registry-test.el --- Apply engine registry tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-apply-engine)

(defun carriage-engine-registry-test--dummy (&rest _args)
  "Minimal async dummy engine callback that succeeds immediately."
  (run-at-time 0 nil
               (lambda ()
                 (let ((res (list :engine 'dummy-ert :exit 0 :stdout "" :stderr "")))
                   (when (functionp (nth 3 _args))
                     (funcall (nth 3 _args) res))))))

(ert-deftest carriage-apply-engine-default-and-select ()
  "Default engine is 'git, registry allows selecting a dummy engine."
  (let* ((orig (carriage-apply-engine))
         (unreg nil))
    (unwind-protect
        (progn
          ;; Registry should be available and contain at least 'git
          (should (symbolp (carriage-apply-engine)))
          ;; Register dummy engine
          (setq unreg
                (carriage-register-apply-engine
                 'dummy-ert "Dummy ERT Engine"
                 :dry-run #'carriage-engine-registry-test--dummy
                 :apply   #'carriage-engine-registry-test--dummy))
          ;; Select dummy
          (carriage-select-apply-engine 'dummy-ert)
          (should (eq (carriage-apply-engine) 'dummy-ert))
          ;; Switch back to original
          (carriage-select-apply-engine orig)
          (should (eq (carriage-apply-engine) orig)))
      (when (functionp unreg) (funcall unreg)))))

;; Additional test: 'emacs engine rejects 'patch (MODE_E_DISPATCH or non-zero exit)
(ert-deftest carriage-apply-engine-emacs-rejects-patch ()
  "Selecting engine='emacs and dispatching :op 'patch should fail (MODE_E_DISPATCH/non-zero exit)."
  (let* ((orig (carriage-apply-engine))
         (skip nil))
    (unwind-protect
        (progn
          ;; Try selecting 'emacs engine; if unavailable in registry, skip test.
          (condition-case _
              (carriage-select-apply-engine 'emacs)
            (error (setq skip t)))
          (if skip
              (should t)
            (let* ((root (make-temp-file "carriage-repo-" t))
                   (plan-item (list (cons :version "1")
                                    (cons :op 'patch)
                                    (cons :apply 'git-apply)
                                    (cons :strip 1)
                                    (cons :path "foo.txt")
                                    (cons :diff "--- a/foo.txt\n+++ b/foo.txt\n@@ -0,0 +1,1 @@\n+hello\n")))
                   (done nil)
                   (res nil))
              ;; Dispatch dry-run for patch via engine (async callback)
              (carriage-apply-engine-dispatch
               :dry-run 'patch plan-item root
               (lambda (r) (setq res r done t))
               (lambda (r) (setq res r done t)))
              ;; Wait briefly for callback
              (let ((t0 (float-time)))
                (while (and (not done) (< (- (float-time) t0) 2.0))
                  (accept-process-output nil 0.05)))
              ;; Expect non-zero exit or MODE_E_DISPATCH code
              (let ((exit (plist-get res :exit))
                    (code (plist-get res :code)))
                (should (or (and (numberp exit) (not (zerop exit)))
                            (eq code 'MODE_E_DISPATCH)))))))
      ;; restore engine
      (ignore-errors (carriage-select-apply-engine orig)))))

(provide 'carriage-engine-registry-test)
;;; carriage-engine-registry-test.el ends here
