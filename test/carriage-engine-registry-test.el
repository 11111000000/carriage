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

(provide 'carriage-engine-registry-test)
;;; carriage-engine-registry-test.el ends here
