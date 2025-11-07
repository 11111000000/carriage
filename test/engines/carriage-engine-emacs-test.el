;;; carriage-engine-emacs-test.el --- ERT for 'emacs engine patch skip -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-apply-engine)

(ert-deftest carriage-engine-emacs-patch-skip ()
  "When engine is 'emacs and op='patch, dispatch reports :status 'skip with friendly details."
  (let ((carriage-apply-engine 'emacs)
        (result nil))
    (carriage-apply-engine-dispatch
     'dry-run 'patch nil default-directory
     (lambda (res) (setq result res))
     (lambda (err) (setq result (list :status 'fail :error err))))
    ;; Allow run-at-time 0 callbacks to execute.
    (sleep-for 0.05)
    (should (plist-get result :engine))
    (should (eq (plist-get result :op) 'patch))
    (should (eq (plist-get result :status) 'skip))
    (should (string-match-p "unsupported" (or (plist-get result :details) "")))))

(provide 'carriage-engine-emacs-test)
;;; carriage-engine-emacs-test.el ends here
