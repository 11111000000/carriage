;;; carriage-apply-patch-async-test.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'carriage-apply)

(defun carriage-test--with-temp-repo (fn)
  "Create a temporary git repo and call FN with its root."
  (let* ((dir (make-temp-file "carriage-repo-" t)))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init")
      ;; Configure identity to avoid interactive prompts in ancillary tests
      (call-process "git" nil nil nil "config" "user.email" "carriage@example.com")
      (call-process "git" nil nil nil "config" "user.name" "Carriage"))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-apply-patch-async-git-forced ()
  "Async apply for patch should use git engine and succeed on a simple create diff."
  (carriage-test--with-temp-repo
   (lambda (root)
     (let* ((plan (list (list :version "1" :op 'patch :strip 1
                              :path "docs/intro.md"
                              :diff (mapconcat #'identity
                                               '("--- /dev/null"
                                                 "+++ b/docs/intro.md"
                                                 "@@ -0,0 +1 @@"
                                                 "+hello")
                                               "\n"))))
            (done nil)
            (report nil))
       (carriage-apply-plan-async
        plan root
        (lambda (rep) (setq done t report rep)))
       (let ((limit 200) (i 0))
         (while (and (not done) (< i limit))
           (sleep-for 0.05)
           (setq i (1+ i))))
       (should done)
       (let* ((sum (plist-get report :summary)))
         (should (numberp (plist-get sum :ok)))
         (should (> (plist-get sum :ok) 0)))))))
