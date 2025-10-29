;;; carriage-apply-async-token-test.el --- Async apply token/callback tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)

(defun carriage-apply-async-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-apply-async-test--write (dir rel content)
  "Write CONTENT to DIR/REL, creating directories as needed."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-apply-async-returns-token-and-calls-callback ()
  "Async apply should return a token and invoke the callback later."
  (let* ((dir (make-temp-file "carriage-apply-async-" t))
         (done nil)
         (report nil))
    (unwind-protect
        (progn
          ;; init repo
          (should (zerop (carriage-apply-async-test--git dir "init")))
          (should (zerop (carriage-apply-async-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-apply-async-test--git dir "config" "user.name" "Tester")))
          ;; plan: create a file
          (let* ((default-directory (file-name-as-directory dir))
                 (item (list (cons :version "1")
                             (cons :op 'create)
                             (cons :file "a/b/new.txt")
                             (cons :content "hello\n")
                             (cons :mkdir t)))
                 (tok (carriage-apply-plan-async
                       (list item) dir
                       (lambda (rep) (setq report rep done t)))))
            (should (listp tok))
            (should (functionp (plist-get tok :abort-fn)))
            ;; allow timers/sentinels to run
            (let ((deadline (+ (float-time) 3.0)))
              (while (and (not done) (< (float-time) deadline))
                (sleep-for 0.01)))
            (should (consp report))
            (let* ((summary (plist-get report :summary)))
              (should (numberp (plist-get summary :ok)))
              (should (> (plist-get summary :ok) 0))))))
    (ignore-errors (delete-directory dir t))))

(provide 'carriage-apply-async-token-test)
;;; carriage-apply-async-token-test.el ends here
