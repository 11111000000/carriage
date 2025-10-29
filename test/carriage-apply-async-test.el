;;; carriage-apply-async-test.el --- Async apply pipeline tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-report)
(require 'carriage-utils)
(require 'carriage-logging)

(defun carriage-apply-async-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-apply-async-test--git-out (dir &rest args)
  "Run git ARGS in DIR and return trimmed stdout."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(defun carriage-apply-async-test--wait (pred &optional timeout)
  "Wait up to TIMEOUT seconds until PRED returns non-nil. Return that value or nil."
  (let* ((deadline (+ (float-time) (or timeout 5)))
         (res nil))
    (while (and (not (setq res (ignore-errors (funcall pred))))
                (< (float-time) deadline))
      (sleep-for 0.05))
    res))

(ert-deftest carriage-apply-plan-async-create-sre-index ()
  "Apply create→sre with policy='index asynchronously; callback fires; index updated."
  (let* ((dir (make-temp-file "carriage-apply-async-" t))
         (carriage-apply-stage-policy 'index))
    (unwind-protect
        (progn
          ;; Init repo
          (should (zerop (carriage-apply-async-test--git dir "init")))
          (should (zerop (carriage-apply-async-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-apply-async-test--git dir "config" "user.name" "Tester")))
          ;; Build a plan: create a.txt with A\n, then sre A->B
          (let* ((create (list (cons :version "1")
                               (cons :op 'create)
                               (cons :file "a.txt")
                               (cons :content "A\n")
                               (cons :mkdir t)))
                 (sre    (list (cons :version "1")
                               (cons :op 'sre)
                               (cons :file "a.txt")
                               (cons :pairs (list (list :from "A" :to "B" :opts '(:occur first :match literal))))))
                 (plan (list create sre))
                 (done nil)
                 (report nil)
                 (tok (carriage-apply-plan-async
                       plan dir
                       (lambda (rep)
                         (setq report rep)
                         (setq done t)))))
            ;; Token should be a plist with :abort-fn
            (should (and (listp tok) (functionp (plist-get tok :abort-fn))))
            ;; Wait for completion
            (should (carriage-apply-async-test--wait (lambda () done) 10))
            ;; Verify working tree content
            (let* ((abs (expand-file-name "a.txt" dir)))
              (should (file-exists-p abs))
              (should (string= (carriage-read-file-string abs) "B\n")))
            ;; Verify index contains the file (staged)
            (let* ((cached (carriage-apply-async-test--git-out dir "diff" "--cached" "--name-only")))
              (should (string-match-p "^a.txt\\b" cached)))
            ;; Engine logs are optional depending on packaging; presence not enforced here.))
            (ignore-errors (delete-directory dir t)))))))

(provide 'carriage-apply-async-test)
;;; carriage-apply-async-test.el ends here
