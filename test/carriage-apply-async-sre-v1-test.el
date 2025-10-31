;;; carriage-apply-async-sre-v1-test.el --- Async apply SRE v1 without WIP -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-apply-async-test--wait (pred timeout-sec)
  "Wait until (funcall PRED) is non-nil or TIMEOUT-SEC elapses."
  (let* ((deadline (+ (float-time) (max 0 (or timeout-sec 1))))
         (ok nil))
    (while (and (not ok) (< (float-time) deadline))
      (setq ok (ignore-errors (funcall pred)))
      (unless ok (accept-process-output nil 0.05)))
    ok))

(ert-deftest carriage-apply-plan-async-sre-v1 ()
  "Apply a single SRE change asynchronously with WIP disabled."
  (let* ((tmp (make-temp-file "carriage-sre-apply-" t))
         (default-directory (file-name-as-directory tmp))
         (file (expand-file-name "a.txt" tmp))
         (carriage-apply-require-wip-branch nil)
         (done nil)
         (report nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello\nworld\n"))
          (let* ((item (list (cons :version "1")
                             (cons :op 'sre)
                             (cons :file "a.txt")
                             (cons :pairs
                                   (list (list (cons :from "hello")
                                               (cons :to "hi")
                                               (cons :opts '(:occur first :match literal))))))))
            (carriage-apply-plan-async
             (list item) tmp
             (lambda (rep) (setq report rep done t))))
          (should (carriage-apply-async-test--wait (lambda () done) 5))
          (should (alist-get :summary report))
          (let* ((abs (expand-file-name "a.txt" tmp))
                 (s (with-temp-buffer (insert-file-contents abs) (buffer-string))))
            (should (string-match-p "^hi$" (car (last (split-string s "\n" t)))))))
      (ignore-errors (delete-directory tmp t)))))
(provide 'carriage-apply-async-sre-v1-test)
;;; carriage-apply-async-sre-v1-test.el ends here
