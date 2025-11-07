;;; carriage-engine-emacs-udiff-test.el --- ERT for 'emacs udiff (create) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-apply-engine)

(defun carriage-test--with-temp-dir (fn)
  "Create a temporary directory and call FN with its path."
  (let ((dir (make-temp-file "carriage-tmp-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defconst carriage-engine-emacs-udiff-test--diff
  (mapconcat #'identity
             '("--- /dev/null"
               "+++ b/CHANGELOG.md"
               "@@ -0,0 +1,3 @@"
               "+## Changelog"
               "+- Initial"
               "+- Notes")
             "\n")
  "Minimal udiff (create) for testing.")

(ert-deftest carriage-engine-emacs-udiff-dry-run-and-apply-create ()
  "When emacs udiff is enabled, dispatch should dry-run and apply create diff."
  (let ((carriage-apply-engine 'emacs)
        (carriage-engine-emacs-enable-udiff t))
    (carriage-test--with-temp-dir
     (lambda (root)
       ;; Dry-run
       (let ((done nil) (res nil))
         (carriage-apply-engine-dispatch
          :dry-run 'patch
          (list :op 'patch :strip 1 :diff carriage-engine-emacs-udiff-test--diff)
          root
          (lambda (r) (setq res r done t))
          (lambda (e) (setq res e done t)))
         (let ((limit 100) (i 0))
           (while (and (not done) (< i limit))
             (sleep-for 0.01) (setq i (1+ i))))
         (should done)
         (should (eq (plist-get res :engine) 'emacs))
         (should (numberp (plist-get res :exit)))
         (should (zerop (plist-get res :exit))))
       ;; Apply
       (let ((done nil) (res nil))
         (carriage-apply-engine-dispatch
          :apply 'patch
          (list :op 'patch :strip 1 :diff carriage-engine-emacs-udiff-test--diff)
          root
          (lambda (r) (setq res r done t))
          (lambda (e) (setq res e done t)))
         (let ((limit 100) (i 0))
           (while (and (not done) (< i limit))
             (sleep-for 0.01) (setq i (1+ i))))
         (should done)
         (should (eq (plist-get res :engine) 'emacs))
         (should (numberp (plist-get res :exit)))
         (should (zerop (plist-get res :exit))))
       ;; Verify file created with expected content
       (let* ((file (expand-file-name "CHANGELOG.md" root)))
         (should (file-exists-p file))
         (with-temp-buffer
           (insert-file-contents file)
           (let ((txt (buffer-string)))
             (should (string-match-p "^## Changelog\n- Initial\n- Notes\n\\'" txt)))))))))

(provide 'carriage-engine-emacs-udiff-test)
;;; carriage-engine-emacs-udiff-test.el ends here
