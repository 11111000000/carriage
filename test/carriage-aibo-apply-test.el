;;; carriage-aibo-apply-test.el --- AIBO apply tests (literal-only) -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(ignore-errors (require 'carriage-op-aibo))

(defun carriage-aibo-apply-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-aibo-apply-test--write (dir rel content)
  "Write CONTENT to DIR/REL, creating directories."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-aibo-apply-test--read (dir rel)
  "Read file DIR/REL and return its contents."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-aibo-dry-run-and-apply ()
  "AIBO dry-run returns matches and apply rewrites file (literal-only)."
  (skip-unless (and (fboundp 'carriage-dry-run-aibo)
                    (fboundp 'carriage-apply-aibo)))
  (let* ((dir (make-temp-file "carriage-aibo-apply-" t)))
    (unwind-protect
        (progn
          ;; init repo (WIP branch usage may be external to this unit; we only check file change)
          (should (zerop (carriage-aibo-apply-test--git dir "init")))
          (should (zerop (carriage-aibo-apply-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-aibo-apply-test--git dir "config" "user.name" "Tester")))
          (carriage-aibo-apply-test--write dir "x.txt" "foo\n")
          (should (zerop (carriage-aibo-apply-test--git dir "add" "--" "x.txt")))
          (should (zerop (carriage-aibo-apply-test--git dir "commit" "-m" "init")))
          ;; plan: AIBO foo->bar
          (let* ((item (list (cons :version "1")
                             (cons :op 'aibo)
                             (cons :file "x.txt")
                             (cons :pairs
                                   (list (list (cons :from "foo")
                                               (cons :to   "bar")
                                               (cons :opts '(:occur first)))))))
                 (dry (carriage-dry-run-aibo item dir)))
            (should (eq (plist-get dry :status) 'ok))
            (should (equal (plist-get dry :matches) 1))
            ;; Apply
            (let ((ap (carriage-apply-aibo item dir)))
              (should (eq (plist-get ap :status) 'ok))
              (should (equal (carriage-aibo-apply-test--read dir "x.txt") "bar\n")))
            ;; Second apply is NOOP → 'skip
            (let ((ap2 (carriage-apply-aibo item dir)))
              (should (eq (plist-get ap2 :status) 'skip))
              (should (numberp (plist-get ap2 :changed-bytes)))
              (should (zerop (plist-get ap2 :changed-bytes)))))))
    (ignore-errors (delete-directory dir t)))))

(provide 'carriage-aibo-apply-test)
;;; carriage-aibo-apply-test.el ends here
