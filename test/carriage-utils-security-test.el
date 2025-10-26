;;; carriage-utils-security-test.el --- Path security/TRAMP tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-utils)

(defun carriage--test--tempdir ()
  (file-name-as-directory (make-temp-file "carriage-sec-" t)))

(ert-deftest carriage-normalize-path-refuses-absolute ()
  "Absolute RELPATH must be rejected."
  (let* ((root (carriage--test--tempdir))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-normalize-path root "/etc/passwd")
          (setq raised nil))
      (error
       (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'OPS_E_PATH)))))

(ert-deftest carriage-normalize-path-refuses-dotdot ()
  "RELPATH with .. must be rejected."
  (let* ((root (carriage--test--tempdir))
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-normalize-path root "../evil.txt")
          (setq raised nil))
      (error
       (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'OPS_E_PATH)))))

(ert-deftest carriage-normalize-path-refuses-tramp-root ()
  "TRAMP root is not supported in v1."
  (let* ((root "/ssh:demo@host:/tmp/repo")
         (raised '())
         (sym nil))
    (condition-case e
        (progn
          (carriage-normalize-path root "x.txt")
          (setq raised nil))
      (error
       (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'IO_E_PATH)))))

(ert-deftest carriage-normalize-path-refuses-symlink-outside ()
  "Symlink chain escaping the root must be rejected."
  (let* ((outer (carriage--test--tempdir))
         (root  (carriage--test--tempdir))
         (link  (expand-file-name "out" root))
         (raised '())
         (sym nil))
    ;; Point symlink =root/out' to =outer'
    (make-symbolic-link outer link t)
    (condition-case e
        (progn
          (carriage-normalize-path root "out/file.txt")
          (setq raised nil))
      (error
       (setq raised t sym (car e))))
    (should raised)
    (should (eq sym (carriage-error-symbol 'OPS_E_PATH)))))

(ert-deftest carriage-normalize-path-ok-under-root ()
  "Valid relative path stays within the root and returns truename."
  (let* ((root (carriage--test--tempdir))
         (sub  (expand-file-name "sub" root))
         (abs  nil))
    (make-directory sub t)
    (setq abs (carriage-normalize-path root "sub/x.txt"))
    (should (string-prefix-p (file-truename root) (file-name-directory abs)))))

;;; carriage-utils-security-test.el ends here
