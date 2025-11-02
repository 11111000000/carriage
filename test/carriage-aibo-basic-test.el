;;; carriage-aibo-basic-test.el --- Tests for AIBO op  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage)
(ignore-errors (require 'carriage-op-aibo))

(defun carriage-aibo-basic-test--write (dir rel content)
  "Write CONTENT to DIR/REL, creating directories as needed."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-aibo-basic-test--read (dir rel)
  "Read file DIR/REL and return contents."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-aibo-apply-change-ok ()
  "AIBO apply should modify file content for a literal replacement and report matches/Δbytes."
  (skip-unless (and (fboundp 'carriage-apply-aibo)))
  (let* ((dir (make-temp-file "carriage-aibo-" t)))
    (unwind-protect
        (progn
          (carriage-aibo-basic-test--write dir "x.txt" "Hello\n")
          (let* ((item (list :version "1" :op 'aibo :file "x.txt"
                             :pairs (list (list :from "Hello" :to "Hello, world" :opts '(:occur first))))))
            (res (carriage-apply-aibo item dir)))
          (should (eq (plist-get res :status) 'ok))
          (should (equal (carriage-aibo-basic-test--read dir "x.txt") "Hello, world\n"))
          (should (numberp (plist-get res :matches)))
          (should (>= (plist-get res :matches) 1))
          (should (numberp (plist-get res :changed-bytes)))
          (should (> (plist-get res :changed-bytes) 0))))
    (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-aibo-apply-noop-skip ()
  "AIBO apply should return 'skip when after==before, with matches=1 and Δbytes=0."
  (skip-unless (and (fboundp 'carriage-apply-aibo)))
  (let* ((dir (make-temp-file "carriage-aibo-noop-" t)))
    (unwind-protect
        (progn
          (carriage-aibo-basic-test--write dir "x.txt" "abc\n")
          (let* ((item (list :version "1" :op 'aibo :file "x.txt"
                             :pairs (list (list :from "abc" :to "abc" :opts '(:occur first))))))
            (res (carriage-apply-aibo item dir)))
          (should (eq (plist-get res :status) 'skip))
          (should (equal (plist-get res :matches) 1))
          (should (equal (plist-get res :changed-bytes) 0))
          (should (equal (carriage-aibo-basic-test--read dir "x.txt") "abc\n"))))
    (ignore-errors (delete-directory dir t)))))

(provide 'carriage-aibo-basic-test)
;;; carriage-aibo-basic-test.el ends here
