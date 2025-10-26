;;; carriage-report-messages-test.el --- Report messages aggregation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-parser)
(require 'carriage-apply)

(defun carriage-report-messages--write-file (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-report-messages-range-clamp ()
  "SRE dry-run should append a range clamp warning and aggregate it in report :messages."
  (let* ((dir (make-temp-file "carriage-msg-clamp-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (carriage-report-messages--write-file dir "x.txt" "a\nb\nc\n")
          (let* ((plan (list (list (cons :version "1")
                                   (cons :op 'sre)
                                   (cons :file "x.txt")
                                   (cons :pairs (list (list (cons :from "b")
                                                            (cons :to   "B")
                                                            (cons :opts '(:occur first :match literal :range (:start-line 0 :end-line 999)))))))))
                 (rep (carriage-dry-run-plan plan dir))
                 (msgs (plist-get rep :messages)))
            (should (listp msgs))
            (should (seq-some (lambda (m) (eq (plist-get m :code) 'SRE_W_RANGE_CLAMP)) msgs))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-report-messages-delim-resync ()
  "SRE parse should resync DELIM on collision and dry-run should aggregate a resync warning."
  (let* ((dir (make-temp-file "carriage-msg-resync-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Target file
          (carriage-report-messages--write-file dir "x.txt" "hello\n")
          ;; Header/body that collide: payload contains a literal opening marker line.
          (let* ((hdr (list :version "1" :op "sre" :file "x.txt" :delim "aaaaaa"))
                 (body (mapconcat #'identity
                                  '("<<aaaaaa"
                                    "hello"
                                    "<<aaaaaa"
                                    ":aaaaaa"
                                    "<<aaaaaa"
                                    "world"
                                    ":aaaaaa")
                                  "\n"))
                 (item (carriage-parse 'sre hdr body dir))
                 (rep  (carriage-dry-run-plan (list item) dir))
                 (msgs (plist-get rep :messages)))
            (should (listp msgs))
            (should (seq-some (lambda (m) (eq (plist-get m :code) 'SRE_W_DELIM_RESYNC)) msgs))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-report-messages-test.el ends here
