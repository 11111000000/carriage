;;; carriage-sre-delim-collision-test.el --- Tests for SRE DELIM resync  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-parser)
(require 'carriage-logging)

(ert-deftest carriage-sre-delim-resync-basic ()
  "When payload contains lines equal to <<DELIM or :DELIM, parser should resync token and proceed."
  (carriage-clear-logs)
  (let* ((dir (make-temp-file "carriage-sre-resync-" t))
         (default-directory dir)
         (header '(:version "1" :op "sre" :file "x.txt" :delim "a1b2c3"))
         ;; Body with collision: extra '<<a1b2c3' inside FROM payload.
         (body (mapconcat #'identity
                          '("<<a1b2c3"
                            "hello"
                            "<<a1b2c3"
                            "world"
                            ":a1b2c3"
                            "<<a1b2c3"
                            "HELLO"
                            ":a1b2c3")
                          "\n")))
    (let* ((it (carriage-parse-sre header body dir)))
      (should (listp it))
      (should (equal (alist-get :op it) 'sre))
      (let ((pairs (alist-get :pairs it)))
        (should (listp pairs))
        (should (= (length pairs) 1))))
    ;; Ensure we logged a resync message
    (with-current-buffer (carriage-log-buffer)
      (goto-char (point-min))
      (should (re-search-forward "SRE: resynced DELIM" nil t)))))

(provide 'carriage-sre-delim-collision-test)
;;; carriage-sre-delim-collision-test.el ends here
