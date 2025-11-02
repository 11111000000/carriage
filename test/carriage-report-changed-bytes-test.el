;;; carriage-report-changed-bytes-test.el --- Report should show Δbytes in details  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(require 'carriage-report)

(ert-deftest carriage-report-renders-changed-bytes-in-details ()
  "When item has :changed-bytes, details should include Δbytes=…"
  (let* ((rep '(:summary (:ok 1 :fail 0 :skipped 0)
                         :items ((:op 'sre :file "x.txt" :status 'ok
                                      :matches 1 :changed-bytes 5
                                      :details "Applied 1 replacements"))))
         (buf (carriage-report-render rep))
         (txt ""))
    (with-current-buffer buf
      (setq txt (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "Δbytes=5" txt))))

(provide 'carriage-report-changed-bytes-test)
;;; carriage-report-changed-bytes-test.el ends here
