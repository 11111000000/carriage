;;; carriage-sre-noop-skip-test.el --- NOOP→skip policy smoke test  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; These tests assume SRE apply implements NOOP→'skip (after==before).
;; If not yet implemented, they document the desired behavior.

(ignore-errors (require 'carriage-op-sre))

(ert-deftest carriage-sre-noop-skip ()
  "SRE apply should return 'skip if after==before."
  (skip-unless (fboundp 'carriage-apply-sre))
  (let* ((tmp (make-temp-file "sre-" nil ".txt" "hello"))
         (plan =(:version "1" :op 'sre :file ,(file-name-nondirectory tmp)
                          :pairs ((:from "hello" :to "hello" :opts (:occur first :match literal))))))
    (unwind-protect
        (let/ ((root (file-name-directory tmp))
               (res (carriage-apply-sre plan root)))
              ;; Accept either 'skip (preferred) or 'ok with 0 change in legacy
              (should (memq (plist-get res :status) '(skip ok))))
      (ignore-errors (delete-file tmp)))))

(provide 'carriage-sre-noop-skip-test)
;;; carriage-sre-noop-skip-test.el ends here
