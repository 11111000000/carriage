;;; carriage-create-delim-rewrite-test.el --- Rewrite :delim markers tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-utils)

(ert-deftest carriage-delim-rewrite-basic ()
  "Rewrite both open and close markers at BOL; keep inner payload intact."
  (let* ((old "cafe01")
         (new "deadbe")
         (src (concat
               "<<cafe01\n"
               "payload line with cafe01 inside should remain cafe01\n"
               ":cafe01\n"))
         (dst (carriage--sre--rewrite-delim-markers src old new)))
    (should (string-match-p (concat "^<<" new) dst))
    (should (string-match-p (concat "^:" new) dst))
    (should (string-match-p "remain cafe01" dst))
    (should-not (string-match-p (concat "^<<" old) dst))
    (should-not (string-match-p (concat "^:" old) dst))))

(ert-deftest carriage-delim-rewrite-idempotent-when-same ()
  "When OLD equals NEW the function should return identical text."
  (let* ((tok "a1b2c3")
         (src (concat "<<a1b2c3\nhello\n:a1b2c3\n"))
         (dst (carriage--sre--rewrite-delim-markers src tok tok)))
    (should (string= src dst))))

(provide 'carriage-create-delim-rewrite-test)
;;; carriage-create-delim-rewrite-test.el ends here
