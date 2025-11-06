;;; carriage-sre-delim-rewrite-test.el --- Tests for DELIM rewriter  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-sre-delim)

(ert-deftest carriage-sre-delim-rewrite-positive ()
  "Only exact whole-line markers are rewritten."
  (let* ((txt "<<deadbe\nhello\n:deadbe\n")
         (res (carriage-sre-rewrite-delim-markers txt "deadbe" "a1b2c3"))
         (lines (split-string res "\n" t)))
    (should (string= (nth 0 lines) "<<a1b2c3"))
    (should (string= (nth 1 lines) "hello"))
    (should (string= (nth 2 lines) ":a1b2c3"))))

(ert-deftest carriage-sre-delim-rewrite-negative-similar ()
  "Similar-looking lines must not be rewritten."
  (let* ((txt "<< DEADBE\n: DELIM\n::DELIM\nend\n")
         (res (carriage-sre-rewrite-delim-markers txt "deadbe" "a1b2c3")))
    (should (string= res txt))))

(provide 'carriage-sre-delim-rewrite-test)
;;; carriage-sre-delim-rewrite-test.el ends here
