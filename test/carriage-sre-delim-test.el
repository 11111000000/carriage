;;; carriage-sre-delim-test.el --- ERT tests for DELIM rewriter -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-sre-delim)


(ert-deftest carriage-sre-delim-rewrite-negative-similar-lines ()
  "Similar but non-exact lines must remain unchanged."
  (let* ((old "1db651")
         (new "a1b2c3")
         (text (mapconcat #'identity
                          '(" <<1db651"
                            "<< 1db651"
                            ": 1db651"
                            "::1db651"
                            ":1db651 "
                            "payload <<1db651 inside"
                            "payload :1db651 inside")
                          "\n")))
    (should (string= (carriage-sre-rewrite-delim-markers text old new) text))))

(ert-deftest carriage-sre-delim-invalid-tokens-signal-error ()
  "OLD/NEW must be exactly 6 lower-hex chars; invalid tokens signal error."
  (should-error (carriage-sre-rewrite-delim-markers "<<zzzzzz\n:zzzzzz\n" "zzzzzz" "a1b2c3"))
  (should-error (carriage-sre-rewrite-delim-markers "<<1db651\n:1db651\n" "1db651" "ABCDEF")))

(provide 'carriage-sre-delim-test)
;;; carriage-sre-delim-test.el ends here
