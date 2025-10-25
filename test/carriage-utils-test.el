;;; carriage-utils-test.el --- Tests for utilities  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-delim-hex ()
  (let ((d (carriage-generate-delim)))
    (should (string-match-p "^[0-9a-f]\\{6\\}$" d))))

(ert-deftest carriage-modules-present ()
  (should (featurep 'carriage))
  (should (fboundp 'carriage-project-root))
  (should (fboundp 'carriage-parse))
  (should (fboundp 'carriage-dry-run-diff))
  (should (fboundp 'carriage-apply-diff)))
