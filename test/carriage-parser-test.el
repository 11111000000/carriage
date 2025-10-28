;;; carriage-parser-test.el --- Parser presence tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-op-sre)
(require 'carriage-op-patch)
(require 'carriage-op-file)

(ert-deftest carriage-parser-sre-fns-present ()
  (should (fboundp 'carriage-parse-sre)))

(ert-deftest carriage-parser-diff-fns-present ()
  (should (fboundp 'carriage-parse-diff)))

(ert-deftest carriage-file-ops-fns-present ()
  (should (and (fboundp 'carriage-parse-create)
               (fboundp 'carriage-parse-delete)
               (fboundp 'carriage-parse-rename))))
