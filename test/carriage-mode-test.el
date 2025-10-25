;;; carriage-mode-test.el --- Mode/UI presence tests  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-mode-present ()
  (should (fboundp 'carriage-mode))
  (should (boundp 'carriage-mode-map)))

(ert-deftest carriage-report-present ()
  (should (fboundp 'carriage-report-open))
  (should (fboundp 'carriage-report-render)))
