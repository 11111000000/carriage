;;; profile-carriage.el --- Development profile snippets  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: dev, profile
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/testing-v2.org
;;
;;; Commentary:
;; Development profile helpers used by CI and local development.
;;
;;; Code:

;;; Commentary:
;; Run Emacs' built-in profiler in batch mode and export the report.

;;; Code:

(require 'profiler)
(require 'subr-x)

(defvar profile-carriage--default-duration 10
  "Default profiling duration in seconds.")

(defvar profile-carriage--default-output "carriage-profile.txt"
  "Default output file for profiler reports.")

(defun profile-carriage--parse-duration ()
  (let ((raw (getenv "CARRIAGE_PROFILE_DURATION")))
    (if (and raw (not (string-empty-p raw)))
        (max 1 (truncate (string-to-number raw)))
      profile-carriage--default-duration)))

(defun profile-carriage--profile-kind ()
  (let ((raw (getenv "CARRIAGE_PROFILE_KIND")))
    (pcase (and raw (downcase raw))
      ("mem" 'mem)
      (_ 'cpu))))

(defun profile-carriage--output-path ()
  (let ((raw (getenv "CARRIAGE_PROFILE_OUTPUT")))
    (if (and raw (not (string-empty-p raw)))
        raw
      profile-carriage--default-output)))

(defun profile-carriage--write-report (kind output)
  (let ((buffer (profiler-report kind)))
    (unwind-protect
        (with-current-buffer buffer
          (write-region (point-min) (point-max) output))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun profile-carriage-run ()
  "Run a batch profiler session for Carriage."
  (let ((kind (profile-carriage--profile-kind))
        (duration (profile-carriage--parse-duration))
        (output (profile-carriage--output-path)))
    (message "Carriage profiler starting (kind=%s duration=%ss output=%s)" kind duration output)
    (profiler-start kind)
    (unwind-protect
        (progn
          (sleep-for duration)
          (profiler-stop)
          (profile-carriage--write-report kind output))
      (when (eq (profiler-status) 'running)
        (ignore-errors (profiler-stop))))
    (message "Carriage profiler report saved to %s" output)))

(provide 'profile-carriage)

;;; profile-carriage.el ends here
