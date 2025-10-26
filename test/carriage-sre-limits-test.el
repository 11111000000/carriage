;;; carriage-sre-limits-test.el --- Limits and range policies tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-sre-limits-test--org-with-batch (pairs)
  "Build an org SRE-BATCH block with PAIRS pairs (FROMi→TOi)."
  (let ((delim "deadbe"))
    (concat
     "#+begin_patch (:version \"1\" :op \"sre-batch\" :file \"x.txt\" :delim \"" delim "\")\n"
     (mapconcat
      (lambda (i)
        (format "<<%s\nFROM-%d\n:%s\n<<%s\nTO-%d\n:%s"
                delim i delim delim i delim))
      (number-sequence 1 pairs)
      "\n")
     "\n#+end_patch")))

(ert-deftest carriage-sre-batch-limit-exceeded-signals ()
  "SRE-BATCH parser must signal when number of pairs exceeds configured limit."
  (let* ((dir (make-temp-file "carriage-sre-lim-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (with-temp-buffer
          ;; Limit to 2, provide 3 pairs
          (let ((carriage-mode-max-batch-pairs 2))
            (insert (carriage-sre-limits-test--org-with-batch 3) "\n")
            (goto-char (point-min))
            (should-error
             (carriage-parse-blocks-in-region (point-min) (point-max) dir))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-sre-segment-size-limit-exceeded-signals ()
  "SRE parser must signal when any segment exceeds 512KiB."
  (let* ((dir (make-temp-file "carriage-sre-lim-seg-" t))
         (default-directory (file-name-as-directory dir))
         (delim "deadbe")
         (big (make-string (1+ (* 512 1024)) ?a)))
    (unwind-protect
        (with-temp-buffer
          (insert (format "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"%s\")\n" delim))
          (insert (format "<<%s\n%s\n:%s\n" delim big delim))
          (insert (format "<<%s\nTO\n:%s\n" delim delim))
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-sre-body-size-limit-exceeded-signals ()
  "SRE parser must signal when total body exceeds 4MiB."
  (let* ((dir (make-temp-file "carriage-sre-lim-body-" t))
         (default-directory (file-name-as-directory dir))
         (delim "deadbe")
         ;; Create a large comment blob to exceed 4MiB without exceeding segment size.
         (pad (make-string (+ (* 4 1024 1024) 100) ?x)))
    (unwind-protect
        (with-temp-buffer
          (insert (format "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"%s\")\n" delim))
          (insert pad "\n")
          (insert (format "<<%s\nFROM\n:%s\n" delim delim))
          (insert (format "<<%s\nTO\n:%s\n" delim delim))
          (insert "#+end_patch\n")
          (goto-char (point-min))
          (should-error
           (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-sre-limits-test.el ends here
