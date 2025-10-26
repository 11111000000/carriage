;;; carriage-parser-fallback-test.el --- SRE fallback delim tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-parser-fallback-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(ert-deftest carriage-sre-fallback-delim-dry-run ()
  "SRE parser should ignore incorrect :delim in header via fallback pass and extract segments."
  (let* ((dir (make-temp-file "carriage-sre-fallback-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Prepare target file
          (carriage-parser-fallback-test--write dir "x.txt" "hello\n")
          ;; Build org with wrong header :delim but proper segment tokens in body
          (let* ((org (mapconcat
                       #'identity
                       '("#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"abcdef\")"
                         "<<cafe01"
                         "hello"
                         ":cafe01"
                         "<<cafe01"
                         "world"
                         ":cafe01"
                         "#+end_patch")
                       "\n")))
            (with-temp-buffer
              (insert org "\n")
              (goto-char (point-min))
              (let* ((plan (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
                (should (= (length plan) 1))
                ;; Dry-run should succeed and find 1 match
                (let* ((rep (carriage-dry-run-plan plan dir))
                       (items (plist-get rep :items))
                       (it (car items)))
                  (should (eq (plist-get it :status) 'ok))
                  (should (eq (plist-get it :matches) 1)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-group-create-then-sre-fallback-delim-dry-run ()
  "Group plan create → sre with wrong :delim in SRE header must still dry-run OK via generic fallback."
  (let* ((dir (make-temp-file "carriage-sre-group-fallback-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Build org with create (valid) then sre (wrong header delim; body tokens valid)
          (let* ((org (mapconcat
                       #'identity
                       '("#+begin_patch (:version \"1\" :op \"create\" :file \"x.txt\" :delim \"cafe01\")"
                         "<<cafe01"
                         "hello"
                         ":cafe01"
                         "#+end_patch"
                         ""
                         "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"abcdef\")"
                         "<<deadbe"
                         "hello"
                         ":deadbe"
                         "<<deadbe"
                         "world"
                         ":deadbe"
                         "#+end_patch")
                       "\n")))
            ;; Debug logs: extract SRE body of second block and log open-count and extractor results
            (let ((sre-body (with-temp-buffer
                              (insert org "\n")
                              (goto-char (point-min))
                              (re-search-forward "^[ \t]*#\\+begin_patch\\b.*:op \"sre\".*$" nil t)
                              (forward-line 1)
                              (let ((beg (point)))
                                (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                                (buffer-substring-no-properties beg (line-beginning-position))))))
              (ignore-errors
                (carriage-log "group-fallback test: opens=%d idx2=%d naive2=%d body=%s"
                              (cl-loop for ln in (split-string sre-body "\n" nil nil)
                                       count (string-prefix-p "<<" (string-trim ln)))
                              (length (carriage--sre--extract-first-two-by-indices sre-body))
                              (length (carriage--sre--extract-first-two sre-body))
                              (let ((s (substring sre-body 0 (min 160 (length sre-body)))))
                                (replace-regexp-in-string "\n" "\\n" s))))))
          (let* ((org (mapconcat
                       #'identity
                       '("#+begin_patch (:version \"1\" :op \"create\" :file \"x.txt\" :delim \"cafe01\")"
                         "<<cafe01"
                         "hello"
                         ":cafe01"
                         "#+end_patch"
                         ""
                         "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"abcdef\")"
                         "<<deadbe"
                         "hello"
                         ":deadbe"
                         "<<deadbe"
                         "world"
                         ":deadbe"
                         "#+end_patch")
                       "\n")))
            (with-temp-buffer
              (insert org "\n")
              (goto-char (point-min))
              (let* ((plan (carriage-parse-blocks-in-region (point-min) (point-max) dir)))
                (should (= (length plan) 2))
                (let* ((rep (carriage-dry-run-plan plan dir))
                       (sum (plist-get rep :summary)))
                  (should (eq (plist-get sum :fail) 0))
                  (should (eq (plist-get sum :ok) 2))))))
          (ignore-errors (delete-directory dir t))))))

;;; carriage-parser-fallback-test.el ends here
