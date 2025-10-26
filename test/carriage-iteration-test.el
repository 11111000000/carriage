;;; carriage-iteration-test.el --- Group plan order and apply tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'subr-x)

(defun carriage-iteration-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-iteration-test--git-out (dir &rest args)
  "Run git ARGS in DIR and return stdout as string (trimmed)."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(defun carriage-iteration-test--read (dir rel)
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-group-order-create-then-sre ()
  "Plan should sort as create -> sre; dry-run ok; apply modifies file content."
  (let* ((dir (make-temp-file "carriage-iter-" t)))
    (unwind-protect
        (progn
          ;; init repo
          (should (zerop (carriage-iteration-test--git dir "init")))
          (should (zerop (carriage-iteration-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-iteration-test--git dir "config" "user.name" "Tester")))
          (let* ((default-directory (file-name-as-directory dir))
                 (org (mapconcat #'identity
                                 '(
                                   "#+begin_patch (:version \"1\" :op \"create\" :file \"x.txt\" :delim \"cafe01\")"
                                   "<<cafe01"
                                   "hello"
                                   ":cafe01"
                                   "#+end_patch"
                                   ""
                                   "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"deadbe\")"
                                   "<<deadbe"
                                   "hello"
                                   ":deadbe"
                                   "<<deadbe"
                                   "world"
                                   ":deadbe"
                                   "#+end_patch")
                                 "\n")))
            ;; Debug logs: extract SRE body and log open-count and extractor outcomes
            (let ((sre-body (with-temp-buffer
                              (insert org "\n")
                              (goto-char (point-min))
                              (re-search-forward "^[ \t]*#\\+begin_patch\\b.*:op \"sre\".*$" nil t)
                              (forward-line 1)
                              (let ((beg (point)))
                                (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                                (buffer-substring-no-properties beg (line-beginning-position))))))
              (ignore-errors
                (carriage-log "iter-order test: opens=%d idx2=%d naive2=%d body=%s"
                              (cl-loop for ln in (split-string sre-body "\n" nil nil)
                                       count (string-prefix-p "<<" (string-trim ln)))
                              (length (carriage--sre--extract-first-two-by-indices sre-body))
                              (length (carriage--sre--extract-first-two sre-body))
                              (let ((s (substring sre-body 0 (min 160 (length sre-body)))))
                                (replace-regexp-in-string "\n" "\\n" s))))))
          (let* ((org (mapconcat #'identity
                                 '(
                                   "#+begin_patch (:version \"1\" :op \"create\" :file \"x.txt\" :delim \"cafe01\")"
                                   "<<cafe01"
                                   "hello"
                                   ":cafe01"
                                   "#+end_patch"
                                   ""
                                   "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"deadbe\")"
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
                ;; dry-run
                (let* ((rep (carriage-dry-run-plan plan dir))
                       (items (plist-get rep :items))
                       (ops (mapcar (lambda (it) (plist-get it :op)) items))
                       (sum (plist-get rep :summary)))
                  (should (equal ops '(create sre)))
                  (should (eq (plist-get sum :fail) 0))
                  (should (eq (plist-get sum :ok) 2))
                  ;; SRE item should carry :matches
                  (let* ((sre-it (cadr items)))
                    (should (plist-get sre-it :matches))))
                ;; apply
                (let* ((ap (carriage-apply-plan plan dir)))
                  (should (eq (plist-get (plist-get ap :summary) :fail) 0))
                  (should (string= (carriage-iteration-test--read dir "x.txt") "world\n"))
                  ;; ensure we are on WIP branch after apply
                  (let ((branch (carriage-iteration-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
                    (should (string= branch "carriage/WIP"))))))))))
  (ignore-errors (delete-directory dir t)))

;;; carriage-iteration-test.el ends here
