;;; carriage-llm-accept-test.el --- Accept LLM response smoke test -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-mode)

(ert-deftest carriage-accept-llm-response-sre-ok ()
  "Accept a minimal SRE block via =carriage-accept-llm-response' and ensure dry-run ok."
  (let* ((dir (make-temp-file "carriage-llm-accept-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Prepare target file
          (with-temp-file (expand-file-name "x.txt" dir)
            (insert "hello\n"))
          ;; Minimal SRE block replacing hello->world
          (let* ((resp (mapconcat #'identity
                                  '("#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\" :delim \"abc123\")"
                                    "<<abc123"
                                    "hello"
                                    ":abc123"
                                    "<<abc123"
                                    "world"
                                    ":abc123"
                                    "#+end_patch")
                                  "\n"))
                 rep)
            (with-temp-buffer
              ;; Run accept command non-interactively, passing input string
              (setq rep (carriage-accept-llm-response resp))
              ;; Basic assertions on report
              (should (plist-get rep :summary))
              (let* ((sum (plist-get rep :summary))
                     (ok  (or (plist-get sum :ok) 0))
                     (fail (or (plist-get sum :fail) 0)))
                (should (= fail 0))
                (should (> ok 0))))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-llm-accept-test.el ends here
