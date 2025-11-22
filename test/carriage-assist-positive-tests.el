;;; carriage-assist-positive-tests.el --- Positive tests for Assist context-delta -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-ui)

;; Helper: stub yes/no prompts
(defmacro carriage--with-yes (&rest body)
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) t)))
     ,@body))

(ert-deftest carriage-assist/context-delta-valid-apply ()
  "Valid delta should apply with confirmation; invalid paths are ignored."
  (with-temp-buffer
    (org-mode)
    ;; Minimal buffer with one begin_context block
    (insert "* Dummy\n\n#+begin_context\nsrc/main.c\n#+end_context\n")
    ;; Stub Assist to return a mixed delta: one good, one TRAMP (ignored), one abs (ignored)
    (cl-letf (((symbol-function 'carriage-assist-context-delta)
               (lambda (_ctx)
                 (list :add (list "include/util.h" "/etc/passwd" "/ssh:host:/remote.el")
                       :remove (list "non-existent.py")
                       :why "test"))))
      (carriage--with-yes
       (let ((before (buffer-string)))
         (ignore-errors (carriage-ui-context-delta-assist))
         (let ((after (buffer-string)))
           ;; Good path should appear, bad ones should be absent
           (should (string-match-p "include/util\\.h" after))
           (should-not (string-match-p "/etc/passwd" after))
           (should-not (string-match-p "/ssh:host:/remote\\.el" after))
           ;; Remove may be a no-op if absent; ensure buffer changed
           (should (not (string= before after)))))))))

(provide 'carriage-assist-positive-tests)
;;; carriage-assist-positive-tests.el ends here
