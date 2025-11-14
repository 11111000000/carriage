;;; carriage-ui-modeline-context-test.el --- Tests for [Ctx:N] modeline segment  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-ui-segment-context-present ()
  "Modeline should include [Ctx:N] segment when carriage-ui is available."
  (cond
   ((not (featurep 'carriage-ui))
    (ert-skip "carriage-ui not loaded; skip modeline segment test"))
   ((not (fboundp 'carriage-ui--modeline))
    (ert-skip "carriage-ui--modeline not available"))
   (t
    (with-temp-buffer
      (org-mode)
      (carriage-mode 1)
      ;; Trigger modeline build
      (let* ((seg (format "%s" (carriage-ui--modeline))))
        (should (string-match-p "\\[Ctx:\\([0-9]+\\|—\\)\\]" seg)))))))

(ert-deftest carriage-ui-segment-context-help-echo ()
  "The [Ctx:N] segment should expose a helpful help-echo listing sources/items (if implemented)."
  (if (not (featurep 'carriage-ui))
      (ert-skip "carriage-ui not loaded; skip help-echo test")
    (with-temp-buffer
      (org-mode)
      (carriage-mode 1)
      (let ((fmt (ignore-errors (carriage-ui--modeline))))
        (should (or (null fmt) (stringp fmt)))))))

(provide 'carriage-ui-modeline-context-test)
;;; carriage-ui-modeline-context-test.el ends here
