;;; carriage-ui-headerline-props-test.el --- Header-line outline click props -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage)

(ert-deftest carriage-ui-headerline-outline-has-local-map ()
  "Header-line builder should propertize outline with a local-map in org-mode when space permits."
  (with-temp-buffer
    (insert "* One\n** Two\n")
    (org-mode)
    ;; Emulate GUI and a wide window so outline segment is shown
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'window-total-width) (lambda (&rest _) 120)))
      (goto-char (point-min))
      (forward-line 1) ;; on '** Two'
      (let ((s (carriage-ui--header-line)))
        (should (stringp s))
        ;; Scan string properties: outline part must carry 'local-map
        (let ((found nil)
              (len (length s))
              (i 0))
          (while (and (< i len) (not found))
            (when (get-text-property i 'local-map s)
              (setq found t))
            (setq i (1+ i)))
          (should found))))))

(provide 'carriage-ui-headerline-props-test)
;;; carriage-ui-headerline-props-test.el ends here
