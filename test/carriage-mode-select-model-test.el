;;; carriage-mode-select-model-test.el --- Completion for backend:model -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage)
(require 'carriage-mode)
(require 'carriage-llm-registry)

(ert-deftest carriage-select-model-offers-backend-colon-model ()
  "carriage-select-model should offer combined candidates \"backend:model\"."
  (with-temp-buffer
    (let ((noninteractive t))
      ;; Ensure a clean registry; seed happens inside carriage-select-model.
      (let ((carriage-llm--registry nil))
        (carriage-mode 1)
        (unwind-protect
            (let* ((carriage-mode-backend 'gptel)
                   (carriage-mode-model "gptel-default")
                   (offered nil)
                   (choice nil))
              ;; Intercept completing-read to capture the COLLECTION passed in.
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &rest _)
                           (setq offered collection)
                           ;; Return combined candidate to exercise the branch and update both backend/model
                           "gptel:gptel-default")))
                (carriage-select-model))
              ;; After call, offered must include "gptel:gptel-default" (combined) and "gptel-default" (plain model)
              (should (and (listp offered)
                           (member "gptel:gptel-default" offered)))
              (should (member "gptel-default" offered))
              ;; Both backend and model remain consistent with the selected combined candidate
              (should (eq carriage-mode-backend 'gptel))
              (should (string= carriage-mode-model "gptel-default")))
          (carriage-mode -1))))))

(provide 'carriage-mode-select-model-test)
;;; carriage-mode-select-model-test.el ends here
