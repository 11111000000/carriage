;;; carriage-global-mode-test.el --- Tests for global prefix and keyspec v1.2 -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(ert-deftest carriage-global-prefix-binds-open-buffer ()
  "Global mode with transient=nil should bind C-c e e → carriage-open-buffer."
  (let* ((carriage-global-use-transient nil))
    (carriage-global-mode -1)
    (carriage-global-mode 1)
    (unwind-protect
        (should (eq (key-binding (kbd "C-c e e")) 'carriage-open-buffer))
      (carriage-global-mode -1))))

(ert-deftest carriage-global-disable-unbinds ()
  "Disabling global mode should unbind C-c e e."
  (let* ((carriage-global-use-transient nil))
    (carriage-global-mode -1)
    (carriage-global-mode 1)
    (carriage-global-mode -1)
    (should (not (eq (key-binding (kbd "C-c e e")) 'carriage-open-buffer)))))

(ert-deftest carriage-mode-bare-cce-opens-menu-when-transient ()
  "In carriage-mode with transient=t, bare C-c e opens the menu command."
  (with-temp-buffer
    (org-mode)
    (let* ((carriage-mode-use-transient t))
      (carriage-mode 1)
      (unwind-protect
          (should (eq (lookup-key carriage-mode-map (kbd "C-c e")) 'carriage-keys-open-menu))
        (carriage-mode -1)))))

(ert-deftest carriage-mode-prefix-works-when-transient-nil ()
  "In carriage-mode with transient=nil, C-c e is prefix only; C-c e RET sends buffer."
  (with-temp-buffer
    (org-mode)
    (let* ((carriage-mode-use-transient nil))
      (carriage-mode 1)
      (unwind-protect
          (progn
            (should (null (lookup-key carriage-mode-map (kbd "C-c e"))))
            (should (eq (local-key-binding (kbd "C-c e RET")) 'carriage-send-buffer)))
        (carriage-mode -1)))))

(ert-deftest carriage-global-transient-binds-menu ()
  "Global mode with transient=t should bind C-c e to menu command."
  (let* ((carriage-global-use-transient t))
    (carriage-global-mode -1)
    (carriage-global-mode 1)
    (unwind-protect
        (should (eq (key-binding (kbd "C-c e")) 'carriage-keys-open-menu))
      (carriage-global-mode -1))))

(ert-deftest carriage-global-and-carriage-local-coexist ()
  "With carriage-mode and carriage-global-mode enabled, local C-c e (menu) intercepts; global C-c e e is not reachable."
  (with-temp-buffer
    (org-mode)
    (let* ((carriage-mode-use-transient t)
           (carriage-global-use-transient nil))
      (carriage-mode 1)
      (carriage-global-mode -1)
      (carriage-global-mode 1)
      (unwind-protect
          (progn
            (should (eq (lookup-key carriage-mode-map (kbd "C-c e")) 'carriage-keys-open-menu))
            (should (not (eq (key-binding (kbd "C-c e e")) 'carriage-open-buffer))))
        (carriage-mode -1)
        (carriage-global-mode -1)))))

(provide 'carriage-global-mode-test)
;;; carriage-global-mode-test.el ends here
