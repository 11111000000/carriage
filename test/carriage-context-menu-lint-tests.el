;;; carriage-context-menu-lint-tests.el --- Lint tests for Context transient/menu  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-keyspec)

(defun carriage--menu-key-of (pl)
  "Best-effort derive transient menu key for action PL.
Prefers explicit :menu-key; otherwise, emulate builder's rules for simple cases."
  (or (plist-get pl :menu-key)
      (let* ((k (car (plist-get pl :keys))))
        (cond
         ;; two-stroke written as compact like \"tc\"
         ((and (stringp k)
               (string-match-p "\\`t[[:alnum:]]\\'" k))
          (format "t %s" (substring k 1 2)))
         ;; already has a space → leave as-is trimmed
         ((and (stringp k)
               (string-match-p " " k))
          (string-trim k))
         ;; fallback to last token of full binding
         ((stringp k) (substring (or (ignore-errors
                                       (key-description
                                        (carriage-keys--ensure-kbd k)))
                                     k)
                                 -1))
         (t nil)))))

(defun carriage--menu-label-of (pl)
  "Compute a robust label like the transient builder does (i18n → :label → cmd → id).
No bracketed key hints are appended here by design."
  (let* ((_ (require 'carriage-i18n nil t))
         (cmd (plist-get pl :cmd))
         (id  (plist-get pl :id))
         (desc-key (plist-get pl :desc-key))
         (raw (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                  (plist-get pl :label)
                  (and (symbolp cmd) (symbol-name cmd))
                  (and (symbolp id) (symbol-name id))
                  (format "%s" id)))
         (lbl (if (and (stringp raw) (string-match-p "\\`[ \t]*\\'" raw))
                  (symbol-name (or id 'unknown))
                raw)))
    lbl))

(ert-deftest carriage-context-menu-no-single-t-when-prefix-used ()
  "If any \"t x\" exists, no plain single \"t\" menu key shall be present."
  (let* ((menu-keys (delq nil
                          (mapcar #'carriage--menu-key-of carriage-keys--spec)))
         (has-tx (cl-some (lambda (mk) (and (stringp mk)
                                            (string-prefix-p "t " mk)))
                          menu-keys))
         (has-t (member "t" menu-keys)))
    (when has-tx
      (should-not has-t))))

(ert-deftest carriage-context-menu-keys-unique-under-full-seq ()
  "Two-stroke keys are unique by full sequence (\"t c\" ≠ \"c\")."
  (let* ((tx (cl-loop for pl in carriage-keys--spec
                      for mk = (carriage--menu-key-of pl)
                      when (and (stringp mk) (string-prefix-p "t " mk))
                      collect mk))
         (dups (cl-set-difference tx (delete-dups (copy-sequence tx)) :test #'equal)))
    (should (null dups))))

(ert-deftest carriage-context-menu-no-empty-or-nil-labels ()
  "Every item label must be non-empty after fallbacks."
  (dolist (pl carriage-keys--spec)
    (let ((lbl (carriage--menu-label-of pl)))
      (should (and (stringp lbl)
                   (not (string-match-p "\\`[ \t]*\\'" lbl)))))))

(ert-deftest carriage-context-menu-no-trailing-bracket-hints ()
  "Labels must not include postfix bracketed key hints like \" [tc]\"."
  (dolist (pl carriage-keys--spec)
    (let ((lbl (carriage--menu-label-of pl)))
      (should (not (and (stringp lbl)
                        (string-match-p " \\[[^][]+\\]\\'" lbl)))))))

(provide 'carriage-context-menu-lint-tests)
;;; carriage-context-menu-lint-tests.el ends here
