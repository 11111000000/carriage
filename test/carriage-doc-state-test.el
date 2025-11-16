;;; carriage-doc-state-test.el --- Tests for document state layer  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(ert-deftest carriage-doc-state-exports ()
  (should (require 'carriage-doc-state nil t))
  (should (fboundp 'carriage-doc-state-read))
  (should (fboundp 'carriage-doc-state-write))
  (should (fboundp 'carriage-doc-state-restore))
  (should (fboundp 'carriage-doc-state-hide))
  (should (fboundp 'carriage-doc-state-auto-enable)))

(ert-deftest carriage-doc-state-create-and-write ()
  (with-temp-buffer
    (org-mode)
    (insert "* Title\nSome text\n")
    (require 'carriage-doc-state)
    (carriage-doc-state-write '(:CAR_MODE "t" :CAR_INTENT "Code" :CAR_SUITE "udiff"))
    (goto-char (point-min))
    (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
    (let ((block-beg (line-end-position))
          (block-end (progn
                       (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
                       (match-beginning 0))))
      (save-excursion
        (goto-char block-beg)
        (should (re-search-forward "^[ \t]*CARRIAGE_MODE[ \t]+t\\b" block-end t))
        (goto-char block-beg)
        (should (re-search-forward "^[ \t]*CARRIAGE_INTENT[ \t]+Code\\b" block-end t))
        (goto-char block-beg)
        (should (re-search-forward "^[ \t]*CARRIAGE_SUITE[ \t]+udiff\\b" block-end t))))))

(ert-deftest carriage-doc-state-read-plist ()
  (with-temp-buffer
    (org-mode)
    (insert "* Carriage State\n:PROPERTIES:\n:CAR_MODE: t\n:CAR_INTENT: Ask\n:END:\n")
    (require 'carriage-doc-state)
    (let ((pl (carriage-doc-state-read)))
      (should (equal (plist-get pl :CAR_MODE) "t"))
      (should (equal (plist-get pl :CAR_INTENT) "Ask")))))

;;; carriage-doc-state-test.el ends here
