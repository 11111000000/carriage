;;; carriage-task-test.el --- Tests for carriage-task (task docs) -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(require 'carriage-task nil t)
(require 'carriage-keyspec nil t)

(ert-deftest carriage-task--slugify-russian ()
  "Slugify should transliterate RU→EN and normalize to ASCII dashed."
  (should (fboundp 'carriage-task-default-slugify))
  (let* ((title "Задача Пример")
         (slug (carriage-task-default-slugify title)))
    ;; Expect ascii, no spaces, only [a-z0-9-]
    (should (string-match-p "\\`[a-z0-9-]+\\'" slug))
    ;; Basic expected transliteration fragments
    (should (string-match-p "zadacha" slug))
    (should (string-match-p "primer" slug))
    ;; Spaces replaced with single dash
    (should (string-match-p "-" slug))
    ;; No multiple consecutive dashes
    (should-not (string-match-p "--" slug))))


(provide 'carriage-task-test)
;;; carriage-task-test.el ends here
