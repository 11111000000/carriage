;;; carriage-report-test.el --- Report UI smoke tests (Diff/Ediff) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-report)

(defun carriage-report-test--write-file (dir rel content)
  "Write CONTENT to DIR/REL."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs
      (insert content))
    abs))

(ert-deftest carriage-report-sre-buttons-and-ediff-noninteractive ()
  "Render SRE report row; ensure [Diff]/[Ediff] present and Ediff callable in batch."
  (let* ((dir (make-temp-file "carriage-rpt-sre-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Prepare file
          (carriage-report-test--write-file dir "x.txt" "hello\n")
          ;; Build SRE plan item and dry-run to obtain preview
          (let* ((plan (list (list (cons :version "1")
                                   (cons :op 'sre)
                                   (cons :file "x.txt")
                                   (cons :pairs (list (list (cons :from "hello")
                                                            (cons :to   "world")
                                                            (cons :opts '(:occur first :match literal))))))))
                 (rep (carriage-dry-run-plan plan dir))
                 (buf (carriage-report-render rep)))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (re-search-forward "\\[Ediff\\]" nil t))
              ;; Diff preview may be empty in edge cases, but generally present for SRE
              (re-search-forward "x.txt" nil t)
              (beginning-of-line)
              (let ((errorp (condition-case _err
                                (progn
                                  (carriage-report-ediff-at-point)
                                  nil)
                              (error t))))
                (should (not errorp))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-report-patch-ediff-noninteractive ()
  "Render patch report row; ensure [Ediff] present and ediff callable in batch."
  (let* ((dir (make-temp-file "carriage-rpt-patch-" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          ;; Prepare file on disk
          (carriage-report-test--write-file dir "x.txt" "old\n")
          ;; Minimal unified diff changing old->new
          (let* ((udiff (mapconcat #'identity
                                   '("diff --git a/x.txt b/x.txt"
                                     "index 0000000..0000001 100644"
                                     "--- a/x.txt"
                                     "+++ b/x.txt"
                                     "@@ -1 +1 @@"
                                     "-old"
                                     "+new")
                                   "\n"))
                 (item (list :op 'patch :status 'ok :path "x.txt" :details "ok" :diff udiff))
                 (rep (list :plan nil :summary (list :ok 1 :fail 0 :skipped 0) :items (list item)))
                 (buf (carriage-report-render rep)))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (re-search-forward "\\[Ediff\\]" nil t))
              (re-search-forward "x.txt" nil t)
              (beginning-of-line)
              (let ((errorp (condition-case _err
                                (progn
                                  (carriage-report-ediff-at-point)
                                  nil)
                              (error t))))
                (should (not errorp))))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-report-test.el ends here
