;;; commit-commands-test.el --- ERT tests for commit commands  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'org)

(load "stage-policy-test.el" nil t) ;; reuse helpers if available

(defun carriage-commit-test--with-temp-repo (fn)
  (carriage-test--with-temp-repo fn))

(defun carriage-commit-test--prepare-change (root from to)
  "Apply a simple patch foo.txt FROM->TO with policy none."
  (let* ((carriage-apply-stage-policy 'none)
         (diff (carriage-test--make-diff from to))
         (item (list (cons :version "1") (cons :op 'patch) (cons :apply 'git-apply) (cons :strip 1)
                     (cons :path "foo.txt") (cons :diff diff))))
    (carriage-apply-diff item root)))

(ert-deftest carriage-commit-last-iteration-no-files-errors ()
  "When there is no 'last iteration' marked, the command should error."
  (carriage-commit-test--with-temp-repo
   (lambda (root)
     (with-temp-buffer
       (org-mode)
       (carriage-mode 1)
       (setq default-directory root)
       (should-error (carriage-commit-last-iteration "msg"))
       (carriage-mode -1)))))

(ert-deftest carriage-commit-last-iteration-commits-only-listed-files ()
  "Mark a last iteration with a single patch block, then verify the commit includes only that file."
  (carriage-commit-test--with-temp-repo
   (lambda (root)
     ;; first change and commit to get to rev 2
     (carriage-commit-test--prepare-change root "hello" "hello X")
     (with-temp-buffer (setq default-directory root)
                       (carriage-commit-changes "seed"))
     ;; apply second change but do not commit yet
     (carriage-commit-test--prepare-change root "hello X" "hello Y")
     ;; insert a matching patch block via accept to mark 'last iteration'
     (with-current-buffer (get-buffer-create "*carriage-commit-it*")
       (erase-buffer)
       (org-mode)
       (setq default-directory root)
       (carriage-mode 1)
       (let* ((block (concat
                      "#+begin_patch (:version \"1\" :op \"patch\" :apply \"git-apply\" :strip 1)\n"
                      "--- a/foo.txt\n"
                      "+++ b/foo.txt\n"
                      "@@ -1,1 +1,1 @@\n"
                      "-hello X\n"
                      "+hello Y\n"
                      "#+end_patch\n")))
         (carriage-accept-llm-response block)
         ;; now commit only last iteration files
         (carriage-commit-last-iteration "carriage: last-iteration")
         (carriage-mode -1)))
     ;; verify only foo.txt is in the last commit
     (let* ((out (plist-get (carriage-test--call root "show" "--name-only" "--pretty=format:" "HEAD") :stdout))
            (files (seq-filter (lambda (s) (not (string-empty-p s)))
                               (split-string out "\n"))))
       (should (= (length files) 1))
       (should (string= (car files) "foo.txt"))))))
