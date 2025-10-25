;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)

(defun carriage--report-ok (op &key file path details)
  "Build a simple ok report item alist."
  (list :op op :status 'ok :file (or file path) :details (or details "ok")))

(defun carriage--report-fail (op &key file path details extra)
  "Build a simple fail report item alist."
  (append (list :op op :status 'fail :file (or file path) :details (or details "fail"))
          (when extra (list :extra extra))))

;;; SRE

(defun carriage-dry-run-sre (plan-item repo-root)
  "Dry-run SRE: count matches per plan-item. Minimal stub for v1."
  (let* ((file (alist-get :file plan-item))
         (abs (ignore-errors (carriage-normalize-path (or repo-root default-directory) file)))
         (exists (and abs (file-exists-p abs))))
    (if (not exists)
        (carriage--report-fail 'sre :file file :details "File not found")
      (carriage--report-ok (alist-get :op plan-item) :file file :details "SRE dry-run stub: ok"))))

(defun carriage-apply-sre (plan-item repo-root)
  "Apply SRE pairs by rewriting file. Minimal stub for v1."
  (let* ((file (alist-get :file plan-item)))
    (carriage--report-ok (alist-get :op plan-item) :file file :details "SRE apply stub: noop")))

;;; PATCH (unified diff)

(defun carriage-dry-run-diff (plan-item repo-root)
  "Run git apply --check for unified diff."
  (let* ((diff (alist-get :diff plan-item))
         (strip (alist-get :strip plan-item))
         (res (carriage-git-apply-check repo-root diff :strip strip)))
    (if (and (plist-get res :exit) (zerop (plist-get res :exit)))
        (carriage--report-ok 'patch :path (alist-get :path plan-item) :details "git apply --check ok")
      (carriage--report-fail 'patch
                             :path (alist-get :path plan-item)
                             :details "git apply --check failed"
                             :extra (list :exit (plist-get res :exit)
                                          :stderr (plist-get res :stderr)
                                          :stdout (plist-get res :stdout))))))

(defun carriage-apply-diff (plan-item repo-root)
  "Apply unified diff with git apply --index; then git add/commit."
  (let* ((diff (alist-get :diff plan-item))
         (strip (alist-get :strip plan-item))
         (path (alist-get :path plan-item))
         (apply-res (carriage-git-apply-index repo-root diff :strip strip)))
    (if (and (plist-get apply-res :exit) (zerop (plist-get apply-res :exit)))
        (progn
          (when path (carriage-git-add repo-root path))
          (carriage-git-commit repo-root (format "carriage: patch %s" (or path "<unknown>")))
          (carriage--report-ok 'patch :path path :details "git apply --index ok"))
      (carriage--report-fail 'patch :path path :details "git apply --index failed"
                             :extra (list :exit (plist-get apply-res :exit)
                                          :stderr (plist-get apply-res :stderr))))))

;;; FILE OPS

(defun carriage-dry-run-create (plan-item repo-root)
  "Validate create preconditions."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path repo-root file)))
    (if (file-exists-p abs)
        (carriage--report-fail 'create :file file :details "Already exists")
      (carriage--report-ok 'create :file file :details (format "Will create (%d bytes)" (length (or (alist-get :content plan-item) "")))))))

(defun carriage-dry-run-delete (plan-item repo-root)
  "Validate delete preconditions."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path repo-root file)))
    (if (not (file-exists-p abs))
        (carriage--report-fail 'delete :file file :details "Not found")
      (carriage--report-ok 'delete :file file :details "Will delete"))))

(defun carriage-dry-run-rename (plan-item repo-root)
  "Validate rename preconditions."
  (let* ((from (alist-get :from plan-item))
         (to   (alist-get :to plan-item))
         (abs-from (carriage-normalize-path repo-root from))
         (abs-to   (carriage-normalize-path repo-root to)))
    (cond
     ((not (file-exists-p abs-from))
      (carriage--report-fail 'rename :file from :details "Source not found"))
     ((file-exists-p abs-to)
      (carriage--report-fail 'rename :file to :details "Target exists"))
     (t (carriage--report-ok 'rename :file (format "%s -> %s" from to) :details "Will rename")))))

(defun carriage-apply-create (plan-item repo-root)
  "Create a new file and commit."
  (let* ((file (alist-get :file plan-item))
         (content (alist-get :content plan-item))
         (mkdir (alist-get :mkdir plan-item))
         (abs (carriage-normalize-path repo-root file)))
    (carriage-write-file-string abs (or content "") mkdir)
    (carriage-git-add repo-root file)
    (carriage-git-commit repo-root (format "carriage: create %s" file))
    (carriage--report-ok 'create :file file :details "Created")))

(defun carriage-apply-delete (plan-item repo-root)
  "Delete file via git rm and commit."
  (let* ((file (alist-get :file plan-item)))
    (carriage-git-rm repo-root file)
    (carriage-git-commit repo-root (format "carriage: delete %s" file))
    (carriage--report-ok 'delete :file file :details "Deleted")))

(defun carriage-apply-rename (plan-item repo-root)
  "Rename file via git mv and commit."
  (let* ((from (alist-get :from plan-item))
         (to   (alist-get :to plan-item)))
    (carriage-git-mv repo-root from to)
    (carriage-git-commit repo-root (format "carriage: rename %s -> %s" from to))
    (carriage--report-ok 'rename :file (format "%s -> %s" from to) :details "Renamed")))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
