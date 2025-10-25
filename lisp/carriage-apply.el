;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)

(defun carriage--report-ok (op &rest kv)
  "Build ok report alist with OP and extra KV plist."
  (append (list :op op :status 'ok) kv))

(defun carriage--report-fail (op &rest kv)
  "Build fail report alist with OP and extra KV plist."
  (append (list :op op :status 'fail) kv))

;;; SRE

(defun carriage--sre-make-regexp (from match-kind)
  "Return regexp for FROM according to MATCH-KIND ('literal or 'regex)."
  (if (eq match-kind 'regex) from (regexp-quote (or from ""))))

(defun carriage--sre-count-nonoverlapping (text regexp)
  "Count non-overlapping matches of REGEXP in TEXT."
  (let ((pos 0) (cnt 0))
    (while (and (< pos (length text)) (string-match regexp text pos))
      (setq cnt (1+ cnt))
      (setq pos (match-end 0)))
    cnt))

(defun carriage--sre-slice-by-lines (text range-plist)
  "Return (PRE REGION POST) for TEXT restricted by RANGE-PLIST (:start-line N :end-line M).
Lines are 1-based; END-LINE inclusive. If RANGE-PLIST is nil, REGION = TEXT and PRE/POST empty."
  (if (not range-plist)
      (list "" text "")
    (let* ((start (plist-get range-plist :start-line))
           (end   (plist-get range-plist :end-line)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let* ((start-line (max 1 (or start 1)))
               (end-line (max start-line (or end (line-number-at-pos (point-max)))))
               (beg (progn (goto-char (point-min)) (forward-line (1- start-line)) (point)))
               (endp (progn (goto-char (point-min)) (forward-line end-line) (point))))
          (list (buffer-substring-no-properties (point-min) beg)
                (buffer-substring-no-properties beg endp)
                (buffer-substring-no-properties endp (point-max))))))))

(defun carriage--sre-replace-first (text regexp to replacement-literal-p)
  "Replace first match of REGEXP in TEXT with TO. Return (NEW . COUNT)."
  (if (not (string-match regexp text))
      (cons text 0)
    (cons (replace-match to nil replacement-literal-p text) 1)))

(defun carriage--sre-replace-all (text regexp to replacement-literal-p)
  "Replace all matches of REGEXP in TEXT with TO. Return (NEW . COUNT)."
  (let ((count (carriage--sre-count-nonoverlapping text regexp))
        (new (replace-regexp-in-string regexp to text nil replacement-literal-p)))
    (cons new count)))

(defun carriage--sre-count-matches (text from opts)
  "Count matches of FROM in TEXT honoring OPTS (:match, :occur)."
  (let* ((match-kind (or (plist-get opts :match) 'literal))
         (occur (or (plist-get opts :occur) 'first))
         (rx (carriage--sre-make-regexp from match-kind)))
    (cond
     ((eq occur 'first)
      (if (string-match rx text) 1 0))
     ((eq occur 'all)
      (carriage--sre-count-nonoverlapping text rx))
     (t (carriage--sre-count-nonoverlapping text rx)))))

(defun carriage-dry-run-sre (plan-item repo-root)
  "Dry-run SRE: count matches per pair; check :expect for :occur all."
  (let* ((file (alist-get :file plan-item))
         (abs (ignore-errors (carriage-normalize-path (or repo-root default-directory) file))))
    (if (not (and abs (file-exists-p abs)))
        (carriage--report-fail 'sre :file file :details "File not found")
      (let* ((text (carriage-read-file-string abs))
             (pairs (or (alist-get :pairs plan-item) '()))
             (total-matches 0)
             (errors nil))
        (dolist (p pairs)
          (let* ((from (alist-get :from p))
                 (opts (alist-get :opts p))
                 (range (plist-get opts :range))
                 (occur (or (plist-get opts :occur) 'first))
                 (expect (plist-get opts :expect))
                 (region (if range (cadr (carriage--sre-slice-by-lines text range)) text))
                 (count (condition-case e
                            (carriage--sre-count-matches region from opts)
                          (error
                           (push (format "Regex error: %s" (error-message-string e)) errors)
                           -1))))
            (when (>= count 0)
              (setq total-matches (+ total-matches count))
              (when (and (eq occur 'all) (integerp expect) (not (= count expect)))
                (push (format "Expect mismatch: have %d, expect %d" count expect) errors))
              (when (and (eq occur 'first) (= count 0))
                (push "No matches for :occur first" errors)))))
        (if errors
            (carriage--report-fail 'sre :file file
                                   :details (format "fail: pairs:%d matches:%d; %s"
                                                    (length pairs) total-matches
                                                    (mapconcat #'identity (nreverse errors) "; ")))
          (carriage--report-ok 'sre :file file
                               :details (format "ok: pairs:%d matches:%d"
                                                (length pairs) total-matches)))))))

(defun carriage-apply-sre (plan-item repo-root)
  "Apply SRE pairs by rewriting file and committing via Git."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path (or repo-root default-directory) file)))
    (unless (file-exists-p abs)
      (cl-return-from carriage-apply-sre
        (carriage--report-fail 'sre :file file :details "File not found")))
    (let* ((text (carriage-read-file-string abs))
           (pairs (or (alist-get :pairs plan-item) '()))
           (changed 0)
           (new-text text))
      (dolist (p pairs)
        (let* ((from (alist-get :from p))
               (to   (alist-get :to p))
               (opts (alist-get :opts p))
               (range (plist-get opts :range))
               (match-kind (or (plist-get opts :match) 'literal))
               (occur (or (plist-get opts :occur) 'first))
               (rx (carriage--sre-make-regexp from match-kind))
               (slice (carriage--sre-slice-by-lines new-text range))
               (pre (car slice))
               (region (cadr slice))
               (post (caddr slice))
               (rep (pcase occur
                      ('first (carriage--sre-replace-first region rx to (not (eq match-kind 'regex))))
                      ('all   (carriage--sre-replace-all region rx to (not (eq match-kind 'regex))))
                      (_      (carriage--sre-replace-all region rx to (not (eq match-kind 'regex))))))
               (region-new (car rep))
               (cnt (cdr rep)))
          (setq changed (+ changed cnt))
          (setq new-text (concat pre region-new post))))
      (if (<= changed 0)
          (carriage--report-fail 'sre :file file :details "No changes")
        (progn
          (carriage-write-file-string abs new-text t)
          (carriage-git-add repo-root file)
          (carriage-git-commit repo-root (format "carriage: sre %s (replaced %d)" file changed))
          (carriage--report-ok 'sre :file file :details (format "Applied %d replacements" changed)))))))

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

;;; Plan-level pipeline

(defun carriage--op-rank (op)
  "Return rank for OP to sort plan: delete→rename→create→patch→sre."
  (pcase op
    ('delete 1)
    ('rename 2)
    ('create 3)
    ('patch  4)
    (_       5)))

(defun carriage--plan-sort (plan)
  "Return PLAN items sorted by operation type according to v1 order."
  (seq-sort (lambda (a b)
              (< (carriage--op-rank (alist-get :op a))
                 (carriage--op-rank (alist-get :op b))))
            plan))

(defun carriage--dry-run-dispatch (item repo-root)
  "Dispatch dry-run for a single ITEM with REPO-ROOT."
  (pcase (alist-get :op item)
    ((or 'sre 'sre-batch) (carriage-dry-run-sre item repo-root))
    ('patch  (carriage-dry-run-diff item repo-root))
    ('create (carriage-dry-run-create item repo-root))
    ('delete (carriage-dry-run-delete item repo-root))
    ('rename (carriage-dry-run-rename item repo-root))
    (_ (carriage--report-fail (or (alist-get :op item) 'unknown)
                              :details "Unknown op"))))

(defun carriage--apply-dispatch (item repo-root)
  "Dispatch apply for a single ITEM with REPO-ROOT."
  (pcase (alist-get :op item)
    ((or 'sre 'sre-batch) (carriage-apply-sre item repo-root))
    ('patch  (carriage-apply-diff item repo-root))
    ('create (carriage-apply-create item repo-root))
    ('delete (carriage-apply-delete item repo-root))
    ('rename (carriage-apply-rename item repo-root))
    (_ (carriage--report-fail (or (alist-get :op item) 'unknown)
                              :details "Unknown op"))))

(defun carriage-dry-run-plan (plan repo-root)
  "Dry-run PLAN (list of plan items) under REPO-ROOT.
Return report alist: (:plan PLAN :summary (:ok N :fail M :skipped K) :items ...)."
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0))
    (dolist (it sorted)
      (let* ((res (carriage--dry-run-dispatch it repo-root))
             (status (plist-get res :status)))
        (push res items)
        (pcase status
          ('ok   (setq ok (1+ ok)))
          ('fail (setq fail (1+ fail)))
          (_     (setq skip (1+ skip))))))
    (list :plan plan
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items))))

(defun carriage-apply-plan (plan repo-root)
  "Apply PLAN (list of plan items) under REPO-ROOT sequentially.
Stops on first failure. Returns report alist as in carriage-dry-run-plan."
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0)
         (stop nil))
    (dolist (it sorted)
      (unless stop
        (let* ((res (carriage--apply-dispatch it repo-root))
               (status (plist-get res :status)))
          (push res items)
          (pcase status
            ('ok   (setq ok (1+ ok)))
            ('fail (setq fail (1+ fail))
                   (setq stop t))
            (_     (setq skip (1+ skip)))))))
    (list :plan plan
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items))))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
