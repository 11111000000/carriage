;;; carriage-parser.el --- Parsers for patch blocks  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-logging)

(defun carriage-parse (op header-plist body-text repo-root)
  "Dispatch parse by OP for HEADER-PLIST and BODY-TEXT under REPO-ROOT."
  (pcase (intern (format "%s" op))
    ('sre        (carriage-parse-sre header-plist body-text repo-root))
    ('sre-batch  (carriage-parse-sre header-plist body-text repo-root))
    ('patch      (carriage-parse-diff header-plist body-text repo-root))
    ('create     (carriage-parse-create header-plist body-text repo-root))
    ('delete     (carriage-parse-delete header-plist body-text repo-root))
    ('rename     (carriage-parse-rename header-plist body-text repo-root))
    (_ (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list (format "Unknown op: %S" op))))))

;;;; SRE parsing

(defun carriage--sre-validate-header (hdr op)
  "Validate SRE HDR plist for OP ('sre or 'sre-batch)."
  (let ((version (plist-get hdr :version))
        (file (plist-get hdr :file))
        (delim (plist-get hdr :delim)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'SRE_E_VERSION) (list version)))
    (unless (member op '(sre sre-batch))
      (signal (carriage-error-symbol 'SRE_E_OP) (list op)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'SRE_E_PATH) (list file)))
    (unless (and (stringp delim) (string-match-p "\\`[0-9a-f]\\{6\\}\\'" delim))
      (signal (carriage-error-symbol 'SRE_E_OP) (list "Invalid :delim")))
    t))

(defun carriage--sre-default-opts ()
  "Return default SRE pair options plist."
  '(:occur first :match literal))

(defun carriage--sre-merge-opts (pending)
  "Merge PENDING opts plist with defaults."
  (let ((defs (carriage--sre-default-opts)))
    (cl-loop for (k v) on pending by #'cddr do (setq defs (plist-put defs k v)))
    defs))

(defun carriage--sre-parse-pair-directive (line)
  "If LINE is a #+pair directive, return its plist; else nil."
  (when (string-match "\\`\\s-*#\\+pair\\s-+\\((.*)\\)\\s-*\\'" line)
    (car (read-from-string (match-string 1 line)))))

;; Internal helpers to scan SRE segments. Each pass focuses on a single strategy
;; and signals SRE_E_UNCLOSED_SEGMENT on unmatched openers to preserve behavior.

(defun carriage--sre--scan-linewise-delim (body open close)
  "Scan BODY line-wise using explicit OPEN/CLOSE tokens. Return list of payloads."
  (let ((segments '())
        (state 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil)) ;; keep empty lines
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq state 'idle) (string= tln open))
          (setq state 'in acc nil))
         ((and (eq state 'in) (string= tln close))
          (push (mapconcat #'identity (nreverse acc) "\n") segments)
          (setq acc nil state 'idle))
         ((eq state 'in)
          (push ln acc)))))
    (when (eq state 'in)
      (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT) (list "Unclosed segment")))
    (nreverse segments)))

(defun carriage--sre--scan-regexp-delim (body open close)
  "Scan BODY using anchored regex with explicit OPEN/CLOSE tokens.
Return list of payloads."
  (let ((res '()))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let* ((open-rx  (concat "^[ \t]*" (regexp-quote open)  "[ \t]*$"))
             (close-rx (concat "^[ \t]*" (regexp-quote close) "[ \t]*$")))
        (while (re-search-forward open-rx nil t)
          (forward-line 1)
          (let ((beg (point)))
            (unless (re-search-forward close-rx nil t)
              (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT) (list "Unclosed segment")))
            (let ((end (line-beginning-position)))
              (push (buffer-substring-no-properties beg end) res)
              (forward-line 1))))))
    (nreverse res)))

(defun carriage--sre--scan-generic-token (body)
  "Generic scan ignoring header :delim: match any <<TOKEN ... :TOKEN with TOKEN=[0-9a-f]{6}."
  (let ((res '()))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let ((open-any-rx "^[ \t]*<<\\([0-9a-f]\\{6\\}\\)[ \t]*$"))
        (while (re-search-forward open-any-rx nil t)
          (let ((tok (match-string 1)))
            (forward-line 1)
            (let ((beg (point)))
              (unless (re-search-forward (concat "^[ \t]*:" tok "[ \t]*$") nil t)
                (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT) (list "Unclosed segment")))
              (let ((end (line-beginning-position)))
                (push (buffer-substring-no-properties beg end) res)
                (forward-line 1)))))))
    (nreverse res)))

(defun carriage--sre--scan-greedy-any (body)
  "Greedy token-agnostic scan: from a line starting with << until a line starting with :."
  (let ((greedy '())
        (gstate 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil))
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq gstate 'idle) (string-prefix-p "<<" tln))
          (setq gstate 'in acc nil))
         ((and (eq gstate 'in)
               (not (string-empty-p tln))
               (eq (aref tln 0) ?:))
          (push (mapconcat #'identity (nreverse acc) "\n") greedy)
          (setq acc nil gstate 'idle))
         ((eq gstate 'in)
          (push ln acc)))))
    (nreverse greedy)))

(defun carriage--sre--scan-generic-linewise (body)
  "Line-wise generic scan: any <<[0-9a-f]{6} ... :[0-9a-f]{6} blocks."
  (let ((res '())
        (state5 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil)) ;; keep empties
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq state5 'idle)
               (string-match "\\=<<[0-9a-f]\\{6\\}[ \t]*\\'" tln))
          (setq state5 'in acc nil))
         ((and (eq state5 'in)
               (string-match "\\=:[0-9a-f]\\{6\\}[ \t]*\\'" tln))
          (push (mapconcat #'identity (nreverse acc) "\n") res)
          (setq acc nil state5 'idle))
         ((eq state5 'in)
          (push ln acc)))))
    (nreverse res)))

(defun carriage--sre-scan-segments (body delim)
  "Scan BODY for segments delimited by DELIM and return a list of payload strings.
Implementation uses multiple passes (helpers) and prefers the result with the
greater number of segments to maximize robustness."
  (let* ((open (concat "<<" delim))
         (close (concat ":" delim))
         ;; Pass 1: line-wise tolerant scan with header DELIM
         (segments (carriage--sre--scan-linewise-delim body open close)))
    ;; Pass 2: regex scan with header DELIM (anchored)
    (let ((pass2 (carriage--sre--scan-regexp-delim body open close)))
      (when (> (length pass2) (length segments))
        (setq segments pass2)))
    ;; Pass 3: generic scan ignoring header DELIM (any 6-hex token)
    (let ((pass3 (carriage--sre--scan-generic-token body)))
      (when (> (length pass3) (length segments))
        (setq segments pass3)))
    ;; Pass 4: greedy token-agnostic scan
    (let ((greedy (carriage--sre--scan-greedy-any body)))
      (when (> (length greedy) (length segments))
        (setq segments greedy)))
    ;; Pass 5: line-wise generic scan
    (let ((pass5 (carriage--sre--scan-generic-linewise body)))
      (when (> (length pass5) (length segments))
        (setq segments pass5)))
    segments))


(defun carriage--sre-group-pairs (segments op)
  "Group SEGMENTS into FROM/TO pairs for OP."
  (when (null segments)
    (signal (carriage-error-symbol 'SRE_E_SEGMENTS_COUNT) (list 0)))
  (cond
   ((eq op 'sre)
    (unless (= (length segments) 2)
      (signal (carriage-error-symbol 'SRE_E_SEGMENTS_COUNT) (list (length segments))))
    (list (cons :from (nth 0 segments))
          (cons :to   (nth 1 segments))))
   ((eq op 'sre-batch)
    (when (cl-oddp (length segments))
      (signal (carriage-error-symbol 'SRE_E_SEGMENTS_ODD) (list (length segments))))
    (let ((pairs '()))
      (cl-loop for i from 0 below (length segments) by 2
               do (push (list (cons :from (nth i segments))
                              (cons :to   (nth (1+ i) segments)))
                        pairs))
      (nreverse pairs)))))

(defun carriage--sre-attach-opts-to-pairs (pairs body)
  "Attach options to PAIRS by consuming #+pair directives in BODY in order."
  (let ((lines (split-string body "\n" t nil))
        (pending nil)
        (idx 0)
        (result '()))
    (cl-labels ((next-opts ()
                  (prog1 (carriage--sre-merge-opts pending)
                    (setq pending nil))))
      (dolist (p pairs)
        (while (and (< idx (length lines))
                    (not (string-match "\\`<<[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (let ((opts (carriage--sre-parse-pair-directive (nth idx lines))))
            (when opts (setq pending opts)))
          (setq idx (1+ idx)))
        ;; consume FROM
        (while (and (< idx (length lines))
                    (not (string-match "\\`:[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (setq idx (1+ idx)))
        (setq idx (1+ idx)) ;; skip close
        ;; TO open
        (while (and (< idx (length lines))
                    (not (string-match "\\`<<[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (setq idx (1+ idx)))
        ;; finalize this pair with options
        (push (list (cons :from (alist-get :from p))
                    (cons :to   (alist-get :to p))
                    (cons :opts (next-opts)))
              result)
        ;; skip to end of TO
        (while (and (< idx (length lines))
                    (not (string-match "\\`:[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (setq idx (1+ idx)))
        (setq idx (1+ idx))))
    (nreverse result)))

(defun carriage-parse-sre (header body repo-root)
  "Parse SRE/SRE-BATCH block. Return plan item alist."
  (let* ((op (intern (or (plist-get header :op) "sre")))
         (_ (carriage--sre-validate-header header op))
         (file (plist-get header :file))
         (delim (plist-get header :delim))
         (segments (carriage--sre-scan-segments body delim))
         (pairs-raw (carriage--sre-group-pairs segments op))
         (pairs (if (eq op 'sre)
                    (list (list (cons :from (alist-get :from pairs-raw))
                                (cons :to   (alist-get :to pairs-raw))
                                (cons :opts (carriage--sre-merge-opts nil))))
                  (carriage--sre-attach-opts-to-pairs pairs-raw body)))
         (norm-path (carriage-normalize-path repo-root file)))
    (dolist (p pairs)
      (let* ((opts (alist-get :opts p))
             (occur (plist-get opts :occur))
             (expect (plist-get opts :expect)))
        (when (eq occur 'all)
          (unless (and (integerp expect) (>= expect 0))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) (list "Missing :expect for :occur all"))))))
    (list (cons :version "1")
          (cons :op op)
          (cons :file (file-relative-name norm-path repo-root))
          (cons :pairs pairs))))

;;;; PATCH parsing

(defun carriage--diff-extract-paths (body)
  "Extract --- and +++ paths from BODY. Return (A B)."
  (let ((a nil) (b nil) (count 0))
    (dolist (line (split-string body "\n"))
      (cond
       ((string-match "\\`--- \\(.*\\)\\'" line)
        (setq a (match-string 1 line))
        (setq count (1+ count)))
       ((string-match "\\`\\+\\+\\+ \\(.*\\)\\'" line)
        (setq b (match-string 1 line))
        (setq count (1+ count)))))
    (unless (= count 2)
      (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Missing ---/+++")))
    (list a b)))

(defun carriage--diff-validate-single-file (a b)
  "Validate A and B paths refer to a single file or /dev/null cases."
  (cond
   ((and (string= a "/dev/null")
         (string-match "\\`b/\\(.+\\)\\'" b))
    (cons nil (match-string 1 b)))
   ((and (string-match "\\`a/\\(.+\\)\\'" a)
         (string= b "/dev/null"))
    (cons (match-string 1 a) nil))
   ((and (string-match "\\`a/\\(.+\\)\\'" a)
         (string-match "\\`b/\\(.+\\)\\'" b))
    (let ((ap (match-string 1 a))
          (bp (match-string 1 b)))
      (unless (string= ap bp)
        (signal (carriage-error-symbol 'PATCH_E_PATH_MISMATCH) (list a b)))
      (cons ap bp)))
   (t
    (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Unexpected paths")))))

(defun carriage-parse-diff (header body repo-root)
  "Parse unified diff block BODY with HEADER under REPO-ROOT."
  (let* ((version (plist-get header :version))
         (op (plist-get header :op))
         (strip (or (plist-get header :strip) 1)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'PATCH_E_VERSION) (list version)))
    (unless (string= op "patch")
      (signal (carriage-error-symbol 'PATCH_E_OP) (list op)))
    (let* ((ab (carriage--diff-extract-paths body))
           (pair (carriage--diff-validate-single-file (car ab) (cadr ab)))
           (rel (or (car pair) (cdr pair))))
      (when (carriage--path-looks-unsafe-p rel)
        (signal (carriage-error-symbol 'PATCH_E_PATH) (list rel)))
      (list (cons :version "1")
            (cons :op 'patch)
            (cons :apply 'git-apply)
            (cons :strip strip)
            (cons :path rel)
            (cons :diff body)))))

;;;; File ops parsing

(defun carriage-parse-create (header body repo-root)
  "Parse create block."
  (let* ((file (plist-get header :file))
         (delim (plist-get header :delim))
         (_v (plist-get header :version))
         (mkdir (if (plist-member header :mkdir)
                    (plist-get header :mkdir) t)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list file)))
    (unless (and (stringp delim) (string-match-p "\\`[0-9a-f]\\{6\\}\\'" delim))
      (signal (carriage-error-symbol 'SRE_E_OP) (list "Invalid :delim")))
    (let* ((segments (carriage--sre-scan-segments body delim)))
      (unless (= (length segments) 1)
        (signal (carriage-error-symbol 'SRE_E_SEGMENTS_COUNT) (list (length segments))))
      (carriage-normalize-path repo-root file)
      (list (cons :version "1")
            (cons :op 'create)
            (cons :file file)
            (cons :content (car segments))
            (cons :mkdir mkdir)))))

(defun carriage-parse-delete (header _body repo-root)
  "Parse delete block."
  (let* ((file (plist-get header :file)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list file)))
    (carriage-normalize-path repo-root file)
    (list (cons :version "1") (cons :op 'delete) (cons :file file))))

(defun carriage-parse-rename (header _body repo-root)
  "Parse rename block."
  (let* ((from (plist-get header :from))
         (to   (plist-get header :to)))
    (dolist (p (list from to))
      (unless (and (stringp p) (not (string-empty-p p)))
        (signal (carriage-error-symbol 'OPS_E_PATH) (list p))))
    (carriage-normalize-path repo-root from)
    (carriage-normalize-path repo-root to)
    (list (cons :version "1") (cons :op 'rename) (cons :from from) (cons :to to))))

;;;; Org buffer helpers

(defun carriage--bounds-of-patch-block-at-point ()
  "Return (BEG . END) bounds of the current #+begin_patch ... #+end_patch block.
Move point is not changed. Return nil if not found."
  (save-excursion
    (let ((beg (save-excursion
                 (when (re-search-backward "^[ \t]*#\\+begin_patch\\b" nil t)
                   (point))))
          (end (save-excursion
                 (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                   (line-beginning-position)))))
      (when (and beg end (> end beg))
        (cons beg end)))))

(defun carriage--read-patch-header-at (pos)
  "Parse patch header plist at line around POS. Return plist or signal error."
  (save-excursion
    (goto-char pos)
    (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
      (unless (string-match "#\\+begin_patch\\s-+\\((.*)\\)\\s-*$" line)
        (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "Invalid begin_patch header")))
      (car (read-from-string (match-string 1 line))))))

(defun carriage-parse-block-at-point (repo-root)
  "Parse current org patch block at point into a plan item under REPO-ROOT."
  (let* ((bounds (carriage--bounds-of-patch-block-at-point)))
    (unless bounds
      (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "No patch block at point")))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (header-plist (save-excursion
                           (goto-char beg)
                           (carriage--read-patch-header-at beg)))
           (body (save-excursion
                   (goto-char beg)
                   (forward-line 1)
                   (let ((body-beg (point)))
                     (goto-char end)
                     ;; end points to beginning of #+end_patch line
                     (buffer-substring-no-properties body-beg (line-beginning-position))))))
      (carriage-parse (plist-get header-plist :op) header-plist body repo-root))))

;;;; Region/group parsing

(defun carriage-parse-blocks-in-region (beg end repo-root)
  "Parse all #+begin_patch blocks between BEG and END into a PLAN under REPO-ROOT.
Return a list of plan items in buffer order."
  (save-excursion
    (goto-char beg)
    (let ((plan '()))
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t))
        (let* ((start (match-beginning 0))
               (header-plist (carriage--read-patch-header-at start))
               (body-beg (progn (goto-char (match-end 0))
                                (forward-line 1)
                                (point)))
               (block-end (save-excursion
                            (goto-char body-beg)
                            (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
                              (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT)
                                      (list "Unclosed #+begin_patch block")))
                            (line-beginning-position)))
               (body (buffer-substring-no-properties body-beg block-end))
               (op (plist-get header-plist :op))
               (item (carriage-parse op header-plist body repo-root)))
          (push item plan)
          (goto-char block-end)))
      (nreverse plan))))

(defun carriage-collect-last-iteration-blocks (&optional repo-root)
  "Collect blocks of the 'last iteration' in current buffer and parse to a PLAN.
For v1 stub, this returns ALL patch blocks in the buffer.
If REPO-ROOT is nil, detect via =carriage-project-root' or use =default-directory'."
  (let* ((root (or repo-root (carriage-project-root) default-directory)))
    (carriage-parse-blocks-in-region (point-min) (point-max) root)))

(provide 'carriage-parser)
;;; carriage-parser.el ends here
