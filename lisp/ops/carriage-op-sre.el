;;; carriage-op-sre.el --- SRE ops (sre, sre-batch) handlers and prompt fragment  -*- lexical-binding: t; -*-
;;

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)
(require 'carriage-sre-core)

(defvar carriage-mode-sre-preview-max 3
  "Default maximum number of SRE preview chunks when Customize is not loaded.")

;;;; Prompt fragment

(defun carriage-op-sre-prompt-fragment (ctx)
  "Return prompt fragment for SRE ops. CTX may contain :delim, :file hints."
  (let* ((delim (or (plist-get ctx :delim) (carriage-generate-delim))))
    (concat
     "Формат SRE (поисково-заменяющий блок для одного файла):\n"
     "#+begin_patch (:version \"1\" :op \"sre\" :file \"RELATIVE/PATH\" :delim \"" delim "\")\n"
     "<<" delim "\nFROM-строки\n:" delim "\n"
     "<<" delim "\nTO-строки\n:" delim "\n"
     "#+end_patch\n\n"
     "SRE-BATCH (несколько пар для одного файла):\n"
     "#+begin_patch (:version \"1\" :op \"sre-batch\" :file \"RELATIVE/PATH\" :delim \"" delim "\")\n"
     "#+pair (:occur all :expect K) ; опционально для СЛЕДУЮЩЕЙ пары\n"
     "<<" delim "\nFROM\n:" delim "\n"
     "<<" delim "\nTO\n:" delim "\n"
     "#+end_patch\n"
     "- Для :occur all обязателен :expect.\n"
     "- Ничего вне блоков. DELIM не менять.\n")))

;;;; Internal helpers (SRE)

(defun carriage--sre-make-regexp (from match-kind)
  "Return regexp for FROM according to MATCH-KIND ('literal or 'regex)."
  (if (eq match-kind 'regex) from (regexp-quote (or from ""))))

(defun carriage--sre-count-nonoverlapping (text regexp)
  "Count non-overlapping matches of REGEXP in TEXT.
Guards against zero-length matches to avoid infinite loops: when
(match-end 0) equals POS, advance POS by one character."
  (let ((pos 0) (cnt 0))
    (while (and (< pos (length text))
                (string-match regexp text pos))
      (setq cnt (1+ cnt))
      (let ((next (match-end 0)))
        (setq pos (if (= next pos) (1+ pos) next))))
    cnt))

(defun carriage--sre-slice-by-lines (text range-plist)
  "Return (PRE REGION POST) for TEXT restricted by RANGE-PLIST (:start-line N :end-line M)."
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

(defun carriage--sre-effective-range (text range-plist)
  "Return normalized range plist (:start-line N :end-line M :clamped t/nil) within TEXT."
  (if (not range-plist)
      nil
    (let* ((start0 (plist-get range-plist :start-line))
           (end0   (plist-get range-plist :end-line))
           (total-lines (with-temp-buffer
                          (insert text)
                          (goto-char (point-max))
                          (line-number-at-pos (point-max))))
           (s (max 1 (or start0 1)))
           (e (or end0 total-lines))
           (s1 (min s (max 1 total-lines)))
           (e1 (min (max s1 e) total-lines))
           (clamped (or (not (equal s1 s)) (not (equal e1 e)))))
      (list :start-line s1 :end-line e1 :clamped (and clamped t)))))

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

(defun carriage--sre-first-match-pos (text regexp)
  "Return cons of (START . END) for first match of REGEXP in TEXT, or nil."
  (let ((p (string-match regexp text)))
    (and p (cons p (match-end 0)))))

(defun carriage--sre-line-bounds-at (text pos)
  "Return cons (LINE-START . LINE-END) bounds around POS in TEXT (without newline)."
  (let ((start pos) (end pos)
        (len (length text)))
    (while (and (> start 0) (not (eq (aref text (1- start)) ?\n)))
      (setq start (1- start)))
    (while (and (< end len) (not (eq (aref text end) ?\n)))
      (setq end (1+ end)))
    (cons start end)))

(defun carriage--sre-prev-line-bounds (text ls)
  "Return bounds (START . END) of the previous line before LS in TEXT, or nil."
  (when (> ls 0)
    (let ((i (1- ls)))
      (while (and (>= i 0) (not (eq (aref text i) ?\n)))
        (setq i (1- i)))
      (when (>= i 0)
        (let ((j (1- i)))
          (while (and (>= j 0) (not (eq (aref text j) ?\n)))
            (setq j (1- j)))
          (cons (1+ j) i))))))

(defun carriage--sre-next-line-bounds (text le)
  "Return bounds (START . END) of the next line after LE in TEXT, or nil."
  (let ((len (length text)))
    (when (< le len)
      (let ((i le))
        (when (and (< i len) (eq (aref text i) ?\n))
          (setq i (1+ i)))
        (when (< i len)
          (let ((j i))
            (while (and (< j len) (not (eq (aref text j) ?\n)))
              (setq j (1+ j)))
            (cons i j)))))))

(defun carriage--sre-build-preview-at (region start end to match-kind)
  "Build a single-line preview for REGION by replacing the match [START,END) with TO."
  (ignore match-kind)
  (let* ((lb (carriage--sre-line-bounds-at region start))
         (ls (car lb)) (le (cdr lb))
         (line (substring region ls le))
         (end1 (min end le))
         (s (- start ls))
         (e (- end1 ls))
         (before (substring line 0 s))
         (after (substring line e))
         (mid-rep to))
    (format "-%s\n+%s" line (concat before mid-rep after))))

(defun carriage--sre-build-preview-with-context (region start end to match-kind context-lines)
  "Build preview with CONTEXT-LINES lines above and below the changed line."
  (let* ((lb (carriage--sre-line-bounds-at region start))
         (ls (car lb)) (le (cdr lb))
         (plus-minus (carriage--sre-build-preview-at region start end to match-kind))
         (before-bounds '())
         (after-bounds '())
         (prev-ls ls)
         (next-le le))
    (dotimes (_ context-lines)
      (let ((pb (carriage--sre-prev-line-bounds region prev-ls)))
        (when pb (push pb before-bounds) (setq prev-ls (car pb)))))
    (dotimes (_ context-lines)
      (let ((nb (carriage--sre-next-line-bounds region next-le)))
        (when nb (push nb after-bounds) (setq next-le (cdr nb)))))
    (let ((parts '()))
      (dolist (b (nreverse before-bounds))
        (push (substring region (car b) (cdr b)) parts))
      (push plus-minus parts)
      (dolist (b (nreverse after-bounds))
        (push (substring region (car b) (cdr b)) parts))
      (mapconcat #'identity (nreverse parts) "\n"))))

(defun carriage--sre-previews-for-region (region rx to match-kind occur count)
  "Return list of preview chunks for REGION using RX/TO; OCCUR is 'first or 'all."
  (ignore count)
  (let ((maxn (or (and (boundp 'carriage-mode-sre-preview-max) carriage-mode-sre-preview-max) 3))
        (ctx (or (and (boundp 'carriage-mode-sre-preview-context-lines)
                      carriage-mode-sre-preview-context-lines)
                 0)))
    (pcase occur
      ('first
       (let* ((mpos (carriage--sre-first-match-pos region rx))
              (pv (when mpos
                    (carriage--sre-build-preview-with-context region (car mpos) (cdr mpos) to match-kind ctx))))
         (if pv (list pv) '())))
      (_
       (let* ((pos-list (let ((pos 0) (acc '()))
                          (while (and (< pos (length region))
                                      (string-match rx region pos)
                                      (< (length acc) (max 0 (or maxn 0))))
                            (push (cons (match-beginning 0) (match-end 0)) acc)
                            (let ((next (match-end 0)))
                              (setq pos (if (= next pos) (1+ pos) next))))
                          (nreverse acc)))
              (previews (mapcar (lambda (p)
                                  (carriage--sre-build-preview-with-context region (car p) (cdr p) to match-kind ctx))
                                pos-list)))
         previews)))))

;;;; Parse (sre, sre-batch)

(defun carriage--sre-validate-header (hdr op)
  "Validate SRE HDR plist for OP (=sre' or =sre-batch')."
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
      (signal (carriage-error-symbol 'SRE_E_DELIM) (list "Invalid :delim")))
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

(defun carriage--sre-kv (obj key)
  "Get KEY from OBJ which may be an alist or a plist."
  (if (plist-member obj key) (plist-get obj key) (alist-get key obj)))


(defun carriage--sre--scan-linewise-delim (body open close)
  "Scan BODY line-wise using explicit OPEN/CLOSE tokens. Return list of payloads."
  (let ((segments '())
        (state 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil))
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq state 'idle) (string= tln open)) (setq state 'in acc nil))
         ((and (eq state 'in) (string= tln close))
          (push (mapconcat #'identity (nreverse acc) "\n") segments)
          (setq acc nil state 'idle))
         ((eq state 'in) (push ln acc)))))
    (when (eq state 'in)
      (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT) (list "Unclosed segment")))
    (nreverse segments)))

(defun carriage--sre--scan-regexp-delim (body open close)
  "Scan BODY using anchored regex with explicit OPEN/CLOSE tokens."
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
  "Generic scan ignoring header :delim: any <<[0-9a-f]{6} ... :[0-9a-f]{6}."
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
         ((and (eq gstate 'idle) (string-prefix-p "<<" tln)) (setq gstate 'in acc nil))
         ((and (eq gstate 'in) (not (string-empty-p tln)) (eq (aref tln 0) ?:))
          (push (mapconcat #'identity (nreverse acc) "\n") greedy)
          (setq acc nil gstate 'idle))
         ((eq gstate 'in) (push ln acc)))))
    (nreverse greedy)))

(defun carriage--sre--scan-generic-linewise (body)
  "Line-wise generic scan: any <<[0-9a-f]{6} ... :[0-9a-f]{6} blocks."
  (let ((res '())
        (state5 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil))
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq state5 'idle) (string-match "\\`<<[0-9a-f]\\{6\\}[ \t]*\\'" tln)) (setq state5 'in acc nil))
         ((and (eq state5 'in) (string-match "\\`:[0-9a-f]\\{6\\}[ \t]*\\'" tln))
          (push (mapconcat #'identity (nreverse acc) "\n") res)
          (setq acc nil state5 'idle))
         ((eq state5 'in) (push ln acc)))))
    (nreverse res)))

(defun carriage--sre--extract-first-two-by-indices (body)
  "Robust tolerant extractor: find the first two <<.../:... payloads by line indices."
  (let* ((lines (split-string body "\n" nil nil))
         (n (length lines))
         (i 0)
         (res '()))
    (cl-labels
        ((line (k) (nth k lines))
         (is-open (s) (and s (let ((ts (string-trim s))) (and (>= (length ts) 2) (string-prefix-p "<<" ts)))))
         (is-close (s) (and s (let ((ts (string-trim s))) (and (>= (length ts) 1) (eq (aref ts 0) ?:))))))
      (while (and (< i n) (< (length res) 2))
        (while (and (< i n) (not (is-open (line i)))) (setq i (1+ i)))
        (when (< i n)
          (setq i (1+ i))
          (let ((beg i))
            (while (and (< i n) (not (is-close (line i)))) (setq i (1+ i)))
            (let ((end i))
              (when (<= beg end)
                (push (mapconcat #'identity (cl-subseq lines beg end) "\n") res))
              (when (< i n) (setq i (1+ i))))))))
    (nreverse res)))

(defun carriage--sre-delim-collision-p (body delim)
  "Heuristically detect potential DELIM collision inside BODY."
  (let* ((open (concat "<<" delim))
         (close (concat ":" delim))
         (open-count (cl-loop for ln in (split-string body "\n" nil nil)
                              count (string= (string-trim ln) open)))
         (close-count (cl-loop for ln in (split-string body "\n" nil nil)
                               count (string= (string-trim ln) close)))
         (scan1 (ignore-errors (carriage--sre--scan-linewise-delim body open close)))
         (scan3 (ignore-errors (carriage--sre--scan-generic-token body)))
         (greedy (ignore-errors (carriage--sre--scan-greedy-any body))))
    (or (/= open-count close-count)
        (and scan1 scan3 (> (length scan3) (length scan1)))
        (and scan1 greedy (> (length greedy) (length scan1))))))



(defun carriage--sre-generate-delim ()
  "Generate a random 6-hex lowercase token."
  (if (fboundp 'carriage-generate-delim)
      (carriage-generate-delim)
    (let ((b1 (random 256))
          (b2 (random 256))
          (b3 (random 256)))
      (format "%02x%02x%02x" b1 b2 b3))))

(defun carriage--sre-resync-delim (body delim &optional max-tries)
  "If BODY collides with DELIM, try to resync by rewriting marker lines to a new token."
  (let ((tries (or max-tries 8)))
    (if (not (carriage--sre-delim-collision-p body delim))
        nil
      (let ((cur-body body)
            (cur-delim delim))
        (catch 'carriage--sre-resynced
          (dotimes (_ tries)
            (let ((next (carriage--sre-generate-delim)))
              (when (not (string= next cur-delim))
                (let ((nb (carriage--sre--rewrite-delim-markers cur-body cur-delim next)))
                  (if (carriage--sre-delim-collision-p nb next)
                      (progn (setq cur-body nb) (setq cur-delim next))
                    (throw 'carriage--sre-resynced (cons nb next)))))))
          (signal (carriage-error-symbol 'SRE_E_COLLISION_DELIM)
                  (list "Failed to resynchronize DELIM after multiple attempts")))))))

(defun carriage--sre-scan-segments (body delim)
  "Scan BODY for segments delimited by DELIM and return a list of payload strings."
  (let* ((open (concat "<<" delim))
         (close (concat ":" delim))
         (segments (carriage--sre--scan-linewise-delim body open close)))
    (let ((pass2 (carriage--sre--scan-regexp-delim body open close)))
      (when (> (length pass2) (length segments)) (setq segments pass2)))
    (let ((pass3 (carriage--sre--scan-generic-token body)))
      (when (> (length pass3) (length segments)) (setq segments pass3)))
    (let ((greedy (carriage--sre--scan-greedy-any body)))
      (when (> (length greedy) (length segments)) (setq segments greedy)))
    (let ((pass5 (carriage--sre--scan-generic-linewise body)))
      (when (> (length pass5) (length segments)) (setq segments pass5)))
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
    (cl-labels ((next-opts () (prog1 (carriage--sre-merge-opts pending) (setq pending nil))))
      (dolist (p pairs)
        (while (and (< idx (length lines))
                    (not (string-match "\\`<<[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (let ((opts (carriage--sre-parse-pair-directive (nth idx lines))))
            (when opts (setq pending opts)))
          (setq idx (1+ idx)))
        (while (and (< idx (length lines))
                    (not (string-match "\\`:[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (setq idx (1+ idx)))
        (setq idx (1+ idx))
        (while (and (< idx (length lines))
                    (not (string-match "\\`<<[0-9a-f]\\{6\\}\\'" (nth idx lines))))
          (setq idx (1+ idx)))
        (push (list (cons :from (alist-get :from p))
                    (cons :to   (alist-get :to p))
                    (cons :opts (next-opts)))
              result)
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
         (res (carriage--sre-resync-delim body delim))
         (body1 (if res (car res) body))
         (delim1 (if res (cdr res) delim))
         (_log (when res
                 (ignore-errors
                   (carriage-log "SRE: resynced DELIM for file=%s old=%s new=%s"
                                 file delim delim1))))
         (collision (carriage--sre-delim-collision-p body1 delim1))
         (open-count (cl-loop for ln in (split-string body1 "\n" nil nil)
                              count (string-prefix-p "<<" (string-trim ln))))
         (_ (when (> (string-bytes body1) (* 4 1024 1024))
              (signal (carriage-error-symbol 'SRE_E_LIMITS)
                      (list "Response body exceeds 4MiB limit"))))
         (segments
          (progn
            (when collision
              (ignore-errors
                (carriage-log "SRE: possible DELIM collision for file=%s delim=%s; using tolerant extraction"
                              file delim1)))
            (cond
             ((and (eq op 'sre) (>= open-count 2))
              (carriage--sre--extract-first-two-by-indices body1))
             (t
              (let ((scan (carriage--sre-scan-segments body1 delim1)))
                (if (and (eq op 'sre) (/= (length scan) 2))
                    (carriage--sre--extract-first-two-by-indices body1)
                  scan))))))
         (_ (dolist (seg segments)
              (when (> (string-bytes seg) (* 512 1024))
                (signal (carriage-error-symbol 'SRE_E_LIMITS)
                        (list "Segment exceeds 512KiB limit")))))
         (pairs-raw (carriage--sre-group-pairs segments op))
         (pairs (carriage--sre-attach-opts-to-pairs
                 (if (eq op 'sre) (list pairs-raw) pairs-raw)
                 body1))
         ;; Enforce FREEZE: limit pairs for sre-batch
         (_ (let ((maxn (or (and (boundp 'carriage-mode-max-batch-pairs) carriage-mode-max-batch-pairs) 200)))
              (when (and (eq op 'sre-batch) (> (length pairs) maxn))
                (signal (carriage-error-symbol 'SRE_E_LIMITS)
                        (list (format "Too many pairs: %d (max %d)" (length pairs) maxn))))))
         (norm-path (carriage-normalize-path repo-root file)))
    (dolist (p pairs)
      (let* ((opts (alist-get :opts p))
             (occur (plist-get opts :occur))
             (expect (plist-get opts :expect))
             (match-kind (plist-get opts :match)))
        (when (eq occur 'all)
          (unless (and (integerp expect) (>= expect 0))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) (list "Missing :expect for :occur all"))))
        (when (eq match-kind 'regex)
          (let ((pat (alist-get :from p)))
            (when (and (stringp pat)
                       (or (string-match-p "(?<=" pat)
                           (string-match-p "(?<!"
                                           pat)
                           (string-match-p "(?>"
                                           pat)
                           (string-match-p "(?|"
                                           pat)))
              (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
                      (list "Unsupported regex construct (PCRE-style)")))))))
    (list (cons :version "1")
          (cons :op op)
          (cons :file (file-relative-name norm-path repo-root))
          (cons :meta (and res (list :resynced-delim (list :old delim :new delim1))))
          (cons :pairs pairs))))

;;;; Dry-run and apply (SRE)

(defun carriage-dry-run-sre (plan-item repo-root)
  "Dry-run SRE: count matches per pair; check :expect for :occur all; produce preview."
  (let* ((file (alist-get :file plan-item))
         (abs (ignore-errors (carriage-normalize-path (or repo-root default-directory) file))))
    (if (not (and abs (file-exists-p abs)))
        (append (list :op 'sre :status 'fail) (list :file file :details "File not found"))
      (let* ((text (carriage-read-file-string abs)))
        (carriage-sre-dry-run-on-text plan-item text)))))

(defun carriage-sre-dry-run-on-text (plan-item text)
  "Dry-run SRE on TEXT content. Return report alist."
  (let* ((file (alist-get :file plan-item))
         (pairs (or (alist-get :pairs plan-item) '()))
         (total-matches 0)
         (errors nil)
         (warns nil)
         (previews '())
         (any-noop nil))
    (dolist (p pairs)
      (let* ((from (carriage--sre-kv p :from))
             (to   (carriage--sre-kv p :to))
             (opts (carriage--sre-kv p :opts))
             (range (plist-get opts :range))
             (occur (or (plist-get opts :occur) 'first))
             (expect (plist-get opts :expect))
             (match-kind (or (plist-get opts :match) 'literal))
             (eff-range (and range (carriage--sre-effective-range text range)))
             (region (cond
                      (eff-range (cadr (carriage--sre-slice-by-lines text eff-range)))
                      (range     (cadr (carriage--sre-slice-by-lines text range)))
                      (t         text)))
             (rx (carriage--sre-make-regexp from match-kind))
             (count (condition-case e
                        (carriage--sre-count-matches region from opts)
                      (error
                       (push (format "Regex error: %s" (error-message-string e)) errors)
                       -1))))
        (when (and eff-range (plist-get eff-range :clamped))
          (push (format "range clamped to %d..%d"
                        (plist-get eff-range :start-line)
                        (plist-get eff-range :end-line))
                warns))
        (when (>= count 0)
          (setq total-matches (+ total-matches count))
          (let ((pvs (carriage--sre-previews-for-region region rx to match-kind occur count)))
            (dolist (pv pvs) (push pv previews))
            (when (and (eq occur 'all) (> count (length pvs)))
              (push (format "(+%d more)" (- count (length pvs))) previews)))
          (when (and (eq occur 'all) (integerp expect) (not (= count expect)))
            (push (format "Expect mismatch: have %d, expect %d" count expect) errors))
          (when (and (eq occur 'first) (= count 0))
            (if (and (boundp 'carriage-mode-sre-noop-on-zero-matches)
                     carriage-mode-sre-noop-on-zero-matches)
                (setq any-noop t)
              (push "No matches for :occur first" errors))))))
    (let* ((preview (when previews (mapconcat #'identity (nreverse previews) "\n\n")))
           (warn-tail (when warns (format "; %s" (mapconcat #'identity (nreverse warns) "; "))))
           (itm-messages
            (let ((acc '()))
              (when warns
                (dolist (w (nreverse warns))
                  (push (list :code 'SRE_W_RANGE_CLAMP :severity 'warn :file file :details w) acc)))
              (let* ((meta (and plan-item (alist-get :meta plan-item)))
                     (rs   (and meta (plist-get meta :resynced-delim))))
                (when rs
                  (push (list :code 'SRE_W_DELIM_RESYNC :severity 'warn :file file
                              :details (format "DELIM resynced %s→%s"
                                               (plist-get rs :old) (plist-get rs :new)))
                        acc)))
              (when any-noop
                (push (list :code 'SRE_W_NOOP :severity 'warn :file file
                            :details "No matches (occur first)") acc))
              (nreverse acc))))
      (if errors
          (let ((base (list :op 'sre :status 'fail
                            :file file
                            :matches total-matches
                            :details (concat
                                      (format "fail: pairs:%d matches:%d; %s"
                                              (length pairs) total-matches
                                              (mapconcat #'identity (nreverse errors) "; "))
                                      (or warn-tail ""))
                            :diff (or preview ""))))
            (if itm-messages (append base (list :_messages itm-messages)) base))
        (let* ((status (if (and any-noop (= total-matches 0)) 'skip 'ok))
               (details (if (eq status 'skip)
                            (concat
                             (format "skip: noop (occur first); pairs:%d matches:%d"
                                     (length pairs) total-matches)
                             (or warn-tail ""))
                          (concat
                           (format "ok: pairs:%d matches:%d"
                                   (length pairs) total-matches)
                           (or warn-tail ""))))
               (base (list :op 'sre :status status
                           :file file
                           :matches total-matches
                           :details details
                           :diff (or preview ""))))
          (if itm-messages (append base (list :_messages itm-messages)) base))))))

(defun carriage-sre-simulate-apply (plan-item repo-root)
  "Simulate apply for PLAN-ITEM under REPO-ROOT and return (:after STRING :count N).
Does not touch filesystem or Git; replaces content in-memory."
  (let* ((file (alist-get :file plan-item))
         (abs (and file (ignore-errors (carriage-normalize-path (or repo-root default-directory) file)))))
    (if (not (and abs (file-exists-p abs)))
        (list :after "" :count 0)
      (let* ((text (carriage-read-file-string abs))
             (pairs (or (alist-get :pairs plan-item) '()))
             (changed 0)
             (new-text text))
        (dolist (p pairs)
          (let* ((from (carriage--sre-kv p :from))
                 (to   (carriage--sre-kv p :to))
                 (opts (carriage--sre-kv p :opts))
                 (range (plist-get opts :range))
                 (match-kind (or (plist-get opts :match) 'literal))
                 (occur (or (plist-get opts :occur) 'first))
                 (rx (carriage--sre-make-regexp from match-kind))
                 (eff-range (and range (carriage--sre-effective-range new-text range)))
                 (slice (carriage--sre-slice-by-lines new-text (or eff-range range)))
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
        (list :after new-text :count changed)))))

(defun carriage-apply-sre (plan-item repo-root)
  "Apply SRE pairs by rewriting file. Commit is not performed here.
Optional staging per `carriage-apply-stage-policy'."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path (or repo-root default-directory) file))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy)))
    (unless (file-exists-p abs)
      (cl-return-from carriage-apply-sre
        (list :op 'sre :status 'fail :file file :details "File not found")))
    (let* ((text (carriage-read-file-string abs))
           (pairs (or (alist-get :pairs plan-item) '()))
           (changed 0)
           (new-text text))
      (dolist (p pairs)
        (let* ((from (carriage--sre-kv p :from))
               (to   (carriage--sre-kv p :to))
               (opts (carriage--sre-kv p :opts))
               (range (plist-get opts :range))
               (match-kind (or (plist-get opts :match) 'literal))
               (occur (or (plist-get opts :occur) 'first))
               (rx (carriage--sre-make-regexp from match-kind))
               (eff-range (and range (carriage--sre-effective-range new-text range)))
               (slice (carriage--sre-slice-by-lines new-text (or eff-range range)))
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
          (list :op 'sre :status 'fail :file file :details "No changes")
        (progn
          (carriage-write-file-string abs new-text t)
          (when (eq stage 'index)
            (carriage-git-add repo-root file))
          (list :op 'sre :status 'ok :file file :details (format "Applied %d replacements" changed)))))))

;;;; Registration

(carriage-format-register 'sre "1"
                          :parse #'carriage-parse-sre
                          :dry-run #'carriage-dry-run-sre
                          :apply #'carriage-apply-sre
                          :prompt-fragment #'carriage-op-sre-prompt-fragment)

(carriage-format-register 'sre-batch "1"
                          :parse #'carriage-parse-sre
                          :dry-run #'carriage-dry-run-sre
                          :apply #'carriage-apply-sre
                          :prompt-fragment #'carriage-op-sre-prompt-fragment)

(provide 'carriage-op-sre)
;;; carriage-op-sre.el ends here
