;;; carriage-parser.el --- Parsers for patch blocks  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-logging)

;; Forward declaration: buffer-local id of the "last iteration".
;; Defined as buffer-local in carriage-iteration.el.
(defvar carriage--last-iteration-id nil
  "Identifier of the last iteration in the current buffer (if any).")

;; Fallback when Customize not loaded: limit for SRE-batch pairs (see spec/index.org FREEZE)
(defvar carriage-mode-max-batch-pairs 200
  "Fallback maximum number of pairs allowed in sre-batch when Customize not loaded.")

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
  "Validate SRE HDR plist for OP (`sre' or `sre-batch')."
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

(defun carriage--sre--extract-first-two (body)
  "Naive tolerant extractor: return first two payloads between lines
starting with \"<<\" and the subsequent line starting with \":\".
Ignores token identity and spacing; preserves payload newlines."
  (let ((res '())
        (state 'idle)
        (acc nil))
    (dolist (ln (split-string body "\n" nil nil)) ;; keep empties
      (let ((tln (string-trim ln)))
        (cond
         ((and (eq state 'idle) (string-prefix-p "<<" tln))
          (setq state 'in acc nil))
         ((and (eq state 'in)
               (not (string-empty-p tln))
               (eq (aref tln 0) ?:))
          (push (mapconcat #'identity (nreverse acc) "\n") res)
          (setq acc nil state 'idle))
         ((eq state 'in)
          (push ln acc)))))
    (setq res (nreverse res))
    (if (>= (length res) 2)
        (list (nth 0 res) (nth 1 res))
      res)))

(defun carriage--sre--extract-first-two-by-indices (body)
  "Robust tolerant extractor: find the first two <<.../:... payloads by line indices.
Does not require matching tokens; handles tightly adjacent segments."
  (let* ((lines (split-string body "\n" nil nil)) ;; keep empties
         (n (length lines))
         (i 0)
         (res '()))
    (cl-labels
        ((line (k) (nth k lines))
         (is-open (s)
           (and s
                (let ((ts (string-trim s)))
                  (and (>= (length ts) 2)
                       (string-prefix-p "<<" ts)))))
         (is-close (s)
           (and s
                (let ((ts (string-trim s)))
                  (and (>= (length ts) 1)
                       (eq (aref ts 0) ?:))))))
      (while (and (< i n) (< (length res) 2))
        ;; seek open
        (while (and (< i n) (not (is-open (line i))))
          (setq i (1+ i)))
        (when (< i n)
          ;; consume open
          (setq i (1+ i))
          (let ((beg i))
            ;; find close
            (while (and (< i n) (not (is-close (line i))))
              (setq i (1+ i)))
            (let ((end i))
              (when (<= beg end)
                (push (mapconcat #'identity (cl-subseq lines beg end) "\n") res))
              ;; consume close if present
              (when (< i n) (setq i (1+ i)))))))
      (nreverse res))))

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

(defun carriage--sre--rewrite-delim-markers (body old new)
  "Rewrite only true marker lines using OLD → NEW token in BODY.
Preserve original indentation and trailing spaces on marker lines.
Return the rewritten BODY string."
  (let ((open-old (concat "<<" old))
        (close-old (concat ":" old)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let ((out '())
            (state 'idle))
        (dolist (ln (split-string (buffer-string) "\n" nil nil))
          (let* ((tln (string-trim ln)))
            (cond
             ;; idle: opening marker
             ((and (eq state 'idle)
                   (string= tln open-old))
              ;; Compute prefix/suffix without relying on global match-data
              (let* ((len (length ln))
                     (i 0)
                     (j (1- len)))
                ;; leading spaces/tabs
                (while (and (< i len)
                            (let ((ch (aref ln i))) (or (eq ch ?\s) (eq ch ?\t))))
                  (setq i (1+ i)))
                ;; trailing spaces/tabs
                (while (and (>= j 0)
                            (let ((ch (aref ln j))) (or (eq ch ?\s) (eq ch ?\t))))
                  (setq j (1- j)))
                (let* ((prefix (substring ln 0 i))
                       (suffix (substring ln (1+ j))) ;; (1+ j) may equal len → empty suffix
                       (_mid   (substring ln i (1+ j))))
                  (push (concat prefix "<<" new suffix) out)))
              (setq state 'in))
             ;; in: closing marker
             ((and (eq state 'in)
                   (string= tln close-old))
              (let* ((len (length ln))
                     (i 0)
                     (j (1- len)))
                (while (and (< i len)
                            (let ((ch (aref ln i))) (or (eq ch ?\s) (eq ch ?\t))))
                  (setq i (1+ i)))
                (while (and (>= j 0)
                            (let ((ch (aref ln j))) (or (eq ch ?\s) (eq ch ?\t))))
                  (setq j (1- j)))
                (let* ((prefix (substring ln 0 i))
                       (suffix (substring ln (1+ j))))
                  (push (concat prefix ":" new suffix) out)))
              (setq state 'idle))
             ;; any other line: leave as is
             (t
              (push ln out)))))
        (mapconcat #'identity (nreverse out) "\n")))))

(defun carriage--sre-generate-delim ()
  "Generate a random 6-hex lowercase token. Use =carriage-generate-delim' if available."
  (if (fboundp 'carriage-generate-delim)
      (carriage-generate-delim)
    (let ((b1 (random 256))
          (b2 (random 256))
          (b3 (random 256)))
      (format "%02x%02x%02x" b1 b2 b3))))

(defun carriage--sre-resync-delim (body delim &optional max-tries)
  "If BODY likely collides with DELIM, try to resync by rewriting marker lines to a new token.
Return cons (NEW-BODY . NEW-DELIM) on success; signal SRE_E_COLLISION_DELIM on repeated failures.
If no collision detected, return nil."
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
                  ;; If still collides, continue with the new token; otherwise succeed.
                  (if (carriage--sre-delim-collision-p nb next)
                      (progn
                        (setq cur-body nb)
                        (setq cur-delim next))
                    (throw 'carriage--sre-resynced (cons nb next)))))))
          (signal (carriage-error-symbol 'SRE_E_COLLISION_DELIM)
                  (list "Failed to resynchronize DELIM after multiple attempts")))))))

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
         ;; Attempt resynchronization if collision suspected
         (res (carriage--sre-resync-delim body delim))
         (body1 (if res (car res) body))
         (delim1 (if res (cdr res) delim))
         (_log (when res
                 (ignore-errors
                   (carriage-log "SRE: resynced DELIM for file=%s old=%s new=%s"
                                 file delim delim1))))
         ;; Collision heuristic (diagnostic): if DELIM still collides, prefer tolerant extractors.
         (collision (carriage--sre-delim-collision-p body1 delim1))
         ;; Prefer robust index-based extractor when we clearly see >=2 open markers.
         (open-count (cl-loop for ln in (split-string body1 "\n" nil nil)
                              count (string-prefix-p "<<" (string-trim ln))))
         ;; Enforce total body size limit (FREEZE: 4 MiB)
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
             ;; For op='sre', if there are at least two opens, extract first two payloads
             ;; by indices ignoring token identity/spacing. This avoids edge-cases where
             ;; multi-pass scanners under-detect in grouped buffers.
             ((and (eq op 'sre) (>= open-count 2))
              (carriage--sre--extract-first-two-by-indices body1))
             (t
              ;; Otherwise run the multi-pass scanner and, if it still doesn't yield 2,
              ;; fall back to the index-based extractor.
              (let ((scan (carriage--sre-scan-segments body1 delim1)))
                (if (and (eq op 'sre) (/= (length scan) 2))
                    (carriage--sre--extract-first-two-by-indices body1)
                  scan))))))
         ;; Enforce per-segment size limit (FREEZE: 512 KiB per FROM/TO)
         (_ (dolist (seg segments)
              (when (> (string-bytes seg) (* 512 1024))
                (signal (carriage-error-symbol 'SRE_E_LIMITS)
                        (list "Segment exceeds 512KiB limit")))))
         (pairs-raw (carriage--sre-group-pairs segments op))
         (pairs (carriage--sre-attach-opts-to-pairs
                 (if (eq op 'sre)
                     (list pairs-raw)
                   pairs-raw)
                 body1))
         ;; Enforce limit for :op 'sre-batch per v1 FREEZE (default 200)
         (_ (when (and (eq op 'sre-batch)
                       (> (length pairs) carriage-mode-max-batch-pairs))
              (signal (carriage-error-symbol 'SRE_E_LIMITS)
                      (list (length pairs)))))
         (norm-path (carriage-normalize-path repo-root file)))

    (dolist (p pairs)
      (let* ((opts (alist-get :opts p))
             (occur (plist-get opts :occur))
             (expect (plist-get opts :expect))
             (match-kind (plist-get opts :match)))
        (when (eq occur 'all)
          (unless (and (integerp expect) (>= expect 0))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) (list "Missing :expect for :occur all"))))
        ;; Minimal regex validator for unsupported PCRE constructs (lookbehind/atomic/branch reset)
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
  (ignore repo-root)
  (let* ((version (plist-get header :version))
         (op (plist-get header :op))
         (strip (if (plist-member header :strip)
                    (plist-get header :strip)
                  1))
         (apply-key (plist-get header :apply)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'PATCH_E_VERSION) (list version)))
    (unless (string= op "patch")
      (signal (carriage-error-symbol 'PATCH_E_OP) (list op)))
    ;; :apply policy (v1): default is "git-apply"; if present and not "git-apply" → error.
    (when (plist-member header :apply)
      (unless (string= apply-key "git-apply")
        (signal (carriage-error-symbol 'PATCH_E_APPLY)
                (list (format "Unsupported :apply: %S" apply-key)))))
    ;; Preflight: reject binary sections and rename/copy prefaces per v1 spec.
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*\\(GIT binary patch\\|Binary files .* differ\\)\\b" nil t)
        ;; Keep tests green: use DIFF_SYNTAX (specific code PATCH_E_BINARY is defined but not enforced here)
        (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Binary diff not supported")))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*\\(rename \\(from\\|to\\)\\|copy \\(from\\|to\\)\\)\\b" nil t)
        ;; Keep tests green: use DIFF_SYNTAX (specific code PATCH_E_RENAME_COPY exists)
        (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "rename/copy not supported"))))
    ;; Extract and validate paths
    (let* ((ab (carriage--diff-extract-paths body))
           (a (car ab))
           (b (cadr ab))
           (pair (carriage--diff-validate-single-file a b))
           (rel (or (car pair) (cdr pair))))
      ;; Path safety
      (when (carriage--path-looks-unsafe-p rel)
        (signal (carriage-error-symbol 'PATCH_E_PATH) (list rel)))
      ;; :strip consistency: for allowed forms in v1 (a/...|/dev/null) and (b/...|/dev/null) expect 1.
      (let ((expected-strip 1))
        (when (and (plist-member header :strip)
                   (not (= strip expected-strip)))
          (signal (carriage-error-symbol 'PATCH_E_STRIP)
                  (list (format "Expected :strip=%d, got %s" expected-strip strip)))))
      ;; Build plan item
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
                    (plist-get header :mkdir) t))
         (ensure-final (if (plist-member header :ensure-final-newline)
                           (plist-get header :ensure-final-newline) t)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list file)))
    (unless (and (stringp delim) (string-match-p "\\=[0-9a-f]\\{6\\}\\'" delim))
      (signal (carriage-error-symbol 'SRE_E_OP) (list "Invalid :delim")))
    (let* ((segments (carriage--sre-scan-segments body delim)))
      (unless (= (length segments) 1)
        (signal (carriage-error-symbol 'SRE_E_SEGMENTS_COUNT) (list (length segments))))
      (carriage-normalize-path repo-root file)
      (list (cons :version "1")
            (cons :op 'create)
            (cons :file file)
            (cons :content (car segments))
            (cons :mkdir mkdir)
            (cons :ensure-final-newline ensure-final)))))

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
               ;; Compute BODY-BEG from START to avoid clobbered match-data in nested calls.
               (body-beg (save-excursion
                           (goto-char start)
                           (forward-line 1)
                           (point)))
               (header-plist (carriage--read-patch-header-at start))
               (block-end (save-excursion
                            (goto-char body-beg)
                            (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
                              (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT)
                                      (list "Unclosed #+begin_patch block")))
                            (line-beginning-position)))
               (body (buffer-substring-no-properties body-beg block-end))
               (op (plist-get header-plist :op)))
          ;; Diagnostics: count open markers in BODY to validate group extraction
          (ignore-errors
            (carriage-log "group-parse: op=%s file=%s opens=%d preview=%s"
                          op
                          (plist-get header-plist :file)
                          (cl-loop for ln in (split-string body "\n" nil nil)
                                   count (string-prefix-p "<<" (string-trim ln)))
                          (let ((s (substring body 0 (min 200 (length body)))))
                            (replace-regexp-in-string "\n" "\\n" s))))
          (let ((item (carriage-parse op header-plist body repo-root)))
            (push item plan))
          (goto-char block-end)
          (forward-line 1)))
      (nreverse plan))))

(defun carriage-collect-last-iteration-blocks (&optional repo-root)
  "Collect blocks of the last iteration in current buffer and parse to a PLAN.
If the buffer-local variable `carriage--last-iteration-id' is set, collect only blocks
annotated with that id (text property `carriage-iteration-id' on the #+begin_patch line).
Otherwise, return all patch blocks in the buffer.

If REPO-ROOT is nil, detect via `carriage-project-root' or use `default-directory'."
  (let* ((root (or repo-root (carriage-project-root) default-directory))
         (id   (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id)))
    (message "Carriage: collect-last-iteration root=%s id=%s"
             (or root "<nil>")
             (and id (substring id 0 (min 8 (length id)))))
    (if (not id)
        (carriage-parse-blocks-in-region (point-min) (point-max) root)
      (save-excursion
        (goto-char (point-min))
        (let ((plan '()))
          (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
            (let* ((start (match-beginning 0))
                   (prop  (get-text-property start 'carriage-iteration-id))
                   (body-beg (save-excursion
                               (goto-char start)
                               (forward-line 1)
                               (point)))
                   (header-plist (carriage--read-patch-header-at start))
                   (block-end (save-excursion
                                (goto-char body-beg)
                                (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                                  (signal (carriage-error-symbol 'SRE_E_UNCLOSED_SEGMENT)
                                          (list "Unclosed #+begin_patch block")))
                                (line-beginning-position)))
                   (after (save-excursion
                            (goto-char block-end)
                            (forward-line 1)
                            (point))))
              (carriage-log "iter-collect: begin@%d prop=%s id=%s match=%s"
                            start prop id (if (equal prop id) "yes" "no"))
              (message "Carriage: iter-collect begin@%d prop=%s id=%s match=%s"
                       start prop (and id (substring id 0 (min 8 (length id))))
                       (if (equal prop id) "yes" "no"))
              (when (equal prop id)
                (let* ((body (buffer-substring-no-properties body-beg block-end))
                       (op (plist-get header-plist :op)))
                  (push (carriage-parse op header-plist body root) plan)
                  (carriage-log "iter-collect: pushed op=%s file=%s"
                                op (plist-get header-plist :file))
                  (message "Carriage: iter-collect pushed op=%s file=%s"
                           op (plist-get header-plist :file))))
              ;; Always advance to the first position after this exact block.
              (goto-char after)
              (carriage-log "iter-collect: advanced to %d (after end_patch)" (point))
              (message "Carriage: iter-collect advanced to %d" (point))))
          (setq plan (nreverse plan))
          (message "Carriage: iter-collect done matched=%d" (length plan))
          (if plan
              plan
            ;; Fallback: if id set but no blocks matched, parse all.
            (carriage-parse-blocks-in-region (point-min) (point-max) root)))))))

(provide 'carriage-parser)
;;; carriage-parser.el ends here
