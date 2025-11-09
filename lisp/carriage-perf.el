;;; carriage-perf.el --- Lightweight UI/Perf optimizations (caching and gating) -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup carriage-perf nil
  "Performance helpers for Carriage UI: caching icons, outline and project root."
  :group 'carriage)

(defcustom carriage-perf-cache-icons t
  "Cache results of icon availability and built icon strings."
  :type 'boolean :group 'carriage-perf)

(defcustom carriage-perf-cache-outline t
  "Cache org outline string used in header-line to avoid repeated parsing during redisplay."
  :type 'boolean :group 'carriage-perf)

(defcustom carriage-perf-cache-project t
  "Cache project root/name buffer-locally to avoid expensive detection during redisplay."
  :type 'boolean :group 'carriage-perf)

;; Global generation for theme/frame changes to invalidate caches.
(defvar carriage-perf--gen 0
  "Global generation used to invalidate cached values on theme/frame changes.")

(defun carriage-perf-reset ()
  "Invalidate cached values across buffers."
  (interactive)
  (cl-incf carriage-perf--gen))

;; Invalidate caches on theme enable/disable and new frame creation.
(advice-add 'enable-theme :after (lambda (&rest _ignore) (carriage-perf-reset)))
(advice-add 'disable-theme :after (lambda (&rest _ignore) (carriage-perf-reset)))
(with-eval-after-load 'frame
  (add-hook 'after-make-frame-functions (lambda (_f) (carriage-perf-reset))))

;;;; Icons: availability + icon string memoization

(defvar-local carriage-perf--icons-available-cache :unset)
(defvar-local carriage-perf--icons-available-gen -1)

(defvar carriage-perf--icon-table (make-hash-table :test 'equal))
(defvar carriage-perf--icon-table-gen -1)

(defun carriage-perf--icons-available-around (orig &rest args)
  "Memoize `carriage-ui--icons-available-p' result per buffer and generation."
  (if (not carriage-perf-cache-icons)
      (apply orig args)
    (if (= carriage-perf--icons-available-gen carriage-perf--gen)
        carriage-perf--icons-available-cache
      (let ((val (apply orig args)))
        (setq carriage-perf--icons-available-cache val
              carriage-perf--icons-available-gen carriage-perf--gen)
        val))))

(defun carriage-perf--icon-around (orig &rest args)
  "Memoize `carriage-ui--icon' string based on ARGS and visual context."
  (if (not carriage-perf-cache-icons)
      (apply orig args)
    ;; Reset table on generation bump (theme/frame change)
    (when (/= carriage-perf--icon-table-gen carriage-perf--gen)
      (setq carriage-perf--icon-table (make-hash-table :test 'equal)
            carriage-perf--icon-table-gen carriage-perf--gen))
    (let* ((key (list args (display-graphic-p) (frame-parameter nil 'background-mode)))
           (hit (gethash key carriage-perf--icon-table 'miss)))
      (if (eq hit 'miss)
          (let ((val (apply orig args)))
            (puthash key val carriage-perf--icon-table)
            val)
        hit))))

;;;; Outline path caching

(defvar-local carriage-perf--outline-cache nil)
(defvar-local carriage-perf--outline-point nil)
(defvar-local carriage-perf--outline-tick nil)
(defvar-local carriage-perf--outline-gen -1)

(defun carriage-perf--outline-around (orig &rest args)
  "Cache `carriage-ui--org-outline-path' per buffer based on point and buffer tick.
Avoids repeated Org parsing in redisplay hot-path."
  (let ((show-outline (and (boundp 'carriage-mode-headerline-show-outline)
                           (symbol-value 'carriage-mode-headerline-show-outline))))
    (if (or (not carriage-perf-cache-outline)
            (not show-outline))
        (apply orig args)
      (let* ((tick (buffer-chars-modified-tick))
             (pt (point)))
        (if (and (= carriage-perf--outline-gen carriage-perf--gen)
                 (eq carriage-perf--outline-tick tick)
                 (eq carriage-perf--outline-point pt))
            carriage-perf--outline-cache
          (let ((val (apply orig args)))
            (setq carriage-perf--outline-cache val
                  carriage-perf--outline-point pt
                  carriage-perf--outline-tick tick
                  carriage-perf--outline-gen carriage-perf--gen)
            val))))))

;;;; Project root caching

(defvar-local carriage-perf--project-root-cache :unset)

(defun carriage-perf--clear-project-cache ()
  "Clear buffer-local project root cache."
  (kill-local-variable 'carriage-perf--project-root-cache))

(add-hook 'find-file-hook #'carriage-perf--clear-project-cache)
(add-hook 'after-revert-hook #'carriage-perf--clear-project-cache)

(defun carriage-perf--project-root-around (orig &rest args)
  "Buffer-local cache around `carriage-project-root'."
  (if (not carriage-perf-cache-project)
      (apply orig args)
    (if (not (eq carriage-perf--project-root-cache :unset))
        carriage-perf--project-root-cache
      (let ((val (apply orig args)))
        (setq carriage-perf--project-root-cache val)
        val))))

;;;; Install advices when corresponding features are loaded

(with-eval-after-load 'carriage-ui
  (when (fboundp 'carriage-ui--icons-available-p)
    (advice-add 'carriage-ui--icons-available-p :around #'carriage-perf--icons-available-around))
  (when (fboundp 'carriage-ui--icon)
    (advice-add 'carriage-ui--icon :around #'carriage-perf--icon-around))
  (when (fboundp 'carriage-ui--org-outline-path)
    (advice-add 'carriage-ui--org-outline-path :around #'carriage-perf--outline-around)))

(with-eval-after-load 'carriage
  (when (fboundp 'carriage-project-root)
    (advice-add 'carriage-project-root :around #'carriage-perf--project-root-around)))

(provide 'carriage-perf)
;;; carriage-perf.el ends here
