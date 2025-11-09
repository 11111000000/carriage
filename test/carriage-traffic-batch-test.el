;;; carriage-traffic-batch-test.el --- ERT tests for traffic batching -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-logging)
(require 'carriage-traffic-batch)

(ert-deftest carriage-traffic-batch-basic-flush ()
  "Enqueue several traffic log entries and ensure they are flushed in a batch."
  (let ((carriage-traffic-batch-enabled t)
        (carriage-traffic-batch-interval 0.02))
    ;; Reset diagnostics
    (setq carriage-traffic-batch--flush-count 0)
    (carriage-clear-logs)
    ;; Enqueue several lines quickly
    (dotimes (i 5)
      (carriage-traffic-log 'in "batch-line-%d" i))
    ;; Allow timer to fire
    (sleep-for 0.1)
    ;; Expect at least one flush
    (should (>= carriage-traffic-batch--flush-count 1))
    ;; Verify lines are present
    (let* ((buf (carriage-traffic-buffer))
           (txt (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (dotimes (i 5)
        (should (string-match-p (format "batch-line-%d" i) txt))))))

(provide 'carriage-traffic-batch-test)
;;; carriage-traffic-batch-test.el ends here
