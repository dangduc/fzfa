;;; presentation-budget.el --- Adaptive multi-source budget benchmark -*- lexical-binding: t; -*-

;; Run from the repository root with:
;;   emacs -Q --batch -L . -L /path/to/fzf-native \
;;     -l benchmarks/presentation-budget.el

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'fzfa)

(defun fzfa-benchmark--static-shares (limit source-count)
  "Return the former fixed LIMIT shares for SOURCE-COUNT sources."
  (cl-loop for idx below source-count
           collect (+ (/ limit source-count)
                      (if (< idx (% limit source-count)) 1 0))))

(defun fzfa-benchmark--adaptive-pass (limit visible-counts &optional pending-p)
  "Run one adaptive LIMIT pass over VISIBLE-COUNTS.

When PENDING-P is non-nil, reserve every allocation as unfinished work.
Return (ALLOCATIONS RETAINED REMAINING)."
  (let ((budget (fzfa--presentation-budget-create
                 limit (length visible-counts) nil))
        allocations
        (retained 0))
    (dolist (visible visible-counts)
      (let* ((allocation (fzfa--presentation-budget-next budget))
             (bounded (min visible allocation)))
        (push allocation allocations)
        (cl-incf retained bounded)
        (fzfa--presentation-budget-finish
         budget allocation bounded pending-p)))
    (list (nreverse allocations) retained (aref budget 0))))

(defun fzfa-benchmark-presentation-budget (&optional iterations)
  "Benchmark fixed and adaptive presentation allocation ITERATIONS times."
  (let* ((iterations (or iterations 100000))
         (limit 10000)
         (sources 14)
         (saturated (make-list sources limit))
         (sparse (append (make-list (1- sources) 0) (list limit)))
         (static-result (fzfa-benchmark--static-shares limit sources))
         (saturated-result
          (fzfa-benchmark--adaptive-pass limit saturated))
         (sparse-result (fzfa-benchmark--adaptive-pass limit sparse))
         (pending-result
          (fzfa-benchmark--adaptive-pass
           limit (make-list sources 0) t))
         (static-time
          (benchmark-run iterations
            (fzfa-benchmark--static-shares limit sources)))
         (saturated-time
          (benchmark-run iterations
            (fzfa-benchmark--adaptive-pass limit saturated)))
         (sparse-time
          (benchmark-run iterations
            (fzfa-benchmark--adaptive-pass limit sparse)))
         (pending-time
          (benchmark-run iterations
            (fzfa-benchmark--adaptive-pass
             limit (make-list sources 0) t))))
    ;; These invariants make the benchmark double as a work-bound probe.
    (cl-assert (= (nth 1 saturated-result) limit))
    (cl-assert (= (nth 1 sparse-result) limit))
    (cl-assert (= (nth 1 pending-result) 0))
    (cl-assert (= (- limit (nth 2 pending-result)) limit))
    (cl-assert (equal (car saturated-result) static-result))
    (princ
     (format
      (concat "iterations=%d sources=%d limit=%d\n"
              "static-saturated  %S\n"
              "adaptive-full     %S allocations=%S retained=%d\n"
              "adaptive-sparse   %S last-allocation=%d retained=%d\n"
              "adaptive-pending  %S allocations=%S reserved=%d\n")
      iterations sources limit
      static-time
      saturated-time (car saturated-result) (nth 1 saturated-result)
      sparse-time (car (last (car sparse-result))) (nth 1 sparse-result)
      pending-time (car pending-result)
      (- limit (nth 2 pending-result))))))

(when noninteractive
  (fzfa-benchmark-presentation-budget))

(provide 'fzfa-benchmark-presentation-budget)
;;; presentation-budget.el ends here
