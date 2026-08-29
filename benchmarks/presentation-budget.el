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

(defun fzfa-benchmark--waterfill-pass (limit demands)
  "Water-fill LIMIT across DEMANDS and return (PLAN RETAINED REMAINING).

Integer demands are completed full-match counts.  Pending demand uses
`fzfa--presentation-pending' and reserves its allocation without retaining a
candidate yet."
  (let* ((plan (fzfa--presentation-waterfill limit demands))
         (retained
          (cl-loop for demand across demands
                   for allocation across plan
                   when (integerp demand)
                   sum (min demand allocation)))
         (allocated (apply #'+ (append plan nil))))
    (list (append plan nil) retained (- limit allocated))))

(defun fzfa-benchmark-presentation-budget (&optional iterations)
  "Benchmark static and water-fill allocation ITERATIONS times."
  (let* ((iterations (or iterations 100000))
         (limit 10000)
         (sources 14)
         (saturated (make-vector sources limit))
         (sparse-first (make-vector sources 0))
         (sparse-last (make-vector sources 0))
         (pending (make-vector sources fzfa--presentation-pending))
         (static-result (fzfa-benchmark--static-shares limit sources))
         (saturated-result
          (fzfa-benchmark--waterfill-pass limit saturated))
         sparse-first-result sparse-last-result
         (static-time
          (benchmark-run iterations
            (fzfa-benchmark--static-shares limit sources)))
         (saturated-time
          (benchmark-run iterations
            (fzfa-benchmark--waterfill-pass limit saturated)))
         sparse-first-time sparse-last-time
         (pending-result (fzfa-benchmark--waterfill-pass limit pending))
         (pending-time
          (benchmark-run iterations
            (fzfa-benchmark--waterfill-pass limit pending))))
    (aset sparse-first 0 limit)
    (aset sparse-last (1- sources) limit)
    (setq sparse-first-result
          (fzfa-benchmark--waterfill-pass limit sparse-first)
          sparse-last-result
          (fzfa-benchmark--waterfill-pass limit sparse-last)
          sparse-first-time
          (benchmark-run iterations
            (fzfa-benchmark--waterfill-pass limit sparse-first))
          sparse-last-time
          (benchmark-run iterations
            (fzfa-benchmark--waterfill-pass limit sparse-last)))
    ;; These invariants make the benchmark double as a work-bound probe.
    (cl-assert (= (nth 1 saturated-result) limit))
    (cl-assert (= (nth 1 sparse-first-result) limit))
    (cl-assert (= (nth 1 sparse-last-result) limit))
    (cl-assert (= (nth 1 pending-result) 0))
    (cl-assert (= (- limit (nth 2 pending-result)) limit))
    (cl-assert (equal (car saturated-result) static-result))
    (princ
     (format
      (concat "iterations=%d sources=%d limit=%d\n"
              "static-saturated  %S\n"
              "waterfill-full    %S allocations=%S retained=%d\n"
              "waterfill-first   %S first-allocation=%d retained=%d\n"
              "waterfill-last    %S last-allocation=%d retained=%d\n"
              "waterfill-pending %S allocations=%S reserved=%d\n")
      iterations sources limit
      static-time
      saturated-time (car saturated-result) (nth 1 saturated-result)
      sparse-first-time (car (car sparse-first-result))
      (nth 1 sparse-first-result)
      sparse-last-time (car (last (car sparse-last-result)))
      (nth 1 sparse-last-result)
      pending-time (car pending-result)
      (- limit (nth 2 pending-result))))))

(when noninteractive
  (fzfa-benchmark-presentation-budget))

(provide 'fzfa-benchmark-presentation-budget)
;;; presentation-budget.el ends here
