;;; fzfa-fuzz-partition.el --- Stream and source differential fuzzing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Checks two boundaries without changing package code.  First, the native
;; command reader must return the same semantic result no matter where stdout
;; writes are split.  Second, the candidate list, zero-argument function,
;; synchronous producer, asynchronous producer, and command forms must deliver
;; the same logical rows when their contracts overlap.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-producer)

(defvar fzfa-fuzz-partition--result-mutator nil
  "Test-only function for corrupting a partition result view.")

(defvar fzfa-fuzz-partition--source-mutator nil
  "Test-only function for corrupting a candidate-source result.")

(defconst fzfa-fuzz-partition--source-kinds
  '(list zero sync async)
  "In-memory candidate forms compared with the command reader.")

(defun fzfa-fuzz-partition--result-view (result)
  "Return the stable command-reader fields from RESULT."
  (let ((status (plist-get result :status)))
    (list :candidates (plist-get result :candidates)
          :producer-state (plist-get status :producer-state)
          :producer-error-present (and (plist-get status :producer-error) t)
          :message-count (length (plist-get result :messages)))))

(defun fzfa-fuzz-partition--one-cut-partitions (bytes)
  "Return every two-chunk partition of BYTES."
  (cl-loop for offset from 1 below (length bytes)
           collect (list (substring bytes 0 offset)
                         (substring bytes offset))))

(defun fzfa-fuzz-partition--bytewise-partition (bytes)
  "Return BYTES split into one-byte chunks."
  (cl-loop for offset below (length bytes)
           collect (substring bytes offset (1+ offset))))

(defun fzfa-fuzz-partition--selected-partitions (bytes cuts)
  "Return two-chunk partitions of BYTES at valid CUTS."
  (mapcar
   (lambda (offset)
     (list (substring bytes 0 offset) (substring bytes offset)))
   (delete-dups
    (cl-remove-if-not
     (lambda (offset) (< 0 offset (length bytes))) cuts))))

(defun fzfa-fuzz-partition--fixture-partitions (fixture)
  "Return the chunk partitions requested by FIXTURE."
  (let ((bytes (plist-get (plist-get fixture :spec) :bytes)))
    (if (plist-get fixture :exhaustive)
        (append (fzfa-fuzz-partition--one-cut-partitions bytes)
                (list (fzfa-fuzz-partition--bytewise-partition bytes)))
      (fzfa-fuzz-partition--selected-partitions
       bytes (funcall (plist-get fixture :cuts) bytes)))))

(defun fzfa-fuzz-partition--fixtures ()
  "Return fixed byte fixtures and their partition policies."
  (let* ((lf (unibyte-string ?\n))
         (crlf (unibyte-string ?\r ?\n))
         (escape (unibyte-string 27))
         (long-a (make-string 1023 ?a))
         (long-b (make-string 1024 ?b))
         (long-c (make-string 1025 ?c))
         (long-bytes
          (fzfa-fuzz-producer--lines (list long-a long-b long-c))))
    (list
     (list :name 'utf8
           :exhaustive t
           :spec (list :kind 'partition-utf8
                       :bytes (fzfa-fuzz-producer--lines
                               '("café" "你好" "plain"))
                       :exit 0 :failure nil
                       :expected '("café" "你好" "plain")))
     (list :name 'crlf
           :exhaustive t
           :spec (list :kind 'partition-crlf
                       :bytes (fzfa-fuzz-producer--lines
                               '("first" "second") crlf)
                       :exit 0 :failure nil
                       :expected '("first" "second")))
     (list :name 'ansi
           :exhaustive t
           :spec (list
                  :kind 'partition-ansi
                  :bytes
                  (concat escape "[31m"
                          (fzfa-fuzz-producer--utf8 "café")
                          escape "[0m" crlf
                          escape "[1m"
                          (fzfa-fuzz-producer--utf8 "你好")
                          escape "[0m" lf)
                  :exit 0 :failure nil :expected '("café" "你好")))
     (list :name 'newline
           :exhaustive t
           :spec (list :kind 'partition-newline
                       :bytes (concat
                               (fzfa-fuzz-producer--lines
                                '("first" "second"))
                               (fzfa-fuzz-producer--utf8 "unterminated"))
                       :exit 0 :failure nil
                       :expected '("first" "second" "unterminated")))
     (list :name 'nul
           :exhaustive t
           :spec (list :kind 'partition-nul
                       :bytes (concat "valid\ninvalid"
                                      (unibyte-string 0)
                                      "hidden\nlate\n")
                       :exit 0 :failure t :expected '("valid")))
     (list :name 'long-cap
           :exhaustive nil
           :cuts
           (lambda (bytes)
             (let ((first-end (1+ (length long-a)))
                   (second-end (+ 2 (length long-a) (length long-b))))
               (list 1 1023 1024 1025
                     (1- first-end) first-end (1+ first-end)
                     (1- second-end) second-end (1+ second-end)
                     (1- (length bytes)))))
           :spec (list :kind 'partition-long-cap :bytes long-bytes
                       :max-line-length 1024
                       :exit 0 :failure nil
                       :expected (list long-a long-b))))))

(defun fzfa-fuzz-partition--check-fixture (root-seed case-seed fixture)
  "Check FIXTURE partitions and return the number run."
  (let* ((spec (plist-get fixture :spec))
         (baseline
          (fzfa-fuzz-producer--run-spec root-seed case-seed spec))
         (expected (fzfa-fuzz-partition--result-view baseline))
         (partitions (fzfa-fuzz-partition--fixture-partitions fixture))
         (index 0))
    (dolist (chunks partitions)
      (let* ((trace (fzfa-fuzz-producer--trace
                     root-seed case-seed spec chunks 0))
             (result (fzfa-fuzz-producer-run-trace trace))
             (observed (fzfa-fuzz-partition--result-view result)))
        (when fzfa-fuzz-partition--result-mutator
          (setq observed
                (funcall fzfa-fuzz-partition--result-mutator
                         fixture index observed)))
        (unless (equal expected observed)
          (fzfa-fuzz--fail-observation
           case-seed trace expected observed
           "stdout partition changed the command-reader result for %S at %d"
           (plist-get fixture :name) index)))
      (cl-incf index))
    index))

(defun fzfa-fuzz-partition-batch ()
  "Check native stdout invariance across systematic chunk partitions."
  (unless (fzfa--command-api-p)
    (error "Partition fuzz requires the fzf-native 2.7 session API"))
  (let ((root-seed (fzfa-fuzz--seed))
        (case-seed (fzfa-fuzz--seed))
        names
        (partitions 0))
    (dolist (fixture (fzfa-fuzz-partition--fixtures))
      (push (plist-get fixture :name) names)
      (cl-incf partitions
               (fzfa-fuzz-partition--check-fixture
                root-seed case-seed fixture))
      (cl-incf case-seed))
    (princ
     (format "fzfa stream partition fuzz passed (%d partitions across %S)\n"
             partitions (nreverse names)))))

(defun fzfa-fuzz-partition--differential-trace
    (root-seed case-seed rows query)
  "Return a replayable cross-source trace for ROWS and QUERY."
  (fzfa-fuzz-trace-create
   'source-differential root-seed case-seed
   (list :rows (fzfa-fuzz-trace-encode-strings rows) :query query)
   '((compare list) (compare zero) (compare sync)
     (compare async) (compare command))))

(defun fzfa-fuzz-partition--decode-differential-trace (trace)
  "Validate TRACE and return its rows and query."
  (let* ((initial (plist-get trace :initial-state))
         (keys (fzfa-fuzz-trace--plist-keys
                initial "source-differential initial state" '(:rows :query)))
         (rows (fzfa-fuzz-trace-decode-strings (plist-get initial :rows)))
         (query (plist-get initial :query))
         (actions (plist-get trace :actions)))
    (fzfa-fuzz-trace--require-keys
     keys '(:rows :query) "source-differential initial state")
    (unless (and (cl-every #'stringp rows)
                 (stringp query)
                 (equal actions
                        '((compare list) (compare zero) (compare sync)
                          (compare async) (compare command))))
      (error "Malformed source-differential trace: %S" trace))
    (list :rows rows :query query)))

(defun fzfa-fuzz-partition--candidate-result (kind rows query)
  "Return candidate snapshot produced by KIND for ROWS and QUERY."
  (let (callback candidates)
    (setq candidates
          (pcase kind
            ('list (fzfa-fuzz--copy-strings rows))
            ('zero
             (lambda () (fzfa-fuzz--copy-strings rows)))
            ('sync
             (lambda (_input deliver)
               (funcall deliver (fzfa-fuzz--copy-strings rows))))
            ('async
             (lambda (_input deliver) (setq callback deliver)))
            (_ (error "Unknown differential source kind: %S" kind))))
    (let ((source (fzfa-make-source
                   :spec (list :name (format "differential-%s" kind)
                               :candidates candidates))))
      (unwind-protect
          (progn
            (fzfa--source-fetch source query)
            (when (eq kind 'async)
              (unless (functionp callback)
                (error "Async differential source did not save its callback"))
              (funcall callback (fzfa-fuzz--copy-strings rows)))
            (mapcar #'substring-no-properties
                    (fzfa-source-snapshot source)))
        (fzfa-source--stop source)))))

(defun fzfa-fuzz-partition-run-differential-trace (trace)
  "Run one validated cross-source TRACE without random generation."
  (fzfa-fuzz-trace-validate trace)
  (unless (eq (plist-get trace :target) 'source-differential)
    (error "Unsupported differential trace target: %S"
           (plist-get trace :target)))
  (fzfa-fuzz--run-trace
   trace
   (lambda ()
     (pcase-let* ((description
                   (fzfa-fuzz-partition--decode-differential-trace trace))
                  (rows (plist-get description :rows))
                  (query (plist-get description :query))
                  (seed (plist-get trace :case-seed))
                  (spec (list :kind 'source-differential
                              :bytes (fzfa-fuzz-producer--lines rows)
                              :exit 0 :failure nil :expected rows))
                  (command-result
                   (fzfa-fuzz-producer--run-spec
                    (plist-get trace :root-seed) seed spec))
                  (expected (plist-get command-result :candidates)))
       (dolist (kind fzfa-fuzz-partition--source-kinds)
         (let ((observed
                (fzfa-fuzz-partition--candidate-result kind rows query)))
           (when fzfa-fuzz-partition--source-mutator
             (setq observed
                   (funcall fzfa-fuzz-partition--source-mutator
                            kind observed)))
           (unless (equal expected observed)
             (fzfa-fuzz--fail-observation
              seed trace expected observed
              "%S candidates differ from command candidates" kind))))
       expected))))

(defun fzfa-fuzz-partition--generated-rows (rng)
  "Return logical candidate rows generated by RNG."
  (cl-loop repeat (1+ (fzfa-fuzz--integer rng 8))
           collect (copy-sequence
                    (fzfa-fuzz--pick rng fzfa-fuzz-producer--words))))

(defun fzfa-fuzz-partition-differential-batch ()
  "Compare generated logical rows across all supported source forms."
  (unless (fzfa--command-api-p)
    (error "Differential fuzz requires the fzf-native 2.7 session API"))
  (let* ((root-seed (fzfa-fuzz--seed))
         (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 100)))
    (dotimes (index cases)
      (let* ((seed (+ root-seed index))
             (rng (fzfa-fuzz-rng-create :state seed))
             (rows (fzfa-fuzz-partition--generated-rows rng))
             (query (fzfa-fuzz--pick rng '("" "a" "same" "你好"))))
        (fzfa-fuzz-partition-run-differential-trace
         (fzfa-fuzz-partition--differential-trace
          root-seed seed rows query))))
    (princ
     (format
      (concat "fzfa source differential fuzz passed (%d cases x "
              "list/zero/sync/async/command, root seed %d)\n")
      cases root-seed))))

(defun fzfa-fuzz-partition-selftest-batch ()
  "Require partition and source differential oracles to kill canaries."
  (fzfa-fuzz--expect-detection
   "partition-result-changed" "stdout partition changed"
   (lambda ()
     (let* ((spec (list :kind 'partition-canary
                        :bytes (fzfa-fuzz-producer--lines '("a" "b"))
                        :exit 0 :failure nil :expected '("a" "b")))
            (fixture (list :name 'canary :exhaustive nil
                           :cuts (lambda (_bytes) '(1)) :spec spec))
            (fzfa-fuzz-partition--result-mutator
             (lambda (_fixture _index view)
               (plist-put (copy-tree view) :candidates '("a")))))
       (fzfa-fuzz-partition--check-fixture 9501 9501 fixture))))
  (fzfa-fuzz--expect-detection
   "source-form-changed" "zero candidates differ"
   (lambda ()
     (let ((fzfa-fuzz-partition--source-mutator
            (lambda (kind candidates)
              (if (eq kind 'zero) (cdr candidates) candidates))))
       (fzfa-fuzz-partition-run-differential-trace
        (fzfa-fuzz-partition--differential-trace
         9502 9502 '("first" "second") "s")))))
  (princ "fzfa partition fuzz self-test passed (2 canaries killed)\n"))

(provide 'fzfa-fuzz-partition)
;;; fzfa-fuzz-partition.el ends here
