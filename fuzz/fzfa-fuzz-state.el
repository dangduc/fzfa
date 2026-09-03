;;; fzfa-fuzz-state.el --- Model fuzzing for fzfa state  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Drives fzfa's real completion tables and source lifecycle functions with
;; deterministic generated traces.  Dependencies such as timers and native
;; status calls are controlled at their existing boundaries; fzfa itself is
;; neither copied nor patched by this harness.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-core)

(defconst fzfa-fuzz-state--words
  '("alpha" "beta" "gamma" "delta" "same" "naive" "你好" "a b" "x:y")
  "Small candidate alphabet used by the state fuzzer.")

(defun fzfa-fuzz-state--candidate (rng source-index candidate-index)
  "Generate a candidate using RNG for SOURCE-INDEX and CANDIDATE-INDEX."
  (let ((value (copy-sequence (fzfa-fuzz--pick rng fzfa-fuzz-state--words))))
    (add-text-properties
     0 (length value)
     `(fzfa-fuzz-origin (,source-index . ,candidate-index)) value)
    value))

(defun fzfa-fuzz-state--candidate-list (rng source-index)
  "Generate one non-empty candidate list for SOURCE-INDEX using RNG."
  (cl-loop for index below (1+ (fzfa-fuzz--integer rng 7))
           collect (fzfa-fuzz-state--candidate rng source-index index)))

(defun fzfa-fuzz-state--mutate-list (value operation rng)
  "Apply destructive list OPERATION to VALUE using RNG."
  (pcase operation
    ('nconc
     (when value (nconc value (fzfa-fuzz--integer rng 20))))
    ('truncate
     (when value
       (setcdr (nthcdr (fzfa-fuzz--integer rng (length value)) value) nil)))
    ('dot
     (when value
       (setcdr (nthcdr (fzfa-fuzz--integer rng (length value)) value)
               (fzfa-fuzz--integer rng 20))))
    ('reverse (nreverse value))
    ('sort (sort value #'string-lessp))
    ('dedup (delete-dups value))))

(defun fzfa-fuzz-state--mutation-case (seed rng)
  "Run one completion-list ownership case using SEED and RNG."
  (let* ((source-count (1+ (fzfa-fuzz--integer rng 3)))
         (operation (fzfa-fuzz--pick
                     rng '(nconc truncate dot reverse sort dedup)))
         (specs
          (cl-loop for source-index below source-count
                   collect
                   (list :name (format "source-%d" source-index)
                         :candidates
                         (fzfa-fuzz-state--candidate-list rng source-index)
                         :category 'fzfa-fuzz
                         :action #'identity)))
         (trace (list :target 'completion-list
                      :sources source-count :operation operation))
         (scheduler (fzfa-fuzz-scheduler-create))
         (original-maker (symbol-function 'fzfa-make-source))
         made-sources)
    (fzfa-fuzz--call-with-scheduler
     scheduler
     (lambda ()
       (let ((completion-category-overrides nil)
             (minibuffer-setup-hook nil)
             (minibuffer-exit-hook nil)
             (post-command-hook nil))
         (cl-letf (((symbol-function 'fzfa-make-source)
                    (lambda (&rest args)
                      (let ((source (apply original-maker args)))
                        (setq made-sources
                              (append made-sources (list source)))
                        source)))
                   ((symbol-function 'sit-for) (lambda (&rest _) nil))
                   ((symbol-function 'fzfa--sessions-push)
                    (lambda (&rest _) nil))
                   ((symbol-function 'completing-read)
                    (lambda (_prompt table &rest _)
                      (let* ((returned (funcall table "" nil t))
                             (snapshots
                              (mapcar
                               (lambda (source)
                                 (fzfa-fuzz--copy-strings
                                  (fzfa-source-snapshot source)))
                               made-sources)))
                        (unless (fzfa-fuzz--proper-list-p returned)
                          (fzfa-fuzz--fail
                           seed trace "initial result is not a proper list: %S"
                           returned))
                        (fzfa-fuzz-state--mutate-list returned operation rng)
                        (cl-mapc
                         (lambda (source expected)
                           (let ((actual (fzfa-source-snapshot source)))
                             (unless (and (fzfa-fuzz--proper-list-p actual)
                                          (equal-including-properties
                                           actual expected))
                               (fzfa-fuzz--fail
                                seed trace
                                (concat "frontend mutation changed snapshot: "
                                        "%S, expected %S")
                                actual expected))))
                         made-sources snapshots)
                        (let ((second (funcall table "" nil t)))
                          (unless (fzfa-fuzz--proper-list-p second)
                            (fzfa-fuzz--fail
                             seed trace
                             "second result is not reusable: %S" second)))
                        nil))))
           (fzfa--read specs :prompt "fuzz: ")))))
    t))

(cl-defstruct (fzfa-fuzz-state--callback
               (:constructor fzfa-fuzz-state--callback-create))
  token kind function)

(defun fzfa-fuzz-state--producer-trace (rng steps)
  "Generate a producer lifecycle trace from RNG with at most STEPS operations."
  (let ((trace (list (list 'fetch "a")))
        stopped)
    (dotimes (_ (max 0 (1- steps)))
      (let ((roll (fzfa-fuzz--integer rng 100)))
        (push
         (cond
          (stopped
           (if (< roll 65)
               (list 'deliver (fzfa-fuzz--integer rng 32)
                     (fzfa-fuzz-state--candidate-list rng 0))
             (list 'run (fzfa-fuzz--integer rng 32))))
          ((< roll 30)
           (list 'fetch (fzfa-fuzz--pick rng '("" "a" "ab" "b" "same"))))
          ((< roll 60)
           (list 'deliver (fzfa-fuzz--integer rng 32)
                 (fzfa-fuzz-state--candidate-list rng 0)))
          ((< roll 78) (list 'run (fzfa-fuzz--integer rng 32)))
          ((< roll 92)
           (list 'restart (fzfa-fuzz--pick rng '("" "a" "new" "other"))))
          (t (setq stopped t) '(stop)))
         trace)))
    (nreverse trace)))

(defun fzfa-fuzz-state--producer-case (seed rng steps)
  "Run one generated producer lifecycle case for SEED using RNG and STEPS."
  (let* ((trace (fzfa-fuzz-state--producer-trace rng steps))
         (scheduler (fzfa-fuzz-scheduler-create))
         callbacks model-tasks
         source current-kind
         (model-token 0)
         (model-input :unfetched)
         model-snapshot
         (model-total 0)
         model-command
         (model-request-epoch 0)
         (refreshes 0)
         (model-refreshes 0)
         (producer
          (lambda (_input callback)
            (setq callbacks
                  (append
                   callbacks
                   (list
                    (fzfa-fuzz-state--callback-create
                     :token (fzfa-source-prod-token source)
                     :kind current-kind :function callback)))))))
    (setq source (fzfa-make-source
                  :spec (list :name "state" :candidates producer)))
    (fzfa-fuzz--call-with-scheduler
     scheduler
     (lambda ()
       (dolist (operation trace)
         (pcase operation
           (`(fetch ,query)
            (let ((changed (not (equal query model-input))))
              (setq current-kind 'fetch)
              (unwind-protect
                  (fzfa--source-fetch source query
                                      (lambda () (cl-incf refreshes)))
                (setq current-kind nil))
              (when changed
                (setq model-input query)
                (cl-incf model-token))))
           (`(restart ,query)
            (setq current-kind 'restart)
            (unwind-protect
                (fzfa-source--restart
                 source query (lambda () (cl-incf refreshes)))
              (setq current-kind nil))
            (cl-incf model-request-epoch)
            (cl-incf model-token)
            (setq model-command query))
           (`(deliver ,selector ,candidates)
            (when callbacks
              (let* ((entry (nth (% selector (length callbacks)) callbacks))
                     (token (fzfa-fuzz-state--callback-token entry))
                     (kind (fzfa-fuzz-state--callback-kind entry)))
                (funcall (fzfa-fuzz-state--callback-function entry) candidates)
                (when (= token model-token)
                  (setq model-snapshot candidates
                        model-total (length candidates))
                  (if (eq kind 'fetch)
                      (setq model-tasks
                            (append model-tasks (list (cons token nil))))
                    (cl-incf model-refreshes))))))
           (`(run ,selector)
            (let ((pending-model
                   (cl-remove-if #'cdr model-tasks)))
              (when pending-model
                (let* ((index (% selector (length pending-model)))
                       (model-task (nth index pending-model)))
                  (setcdr model-task t)
                  (when (= (car model-task) model-token)
                    (cl-incf model-refreshes))
                  (fzfa-fuzz--run-task scheduler index)))))
           (`(stop)
            (fzfa-source--stop source)
            (cl-incf model-request-epoch)
            (cl-incf model-token)))
         (unless (= (fzfa-source-prod-token source) model-token)
           (fzfa-fuzz--fail
            seed trace "producer token is %S, expected %S after %S"
            (fzfa-source-prod-token source) model-token operation))
         (unless (equal (fzfa-source-prod-input source) model-input)
           (fzfa-fuzz--fail
            seed trace "producer input is %S, expected %S after %S"
            (fzfa-source-prod-input source) model-input operation))
         (unless (= (fzfa-source-request-epoch source) model-request-epoch)
           (fzfa-fuzz--fail
            seed trace "request epoch is %S, expected %S after %S"
            (fzfa-source-request-epoch source) model-request-epoch operation))
         (unless (equal (fzfa-source-current-cmd source) model-command)
           (fzfa-fuzz--fail
            seed trace "current command is %S, expected %S after %S"
            (fzfa-source-current-cmd source) model-command operation))
         (unless (equal-including-properties
                  (fzfa-source-snapshot source) model-snapshot)
           (fzfa-fuzz--fail
            seed trace "snapshot is %S, expected %S after %S"
            (fzfa-source-snapshot source) model-snapshot operation))
         (unless (= (fzfa-source-total source) model-total)
           (fzfa-fuzz--fail
            seed trace "total is %S, expected %S after %S"
            (fzfa-source-total source) model-total operation))
         (unless (= refreshes model-refreshes)
           (fzfa-fuzz--fail
            seed trace "refresh count is %S, expected %S after %S"
            refreshes model-refreshes operation)))))
    ;; Teardown must make every captured callback and queued refresh inert.
    (unless (and trace (eq (caar (last trace)) 'stop))
      (fzfa-source--stop source)
      (cl-incf model-request-epoch)
      (cl-incf model-token))
    (let ((snapshot model-snapshot)
          (total model-total)
          (before-refreshes refreshes))
      (dolist (entry callbacks)
        (funcall (fzfa-fuzz-state--callback-function entry) '("late")))
      (fzfa-fuzz--run-all-tasks scheduler)
      (unless (and (equal-including-properties
                    (fzfa-source-snapshot source) snapshot)
                   (= (fzfa-source-total source) total)
                   (= refreshes before-refreshes))
        (fzfa-fuzz--fail seed trace "teardown allowed stale work to publish")))
    t))

(defun fzfa-fuzz-state--poller-replay (seed)
  "Replay publication after handle replacement for SEED."
  (let* ((trace '((generation old 1) tick (replace-handle) run))
         (source (fzfa-make-source :command "producer"))
         (generations '((old . 1) (new . 0)))
         scheduled
         (refreshes 0)
         (alive t))
    (setf (fzfa-source-handle source) 'old)
    (cl-letf (((symbol-function 'fzfa--poll-generation)
               (lambda (handle) (alist-get handle generations)))
              ((symbol-function 'input-pending-p) (lambda () nil))
              ((symbol-function 'float-time) (lambda (&optional _) 1.0)))
      (let ((poll
             (fzfa--make-poll-fn
              (vector source) (lambda () alive)
              (lambda () (cl-incf refreshes) t)
              (lambda () nil)
              (lambda (transaction) (setq scheduled transaction)))))
        (funcall poll)
        (unless scheduled
          (fzfa-fuzz--fail seed trace "poller did not schedule publication"))
        (setf (fzfa-source-handle source) 'new)
        (funcall scheduled)
        (unless (= (fzfa-source-last-gen source) -1)
          (fzfa-fuzz--fail
           seed trace "old handle generation committed after replacement"))))
    t))

(defun fzfa-fuzz-state--message-events (context)
  "Return `fzfa--print' events for CONTEXT.

CONTEXT is `owner', `process', or `none'.  The owner and process variants
model an active fzfa minibuffer; only the current buffer differs."
  (let* ((window (selected-window))
         (original-buffer (window-buffer window))
         (owner (generate-new-buffer " *fzfa fuzz owner*"))
         (worker (generate-new-buffer " *fzfa fuzz process*"))
         (session (list 'session))
         events)
    (unwind-protect
        (progn
          (set-window-buffer window owner)
          (with-current-buffer owner
            (setq-local fzfa--minibuffer-session session))
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     (lambda () (unless (eq context 'none) window)))
                    ((symbol-function 'minibufferp)
                     (lambda (&optional buffer &rest _)
                       (eq (or buffer (current-buffer)) owner)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (list 'log
                                   (apply #'format format-string args)
                                   inhibit-message (current-buffer))
                             events)))
                    ((symbol-function 'minibuffer-message)
                     (lambda (format-string &rest args)
                       (push (list 'inline
                                   (apply #'format format-string args)
                                   (current-buffer))
                             events))))
            (with-current-buffer (if (eq context 'owner) owner worker)
              (fzfa--print "problem %d" 7))))
      (set-window-buffer window original-buffer)
      (kill-buffer owner)
      (kill-buffer worker))
    (nreverse events)))

(defun fzfa-fuzz-state--message-violation (context events)
  "Return a message ownership violation for CONTEXT and EVENTS, or nil."
  (let* ((log (assq 'log events))
         (inline (assq 'inline events))
         (active (not (eq context 'none))))
    (cond
     ((not (= (length (cl-remove-if-not
                       (lambda (event) (eq (car event) 'log)) events)) 1))
      'log-count)
     ((and active (not (nth 2 log))) 'echo-not-inhibited)
     ((and active (null inline)) 'inline-missing)
     ((and (not active) inline) 'inline-without-owner)
     ((and (not active) (nth 2 log)) 'echo-inhibited-without-owner))))

(defun fzfa-fuzz-state--message-case (seed rng)
  "Run one message ownership case for SEED using RNG.

Return non-nil for the one known worker-buffer ownership gap."
  (let* ((context (fzfa-fuzz--pick rng '(owner process none)))
         (trace (list :target 'message-owner :context context))
         (events (fzfa-fuzz-state--message-events context))
         (violation (fzfa-fuzz-state--message-violation context events)))
    (cond
     ((and (eq context 'process)
           (memq violation '(echo-not-inhibited inline-missing)))
      (list trace violation events))
     (violation
      (fzfa-fuzz--fail seed trace "message events violate ownership: %S (%S)"
                       events violation))
     (t nil))))

(defun fzfa-fuzz-replay-batch ()
  "Run fixed regression seeds in batch mode."
  (let* ((seed (fzfa-fuzz--seed))
         (rng (fzfa-fuzz-rng-create :state seed)))
    (fzfa-fuzz-state--mutation-case seed rng)
    (fzfa-fuzz-state--producer-case seed rng 30)
    (fzfa-fuzz-state--poller-replay seed)
    (let* ((events (fzfa-fuzz-state--message-events 'process))
           (violation (fzfa-fuzz-state--message-violation 'process events)))
      (if violation
          (princ (format "KNOWN message-owner/process: %S\n" violation))
        (princ "RESOLVED message-owner/process\n")))
    (princ (format "fzfa fuzz replay passed (seed %d)\n" seed))))

(defun fzfa-fuzz-state-batch ()
  "Run deterministic randomized state cases in batch mode."
  (let* ((root-seed (fzfa-fuzz--seed))
         (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 300))
         (steps (fzfa-fuzz--env-natural "FZFA_FUZZ_STEPS" 40))
         (known-message-gaps 0))
    (dotimes (index cases)
      (let* ((seed (+ root-seed index))
             (rng (fzfa-fuzz-rng-create :state seed)))
        (fzfa-fuzz-state--mutation-case seed rng)
        (fzfa-fuzz-state--producer-case seed rng steps)
        (when (fzfa-fuzz-state--message-case seed rng)
          (cl-incf known-message-gaps))))
    (princ
     (format
      (concat "fzfa state fuzz passed (%d cases x %d steps, root seed %d); "
              "%d cases reached the known process-buffer message gap\n")
      cases steps root-seed known-message-gaps))))

(provide 'fzfa-fuzz-state)
;;; fzfa-fuzz-state.el ends here
