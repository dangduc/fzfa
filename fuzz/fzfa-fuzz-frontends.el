;;; fzfa-fuzz-frontends.el --- Live frontend matrix for fzfa  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Drives the same recorded completion session through Emacs's built-in
;; completion UI, Vertico, or Helm.  The driver observes each frontend after
;; its real update hook: start with every candidate, narrow to one, erase back
;; to a fresh full list, narrow again, and accept the selected candidate.
;; This file must run in interactive Emacs because all three paths need a live
;; minibuffer and their normal redisplay/event loop.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-core)

(defvar helm-after-update-hook)
(defvar helm-alive-p)
(defvar helm-current-source)
(defvar helm-pattern)
(defvar helm-sources)
(defvar helm-visible-mark-overlays)
(defvar vertico--candidates)
(defvar vertico--index)
(defvar vertico--input)
(defvar vertico--total)
(declare-function helm-buffer-get "helm-lib" ())
(declare-function helm-clear-visible-mark "helm-core" ())
(declare-function helm-get-selection "helm-core"
                  (&optional buffer force-display-part source))
(declare-function helm-exit-minibuffer "helm-core" ())
(declare-function helm-mode "helm-mode" (&optional arg))
(declare-function helm-pos-header-line-p "helm-core" ())
(declare-function helm-update "helm-core" (&optional preselect source candidates))
(declare-function minibuffer-completion-help "minibuffer")
(declare-function vertico--exhibit "vertico")
(declare-function vertico-mode "vertico" (&optional arg))

(defvar fzfa-fuzz-frontends--driver-config nil
  "Dynamically bound configuration copied into the live minibuffer.")

(defvar fzfa-fuzz-frontends--observations nil
  "Chronological logical observations for the current live case.")

(defvar fzfa-fuzz-frontends--sequence 0
  "Sequence number assigned to the next frontend observation.")

(defvar fzfa-fuzz-frontends--observation-mutator nil
  "Test-only function for corrupting a logical frontend observation.")

(defvar fzfa-fuzz-frontends--watchdog-seconds 5
  "Seconds before a live frontend handshake is aborted.")

(defvar-local fzfa-fuzz-frontends--frontend nil)
(defvar-local fzfa-fuzz-frontends--phase nil)
(defvar-local fzfa-fuzz-frontends--expected-query nil)
(defvar-local fzfa-fuzz-frontends--target-query nil)
(defvar-local fzfa-fuzz-frontends--full-count nil)
(defvar-local fzfa-fuzz-frontends--pending-actions nil)
(defvar-local fzfa-fuzz-frontends--remaining-actions-cell nil)
(defvar-local fzfa-fuzz-frontends--event-log-cell nil)
(defvar-local fzfa-fuzz-frontends--failure-cell nil)
(defvar-local fzfa-fuzz-frontends--progress-cell nil)
(defvar-local fzfa-fuzz-frontends--narrow-sequence nil)
(defvar-local fzfa-fuzz-frontends--event-timer nil)
(defvar-local fzfa-fuzz-frontends--refresh-timer nil)
(defvar-local fzfa-fuzz-frontends--watchdog-timer nil)

(defun fzfa-fuzz-frontends--report (text)
  "Write TEXT to standard output and the optional result file."
  (princ text)
  (when-let* ((file (getenv "FZFA_FUZZ_RESULT_FILE")))
    (with-temp-file file
      (insert text))))

(defun fzfa-fuzz-frontends--plain-string (value)
  "Return VALUE without text properties when it is a string."
  (and (stringp value) (substring-no-properties value)))

(defun fzfa-fuzz-frontends--cached-candidates ()
  "Return plain strings from the built-in frontend's dotted cache."
  (let ((tail completion-all-sorted-completions)
        candidates)
    (while (consp tail)
      (when (stringp (car tail))
        (push (substring-no-properties (car tail)) candidates))
      (setq tail (cdr tail)))
    (nreverse candidates)))

(defun fzfa-fuzz-frontends--minibuffer-query (buffer)
  "Return the editable completion query in minibuffer BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))

(defun fzfa-fuzz-frontends--fail-driver (oracle format-string &rest args)
  "Record stable ORACLE and FORMAT-STRING, then make the UI quit."
  (unless (car fzfa-fuzz-frontends--failure-cell)
    (setcar fzfa-fuzz-frontends--failure-cell
            (list :oracle oracle
                  :message (apply #'format format-string args))))
  (setq unread-command-events (list 7)))

(defun fzfa-fuzz-frontends--deliver-event (buffer event)
  "Deliver EVENT to live minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-frontends--event-timer nil)
      (when (minibufferp buffer)
        (setq unread-command-events
              (append unread-command-events (list event)))))))

(defun fzfa-fuzz-frontends--deliver-accept (buffer)
  "Accept the stable selection in live minibuffer BUFFER."
  (when-let* (((buffer-live-p buffer))
              (window (active-minibuffer-window))
              ((eq buffer (window-buffer window))))
    (with-selected-window window
      (with-current-buffer buffer
        (setq fzfa-fuzz-frontends--event-timer nil)
        (if (eq fzfa-fuzz-frontends--frontend 'helm)
            (helm-exit-minibuffer)
          (exit-minibuffer))))))

(defun fzfa-fuzz-frontends--queue-event (event expected-query)
  "Queue EVENT once and wait for EXPECTED-QUERY to be observed."
  (unless fzfa-fuzz-frontends--event-timer
    (setq fzfa-fuzz-frontends--expected-query expected-query
          fzfa-fuzz-frontends--event-timer
          ;; A zero-delay timer may run before the frontend's update callback
          ;; has unwound.  Give the command loop one turn so it reliably reads
          ;; the queued event instead of going idle with it still unread.
          (run-at-time
           0.01 nil #'fzfa-fuzz-frontends--deliver-event
           (current-buffer) event))))

(defun fzfa-fuzz-frontends--queue-next-action ()
  "Queue the next recorded input action."
  (unless fzfa-fuzz-frontends--event-timer
    (if (null fzfa-fuzz-frontends--pending-actions)
        (fzfa-fuzz-frontends--fail-driver
         'trace-actions-exhausted
         "frontend trace ran out of actions in phase %S"
         fzfa-fuzz-frontends--phase)
      (let ((action (car fzfa-fuzz-frontends--pending-actions)))
        (cond
         ((and (eq (car-safe action) 'key)
               (integerp (nth 1 action))
               (stringp (nth 2 action))
               (null (nthcdr 3 action)))
          (setq fzfa-fuzz-frontends--pending-actions
                (cdr fzfa-fuzz-frontends--pending-actions))
          (setcar fzfa-fuzz-frontends--remaining-actions-cell
                  (copy-tree fzfa-fuzz-frontends--pending-actions))
          (setcar fzfa-fuzz-frontends--event-log-cell
                  (append
                   (car fzfa-fuzz-frontends--event-log-cell)
                   (list
                    (list :phase fzfa-fuzz-frontends--phase
                          :after-sequence fzfa-fuzz-frontends--sequence
                          :action (copy-tree action)))))
          (fzfa-fuzz-frontends--queue-event
           (nth 1 action) (nth 2 action)))
         ((and (eq (car-safe action) 'accept)
               (stringp (nth 1 action))
               (null (nthcdr 2 action)))
          (setq fzfa-fuzz-frontends--pending-actions
                (cdr fzfa-fuzz-frontends--pending-actions))
          (setcar fzfa-fuzz-frontends--remaining-actions-cell
                  (copy-tree fzfa-fuzz-frontends--pending-actions))
          (setcar fzfa-fuzz-frontends--event-log-cell
                  (append
                   (car fzfa-fuzz-frontends--event-log-cell)
                   (list
                    (list :phase fzfa-fuzz-frontends--phase
                          :after-sequence fzfa-fuzz-frontends--sequence
                          :action (copy-tree action)))))
          (setq fzfa-fuzz-frontends--expected-query (nth 1 action)
                fzfa-fuzz-frontends--event-timer
                (run-at-time
                 0.01 nil #'fzfa-fuzz-frontends--deliver-accept
                 (current-buffer))))
         (t
          (fzfa-fuzz-frontends--fail-driver
           'malformed-trace-action
           "malformed frontend input action: %S" action)))))))

(defun fzfa-fuzz-frontends--target-observation-p (observation)
  "Return non-nil when OBSERVATION is narrowed to the target."
  (and (> (plist-get observation :candidate-count) 0)
       (< (plist-get observation :candidate-count)
          fzfa-fuzz-frontends--full-count)
       (equal (plist-get observation :selection)
              fzfa-fuzz-frontends--target-query)))

(defun fzfa-fuzz-frontends--advance-driver (observation)
  "Advance the input handshake after logical OBSERVATION."
  (let ((query (plist-get observation :query))
        (count (plist-get observation :candidate-count)))
    (pcase fzfa-fuzz-frontends--phase
      ('initial
       (when (equal query "")
         ;; Vertico can exhibit once before fzfa's collection is installed.
         ;; That empty render is real but is not the initialized state this
         ;; handshake is waiting for.
         (when (= count fzfa-fuzz-frontends--full-count)
           (setq fzfa-fuzz-frontends--phase 'narrowing)
           (setcar fzfa-fuzz-frontends--progress-cell 'initial)
           (fzfa-fuzz-frontends--queue-next-action))))
      ('narrowing
       (when (equal query fzfa-fuzz-frontends--expected-query)
         (if (< (length query)
                (length fzfa-fuzz-frontends--target-query))
             (fzfa-fuzz-frontends--queue-next-action)
           (if (fzfa-fuzz-frontends--target-observation-p observation)
               (progn
                 (setq fzfa-fuzz-frontends--phase 'widening
                       fzfa-fuzz-frontends--narrow-sequence
                       (plist-get observation :sequence))
                 (setcar fzfa-fuzz-frontends--progress-cell 'narrow)
                 (fzfa-fuzz-frontends--queue-next-action))
             (fzfa-fuzz-frontends--fail-driver
              'narrow-selection-wrong
              "narrow frontend update did not select the target: %S"
              observation)))))
      ('widening
       (when (equal query fzfa-fuzz-frontends--expected-query)
         (if (> (length query) 0)
             (fzfa-fuzz-frontends--queue-next-action)
           (if (and (> (plist-get observation :sequence)
                       fzfa-fuzz-frontends--narrow-sequence)
                    (= count fzfa-fuzz-frontends--full-count))
               (progn
                 (setq fzfa-fuzz-frontends--phase 'accepting)
                 (setcar fzfa-fuzz-frontends--progress-cell 'empty-restored)
                 (fzfa-fuzz-frontends--queue-next-action))
             (fzfa-fuzz-frontends--fail-driver
              'empty-candidates-not-restored
              "fresh empty frontend update did not restore all candidates: %S"
              observation)))))
      ('accepting
       (when (equal query fzfa-fuzz-frontends--expected-query)
         (if (< (length query)
                (length fzfa-fuzz-frontends--target-query))
             (fzfa-fuzz-frontends--queue-next-action)
           (if (fzfa-fuzz-frontends--target-observation-p observation)
               (progn
                 (setq fzfa-fuzz-frontends--phase 'exiting)
                 (setcar fzfa-fuzz-frontends--progress-cell 'target-reselected)
                 (fzfa-fuzz-frontends--queue-next-action))
             (fzfa-fuzz-frontends--fail-driver
              'reselected-target-wrong
              "second narrow did not reselect the target: %S"
              observation))))))))

(defun fzfa-fuzz-frontends--record (buffer query count selection)
  "Record an update for minibuffer BUFFER with QUERY, COUNT, and SELECTION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (minibufferp buffer) fzfa-fuzz-frontends--phase)
        (let ((observation
               (list :sequence (cl-incf fzfa-fuzz-frontends--sequence)
                     :frontend fzfa-fuzz-frontends--frontend
                     :query query :candidate-count count
                     :selection
                     (fzfa-fuzz-frontends--plain-string selection))))
          (when fzfa-fuzz-frontends--observation-mutator
            (setq observation
                  (funcall fzfa-fuzz-frontends--observation-mutator
                           observation)))
          (setq fzfa-fuzz-frontends--observations
                (append fzfa-fuzz-frontends--observations
                        (list observation)))
          (fzfa-fuzz-frontends--advance-driver observation))))))

(defun fzfa-fuzz-frontends--observe-vertico (&rest _)
  "Observe Vertico after it has committed an update."
  (when-let* ((window (active-minibuffer-window))
              (buffer (window-buffer window)))
    (fzfa-fuzz-frontends--record
     buffer (fzfa-fuzz-frontends--minibuffer-query buffer)
     vertico--total
     (and (>= vertico--index 0)
          (nth vertico--index vertico--candidates)))))

(defun fzfa-fuzz-frontends--helm-rendered-candidate-count ()
  "Count the single-line candidates rendered in Helm's result buffer.

The live fuzz source only generates non-empty, single-line strings.  Count
those rendered lines directly so the initial observation does not depend on
whether Helm has already moved its window point off the source header."
  (with-current-buffer (helm-buffer-get)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (< (point) (point-max))
          (unless (or (= (line-beginning-position) (line-end-position))
                      (helm-pos-header-line-p))
            (cl-incf count))
          (forward-line 1))
        count))))

(defun fzfa-fuzz-frontends--observe-helm (&rest _)
  "Observe Helm after its normal update function commits a render."
  (condition-case err
      (when-let* ((helm-alive-p)
                  (window (active-minibuffer-window))
                  (buffer (window-buffer window))
                  (result-buffer (helm-buffer-get))
                  (source
                   (with-current-buffer result-buffer
                     (car helm-sources))))
        (fzfa-fuzz-frontends--record
         buffer (fzfa-fuzz-frontends--minibuffer-query buffer)
         (fzfa-fuzz-frontends--helm-rendered-candidate-count)
         ;; Timer-driven force updates run after Helm's dynamic current-source
         ;; binding has unwound.  Pass the one fuzz source explicitly.
         (helm-get-selection result-buffer nil source)))
    (error
     (fzfa-fuzz-frontends--fail-driver
      'helm-observation-error "Helm observation failed: %S" err))))

(defun fzfa-fuzz-frontends--default-refresh (buffer)
  "Refresh and observe built-in completion for minibuffer BUFFER."
  (when-let* (((buffer-live-p buffer))
              (window (active-minibuffer-window))
              ((eq buffer (window-buffer window))))
    (with-selected-window window
      (with-current-buffer buffer
        (setq fzfa-fuzz-frontends--refresh-timer nil)
        (when (minibufferp buffer)
          (minibuffer-completion-help)
          ;; Emacs 32's eager display does not leave the minibuffer's dotted
          ;; cache populated after rendering *Completions*.  Ask the standard
          ;; completion API for the same sorted set before observing it.
          (completion-all-sorted-completions)
          (let ((candidates (fzfa-fuzz-frontends--cached-candidates)))
            (fzfa-fuzz-frontends--record
             buffer (fzfa-fuzz-frontends--minibuffer-query buffer)
             (length candidates) (car candidates))))))))

(defun fzfa-fuzz-frontends--schedule-default-refresh ()
  "Schedule the built-in completion UI to update after the current command."
  (when (and (eq fzfa-fuzz-frontends--frontend 'default)
             (not fzfa-fuzz-frontends--refresh-timer))
    (setq fzfa-fuzz-frontends--refresh-timer
          (run-with-idle-timer
           0 nil #'fzfa-fuzz-frontends--default-refresh
           (current-buffer)))))

(defun fzfa-fuzz-frontends--watchdog (buffer)
  "Abort live BUFFER when its frontend handshake stops progressing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (minibufferp buffer) fzfa-fuzz-frontends--phase)
        (fzfa-fuzz-frontends--fail-driver
         'watchdog-timeout
         "frontend %S timed out in phase %S waiting for query %S"
         fzfa-fuzz-frontends--frontend fzfa-fuzz-frontends--phase
         fzfa-fuzz-frontends--expected-query)))))

(defun fzfa-fuzz-frontends--kick-vertico (buffer)
  "Request Vertico's initial exhibit in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-frontends--refresh-timer nil)
      (when (minibufferp buffer)
        ;; An automatic exhibit can cache the empty input before fzfa installs
        ;; its collection.  Use Vertico's sentinel to force recomputation.
        (setq vertico--input t)
        (vertico--exhibit)))))

(defun fzfa-fuzz-frontends--kick-helm (buffer)
  "Observe Helm's first render after minibuffer BUFFER becomes active."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-frontends--refresh-timer nil)
      (when (minibufferp buffer)
        (if (zerop (fzfa-fuzz-frontends--helm-rendered-candidate-count))
            ;; On a cold terminal session Helm can activate the minibuffer
            ;; before its first result buffer has been populated.  Ask Helm
            ;; for its normal update; the `helm-update' advice observes the
            ;; committed result after the source has really been evaluated.
            (condition-case err
                (let* ((query
                        (fzfa-fuzz-frontends--minibuffer-query buffer))
                       (result-buffer (helm-buffer-get))
                       (source
                        (with-current-buffer result-buffer
                          (car helm-sources))))
                  (with-current-buffer result-buffer
                    ;; Helm stores these as buffer-local state.  Bind them
                    ;; after entering the result buffer so Emacs 30 does not
                    ;; see the cold buffer's nil pattern.  The harness has no
                    ;; user-created marks to revive, and a cold Helm session
                    ;; may leave an incomplete selection overlay behind.
                    (when helm-visible-mark-overlays
                      (helm-clear-visible-mark))
                    (let ((helm-pattern query)
                          (helm-current-source source))
                      ;; This bootstrap only needs source evaluation and
                      ;; render; `helm-force-update' also preserves the old
                      ;; selection and recenters, which assume ordinary
                      ;; command-loop context.
                      (helm-update nil source))))
              (error
               (fzfa-fuzz-frontends--fail-driver
                'helm-initial-refresh-error
                "Helm initial refresh failed: %S" err)))
          (fzfa-fuzz-frontends--observe-helm))))))

(defun fzfa-fuzz-frontends--cleanup ()
  "Cancel timers owned by the current live frontend driver."
  (dolist (timer (list fzfa-fuzz-frontends--event-timer
                       fzfa-fuzz-frontends--refresh-timer
                       fzfa-fuzz-frontends--watchdog-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq fzfa-fuzz-frontends--event-timer nil
        fzfa-fuzz-frontends--refresh-timer nil
        fzfa-fuzz-frontends--watchdog-timer nil))

(defun fzfa-fuzz-frontends--driver-setup ()
  "Install the observation-driven input handshake in the minibuffer."
  (let ((config fzfa-fuzz-frontends--driver-config))
    (setq-local fzfa-fuzz-frontends--frontend
                (plist-get config :frontend))
    (setq-local fzfa-fuzz-frontends--phase 'initial)
    (setq-local fzfa-fuzz-frontends--expected-query "")
    (setq-local fzfa-fuzz-frontends--target-query
                (plist-get config :target))
    (setq-local fzfa-fuzz-frontends--full-count
                (length (plist-get config :candidates)))
    (setq-local fzfa-fuzz-frontends--pending-actions
                (copy-tree (plist-get config :actions)))
    (setq-local fzfa-fuzz-frontends--remaining-actions-cell
                (plist-get config :remaining-actions-cell))
    (setq-local fzfa-fuzz-frontends--event-log-cell
                (plist-get config :event-log-cell))
    (setq-local fzfa-fuzz-frontends--failure-cell
                (plist-get config :failure-cell))
    (setq-local fzfa-fuzz-frontends--progress-cell
                (plist-get config :progress-cell)))
  (add-hook 'minibuffer-exit-hook #'fzfa-fuzz-frontends--cleanup nil t)
  (when (eq fzfa-fuzz-frontends--frontend 'default)
    (add-hook 'post-command-hook
              #'fzfa-fuzz-frontends--schedule-default-refresh nil t))
  (setq fzfa-fuzz-frontends--watchdog-timer
        (run-at-time fzfa-fuzz-frontends--watchdog-seconds nil
                     #'fzfa-fuzz-frontends--watchdog (current-buffer)))
  (pcase fzfa-fuzz-frontends--frontend
    ('default (fzfa-fuzz-frontends--schedule-default-refresh))
    ('vertico
     (setq fzfa-fuzz-frontends--refresh-timer
           (run-with-idle-timer
            0 nil #'fzfa-fuzz-frontends--kick-vertico
            (current-buffer))))
    ('helm
     ;; Helm performs its first update before the minibuffer is active.  The
     ;; update advice therefore cannot associate that render with this driver
     ;; yet.  Observe the committed first render on the next idle turn.
     (setq fzfa-fuzz-frontends--refresh-timer
           (run-with-idle-timer
            0 nil #'fzfa-fuzz-frontends--kick-helm
            (current-buffer))))))

(defun fzfa-fuzz-frontends--target-query (rng)
  "Choose a discriminating target using RNG."
  (fzfa-fuzz--pick rng '("alpha" "quartz" "violet" "mango")))

(defun fzfa-fuzz-frontends--candidates (rng target)
  "Build a completion set around TARGET using RNG."
  (cons (copy-sequence target)
        (cl-loop for index below (+ 12 (fzfa-fuzz--integer rng 12))
                 collect (format "xxxxx-%02d" index))))

(defun fzfa-fuzz-frontends--input-actions (target)
  "Return keys that narrow, erase, narrow again, and accept TARGET."
  (let ((type
         (cl-loop for index from 0 below (length target)
                  collect (list 'key (aref target index)
                                (substring target 0 (1+ index)))))
        (erase
         (cl-loop for length from (1- (length target)) downto 0
                  collect (list 'key 127 (substring target 0 length)))))
    (append type erase type (list (list 'accept target)))))

(defun fzfa-fuzz-frontends--trace (frontend root-seed case-seed)
  "Generate a live FRONTEND trace for ROOT-SEED and CASE-SEED."
  (let* ((rng (fzfa-fuzz-rng-create :state case-seed))
         (target (fzfa-fuzz-frontends--target-query rng))
         (candidates (fzfa-fuzz-frontends--candidates rng target)))
    (fzfa-fuzz-trace-create
     'live-frontend root-seed case-seed
     (list :frontend frontend :target target
           :candidates (fzfa-fuzz--copy-strings candidates)
           :watchdog-seconds fzfa-fuzz-frontends--watchdog-seconds)
     (fzfa-fuzz-frontends--input-actions target))))

(defun fzfa-fuzz-frontends--decode-trace (trace)
  "Validate TRACE and return its live frontend inputs."
  (let* ((initial (plist-get trace :initial-state))
         (keys
          (fzfa-fuzz-trace--plist-keys
           initial "live-frontend initial state"
           '(:frontend :target :candidates :watchdog-seconds)))
         (frontend (plist-get initial :frontend))
         (target (plist-get initial :target))
         (candidates (plist-get initial :candidates))
         (watchdog (plist-get initial :watchdog-seconds))
         (actions (plist-get trace :actions)))
    (fzfa-fuzz-trace--require-keys
     keys '(:frontend :target :candidates :watchdog-seconds)
     "live-frontend initial state")
    (unless (and (memq frontend '(default vertico helm))
                 (stringp target) (> (length target) 0)
                 (fzfa-fuzz--proper-list-p candidates)
                 (cl-every #'stringp candidates)
                 (member target candidates)
                 (numberp watchdog) (> watchdog 0)
                 (equal actions
                        (fzfa-fuzz-frontends--input-actions target)))
      (error "Malformed live frontend trace: %S" trace))
    (list :frontend frontend :target target :candidates candidates
          :watchdog-seconds watchdog :actions actions)))

(defun fzfa-fuzz-frontends-configure (frontend)
  "Load and enable FRONTEND for a live fuzz process."
  (pcase frontend
    ('default
     (when (< emacs-major-version 31)
       (error "Built-in live frontend requires Emacs 31 or newer")))
    ('vertico
     (require 'vertico)
     (vertico-mode 1))
    ('helm
     (require 'helm)
     (require 'helm-mode)
     (require 'fzfa-helm)
     (helm-mode 1))
    (_ (error "Unknown live frontend: %S" frontend))))

(defun fzfa-fuzz-frontends-configure-for-trace (trace)
  "Load and enable the frontend recorded in TRACE."
  (let ((description (fzfa-fuzz-frontends--decode-trace trace)))
    (fzfa-fuzz-frontends-configure
     (plist-get description :frontend))))

(defun fzfa-fuzz-frontends--case (trace)
  "Run one real completion frontend TRACE."
  (let* ((description (fzfa-fuzz-frontends--decode-trace trace))
         (seed (plist-get trace :case-seed))
         (frontend (plist-get description :frontend))
         (target (plist-get description :target))
         (candidates
          (fzfa-fuzz--copy-strings (plist-get description :candidates)))
         (actions (plist-get description :actions))
         (failure-cell (list nil))
         (progress-cell (list 'not-started))
         (remaining-actions-cell (list (copy-tree actions)))
         (event-log-cell (list nil))
         (fzfa-fuzz-frontends--observations nil)
         (fzfa-fuzz-frontends--sequence 0)
         (fzfa-fuzz-frontends--watchdog-seconds
          (plist-get description :watchdog-seconds))
         (fzfa-fuzz-frontends--driver-config
          (list :frontend frontend :target target :candidates candidates
                :actions actions
                :remaining-actions-cell remaining-actions-cell
                :event-log-cell event-log-cell
                :failure-cell failure-cell :progress-cell progress-cell))
         (minibuffer-setup-hook
          (cons #'fzfa-fuzz-frontends--driver-setup minibuffer-setup-hook))
         (completion-auto-help 'always)
         result)
    (when (eq frontend 'vertico)
      (advice-add 'vertico--exhibit :after
                  #'fzfa-fuzz-frontends--observe-vertico))
    (when (eq frontend 'helm)
      ;; Helm makes `helm-after-update-hook' session-local, so a global hook
      ;; added before `helm' starts is not an observation boundary.  Advice
      ;; the real update function instead; this still runs after Helm has
      ;; filtered, rendered, and moved its selection.
      (advice-add 'helm-update :after
                  #'fzfa-fuzz-frontends--observe-helm))
    (unwind-protect
        (progn
          (condition-case err
              (setq result
                    (fzfa-completing-read
                     :prompt "fzfa frontend fuzz: "
                     :candidates candidates
                     :category 'fzfa-fuzz-frontend
                     :require-match t))
            (quit
             (if (car failure-cell)
                 (let ((failure (car failure-cell)))
                   (fzfa-fuzz--fail-observation-key
                    seed trace (plist-get failure :oracle)
                    'completed-handshake
                    (list :driver-failure failure
                          :queued-actions (car event-log-cell)
                          :remaining-actions (car remaining-actions-cell)
                          :observations fzfa-fuzz-frontends--observations)
                    "%s" (plist-get failure :message)))
               (signal (car err) (cdr err)))))
          (when (car failure-cell)
            (let ((failure (car failure-cell)))
              (fzfa-fuzz--fail-observation-key
               seed trace (plist-get failure :oracle) 'completed-handshake
               (list :driver-failure failure
                     :queued-actions (car event-log-cell)
                     :remaining-actions (car remaining-actions-cell)
                     :observations fzfa-fuzz-frontends--observations)
               "%s" (plist-get failure :message))))
          (unless (eq (car progress-cell) 'target-reselected)
            (fzfa-fuzz--fail-observation
             seed trace 'target-reselected
             (list :progress (car progress-cell)
                   :queued-actions (car event-log-cell)
                   :remaining-actions (car remaining-actions-cell)
                   :observations fzfa-fuzz-frontends--observations)
             "frontend exited before target reselection"))
          (unless (null (car remaining-actions-cell))
            (fzfa-fuzz--fail-observation
             seed trace nil (car remaining-actions-cell)
             "frontend left recorded input actions"))
          (unless (equal result target)
            (fzfa-fuzz--fail-observation
             seed trace target result
             "frontend accepted the wrong candidate")))
      (setq unread-command-events nil)
      (when (eq frontend 'vertico)
        (advice-remove 'vertico--exhibit
                       #'fzfa-fuzz-frontends--observe-vertico))
      (when (eq frontend 'helm)
        (advice-remove 'helm-update
                       #'fzfa-fuzz-frontends--observe-helm)))
    t))

(defun fzfa-fuzz-frontends-run-trace (trace)
  "Run one validated live frontend TRACE without generation."
  (fzfa-fuzz-trace-validate trace)
  (unless (eq (plist-get trace :target) 'live-frontend)
    (error "Unsupported frontend trace target: %S"
           (plist-get trace :target)))
  (fzfa-fuzz--run-trace
   trace (lambda () (fzfa-fuzz-frontends--case trace))))

(defun fzfa-fuzz-frontends-selftest (frontend)
  "Qualify FRONTEND exact replay and its logical update oracle."
  (let ((trace (fzfa-fuzz-frontends--trace frontend 9900 9900)))
    (cl-letf (((symbol-function 'fzfa-fuzz--integer)
               (lambda (&rest _) (error "Replay requested randomness")))
              ((symbol-function 'fzfa-fuzz--pick)
               (lambda (&rest _) (error "Replay requested randomness"))))
      (fzfa-fuzz-frontends-run-trace trace)))
  (let* ((seed 9901)
         (trace (fzfa-fuzz-frontends--trace frontend seed seed))
         (initial (plist-get trace :initial-state))
         (target (plist-get initial :target))
         (full-count (length (plist-get initial :candidates))))
    (fzfa-fuzz--expect-detection
     "frontend-filter-disabled" "did not select the target"
     (lambda ()
       (let ((fzfa-fuzz-frontends--observation-mutator
              (lambda (observation)
                (if (equal (plist-get observation :query) target)
                    (plist-put observation :candidate-count full-count)
                  observation))))
         (fzfa-fuzz-frontends-run-trace trace)))))
  (let* ((seed 9902)
         (trace (fzfa-fuzz-frontends--trace frontend seed seed))
         (initial (plist-get trace :initial-state))
         (target (plist-get initial :target))
         saw-target narrow-count)
    (fzfa-fuzz--expect-detection
     "frontend-empty-stale" "did not restore all candidates"
     (lambda ()
       (let ((fzfa-fuzz-frontends--observation-mutator
              (lambda (observation)
                (let ((query (plist-get observation :query)))
                  (cond
                   ((equal query target)
                    (setq saw-target t
                          narrow-count
                          (plist-get observation :candidate-count))
                    observation)
                   ((and saw-target (equal query ""))
                    (plist-put observation :candidate-count narrow-count))
                   (t observation))))))
         (fzfa-fuzz-frontends-run-trace trace)))))
  (princ
   (format
    "fzfa %s frontend self-test passed (exact replay; 2 canaries killed)\n"
    frontend)))

(defun fzfa-fuzz-frontends--selected-frontend ()
  "Return the frontend selected by FZFA_FUZZ_FRONTEND."
  (pcase (getenv "FZFA_FUZZ_FRONTEND")
    ((or "default" (pred null)) 'default)
    ("vertico" 'vertico)
    ("helm" 'helm)
    (value (error "Unknown FZFA_FUZZ_FRONTEND: %S" value))))

(defun fzfa-fuzz-frontends-run ()
  "Run the selected live frontend fuzz cases, then exit Emacs."
  (if noninteractive
      (error "Live frontend fuzz must run without --batch")
    (condition-case err
        (let ((frontend (fzfa-fuzz-frontends--selected-frontend)))
          (fzfa-fuzz-frontends-configure frontend)
          (fzfa-fuzz-frontends-selftest frontend)
          (let* ((root-seed (fzfa-fuzz--seed))
                 (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 8)))
            (dotimes (index cases)
              (let ((seed (+ root-seed index)))
                (fzfa-fuzz-frontends-run-trace
                 (fzfa-fuzz-frontends--trace frontend root-seed seed))))
            (fzfa-fuzz-frontends--report
             (format "fzfa %s frontend fuzz passed (%d cases, root seed %d)\n"
                     frontend cases root-seed)))
          (kill-emacs 0))
      ((error quit)
       (fzfa-fuzz-frontends--report
        (format "fzfa live frontend fuzz failed: %s\n"
                (error-message-string err)))
       (kill-emacs 1)))))

(provide 'fzfa-fuzz-frontends)
;;; fzfa-fuzz-frontends.el ends here
