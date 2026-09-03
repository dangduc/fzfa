;;; fzfa-fuzz-live.el --- Live minibuffer fuzz smoke test  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercises the real icomplete-vertical minibuffer.  It enters an fzfa
;; completion, narrows the list, backspaces to empty input, and observes the
;; mini-window fit call after each actual frontend render.  This file must run
;; in an interactive Emacs; batch mode cannot create a live minibuffer.

;;; Code:

(require 'cl-lib)
(require 'icomplete)
(require 'fzfa-fuzz-core)

(defvar fzfa-fuzz-live--observations nil
  "Chronological mini-window observations for the current live case.")

(defvar fzfa-fuzz-live--case-events nil
  "Events the current case feeds to its minibuffer one at a time.")

(defvar-local fzfa-fuzz-live--pending-events nil)
(defvar-local fzfa-fuzz-live--driver-timer nil)
(defvar-local fzfa-fuzz-live--watchdog-timer nil)

(defun fzfa-fuzz-live--report (text)
  "Write TEXT to standard output and the optional result file."
  (princ text)
  (when-let* ((file (getenv "FZFA_FUZZ_RESULT_FILE")))
    (with-temp-file file
      (insert text))))

(defun fzfa-fuzz-live--after-string ()
  "Return icomplete's displayed completion string, or nil."
  (when (and (bound-and-true-p icomplete-overlay)
             (overlayp icomplete-overlay))
    (overlay-get icomplete-overlay 'after-string)))

(defun fzfa-fuzz-live--fit-observer (function &rest args)
  "Call FUNCTION with ARGS and record its live minibuffer fit context."
  (let* ((window (active-minibuffer-window))
         (buffer (and (window-live-p window) (window-buffer window)))
         (before (and (window-live-p window) (window-text-height window)))
         result)
    (setq result (apply function args))
    (when (and (buffer-live-p buffer) (window-live-p window))
      (with-current-buffer buffer
        (let* ((after-string (fzfa-fuzz-live--after-string))
               (lines (and (stringp after-string)
                           (1+ (cl-count ?\n after-string))))
               (query (and (minibufferp buffer)
                           (buffer-substring-no-properties
                            (minibuffer-prompt-end) (point-max))))
               (session fzfa--minibuffer-session))
          (when session
            (setq fzfa-fuzz-live--observations
                  (append
                   fzfa-fuzz-live--observations
                   (list (list :query query :lines lines
                               :before before
                               :after (window-text-height window)
                               :session (and session t)))))))))
    result))

(defun fzfa-fuzz-live--deliver-event (buffer)
  "Deliver the next fuzz event to live minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-live--driver-timer nil)
      (when (and (minibufferp buffer) fzfa-fuzz-live--pending-events)
        (let ((event (pop fzfa-fuzz-live--pending-events)))
          (setq unread-command-events
                (append unread-command-events (list event))))))))

(defun fzfa-fuzz-live--queue-event ()
  "Queue one pending event after the current frontend render finishes."
  (when (and fzfa-fuzz-live--pending-events
             (null fzfa-fuzz-live--driver-timer))
    (setq fzfa-fuzz-live--driver-timer
          (run-at-time 0.02 nil #'fzfa-fuzz-live--deliver-event
                       (current-buffer)))))

(defun fzfa-fuzz-live--watchdog (buffer)
  "Send quit to BUFFER if a live fuzz case has not exited."
  (when-let* (((buffer-live-p buffer))
              (window (active-minibuffer-window))
              ((eq buffer (window-buffer window))))
    (setq unread-command-events (list 7))))

(defun fzfa-fuzz-live--driver-cleanup ()
  "Cancel the current minibuffer driver's timers."
  (when (timerp fzfa-fuzz-live--driver-timer)
    (cancel-timer fzfa-fuzz-live--driver-timer))
  (when (timerp fzfa-fuzz-live--watchdog-timer)
    (cancel-timer fzfa-fuzz-live--watchdog-timer))
  (setq fzfa-fuzz-live--driver-timer nil
        fzfa-fuzz-live--watchdog-timer nil))

(defun fzfa-fuzz-live--driver-setup ()
  "Install the one-event-at-a-time driver in the active minibuffer."
  (setq-local fzfa-fuzz-live--pending-events
              (copy-sequence fzfa-fuzz-live--case-events))
  (add-hook 'post-command-hook #'fzfa-fuzz-live--queue-event nil t)
  (add-hook 'minibuffer-exit-hook #'fzfa-fuzz-live--driver-cleanup nil t)
  (setq fzfa-fuzz-live--watchdog-timer
        (run-at-time 5 nil #'fzfa-fuzz-live--watchdog (current-buffer)))
  (fzfa-fuzz-live--queue-event))

(defun fzfa-fuzz-live--target-height (lines)
  "Return the mini-window height fzfa requests for LINES."
  (let ((max-lines
         (cond ((floatp max-mini-window-height)
                (max 1 (floor (* max-mini-window-height (frame-height)))))
               ((integerp max-mini-window-height) max-mini-window-height)
               (t 25))))
    (min lines max-lines)))

(defun fzfa-fuzz-live--assert-refcount-lifecycle ()
  "Check the icomplete exhibit advice's nested-session refcount."
  (let ((start-count fzfa--icomplete-exhibit-advice-count)
        (start-member
         (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                          'icomplete-exhibit)))
    (unwind-protect
        (progn
          (fzfa--icomplete-install-fit-advice)
          (fzfa--icomplete-install-fit-advice)
          (unless (and (= fzfa--icomplete-exhibit-advice-count
                          (+ start-count 2))
                       (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                                        'icomplete-exhibit))
            (error "Nested fit advice was not retained"))
          (fzfa--icomplete-uninstall-fit-advice)
          (unless (and (= fzfa--icomplete-exhibit-advice-count
                          (1+ start-count))
                       (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                                        'icomplete-exhibit))
            (error "Inner teardown removed the fit advice")))
      (while (> fzfa--icomplete-exhibit-advice-count start-count)
        (fzfa--icomplete-uninstall-fit-advice)))
    (unless (and (= fzfa--icomplete-exhibit-advice-count start-count)
                 (eq (and (advice-member-p
                            #'fzfa--icomplete-exhibit-fit-advice
                            'icomplete-exhibit)
                           t)
                     (and start-member t)))
      (error "Fit advice did not return to its initial state"))))

(defun fzfa-fuzz-live--candidates (rng)
  "Build a multi-line completion set using RNG."
  (cons
   "alpha"
   (cl-loop for index below (+ 12 (fzfa-fuzz--integer rng 12))
            collect
            (format "%s-%02d"
                    (fzfa-fuzz--pick rng '("beta" "gamma" "delta" "other"))
                    index))))

(defun fzfa-fuzz-live--backspace-to-empty-observation (observations)
  "Return an empty-query observation after a non-empty one in OBSERVATIONS."
  (let (saw-nonempty found)
    (dolist (observation observations found)
      (let ((query (plist-get observation :query)))
        (cond
         ((and (stringp query) (not (string-empty-p query)))
          (setq saw-nonempty t))
         ((and saw-nonempty
               (equal query "")
               (> (or (plist-get observation :lines) 0) 1))
          (setq found observation)))))))

(defun fzfa-fuzz-live--case (seed)
  "Run one real icomplete minibuffer case for SEED."
  (let* ((rng (fzfa-fuzz-rng-create :state seed))
         (candidates (fzfa-fuzz-live--candidates rng))
         (events (append (string-to-list "alpha")
                         (make-list 5 127)
                         (list 13)))
         (initial-count fzfa--icomplete-exhibit-advice-count)
         (initial-member
          (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                           'icomplete-exhibit))
         (fzfa-fuzz-live--observations nil)
         (icomplete-prospects-height 10)
         (max-mini-window-height 10)
         (resize-mini-windows t)
         (fzfa-fuzz-live--case-events events)
         (minibuffer-setup-hook
          (cons #'fzfa-fuzz-live--driver-setup minibuffer-setup-hook))
         result)
    (advice-add 'fzfa--icomplete-fit-mini-window :around
                #'fzfa-fuzz-live--fit-observer)
    (unwind-protect
        (progn
          (setq result
                (fzfa-completing-read
                 :prompt "fzfa live fuzz: "
                 :candidates candidates
                 :category 'fzfa-fuzz-live
                 :require-match nil))
          (let ((observation
                 (fzfa-fuzz-live--backspace-to-empty-observation
                  fzfa-fuzz-live--observations)))
            (unless observation
              (fzfa-fuzz--fail
               seed (list :target 'live-icomplete :events events)
               "no multi-line empty render followed narrowing: %S"
               fzfa-fuzz-live--observations))
            (let ((target (fzfa-fuzz-live--target-height
                           (plist-get observation :lines))))
              (unless (>= (plist-get observation :after) target)
                (fzfa-fuzz--fail
                 seed (list :target 'live-icomplete :events events)
                 "empty render height is %S, expected at least %S: %S"
                 (plist-get observation :after) target observation))))
          (unless (and (= fzfa--icomplete-exhibit-advice-count initial-count)
                       (eq (and (advice-member-p
                                 #'fzfa--icomplete-exhibit-fit-advice
                                 'icomplete-exhibit)
                                t)
                           (and initial-member t)))
            (fzfa-fuzz--fail
             seed (list :target 'live-icomplete :result result)
             "session leaked fit advice: count=%S member=%S"
             fzfa--icomplete-exhibit-advice-count
             (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                              'icomplete-exhibit))))
      (setq unread-command-events nil)
      (advice-remove 'fzfa--icomplete-fit-mini-window
                     #'fzfa-fuzz-live--fit-observer))
    t))

(defun fzfa-fuzz-live-run ()
  "Run live icomplete fuzz cases, then exit Emacs with their status."
  (if noninteractive
      (error "Live fuzz must run without --batch")
    (condition-case err
        (progn
          (icomplete-vertical-mode 1)
          (fzfa-fuzz-live--assert-refcount-lifecycle)
          (let* ((root-seed (fzfa-fuzz--seed))
                 (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 20)))
            (dotimes (index cases)
              (fzfa-fuzz-live--case (+ root-seed index)))
            (fzfa-fuzz-live--report
             (format "fzfa live fuzz passed (%d cases, root seed %d)\n"
                     cases root-seed)))
          (kill-emacs 0))
      ((error quit)
       (fzfa-fuzz-live--report
        (format "fzfa live fuzz failed: %s\n"
                (error-message-string err)))
       (kill-emacs 1)))))

(provide 'fzfa-fuzz-live)
;;; fzfa-fuzz-live.el ends here
