;;; fzfa-helm.el --- Helm frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (helm "3.9"))
;; Keywords: matching, completion, helm
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helm frontend for fzfa.  Auto-loaded by fzfa.el via
;; `with-eval-after-load' on `helm', so users get helm support by
;; doing nothing — install helm, enable `helm-mode', and the existing
;; `fzfa-*' commands route through helm sources backed by fzf-native
;; scoring.  No call needed in user config.
;;
;; When `helm-mode' is active, fzfa entry points dispatch to handlers
;; registered in `fzfa-async-helm-handler', `fzfa-2pass-helm-handler',
;; and `fzfa-multi-helm-handler' instead of running through
;; `completing-read'.
;;
;; Public source constructors for building helm commands directly:
;;
;;   `fzfa-helm-make-async-source'
;;     Streams candidates from a shell command, filtered live by
;;     fzf-native.  Single producer process; each keystroke rescores
;;     in-memory (no respawn).
;;
;;   `fzfa-helm-make-sync-source'
;;     Scores a fixed list of strings with fzf-native on each
;;     `helm-pattern' change.
;;
;; Both return plain `helm-source-sync' instances with
;; `:match-dynamic t', so they slot into any `(helm :sources ...)'
;; invocation alongside ordinary helm sources.

;;; Code:

(require 'cl-lib)
(require 'fzfa)
(require 'helm)
(require 'helm-source)

(defvar helm-alive-p)
(defvar helm-pattern)
(defvar helm-completion-style)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")
(declare-function fzf-native-async-start "fzf-native")
(declare-function fzf-native-async-stop "fzf-native")
(declare-function fzf-native-async-generation "fzf-native")
(declare-function fzf-native-async-candidates "fzf-native")
(declare-function fzf-native-score-all "fzf-native")

;;; Public source constructors

(cl-defun fzfa-helm-make-async-source
    (&key name command directory action
          (candidate-number-limit
           (or (fzfa--candidate-limit) 10000)))
  "Return a helm source that streams candidates from shell COMMAND.

NAME is the source header.  COMMAND is the producer shell command.
DIRECTORY is its working directory (default `default-directory').
ACTION is a one-arg function called with the selection (default
returns it unchanged).  CANDIDATE-NUMBER-LIMIT is helm's display cap.

The producer process and polling timer start eagerly at construction
time, BEFORE helm activates — matches the original (pre-extraction)
timing.  Starting fzf-native (which forks) inside helm's `:init'
provokes fork/malloc-lock interactions that can kill the producer
child silently on macOS.  The source's `:cleanup' stops both."
  (let* ((dir (expand-file-name (or directory default-directory)))
         (limit (or candidate-number-limit 10000))
         (handle (fzf-native-async-start command dir))
         (last-gen -1)
         (stopped nil)
         (stop
          (lambda ()
            (unless stopped
              (setq stopped t)
              (fzf-native-async-stop handle))))
         (timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when (and helm-alive-p (not stopped))
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update))))))))
    (helm-make-source (or name "fzfa") 'helm-source-sync
      :header-name
      (lambda (n) (format "%s [%s]" n (abbreviate-file-name dir)))
      :candidates
      (lambda ()
        (unless stopped
          (fzf-native-async-candidates handle helm-pattern limit)))
      :match-dynamic t
      :nohighlight t
      :candidate-number-limit limit
      :cleanup
      (lambda ()
        (when timer (cancel-timer timer) (setq timer nil))
        (funcall stop))
      :action (or action (lambda (cand) cand)))))

(cl-defun fzfa-helm-make-sync-source
    (&key name items action
          (candidate-number-limit
           (or (fzfa--candidate-limit) 10000)))
  "Return a helm source that scores ITEMS with fzf-native.

NAME is the source header.  ITEMS is a list of strings or a zero-arg
function returning one.  ACTION is a one-arg function called with the
selection (default returns it unchanged).  CANDIDATE-NUMBER-LIMIT is
helm's display cap.

Each `helm-pattern' change re-scores the full ITEMS list via
`fzf-native-score-all'.  Pre-scored output goes straight to helm with
`:nohighlight t' so helm does not paint its own faces over fzf-native's
`completions-common-part' highlights."
  (let ((limit (or candidate-number-limit 10000)))
    (helm-make-source (or name "fzfa") 'helm-source-sync
      :candidates
      (lambda ()
        (let ((all (if (functionp items) (funcall items) items)))
          (if (or (null helm-pattern) (string-empty-p helm-pattern))
              all
            (fzfa--bridge-defcustoms
             #'fzf-native-score-all all helm-pattern))))
      :match-dynamic t
      :nohighlight t
      :candidate-number-limit limit
      :action (or action (lambda (cand) cand)))))

;;; Async handler — registered as `fzfa-async-helm-handler'

(cl-defun fzfa--helm-async-read (&key prompt command directory
                                      skip-executable-check)
  "Helm dispatch for `fzfa-async-completing-read'.
PROMPT, COMMAND, DIRECTORY, SKIP-EXECUTABLE-CHECK as per the caller.
Returns the selected candidate string, or nil on cancel."
  (unless skip-executable-check
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (let* ((prompt (or prompt
                     (when command
                       (concat (car (split-string command nil t)) ": "))))
         (dir (expand-file-name (or directory default-directory)))
         (result nil)
         (helm-completion-style 'emacs)
         (source (fzfa-helm-make-async-source
                  :name (or prompt "fzfa")
                  :command command
                  :directory dir
                  :action (lambda (cand) (setq result cand)))))
    (let ((default-directory dir))
      (helm :sources source :buffer "*helm fzfa*"))
    result))

;;; Handler registration

(setq fzfa-async-helm-handler #'fzfa--helm-async-read)

(provide 'fzfa-helm)
;;; fzfa-helm.el ends here
