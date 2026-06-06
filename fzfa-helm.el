;;; fzfa-helm.el --- Helm frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: matching, completion, helm
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helm frontend for `fzfa'.  Loaded automatically when `helm' is in
;; `fzfa-extensions' and `fzfa-setup' has been called.  Loading this
;; file does NOT activate helm dispatch on its own — that happens in
;; `fzfa-helm-setup', which registers the four handler defvars in
;; `fzfa.el' (`fzfa-async-helm-handler', `fzfa-sync-helm-handler',
;; `fzfa-2pass-helm-handler', `fzfa-multi-helm-handler') and defers
;; the registration via `with-eval-after-load' on `helm' so it kicks
;; in only when helm is actually loaded.
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
;;
;;   `fzfa-helm-source-from-command'
;;     Builds a helm source from an existing arg-less `fzfa-*'
;;     command's keyword args.  Lets users compose existing fzfa
;;     commands into helm-mini-style multi-source helm sessions
;;     without restating each source's command / directory / action.

;;; Code:

(require 'cl-lib)
(require 'fzfa)
(require 'helm nil t)
(require 'helm-source nil t)

(defcustom fzfa-helm-multi-source-candidate-limit 200
  "Per-source candidate cap inside `fzfa-helm--multi-read'.

`helm' renders every candidate the source returns so we cap it here.
This cap is applied to both the `fzf-native'
`limit' (cuts scoring/list-copy cost) and helm's
`:candidate-number-limit' (cuts rendering cost) for each source in
the multi.  The user can always refine the query to surface entries
ranked below the cap.

Has no effect on single-source paths (`fzfa-async-completing-read'
under helm-mode), which use the full `fzfa-max-candidates'."
  :type 'integer
  :group 'fzfa)

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
(declare-function fzfa--history-rank "fzfa")
(declare-function fzfa--2pass-extract-args "fzfa")
(defvar fzfa--multi-mode)

;;; Public source constructors

(defun fzfa-helm--async-source-and-stop
    (name command directory action limit)
  "Return (SOURCE . STOP).  Eagerly starts the fzf-native producer and
the polling timer.  SOURCE's `:cleanup' calls STOP; STOP is idempotent
so callers can also invoke it externally for defense-in-depth (e.g.
multi-source bulk cleanup when helm never gets a chance to call
`:cleanup' itself).

Internal — used by both `fzfa-helm-make-async-source' (single-source)
and `fzfa-helm--multi-read' (batch with bulk-stop)."
  (let* ((dir (expand-file-name (or directory default-directory)))
         (handle (fzf-native-async-start command dir))
         (last-gen -1)
         (stopped nil)
         (timer nil)
         (stop
          (lambda ()
            (unless stopped
              (setq stopped t)
              (when timer (cancel-timer timer) (setq timer nil))
              (fzf-native-async-stop handle)))))
    (setq timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when (and helm-alive-p (not stopped))
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update)))))))
    (cons
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
       :cleanup stop
       :action (or action (lambda (cand) cand)))
     stop)))

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
timing.  The source's `:cleanup' stops it."
  (car (fzfa-helm--async-source-and-stop
        name command directory action
        (or candidate-number-limit 10000))))

(cl-defun fzfa-helm-make-sync-source
    (&key name items action history
          (candidate-number-limit
           (or (fzfa--candidate-limit) 10000)))
  "Return a helm source that scores ITEMS with `fzf-native'.

NAME is the source header.  ITEMS is a list of strings or a zero-arg
function returning one.  ACTION is a one-arg function called with the
selection (default returns it unchanged).  CANDIDATE-NUMBER-LIMIT is
`helm''s display cap.

HISTORY is an optional history variable symbol.  When set and
`helm-pattern' is empty, ITEMS are reordered via `fzfa--history-rank'
so recent picks surface first — helm does not consult the completion
metadata's `display-sort-function', so this sort has to happen at the
candidate level.  HISTORY here governs ORDERING only; pushing the
selection onto HISTORY is the caller's responsibility (the registered
sync/multi handlers wrap their `:action' to do so).

Each `helm-pattern' change re-scores the full ITEMS list via
`fzf-native-score-all'."
  (let ((limit (or candidate-number-limit 10000)))
    (helm-make-source (or name "fzfa") 'helm-source-sync
      :candidates
      (lambda ()
        (let ((all (if (functionp items) (funcall items) items)))
          (if (or (null helm-pattern) (string-empty-p helm-pattern))
              (if history (fzfa--history-rank all history) all)
            (fzfa--bridge-defcustoms
             #'fzf-native-score-all all helm-pattern))))
      :match-dynamic t
      :nohighlight t
      :candidate-number-limit limit
      :action (or action (lambda (cand) cand)))))

;;; Composition helper — fzfa command -> helm source(s)

(defun fzfa-helm--source-from-plist (plist)
  "Build a helm source from a fzfa source PLIST.
PLIST has the shape produced by fzfa's `:extract' mode (and, for
multi-source commands, by `fzfa-multi-read''s inner per-source
plists).  Dispatches `:command' to `fzfa-helm-make-async-source'
and `:items' to `fzfa-helm-make-sync-source'.

The plist's `:action' (typically the `:inject' lambda fzfa-multi-read
installed) is wrapped to also `add-to-history' onto `:history' —
mirroring the HIST push the inner `completing-read' would have done,
which is skipped when we bypass it via `:inject' mode."
  (let* ((name        (or (plist-get plist :name) "fzfa"))
         (cmd         (plist-get plist :command))
         (items       (plist-get plist :items))
         (directory   (or (plist-get plist :directory) default-directory))
         (history     (plist-get plist :history))
         (orig-action (plist-get plist :action))
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (when orig-action (funcall orig-action cand)))))
    (cond
     (cmd
      (fzfa-helm-make-async-source
       :name name :command cmd :directory directory :action action))
     (items
      (fzfa-helm-make-sync-source
       :name name :items items :history history :action action))
     (t
      (error "fzfa source plist has neither :command nor :items: %S" plist)))))

(cl-defun fzfa-helm-source-from-command (cmd &rest overrides)
  "Return a LIST of helm sources built from arg-less fzfa command CMD.

Always returns a list — one element for single-source commands, N
elements for multi-source commands (`fzfa-find-any' etc).  Caller
composes with `append':

  (defun my/helm-mini ()
    (interactive)
    (require \\='helm-buffers)
    (require \\='helm-files)
    (helm :sources
          (append helm-mini-default-sources
                  (fzfa-helm-source-from-command \\='fzfa-find-files)
                  (fzfa-helm-source-from-command \\='fzfa-find-any))
          :buffer \"*helm mini+fzfa*\"))

Mechanism: CMD is funcalled in fzfa's `:extract' mode to retrieve
its keyword args (`:command', `:directory', `:items', `:history')
without running.  The selection is routed back via fzfa's `:inject'
mode so CMD's post-action (e.g. `find-file', grep jump) runs.  The
HIST push that the inner `completing-read' would have done is
mirrored via `add-to-history' wrapping the inject action.

OVERRIDES is a keyword args plist merged on top of the extracted args
— useful for renaming via :name, swapping :action, repointing
:directory.  OVERRIDES is IGNORED for multi-source commands (which
have N inner sources, no single set of args to override).

Caveat for multi-source commands: each inner source gets its own
polling timer (no shared timer like `fzfa-helm--multi-read' uses).
For pure-fzfa multis with several async sources, `fzfa-multi-read'
is faster.  Composition into helm-mini-style sessions is the
intended use case.

Errors if CMD is not an extract-capable fzfa command, or if it is a
two-pass command (`:2pass t' in the extracted args)."
  (let ((args (fzfa--2pass-extract-args cmd)))
    (unless args
      (user-error "`%s' is not an extract-capable fzfa command" cmd))
    (when (plist-get args :2pass)
      (user-error "`%s' is a two-pass fzfa command — not yet composable as a helm source"
                  cmd))
    (cond
     ;; Multi-source command — build one helm source per inner plist.
     ;; Each inner plist already carries an :action closure that
     ;; fzfa-multi-read built during extract (the :inject dispatch),
     ;; so source-from-plist just adds history-push wrapping.
     ((plist-get args :multi-sources)
      (mapcar #'fzfa-helm--source-from-plist
              (plist-get args :multi-sources)))
     ;; Single-source command — build one source, merge overrides.
     (t
      (let* ((name (or (plist-get overrides :name)
                       (replace-regexp-in-string "^fzfa-" ""
                                                 (symbol-name cmd))))
             (cmd-shell (or (plist-get overrides :command)
                            (plist-get args :command)))
             (items (or (plist-get overrides :items)
                        (plist-get args :items)))
             (directory (or (plist-get overrides :directory)
                            (plist-get args :directory)
                            default-directory))
             (history (or (plist-get overrides :history)
                          (plist-get args :history)))
             (action (or (plist-get overrides :action)
                         (lambda (cand)
                           (let ((fzfa--multi-mode (cons :inject cand)))
                             (funcall cmd))))))
        (list (fzfa-helm--source-from-plist
               (list :name name :command cmd-shell :items items
                     :directory directory :history history
                     :action action))))))))

;;; Async handler — registered as `fzfa-async-helm-handler'

(cl-defun fzfa-helm--async-read (&key prompt command directory
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

;;; Sync handler — registered as `fzfa-sync-helm-handler'

(cl-defun fzfa-helm--sync-read (&key candidates prompt category annotate
                                     affix group history require-match
                                     default preview)
  "Helm dispatch for `fzfa-sync-completing-read'.

Bypasses `completing-read' (and therefore helm-mode's advice) so we
can apply per-history candidate ordering — helm doesn't consult the
metadata `display-sort-function' that `vertico' / `icomplete' rely on, so
recent picks would otherwise be lost.  Side effect: bypassing
`completing-read' also bypasses its HIST push, so the action wraps
`add-to-history' to mirror what would have happened.

CATEGORY, ANNOTATE, AFFIX, GROUP, REQUIRE-MATCH, PREVIEW are accepted
for signature parity but unused — helm sources don't consume
completion-read metadata, and preview integration under helm is a
deferred TODO."
  (ignore category annotate affix group require-match preview)
  (let* ((helm-completion-style 'emacs)
         (result nil)
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (setq result cand))))
    (helm :sources (fzfa-helm-make-sync-source
                    :name (or prompt "fzfa")
                    :items candidates
                    :history history
                    :action action)
          :prompt (or prompt "fzf > ")
          :default default
          :buffer "*helm fzfa sync*")
    result))

;;; 2pass handler — registered as `fzfa-2pass-helm-handler'

(defvar fzfa-2pass-split-style)
(defvar fzfa-2pass-split-styles-alist)
(defvar fzfa-shell-command-debounce)
(defvar fzfa-shell-command-throttle)
(declare-function fzfa--defer-async-stop "fzfa")

(cl-defun fzfa-helm--2pass-read (&key prompt directory category group
                                      initial-input split-style)
  "Helm dispatch for `fzfa-2pass-completing-read'.

PROMPT, DIRECTORY, INITIAL-INPUT, SPLIT-STYLE as per the caller.
CATEGORY and GROUP are accepted for signature parity but unused under
helm (helm sources do not consume completion-read metadata)."
  (ignore category group)
  (let* ((prompt (or prompt "fzfa-2pass: "))
         (dir (expand-file-name (or directory default-directory)))
         (style-sym (or split-style fzfa-2pass-split-style 'perl))
         (style (or (alist-get style-sym fzfa-2pass-split-styles-alist)
                    (user-error "Unknown fzfa-2pass split style: %s"
                                style-sym)))
         (splitter (plist-get style :function))
         (limit (or (fzfa--candidate-limit) 10000))
         (init-text (if (consp initial-input)
                        (car initial-input)
                      initial-input))
         (handle nil)
         (current-cmd nil)
         (last-gen -1)
         (last-restart-time 0.0)
         (stopped nil)
         (result nil)
         (helm-completion-style 'emacs)
         restart-timer poll-timer
         (do-restart
          (lambda (cmd)
            (when handle
              (fzfa--defer-async-stop handle)
              (setq handle nil))
            (setq current-cmd cmd
                  last-gen -1
                  last-restart-time (float-time))
            (when (and cmd (not (string-empty-p cmd)))
              (setq handle (fzf-native-async-start cmd dir)))
            (when helm-alive-p (helm-force-update))))
         (cleanup
          (lambda ()
            (unless stopped
              (setq stopped t)
              (when restart-timer
                (cancel-timer restart-timer)
                (setq restart-timer nil))
              (when poll-timer
                (cancel-timer poll-timer)
                (setq poll-timer nil))
              (when handle
                (fzf-native-async-stop handle)
                (setq handle nil))))))
    ;; Pre-arm initial cmd's producer BEFORE helm activates so fork
    ;; happens in quiescent Lisp state (same reason as the async path).
    (when init-text
      (let ((cmd (car (funcall splitter init-text style))))
        (when (and cmd (not (string-empty-p cmd)))
          (funcall do-restart cmd))))
    (setq poll-timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when (and helm-alive-p handle (not stopped))
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update)))))))
    (unwind-protect
        (let ((default-directory dir))
          (helm
           :sources
           (helm-make-source prompt 'helm-source-sync
             :header-name
             (lambda (n) (format "%s [%s]" n (abbreviate-file-name dir)))
             :candidates
             (lambda ()
               (let* ((split (funcall splitter helm-pattern style))
                      (cmd (car split))
                      (query (cdr split)))
                 (cond
                  ((not (equal cmd current-cmd))
                   ;; cmd changed — debounce restart, fetch from
                   ;; the old handle in the meantime so the display
                   ;; doesn't blank.
                   (when restart-timer
                     (cancel-timer restart-timer)
                     (setq restart-timer nil))
                   (let* ((elapsed (- (float-time) last-restart-time))
                          (delay (max fzfa-shell-command-debounce
                                      (- fzfa-shell-command-throttle
                                         elapsed))))
                     (setq restart-timer
                           (run-with-timer
                            (max 0.01 delay) nil
                            (lambda ()
                              (setq restart-timer nil)
                              (funcall do-restart cmd)))))
                   (and handle
                        (fzf-native-async-candidates handle query limit)))
                  ((null handle) nil)
                  (t (fzf-native-async-candidates handle query limit)))))
             :match-dynamic t
             :nohighlight t
             :candidate-number-limit limit
             :cleanup cleanup
             :action (lambda (cand) (setq result cand)))
           :buffer "*helm fzfa 2pass*"
           :input init-text))
      (funcall cleanup))
    result))

;;; Multi handler — registered as `fzfa-multi-helm-handler'

(cl-defun fzfa-helm--multi-read (sources &key prompt)
  "Helm dispatch for `fzfa--multi-read'.

SOURCES is the same list of plists as the completing-read path.
Each `fzfa' source maps to a `helm' source:
  :command  -> async (eager-start, no per-source polling timer)
  :items    -> sync (fzf-native-score-all on each `helm-pattern' change)

A SINGLE shared polling timer watches every async handle and calls
`helm-force-update' at most once per `fzfa-input-throttle' seconds
when any source has new candidates."
  (let* ((helm-completion-style 'emacs)
         ;; Per-source render cap — see `fzfa-helm-multi-source-candidate-limit'.
         (limit (min (or (fzfa--candidate-limit) 10000)
                     fzfa-helm-multi-source-candidate-limit))
         (result nil)
         ;; Per-source state collected during source construction.
         (handles nil)   ; reversed: list of fzf-native handles (async only)
         (stops nil)     ; reversed: list of 0-arg stop closures (async only)
         poll-timer
         (helm-sources
          (mapcar
           (lambda (src)
             (let* ((name        (or (plist-get src :name) "fzfa"))
                    (cmd         (plist-get src :command))
                    (items       (plist-get src :items))
                    (directory   (or (plist-get src :directory)
                                     default-directory))
                    (history     (plist-get src :history))
                    (orig-action (plist-get src :action))
                    (action
                     (lambda (cand)
                       (when (and history (symbolp history)
                                  (not (eq history t)))
                         (add-to-history history cand))
                       (setq result
                             (if orig-action
                                 (funcall orig-action cand)
                               cand)))))
               (cond
                (cmd
                 (let* ((dir (expand-file-name directory))
                        (handle (fzf-native-async-start cmd dir))
                        (stopped nil)
                        ;; Per-source `last-result' cache: when `:candidates'
                        ;; is interrupted by pending input (via
                        ;; `while-no-input'), return the previous good list
                        ;; so the display doesn't blank.
                        (last-result nil)
                        (stop
                         (lambda ()
                           (unless stopped
                             (setq stopped t)
                             (fzf-native-async-stop handle)))))
                   (push handle handles)
                   (push stop stops)
                   (helm-make-source name 'helm-source-sync
                     :header-name
                     (lambda (n)
                       (format "%s [%s]" n (abbreviate-file-name dir)))
                     :candidates
                     (lambda ()
                       (unless stopped
                         (let ((r (while-no-input
                                    (fzf-native-async-candidates
                                     handle helm-pattern limit))))
                           (if (eq r t)
                               last-result
                             (setq last-result r)
                             r))))
                     :match-dynamic t
                     :nohighlight t
                     :candidate-number-limit limit
                     :cleanup stop
                     :action action)))
                (items
                 (fzfa-helm-make-sync-source
                  :name name :items items :action action
                  :history history
                  :candidate-number-limit limit))
                (t
                 (error "fzfa helm multi source has neither :command nor :items: %S"
                        src)))))
           sources)))
    ;; Single shared polling timer over all async handles.  Throttled to
    ;; one `helm-force-update' per `fzfa-input-throttle' to amortize the
    ;; cost of recomputing every source's `:candidates'.  Also skipped
    ;; when input is pending — typing always trumps streamed-candidate
    ;; refreshes.
    (when handles
      (let* ((n         (length handles))
             (handles-v (vconcat (nreverse handles)))
             (last-gen  (make-vector n -1))
             (last-exhibit 0.0))
        (setq poll-timer
              (run-with-timer
               0 fzfa-refresh-delay
               (lambda ()
                 (when helm-alive-p
                   (let (bumped)
                     (dotimes (i n)
                       (when-let* ((g (fzf-native-async-generation
                                       (aref handles-v i))))
                         (when (/= g (aref last-gen i))
                           (aset last-gen i g)
                           (setq bumped t))))
                     (when (and bumped (not (input-pending-p))
                                (>= (- (float-time) last-exhibit)
                                    fzfa-input-throttle))
                       (setq last-exhibit (float-time))
                       (helm-force-update)))))))))
    (unwind-protect
        (helm :sources helm-sources
              :prompt (or prompt "fzf-multi: ")
              :buffer "*helm fzfa multi*")
      (when poll-timer (cancel-timer poll-timer))
      ;; Bulk-stop async producers; idempotent — :cleanup may have
      ;; already fired on normal helm exit.
      (mapc #'funcall stops))
    result))

;;; Setup — registers the four handler defvars after `helm' loads

(defun fzfa-helm-setup ()
  "Wire fzfa's helm dispatch into the current session.

Invoked by `fzfa-setup' when `helm' is listed in `fzfa-extensions'.
The actual `setq's are deferred via `with-eval-after-load' on
`helm', so calling this when helm is not (yet) installed is a no-op
that auto-activates if helm shows up later in the session.  Safe
to call multiple times — `setq' is idempotent."
  (with-eval-after-load 'helm
    (setq fzfa-async-helm-handler #'fzfa-helm--async-read)
    (setq fzfa-sync-helm-handler  #'fzfa-helm--sync-read)
    (setq fzfa-2pass-helm-handler #'fzfa-helm--2pass-read)
    (setq fzfa-multi-helm-handler #'fzfa-helm--multi-read)))

(provide 'fzfa-helm)
;;; fzfa-helm.el ends here
