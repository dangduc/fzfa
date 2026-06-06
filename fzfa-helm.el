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

Companion var for single-source helm paths is
`fzfa-helm-candidate-limit', which derives its default by scaling
this value (see that defcustom for rationale)."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-helm-candidate-limit
  (* fzfa-helm-multi-source-candidate-limit 10)
  "Per-source candidate cap for SINGLE-source helm paths.

Used by `fzfa-helm-make-async-source', `fzfa-helm-make-sync-source',
`fzfa-helm--async-read', `fzfa-helm--sync-read', and
`fzfa-helm--2pass-read'.  Like the multi cap, this controls both
fzf-native's returned-list size and helm's `:candidate-number-limit'.

Default is `fzfa-helm-multi-source-candidate-limit' × 10."
  :type 'integer
  :group 'fzfa)

(defvar helm-alive-p)
(defvar helm-pattern)
(defvar helm-completion-style)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")
(declare-function helm-goto-source "helm-core")
(declare-function fzfa--multi-rank "fzfa")
(declare-function fzf-native-async-start "fzf-native")
(declare-function fzf-native-async-stop "fzf-native")
(declare-function fzf-native-async-generation "fzf-native")
(declare-function fzf-native-async-candidates "fzf-native")
(declare-function fzf-native-async-stats "fzf-native")
(declare-function fzf-native-score-all "fzf-native")
(declare-function fzfa--history-rank "fzfa")
(declare-function fzfa--2pass-extract-args "fzfa")
(declare-function fzfa--commas "fzfa")
(declare-function fzfa--preview-handler "fzfa")
(declare-function fzfa--preview-call "fzfa")
(declare-function fzfa--preview-return "fzfa")
(declare-function fzfa-preview-put "fzfa")
(defvar fzfa--multi-mode)
(defvar fzfa--preview-session)

;;; Stats display helpers

(defun fzfa-helm--async-stats-suffix (handle)
  "Return ` (FILTERED/TOTAL)' suffix string from HANDLE's current stats.
Returns empty string when HANDLE is nil or has no stats yet (producer
process hasn't produced any candidates, e.g. brand-new session).
Numbers comma-formatted via `fzfa--commas' to match the vertico-side
stats overlay shape.
Read by `:header-name' closures so the source's helm-buffer header
updates live as new candidates stream in — helm re-calls `:header-name'
on every `helm-force-update', which is what the polling timers in the
async / 2pass / multi handlers trigger on each generation tick."
  (if-let* ((stats (and handle (fzf-native-async-stats handle))))
      (format " (%s/%s)" (fzfa--commas (car stats))
              (fzfa--commas (cdr stats)))
    ""))

(defun fzfa-helm--sync-stats-suffix (filtered total)
  "Return ` (FILTERED/TOTAL)' suffix string from sync-source counts.
Returns empty string when either count is nil (initial state — `:candidates'
hasn't run yet).  Numbers comma-formatted via `fzfa--commas'."
  (if (and filtered total)
      (format " (%s/%s)" (fzfa--commas filtered) (fzfa--commas total))
    ""))

;;; Display transformer — preserves text properties and optionally annotates

(defun fzfa-helm--make-display-transformer (annotate)
  "Return a `:filtered-candidate-transformer' for fzfa helm sources.

Always returns (DISPLAY . REAL) cons cells.  helm's cons-cell render
path sets `helm-realvalue' on the inserted DISPLAY to REAL, which
`helm-get-selection' reads to preserve the original (propertized)
candidate — counteracting helm's default extraction via
`buffer-substring-no-properties' that would otherwise strip text
properties (load-bearing for candidates like `fzfa-swiper''s, which
carry the buffer/line target on a `fzfa-location' property).  When
ANNOTATE is nil this is the only thing the transformer does — REAL
passthrough, no decoration.

When ANNOTATE is non-nil it's a (CAND) -> STRING function whose
output is rendered as a per-row suffix (faced as
`completions-annotations', column-aligned within the rendered
batch).  Rows whose annotation comes back empty are still wrapped
as (CAND . CAND) so realvalue preservation is uniform.

Renders the helm-side analogue of vertico+marginalia's right-column
annotations — for buffer sources, file sources, etc., where the
candidate string alone doesn't carry enough context."
  (lambda (cands _source)
    (cond
     ;; Annotation path: always returns cons cells (display ≠ real).
     (annotate
      (let* ((entries
              (mapcar (lambda (c)
                        (cons c (or (funcall annotate c) "")))
                      cands))
             (maxw (apply #'max 0
                          (mapcar (lambda (e)
                                    (string-width (car e)))
                                  entries))))
        (mapcar (lambda (e)
                  (let* ((cand (car e))
                         (ann  (cdr e)))
                    (if (string-empty-p ann)
                        (cons cand cand)
                      (let ((pad (- (1+ maxw) (string-width cand))))
                        (cons (concat cand
                                      (make-string (max 1 pad) ?\s)
                                      (propertize ann 'face
                                                  'completions-annotations))
                              cand)))))
                entries)))
     ;; No annotation: wrap as (c . c) ONLY when candidates carry text
     ;; properties (preserving them via `helm-realvalue').  Probe the
     ;; first candidate as a proxy for the whole batch — fzfa sources
     ;; build candidates uniformly via the same propertizer (e.g.
     ;; `fzfa--location-candidate' attaches `fzfa-location' to every
     ;; row), so checking one is a safe heuristic and saves O(N)
     ;; allocations on property-free sources (`fzfa-M-x',
     ;; `fzfa-theme', raw shell-command output).
     ((and (consp cands)
           (stringp (car cands))
           (> (length (car cands)) 0)
           (text-properties-at 0 (car cands)))
      (mapcar (lambda (c) (cons c c)) cands))
     ;; Property-free passthrough: zero cons allocations.
     (t cands))))

;;; Public source constructors

(defun fzfa-helm--async-source-and-stop
    (name command directory action limit &optional annotate)
  "Return (SOURCE . STOP).  Eagerly starts the fzf-native producer and
the polling timer.  SOURCE's `:cleanup' calls STOP; STOP is idempotent
so callers can also invoke it externally for defense-in-depth (e.g.
multi-source bulk cleanup when helm never gets a chance to call
`:cleanup' itself).

ANNOTATE, when non-nil, is a (CAND) -> STRING function rendered as a
per-row suffix via `fzfa-helm--make-display-transformer'.

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
     (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
            :header-name
            (lambda (n)
              (format "%s [%s]%s" n (abbreviate-file-name dir)
                      (fzfa-helm--async-stats-suffix handle)))
            :candidates
            (lambda ()
              (unless stopped
                (fzf-native-async-candidates handle helm-pattern limit)))
            :match-dynamic t
            :nohighlight t
            :candidate-number-limit limit
            :cleanup stop
            :action (or action (lambda (cand) cand))
            ;; Unconditional: transformer also preserves text
            ;; properties on candidates via `helm-realvalue', even
            ;; when ANNOTATE is nil.  See
            ;; `fzfa-helm--make-display-transformer'.
            (list :filtered-candidate-transformer
                  (fzfa-helm--make-display-transformer annotate)))
     stop)))

(cl-defun fzfa-helm-make-async-source
    (&key name command directory action annotate
          (candidate-number-limit fzfa-helm-candidate-limit))
  "Return a helm source that streams candidates from shell COMMAND.

NAME is the source header.  COMMAND is the producer shell command.
DIRECTORY is its working directory (default `default-directory').
ACTION is a one-arg function called with the selection (default
returns it unchanged).  CANDIDATE-NUMBER-LIMIT is helm's display cap.

ANNOTATE, when non-nil, is a (CAND) -> STRING function rendered as a
per-row suffix (faced as `completions-annotations', column-aligned
within the rendered batch).  Affects display only; fzf scoring and
action dispatch operate on the raw candidate.

The producer process and polling timer start eagerly at construction
time, BEFORE helm activates — matches the original (pre-extraction)
timing.  The source's `:cleanup' stops it."
  (car (fzfa-helm--async-source-and-stop
        name command directory action
        (or candidate-number-limit 10000)
        annotate)))

(cl-defun fzfa-helm-make-sync-source
    (&key name items action history annotate
          (candidate-number-limit fzfa-helm-candidate-limit))
  "Return a helm source that scores ITEMS with `fzf-native'.

NAME is the source header.  ITEMS is a list of strings or a zero-arg
function returning one.  ACTION is a one-arg function called with the
selection (default returns it unchanged).  CANDIDATE-NUMBER-LIMIT is
`helm''s display cap.

ANNOTATE, when non-nil, is a (CAND) -> STRING function rendered as a
per-row suffix (faced as `completions-annotations', column-aligned
within the rendered batch).  Affects display only; fzf scoring and
action dispatch operate on the raw candidate.

HISTORY is an optional history variable symbol.  When set and
`helm-pattern' is empty, ITEMS are reordered via `fzfa--history-rank'
so recent picks surface first — helm does not consult the completion
metadata's `display-sort-function', so this sort has to happen at the
candidate level.  HISTORY here governs ORDERING only; pushing the
selection onto HISTORY is the caller's responsibility (the registered
sync/multi handlers wrap their `:action' to do so).

Each `helm-pattern' change re-scores the full ITEMS list via
`fzf-native-score-all'."
  (let* ((limit (or candidate-number-limit 10000))
         (last-filtered nil)
         (last-total nil)
         (last-result nil)
         (retry-timer nil)
         (stop (lambda ()
                 (when retry-timer
                   (cancel-timer retry-timer)
                   (setq retry-timer nil)))))
    (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
           :header-name
           (lambda (n)
             (format "%s%s" n (fzfa-helm--sync-stats-suffix
                               last-filtered last-total)))
           :candidates
           (lambda ()
             (let* ((all (if (functionp items) (funcall items) items))
                    (q (or helm-pattern ""))
                    (r (while-no-input
                         (if (string-empty-p q)
                             (if history (fzfa--history-rank all history) all)
                           (fzfa--bridge-defcustoms
                            #'fzf-native-score-all all q)))))
               (cond
                ((eq r t)
                 (when retry-timer (cancel-timer retry-timer))
                 (setq retry-timer
                       (run-with-idle-timer
                        fzfa-input-debounce nil
                        (lambda ()
                          (setq retry-timer nil)
                          (when helm-alive-p
                            (helm-force-update)))))
                 last-result)
                (t
                 (when retry-timer
                   (cancel-timer retry-timer)
                   (setq retry-timer nil))
                 (setq last-total (length all)
                       last-filtered (length r)
                       last-result r)
                 r))))
           :match-dynamic t
           :nohighlight t
           :candidate-number-limit limit
           :cleanup stop
           :action (or action (lambda (cand) cand))
           ;; Unconditional: transformer also preserves text properties
           ;; on candidates via `helm-realvalue', even when ANNOTATE is
           ;; nil.  Load-bearing for sources like `fzfa-swiper''s whose
           ;; `fzfa-location' property would otherwise be stripped by
           ;; helm's default `buffer-substring-no-properties' extraction.
           (list :filtered-candidate-transformer
                 (fzfa-helm--make-display-transformer annotate)))))

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
         (annotate    (plist-get plist :annotate))
         (orig-action (plist-get plist :action))
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (when orig-action (funcall orig-action cand)))))
    (cond
     (cmd
      (fzfa-helm-make-async-source
       :name name :command cmd :directory directory :action action
       :annotate annotate))
     (items
      (fzfa-helm-make-sync-source
       :name name :items items :history history :action action
       :annotate annotate))
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
                                      skip-executable-check
                                      category preview)
  "Helm dispatch for `fzfa-async-completing-read'.

PROMPT, COMMAND, DIRECTORY, SKIP-EXECUTABLE-CHECK as per the caller.
CATEGORY and PREVIEW are threaded through to the preview framework:
`fzfa--preview-handler' resolves a handler plist; if present we
capture origin window/buffer/`default-directory' into the session
state, fire `:setup' before helm activates, and fire `:exit' +
`:return' on exit.  Replaces the preview pipeline that the vertico
path runs via `minibuffer-with-setup-hook' +
`fzfa--preview-install' (which doesn't translate to helm — helm has
its own input/redisplay cycle that doesn't go through the
minibuffer's setup hook).

Live preview during the session (firing `:preview' as the selection
moves) is not wired here; that needs `:persistent-action' +
`:follow 1' per source — separate work.  This change is the
\"unbreak preview-encoded actions\" minimum: commands like
`fzfa-theme' whose actual action lives in `:preview :return' now
fire correctly under helm.

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
         (handler (fzfa--preview-handler preview category))
         (fzfa--preview-session (and handler (list handler)))
         (source (fzfa-helm-make-async-source
                  :name (or prompt "fzfa")
                  :command command
                  :directory dir
                  :action (lambda (cand) (setq result cand)))))
    (when handler
      (fzfa-preview-put :origin-window (selected-window))
      (fzfa-preview-put :origin-buffer (window-buffer (selected-window)))
      (fzfa-preview-put :default-directory default-directory)
      (fzfa--preview-call :setup))
    (unwind-protect
        (let ((default-directory dir))
          (helm :sources source :buffer "*helm fzfa*"))
      (when handler
        (fzfa--preview-call :exit)
        (fzfa--preview-return result)))
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

PREVIEW (with CATEGORY for registry lookup) wires the preview
lifecycle — handler resolved via `fzfa--preview-handler', session
state captured manually (helm doesn't go through
`minibuffer-with-setup-hook' so `fzfa--preview-install' can't be
used), `:setup' fires before helm activates, `:exit' + `:return'
fire on exit.  Live preview during the session (firing `:preview'
on selection movement) is not wired here.

ANNOTATE, AFFIX, GROUP, REQUIRE-MATCH are accepted for signature
parity but unused — helm sources don't consume completion-read
metadata."
  (ignore annotate affix group require-match)
  (let* ((helm-completion-style 'emacs)
         (result nil)
         (handler (fzfa--preview-handler preview category))
         (fzfa--preview-session (and handler (list handler)))
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (setq result cand))))
    (when handler
      (fzfa-preview-put :origin-window (selected-window))
      (fzfa-preview-put :origin-buffer (window-buffer (selected-window)))
      (fzfa-preview-put :default-directory default-directory)
      (fzfa--preview-call :setup))
    (unwind-protect
        (helm :sources (fzfa-helm-make-sync-source
                        :name (or prompt "fzfa")
                        :items candidates
                        :history history
                        :action action)
              :prompt (or prompt "fzf > ")
              :default default
              :buffer "*helm fzfa sync*")
      (when handler
        (fzfa--preview-call :exit)
        (fzfa--preview-return result)))
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
         (limit fzfa-helm-candidate-limit)
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
             (lambda (n)
               (format "%s [%s]%s" n (abbreviate-file-name dir)
                       (fzfa-helm--async-stats-suffix handle)))
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
when any source has new candidates.

Cursor follows the highest-ranked source: each source's `:candidates'
closure stores its top-fzf-score in a `ranks' vector, and a
`helm-after-update-hook' calls `helm-goto-source' on the leader when
it changes.  Replaces helm's default \"first non-empty source\"
positioning, which is declared-order-arbitrary and structurally wrong
for fuzzy-multi-source UX."
  (let* ((helm-completion-style 'emacs)
         ;; Per-source render cap.  `min' of the multi cap and the
         ;; single cap — multi cap dominates with defaults (200 < 2000),
         ;; but the `min' guard means a user lowering
         ;; `fzfa-helm-candidate-limit' below the multi cap still wins.
         (limit (min fzfa-helm-candidate-limit
                     fzfa-helm-multi-source-candidate-limit))
         (result nil)
         (n-sources (length sources))
         ;; Per-source rank tracking — leader = argmax of `ranks'.
         (ranks        (make-vector n-sources 0))
         (source-names (make-vector n-sources nil))
         (last-leader  nil)
         ;; Per-source state collected during source construction.
         (handles nil)   ; reversed: list of fzf-native handles (async only)
         (stops nil)     ; reversed: list of 0-arg stop closures (async only)
         poll-timer
         (helm-sources
          (cl-loop
           for src in sources
           for src-idx from 0
           collect
           ;; Fresh let-binding so closures below capture each source's
           ;; own index — cl-loop's `for' clause binds the loop variable
           ;; ONCE and mutates per iteration, so without this every
           ;; closure would see the post-loop value of `src-idx' (= N)
           ;; and `aset ranks i' would blow the vector bounds.
           (let* ((i           src-idx)
                  (name        (or (plist-get src :name) "fzfa"))
                  (cmd         (plist-get src :command))
                  (items       (plist-get src :items))
                  (directory   (or (plist-get src :directory)
                                   default-directory))
                  (history     (plist-get src :history))
                  (annotate    (plist-get src :annotate))
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
             (aset source-names i name)
             (cond
              (cmd
               (let* ((dir (expand-file-name directory))
                      (handle (fzf-native-async-start cmd dir))
                      (stopped nil)
                      ;; Per-source `last-result' cache: when
                      ;; `:candidates' is interrupted by pending
                      ;; input (via `while-no-input'), return the
                      ;; previous good list so the display doesn't
                      ;; blank — same convention as the
                      ;; completing-read multi (fzfa.el:~2336).
                      (last-result nil)
                      ;; Idle retry: when an interrupt leaves us
                      ;; showing stale candidates, re-exhibit once
                      ;; typing settles.  Without this the polling
                      ;; timer's generation-based firing never
                      ;; refreshes (the producer process is
                      ;; quiescent post-typing, so no new generation
                      ;; ticks), and the stale display persists
                      ;; until the user types again.  Mirrors the
                      ;; `retry-timer' in `fzfa--multi-read'.
                      (retry-timer nil)
                      (stop
                       (lambda ()
                         (unless stopped
                           (setq stopped t)
                           (when retry-timer
                             (cancel-timer retry-timer)
                             (setq retry-timer nil))
                           (fzf-native-async-stop handle)))))
                 (push handle handles)
                 (push stop stops)
                 (apply #'helm-make-source name 'helm-source-sync
                        :header-name
                        (lambda (n)
                          (format "%s [%s]%s" n (abbreviate-file-name dir)
                                  (fzfa-helm--async-stats-suffix handle)))
                        :candidates
                        (lambda ()
                          (unless stopped
                            (let ((r (while-no-input
                                       (fzf-native-async-candidates
                                        handle helm-pattern limit))))
                              (cond
                               ((eq r t)
                                (when retry-timer (cancel-timer retry-timer))
                                (setq retry-timer
                                      (run-with-idle-timer
                                       fzfa-input-debounce nil
                                       (lambda ()
                                         (setq retry-timer nil)
                                         (when helm-alive-p
                                           (helm-force-update)))))
                                ;; Don't update rank — cached
                                ;; `last-result' is for an earlier query.
                                last-result)
                               (t
                                (when retry-timer
                                  (cancel-timer retry-timer)
                                  (setq retry-timer nil))
                                (setq last-result r)
                                (aset ranks i
                                      (fzfa--multi-rank r (or helm-pattern "") t))
                                r)))))
                        :match-dynamic t
                        :nohighlight t
                        :candidate-number-limit limit
                        :cleanup stop
                        :action action
                        (list :filtered-candidate-transformer
                              (fzfa-helm--make-display-transformer
                               annotate)))))
              (items
               ;; Sync source inlined here (rather than via
               ;; `fzfa-helm-make-sync-source') so its `:candidates'
               ;; can update the multi handler's per-source rank slot.
               ;; Same `while-no-input' + `last-result' cache +
               ;; `retry-timer' pattern as the async branch above.
               (let* ((last-filtered nil)
                      (last-total nil)
                      (last-result nil)
                      (retry-timer nil)
                      (sync-stop
                       (lambda ()
                         (when retry-timer
                           (cancel-timer retry-timer)
                           (setq retry-timer nil)))))
                 (apply #'helm-make-source name 'helm-source-sync
                        :header-name
                        (lambda (n)
                          (format "%s%s" n (fzfa-helm--sync-stats-suffix
                                            last-filtered last-total)))
                        :candidates
                        (lambda ()
                          (let* ((all (if (functionp items)
                                          (funcall items)
                                        items))
                                 (q (or helm-pattern ""))
                                 (r (while-no-input
                                      (if (string-empty-p q)
                                          (if history
                                              (fzfa--history-rank all history)
                                            all)
                                        (fzfa--bridge-defcustoms
                                         #'fzf-native-score-all all q)))))
                            (cond
                             ((eq r t)
                              (when retry-timer (cancel-timer retry-timer))
                              (setq retry-timer
                                    (run-with-idle-timer
                                     fzfa-input-debounce nil
                                     (lambda ()
                                       (setq retry-timer nil)
                                       (when helm-alive-p
                                         (helm-force-update)))))
                              ;; Don't update rank — cache is for an
                              ;; earlier query.
                              last-result)
                             (t
                              (when retry-timer
                                (cancel-timer retry-timer)
                                (setq retry-timer nil))
                              (setq last-total (length all)
                                    last-filtered (length r)
                                    last-result r)
                              (aset ranks i (fzfa--multi-rank r q nil))
                              r))))
                        :match-dynamic t
                        :nohighlight t
                        :candidate-number-limit limit
                        :cleanup sync-stop
                        :action action
                        (list :filtered-candidate-transformer
                              (fzfa-helm--make-display-transformer
                               annotate)))))
              (t
               (error "fzfa helm multi source has neither :command nor :items: %S"
                      src))))))
         ;; Cursor-follows-leader hook.  Runs after every helm update
         ;; (pattern change or force-update).  When the source with the
         ;; highest top-fzf-score changes, jump there.  Stable: ties
         ;; lose to the first source to reach the max (the `>' check
         ;; only succeeds on strictly-greater), matching the
         ;; completing-read multi's stable-sort behavior.  No-op when
         ;; pattern is empty (all ranks stay 0).
         (jump-fn
          (lambda ()
            (when (and helm-alive-p
                       (not (string-empty-p (or helm-pattern ""))))
              (let ((best-i nil)
                    (best-r 0))
                (dotimes (i n-sources)
                  (when (> (aref ranks i) best-r)
                    (setq best-r (aref ranks i)
                          best-i i)))
                (when (and best-i (not (eql best-i last-leader)))
                  (setq last-leader best-i)
                  (helm-goto-source (aref source-names best-i))))))))
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
        (progn
          (add-hook 'helm-after-update-hook jump-fn)
          (helm :sources helm-sources
                :prompt (or prompt "fzf-multi: ")
                :buffer "*helm fzfa multi*"))
      (remove-hook 'helm-after-update-hook jump-fn)
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
