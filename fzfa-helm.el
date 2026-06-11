;;; fzfa-helm.el --- Helm frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helm frontend for `fzfa'.  Loaded automatically when `helm' is in
;; `fzfa-extensions' and `fzfa-setup' has been called.  Once loaded,
;; the two internal entry points (`fzfa-helm--completing-read' and
;; `fzfa-helm--multi-read') are picked up by `fzfa.el's dispatch
;; sites via `fboundp', gated on `helm-mode'.
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
and `fzfa-helm--completing-read'.  Like the multi cap, this controls
both fzf-native's returned-list size and helm's
`:candidate-number-limit'.

Default is `fzfa-helm-multi-source-candidate-limit' × 10."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-helm-apply-follow t
  "Whether helm should auto-fire `:apply' on every selection move.

When non-nil (the default), bridged fzfa sources that declare an
`:apply' lambda set `:follow 1' on their helm source — the apply
function runs on every selection change (preview-style) AND on
`helm-execute-persistent-action'.  Matches helm's native
`:persistent-action' idiom.

Set to nil to disable auto-fire — `:apply' then runs only on
explicit `helm-execute-persistent-action'.  Useful for sources whose
`:apply' has side effects (buffer kill, command execute) that
shouldn't fire on every arrow-key press."
  :type 'boolean
  :group 'fzfa)

(defun fzfa-helm--ensure-loaded ()
  "Load `helm' and `helm-source'."
  (require 'helm)
  (require 'helm-source))

;;;###autoload
(defun fzfa-helm-setup ()
  "Wire fzfa's helm integration into the current session.

Called by `fzfa--ensure-setup' when `helm' is in `fzfa-extensions'.
The body is intentionally empty — invoking it through the autoload
stub loads this file, which is the wiring.  The dispatch sites in
`fzfa.el' (`fzfa-helm--completing-read', `fzfa-helm--multi-read')
become `fboundp' once the file is loaded and pick up
`helm-mode' automatically.")

(defun fzfa-helm--wrap-apply (apply-fn dir origin-window origin-buffer)
  "Return APPLY-FN wrapped so each fire runs against a stable baseline.

DIR / ORIGIN-WINDOW / ORIGIN-BUFFER are captured at session-start.

Helm fires `:persistent-action' in whatever buffer is currently shown
in its persistent-action-display-window.  When the apply is something
like `find-file', the first fire mutates that window (opens dired for
a directory candidate, opens an unrelated buffer for a file
candidate, etc.).  Every subsequent fire then inherits the mutated
buffer's `default-directory', and `expand-file-name' on a relative
candidate compounds the corruption.  Under `:follow 1' this cascades
into completely bogus paths within one or two scroll steps
\(e.g. `~/.emacs.d/user-lisp-30/user-lisp-30/extra/transient.el').

This wrapper restores the captured origin window/buffer/dir before
each fire — mirroring `fzfa--preview-call''s baseline reset — so the
cascade can't take hold.  The other frontends (vertico/icomplete/ivy)
don't need this because their apply only fires on user keypress and
the candidate is pre-resolved to an absolute path upstream of the
action; helm's `:follow 1' fires apply on every selection move with
the raw candidate string, so the baseline must be re-established
inside the action."
  (lambda (cand)
    (if (and (window-live-p origin-window) (buffer-live-p origin-buffer))
        (with-selected-window origin-window
          (with-current-buffer origin-buffer
            (let ((default-directory (or dir default-directory)))
              (funcall apply-fn cand))))
      (let ((default-directory (or dir default-directory)))
        (funcall apply-fn cand)))))

(defvar helm--execute-persistent-action-timer)

(defun fzfa-helm--cancel-stranded-follow-timer ()
  "Cancel helm's global `follow-mode' idle timer if still scheduled.

helm-core's `helm-follow-execute-persistent-action-maybe' schedules
`helm-execute-persistent-action' via a global idle timer
\(`helm--execute-persistent-action-timer', helm-core.el ~8292) but
helm-cleanup does not cancel it, and the callback does not check
`helm-alive-p' before invoking the persistent action — which itself
errors out via `with-helm-alive-p' when helm has exited.

Race: scroll selection (schedules timer) -> RET/ESC to exit helm
before the idle delay elapses -> timer fires post-cleanup ->
\"Running helm command outside of context\".  Only relevant when
:follow 1 is in play (every fzfa-helm source that wires preview)."
  (when (and (boundp 'helm--execute-persistent-action-timer)
             (timerp helm--execute-persistent-action-timer))
    (cancel-timer helm--execute-persistent-action-timer)
    (setq helm--execute-persistent-action-timer nil)))

(defvar helm-alive-p)
(defvar helm-pattern)
(defvar helm-completion-style)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")
(declare-function helm-goto-source "helm-core")
(declare-function helm-set-source-filter "helm-core")
(declare-function fzfa--format-narrow-hint "fzfa")
(defvar helm-map)
(defvar fzfa-multi-narrow-key)
(declare-function fzfa--multi-rank "fzfa")
(declare-function fzf-native-async-start "fzf-native")
(declare-function fzf-native-async-stop "fzf-native")
(declare-function fzf-native-async-generation "fzf-native")
(declare-function fzf-native-async-candidates "fzf-native")
(declare-function fzf-native-async-stats "fzf-native")
(declare-function fzf-native-score-all "fzf-native")
(declare-function fzfa--history-rank "fzfa")
(declare-function fzfa--extract-args "fzfa")
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
async / multi handlers trigger on each generation tick."
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

;;; Live preview wrapper

(defvar fzfa-preview-delay)

(defun fzfa-helm--make-debounced-preview-fn (&optional session-cell)
  "Return a fresh `:persistent-action' closure that debounces preview dispatch.

`helm' fires `:persistent-action' on every selection change when
`:follow 1' is set — no idle-timer debounce of its own.

This wrapper closes over a private `preview-timer' + `preview-last'
pair (per-closure, so each `helm' source in a multi gets its own
debounce state) and:

- Skips dispatch when the candidate equals the previously-previewed
  one (`helm' sometimes re-fires persistent-action on no-op moves).
- Cancels any pending timer before scheduling a new one so fast
  scrolling repeatedly resets the debounce — net effect: preview
  fires only after the user pauses for `fzfa-preview-delay'.
- Bypasses the debounce when `fzfa-preview-delay' is 0 or less,
  firing immediately (matches the vertico path's escape hatch).

SESSION-CELL, when non-nil, is bound to `fzfa--preview-session'
inside the dispatch — required for the multi handler where each
source has its own session cell and the ambient
`fzfa--preview-session' may point at a different cell (or be nil)
when the idle timer fires.  When nil, dispatch uses the ambient
binding (sync/async paths bind it themselves at the handler
level for the whole helm session)."
  (let ((preview-timer nil)
        (preview-last 'unset))
    (lambda (cand)
      (unless (equal cand preview-last)
        (when (timerp preview-timer)
          (cancel-timer preview-timer))
        (if (<= (or fzfa-preview-delay 0) 0)
            (progn
              (setq preview-last cand)
              (let ((fzfa--preview-session
                     (or session-cell fzfa--preview-session)))
                (fzfa--preview-call :preview cand)))
          (setq preview-timer
                (run-with-idle-timer
                 fzfa-preview-delay nil
                 (lambda ()
                   (setq preview-timer nil
                         preview-last cand)
                   (let ((fzfa--preview-session
                          (or session-cell fzfa--preview-session)))
                     (fzfa--preview-call :preview cand))))))))))

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
    (name command directory action limit
          &optional annotate persistent-action apply)
  "Return (SOURCE . STOP) for NAME / COMMAND.

Eagerly start the fzf-native producer in DIRECTORY and the polling
timer, capped at LIMIT candidates.  ACTION is the helm action.
SOURCE's `:cleanup' calls STOP; STOP is idempotent so callers can also
invoke it externally for defense-in-depth (e.g. multi-source bulk
cleanup when helm never gets a chance to call `:cleanup' itself).

ANNOTATE, when non-nil, is a (CAND) -> STRING function rendered as a
per-row suffix via `fzfa-helm--make-display-transformer'.

PERSISTENT-ACTION, when non-nil, is a (CAND) -> ANY function wired to
helm's `:persistent-action' slot with `:follow 1' — used to wire
fzfa's `:preview' dispatch (live preview-as-you-scroll under `helm').
Overridden by APPLY when both are set.

APPLY, when non-nil, is a (CAND) -> ANY function bound to `helm''s
`:persistent-action' slot, taking precedence over PERSISTENT-ACTION.
`:follow' tracks `fzfa-helm-apply-follow' — auto-fire on every
selection move (default) or on `helm-execute-persistent-action'.

Display cycling: `fzfa-display-key' (default `>') is bound in
the source's keymap and cycles `hidden' → `compact' → `full' →
`hidden' using the shared `fzfa--display-{materialize,extract}'
helpers.  Editing the CMD region in compact / full debounce-restarts
the subprocess with the new command.  Same UX as the completing-read
path.

Internal — used by both `fzfa-helm-make-async-source' (single-source)
and `fzfa-helm--multi-read' (batch with bulk-stop)."
  (let* ((dir (expand-file-name (or directory default-directory)))
         (initial-char fzfa-separator)
         (handle (fzf-native-async-start command dir))
         ;; Display-state machinery.  `command' is the cl-defun arg —
         ;; mutated in place by `display-cycle' when the user edits and
         ;; demotes back to hidden.  `current-cmd' is what the running
         ;; subprocess is using; differs from `command' between the user
         ;; editing and the debounced restart actually firing.
         (display-state 'hidden)
         (current-cmd command)
         (separator-overlays nil)
         (display-overlays nil)
         (restart-timer nil)
         (last-restart-time 0.0)
         (last-gen -1)
         (stopped nil)
         (timer nil)
         (do-restart
          (lambda (cmd)
            (when handle (fzfa--defer-async-stop handle))
            (setq handle (and cmd (not (string-empty-p cmd))
                              (fzf-native-async-start cmd dir))
                  current-cmd cmd
                  last-gen -1
                  last-restart-time (float-time))
            (when helm-alive-p (helm-force-update))))
         (display-clear
          (lambda ()
            (mapc #'delete-overlay display-overlays)
            (setq display-overlays nil)))
         (display-apply
          (lambda ()
            ;; Only `compact' installs an overlay (flags collapse to
            ;; `…').  `hidden' has no CMD region in the buffer and
            ;; `full' shows the cmd verbatim — both no-op.  Mirrors the
            ;; `display-apply' closure in the completing-read body.
            (funcall display-clear)
            (when (eq display-state 'compact)
              (when-let* ((bounds (fzfa--display-cmd-bounds
                                   initial-char)))
                (setq display-overlays
                      (fzfa--display-make-overlays
                       (car bounds) (cdr bounds)))))))
         (display-cycle
          (lambda ()
            (interactive)
            (let* ((from display-state)
                   (to (fzfa--display-next-state from)))
              (cond
               ((and (eq from 'hidden) (eq to 'compact))
                (setq separator-overlays
                      (fzfa--display-materialize
                       command initial-char)))
               ((eq to 'hidden)
                (setq command (fzfa--display-extract
                               separator-overlays))
                (setq separator-overlays nil)))
              (setq display-state to)
              (funcall display-apply)
              ;; Sync `helm-pattern' to the post-mutation
              ;; minibuffer-contents.  Otherwise post-command-hook's
              ;; `helm-check-minibuffer-input' would see the mutation
              ;; as a pattern change and fire `helm-update' — which
              ;; erases the helm-buffer and re-renders the entire
              ;; candidate list, even though the filter portion (and
              ;; therefore the candidates) is unchanged.  Real CMD
              ;; edits in compact/full happen via `self-insert-command'
              ;; and DO trigger the natural helm-update path because
              ;; `helm-pattern' is stale by the time post-command-hook
              ;; runs.
              (when helm-alive-p
                (setq helm-pattern (minibuffer-contents))))))
         (stop
          (lambda ()
            (unless stopped
              (setq stopped t)
              (when timer (cancel-timer timer) (setq timer nil))
              (when restart-timer
                (cancel-timer restart-timer)
                (setq restart-timer nil))
              (mapc #'delete-overlay separator-overlays)
              (mapc #'delete-overlay display-overlays)
              (setq separator-overlays nil
                    display-overlays nil)
              (fzfa--defer-async-stop handle)))))
    (setq timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when (and helm-alive-p (not stopped))
               (let ((gen (and handle (fzf-native-async-generation handle))))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update)))))))
    (cons
     (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
            :header-name
            (lambda (n)
              (format "%s [%s]%s" n (abbreviate-file-name dir)
                      (and handle (fzfa-helm--async-stats-suffix handle))))
            :keymap (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map helm-map)
                      (when fzfa-display-key
                        (define-key map (kbd fzfa-display-key)
                                    display-cycle))
                      map)
            :candidates
            (lambda ()
              (unless stopped
                (pcase-let* ((`(,cmd . ,filter)
                              (fzfa--split
                               (or helm-pattern "") display-state command)))
                  (when (not (equal cmd current-cmd))
                    ;; Debounce-then-restart, mirroring the completing-
                    ;; read path's `do-restart' scheduling.  Floor on the
                    ;; restart gap is `fzfa-shell-command-throttle' so
                    ;; rapid edits don't fork a process per keystroke.
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
                               (funcall do-restart cmd))))))
                  (and handle
                       (fzf-native-async-candidates handle filter limit)))))
            :match-dynamic t
            :nohighlight t
            :candidate-number-limit limit
            :cleanup stop
            :action (or action (lambda (cand) cand))
            ;; Unconditional: transformer also preserves text
            ;; properties on candidates via `helm-realvalue', even
            ;; when ANNOTATE is nil.  See
            ;; `fzfa-helm--make-display-transformer'.
            (append
             (list :filtered-candidate-transformer
                   (fzfa-helm--make-display-transformer annotate))
             (cond
              (apply
               (append (list :persistent-action apply)
                       (when fzfa-helm-apply-follow '(:follow 1))))
              (persistent-action
               (list :persistent-action persistent-action :follow 1)))))
     stop)))

(cl-defun fzfa-helm-make-async-source
    (&key name command directory action annotate persistent-action apply
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

PERSISTENT-ACTION wires preview-as-you-scroll under `helm'; APPLY wires
`fzfa''s `:apply' and takes precedence.  See
`fzfa-helm--async-source-and-stop' for the precedence and `:follow'
rules.

The producer process and polling timer start eagerly at construction
time, BEFORE helm activates — matches the original (pre-extraction)
timing.  The source's `:cleanup' stops it."
  (fzfa-helm--ensure-loaded)
  (car (fzfa-helm--async-source-and-stop
        name command directory action
        (or candidate-number-limit 10000)
        annotate persistent-action apply)))

(cl-defun fzfa-helm-make-sync-source
    (&key name items action history annotate persistent-action apply
          display
          (candidate-number-limit fzfa-helm-candidate-limit))
  "Return a helm source that scores ITEMS with `fzf-native'.

NAME is the source header.  ITEMS is a list of strings, a zero-arg
function returning one, or a 2-arg producer fn `(lambda (INPUT
CALLBACK) ...)' (sync- or async-firing).  ACTION is a one-arg function
called with the selection (default returns it unchanged).
CANDIDATE-NUMBER-LIMIT is `helm''s display cap.

ANNOTATE, when non-nil, is a (CAND) -> STRING function rendered as a
per-row suffix (faced as `completions-annotations', column-aligned
within the rendered batch).  Affects display only; fzf scoring and
action dispatch operate on the raw candidate.

PERSISTENT-ACTION wires preview-as-you-scroll under helm; APPLY wires
fzfa's `:apply' (persistent-action proper) and takes precedence.
`:follow' tracks `fzfa-helm-apply-follow' when APPLY is in use, else
defaults to 1 for PERSISTENT-ACTION.

HISTORY is an optional history variable symbol.  When set and
`helm-pattern' is empty, ITEMS are reordered via `fzfa--history-rank'
so recent picks surface first — helm does not consult the completion
metadata's `display-sort-function', so this sort has to happen at the
candidate level.  HISTORY here governs ORDERING only; pushing the
selection onto HISTORY is the caller's responsibility (the registered
sync/multi handlers wrap their `:action' to do so).

For 2-arg producer ITEMS, `helm-pattern' is split via `fzfa--split'
into (CMD . FILTER) based on DISPLAY state — hidden mode keeps CMD in
source-local closure state (editable region in the minibuffer is pure
FILTER); compact/full materialize CMD into the buffer between
`fzfa-separator' chars.  `fzfa-display-key' (default `>') is bound on
the source's `:keymap' and cycles hidden → compact → full → hidden,
mirroring `fzfa-helm-make-async-source' and the substrate.  For lists
and zero-arg fns there is no CMD concept; `helm-pattern' is the FILTER
directly and DISPLAY is ignored.

Producer-kind detection runs once at source construction (a test fire
with empty input observes whether the callback arrives synchronously).
Async-firing producers (jsonrpc, url-retrieve) keep their snapshot in
source-local closure state and trigger `helm-force-update' from the
callback so helm re-reads candidates with the fresh snapshot."
  (fzfa-helm--ensure-loaded)
  (let* ((limit (or candidate-number-limit 10000))
         (last-filtered nil)
         (last-total nil)
         (last-result nil)
         (retry-timer nil)
         (prod-snapshot nil)
         (prod-last-cmd :unfired)
         (prod-token 0)
         (kind
          (cond
           ((listp items) 'list)
           ((functionp items)
            (if (>= (car (func-arity items)) 1)
                (let ((fired nil))
                  (funcall items "" (lambda (_x) (setq fired t)))
                  (if fired 'sync 'async))
              'zero))))
         (producer-kind-p (memq kind '(sync async)))
         ;; Display-state machinery for producer kinds.  `command'
         ;; stores the CMD half when display-state is `hidden' (it
         ;; lives in this closure, not the minibuffer); compact/full
         ;; materialize it into the buffer.  `display-cycle' is bound
         ;; to `fzfa-display-key' below.  Mirrors
         ;; `fzfa-helm-make-async-source' and the substrate body in
         ;; `fzfa-completing-read'.
         (initial-char fzfa-separator)
         (display-state (or display 'hidden))
         (command "")
         (separator-overlays nil)
         (display-overlays nil)
         (display-clear
          (lambda ()
            (mapc #'delete-overlay display-overlays)
            (setq display-overlays nil)))
         (display-apply
          (lambda ()
            ;; Only `compact' installs an overlay (flags collapse to
            ;; `…').  `hidden' has no CMD region; `full' shows the cmd
            ;; verbatim.  Same dispatch as the async source.
            (funcall display-clear)
            (when (eq display-state 'compact)
              (when-let* ((bounds (fzfa--display-cmd-bounds
                                   initial-char)))
                (setq display-overlays
                      (fzfa--display-make-overlays
                       (car bounds) (cdr bounds)))))))
         (display-cycle
          (lambda ()
            (interactive)
            (let* ((from display-state)
                   (to (fzfa--display-next-state from)))
              (cond
               ((and (eq from 'hidden) (eq to 'compact))
                (setq separator-overlays
                      (fzfa--display-materialize
                       command initial-char)))
               ((eq to 'hidden)
                (setq command (fzfa--display-extract
                               separator-overlays))
                (setq separator-overlays nil)))
              (setq display-state to)
              (funcall display-apply)
              ;; Sync `helm-pattern' to the post-mutation
              ;; minibuffer-contents.  Otherwise post-command-hook's
              ;; `helm-check-minibuffer-input' would see the mutation
              ;; as a pattern change and fire `helm-update', erasing
              ;; the helm-buffer and re-rendering candidates even
              ;; though the FILTER (and therefore the candidates) is
              ;; unchanged.
              (when helm-alive-p
                (setq helm-pattern (minibuffer-contents))))))
         (stop (lambda ()
                 (when retry-timer
                   (cancel-timer retry-timer)
                   (setq retry-timer nil))
                 (mapc #'delete-overlay separator-overlays)
                 (mapc #'delete-overlay display-overlays)
                 (setq separator-overlays nil
                       display-overlays nil))))
    (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
           :header-name
           (lambda (n)
             (format "%s%s" n (fzfa-helm--sync-stats-suffix
                               last-filtered last-total)))
           :keymap (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map helm-map)
                     (when (and producer-kind-p fzfa-display-key)
                       (define-key map (kbd fzfa-display-key)
                                   display-cycle))
                     map)
           :candidates
           (lambda ()
             (let* ((pat (or helm-pattern ""))
                    ;; For producer kinds, split CMD from FILTER and
                    ;; route CMD to the producer; for static kinds the
                    ;; whole pattern is the FILTER.
                    (split (and producer-kind-p
                                (fzfa--split pat display-state command)))
                    (cmd (and split (car split)))
                    (filter (if split (cdr split) pat))
                    (all
                     (cl-case kind
                       (list items)
                       (zero (funcall items))
                       (sync (let (snap)
                               (funcall items (or cmd "")
                                        (lambda (x) (setq snap x)))
                               snap))
                       (async
                        (unless (equal cmd prod-last-cmd)
                          (setq prod-last-cmd cmd)
                          (let ((my-token (cl-incf prod-token)))
                            (funcall items (or cmd "")
                                     (lambda (cands-result)
                                       (when (= my-token prod-token)
                                         (setq prod-snapshot cands-result)
                                         (when (and (boundp 'helm-alive-p)
                                                    helm-alive-p)
                                           (run-with-idle-timer
                                            0 nil
                                            #'helm-force-update)))))))
                        prod-snapshot)))
                    (r (while-no-input
                         (if (string-empty-p filter)
                             (if history (fzfa--history-rank all history) all)
                           (fzfa--bridge-defcustoms
                            #'fzf-native-score-all all filter)))))
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
           (append
            (list :filtered-candidate-transformer
                  (fzfa-helm--make-display-transformer annotate))
            (cond
             (apply
              (append (list :persistent-action apply)
                      (when fzfa-helm-apply-follow '(:follow 1))))
             (persistent-action
              (list :persistent-action persistent-action :follow 1)))))))

;;; Composition helper — fzfa command -> helm source(s)

(defun fzfa-helm--source-from-plist (plist)
  "Build a helm source from a fzfa source PLIST.

PLIST has the shape produced by fzfa's `:extract' mode (and, for
multi-source commands, by `fzfa-multi-read''s inner per-source
plists).  Dispatches `:command' to `fzfa-helm-make-async-source'
and `:candidates' to `fzfa-helm-make-sync-source'.

The plist's `:action' (typically the `:inject' lambda fzfa-multi-read
installed) is wrapped to also `add-to-history' onto `:history' —
mirroring the HIST push the inner `completing-read' would have done,
which is skipped when we bypass it via `:inject' mode."
  (let* ((name        (or (plist-get plist :name) "fzfa"))
         (cmd         (plist-get plist :command))
         (cands       (plist-get plist :candidates))
         (directory   (or (plist-get plist :directory) default-directory))
         (history     (plist-get plist :history))
         (annotate    (plist-get plist :annotate))
         (category    (plist-get plist :category))
         (orig-action (plist-get plist :action))
         (apply       (or (plist-get plist :apply)
                          (plist-get
                           (alist-get category fzfa-apply-functions)
                           :apply)))
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (when orig-action (funcall orig-action cand)))))
    (cond
     (cmd
      (fzfa-helm-make-async-source
       :name name :command cmd :directory directory :action action
       :annotate annotate :apply apply))
     (cands
      (fzfa-helm-make-sync-source
       :name name :items cands :history history :action action
       :annotate annotate :apply apply))
     (t
      (error "Fzfa source plist has neither :command nor :candidates: %S" plist)))))

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
its keyword args (`:command', `:directory', `:candidates',
`:history') without running.  The selection is routed back via
fzfa's `:inject' mode so CMD's post-action (e.g. `find-file', grep
jump) runs.  The HIST push that the inner `completing-read' would
have done is mirrored via `add-to-history' wrapping the inject
action.

OVERRIDES is a keyword args plist merged on top of the extracted args
— useful for renaming via :name, swapping :action, repointing
:directory.  OVERRIDES is IGNORED for multi-source commands (which
have N inner sources, no single set of args to override).

Caveat for multi-source commands: each inner source gets its own
polling timer (no shared timer like `fzfa-helm--multi-read' uses).
For pure-fzfa multis with several async sources, `fzfa-multi-read'
is faster.  Composition into helm-mini-style sessions is the
intended use case.

Errors if CMD is not an extract-capable fzfa command."
  (let ((args (fzfa--extract-args cmd)))
    (unless args
      (user-error "`%s' is not an extract-capable fzfa command" cmd))
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
             (cands (or (plist-get overrides :candidates)
                        (plist-get args :candidates)))
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
               (list :name name :command cmd-shell :candidates cands
                     :directory directory :history history
                     :action action))))))))

;;; Unified handler — `fzfa-completing-read' dispatch under helm-mode

(cl-defun fzfa-helm--completing-read
    (&key prompt command candidates directory category annotate
          affix group history require-match default preview apply
          display
          skip-executable-check)
  "Helm dispatch for `fzfa-completing-read'.

Single entry point covering all three source kinds the substrate
supports — `:command' (shell pipeline), `:candidates' as a static
list or zero-arg fn, and `:candidates' as a 2-arg `(INPUT CALLBACK)'
producer fn (sync- or async-firing).  Mirrors the substrate's own
single-entry-point shape.

Source-kind routing:

  :command            → `fzfa-helm-make-async-source' (process pipe).
  :candidates list /  → `fzfa-helm-make-sync-source'.  Whole
    zero-arg fn         `helm-pattern' is the FILTER (no CMD concept).
  :candidates 2-arg   → `fzfa-helm-make-sync-source'.  Producer kind
    fn                  (sync- or async-firing) is detected once at
                        source construction; `helm-pattern' is split
                        via `fzfa--split-input' into (CMD . FILTER).
                        CMD goes to the producer's INPUT slot (refire
                        only when CMD changes); FILTER scores the
                        snapshot via `fzf-native-score-all'.  Async
                        callbacks update a closure-scoped snapshot
                        and schedule `helm-force-update'.

Shared scaffolding: PROMPT, CATEGORY, PREVIEW, APPLY, HISTORY,
DEFAULT, DIRECTORY, SKIP-EXECUTABLE-CHECK behave as in
`fzfa-completing-read'.  ANNOTATE, AFFIX, GROUP, REQUIRE-MATCH are
accepted for signature parity but unused — helm sources don't
consume completion-read metadata.

PREVIEW (with CATEGORY for registry lookup) wires the preview
lifecycle — handler resolved via `fzfa--preview-handler', session
state captured manually (helm doesn't go through
`minibuffer-with-setup-hook' so `fzfa--preview-install' can't be
used), `:setup' fires before helm activates, `:exit' + `:return'
fire on exit.  Live preview during the session (firing `:preview'
on selection movement) wires up via `:persistent-action' +
`:follow 1' when handler is set and APPLY is not — APPLY otherwise
wins the persistent-action slot.

Bypasses `completing-read' (and therefore helm-mode's advice) so
per-history candidate ordering can land at the candidate level —
helm doesn't consult the metadata `display-sort-function' that
vertico / icomplete rely on.  Side effect: bypassing
`completing-read' also bypasses its HIST push, so the action wraps
`add-to-history' to mirror what would have happened."
  (fzfa-helm--ensure-loaded)
  (ignore annotate affix group require-match)
  (when (and command candidates)
    (user-error
     "fzfa-helm--completing-read: :command and :candidates are mutually exclusive"))
  (unless (or skip-executable-check candidates)
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (let* ((prompt (or prompt
                     (when command
                       (concat (car (split-string command nil t)) ": "))
                     (when candidates "fzf > ")))
         (dir (expand-file-name (or directory default-directory)))
         (result nil)
         (helm-completion-style 'emacs)
         (handler (fzfa--preview-handler preview category))
         (fzfa--preview-session (and handler (list handler)))
         (apply-fn (or apply (plist-get
                              (alist-get category fzfa-apply-functions)
                              :apply)))
         (origin-window (selected-window))
         (origin-buffer (window-buffer (selected-window)))
         ;; Wrap apply with baseline restoration — see
         ;; `fzfa-helm--wrap-apply'.  Baseline `default-directory'
         ;; is :DIRECTORY when supplied (typical for :command), else
         ;; the user's invoking buffer dir (typical for :candidates).
         (apply-fn (and apply-fn
                        (fzfa-helm--wrap-apply
                         apply-fn dir origin-window origin-buffer)))
         ;; History-push action wrapper.  When HISTORY is a real symbol,
         ;; mirror `completing-read''s HIST push that helm bypasses.
         (action
          (lambda (cand)
            (when (and history (symbolp history) (not (eq history t)))
              (add-to-history history cand))
            (setq result cand)))
         (source
          (cond
           (command
            (fzfa-helm-make-async-source
             :name (or prompt "fzfa")
             :command command
             :directory dir
             :action action
             :persistent-action
             (and handler (fzfa-helm--make-debounced-preview-fn))
             :apply apply-fn))
           (candidates
            (fzfa-helm-make-sync-source
             :name (or prompt "fzfa")
             :items candidates
             :history history
             :action action
             :display display
             :persistent-action
             (and handler (fzfa-helm--make-debounced-preview-fn))
             :apply apply-fn)))))
    (when handler
      (fzfa-preview-put :origin-window origin-window)
      (fzfa-preview-put :origin-buffer origin-buffer)
      (fzfa-preview-put :default-directory default-directory)
      (fzfa--preview-call :setup))
    ;; Producer-kind `:display' compact/full needs the leading separator(s)
    ;; pre-seeded into the minibuffer — empty preset CMD: `<sep><sep>' with
    ;; point between them.  Helm doesn't honor `:input' for cursor-mid
    ;; placement, so we install a one-shot `helm-minibuffer-set-up-hook'
    ;; that does both.
    (let* ((producer-kind-p
            (and candidates (functionp candidates)
                 (>= (car (func-arity candidates)) 1)))
           (init-text
            (and producer-kind-p
                 (memq display '(compact full))
                 (concat (char-to-string fzfa-separator)
                         (char-to-string fzfa-separator))))
           (init-point (and init-text 1))
           (setup-fn
            (when init-text
              (lambda ()
                (when (active-minibuffer-window)
                  (with-selected-window (active-minibuffer-window)
                    (goto-char (minibuffer-prompt-end))
                    (delete-region (point) (point-max))
                    (insert init-text)
                    (goto-char (+ (minibuffer-prompt-end) init-point))))))))
      (unwind-protect
          (let ((default-directory dir))
            (when setup-fn
              (add-hook 'helm-minibuffer-set-up-hook setup-fn))
            (helm :sources source
                  :prompt prompt
                  :default default
                  :buffer "*helm fzfa*"))
        (when setup-fn
          (remove-hook 'helm-minibuffer-set-up-hook setup-fn))
        (when handler
          (fzfa--preview-call :exit)
          (fzfa--preview-return result))
        (fzfa-helm--cancel-stranded-follow-timer)))
    result))

;;; Multi handler — dispatched from `fzfa--multi-read'

(cl-defun fzfa-helm--multi-read (sources &key prompt)
  "Helm dispatch for `fzfa--multi-read'.

SOURCES is the same list of plists as the `completing-read' path.
PROMPT is the prompt string shown in the helm session.
Each `fzfa' source maps to a `helm' source:
  :command     -> async (eager-start, no per-source polling timer)
  :candidates  -> sync (fzf-native-score-all on each `helm-pattern'
                  change).  Producer kind is detected once at source
                  construction; async-firing producers (jsonrpc etc.)
                  keep their snapshot in source-local closure state and
                  trigger `helm-force-update' on callback arrival.

A SINGLE shared polling timer watches every async handle and calls
`helm-force-update' at most once per `fzfa-input-throttle' seconds
when any source has new candidates.

Cursor follows the highest-ranked source: each source's `:candidates'
closure stores its top-fzf-score in a `ranks' vector, and a
`helm-after-update-hook' calls `helm-goto-source' on the leader when
it changes.  Replaces helm's default \"first non-empty source\"
positioning, which is declared-order-arbitrary and structurally wrong
for fuzzy-multi-source UX."
  (fzfa-helm--ensure-loaded)
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
         ;; Per-source preview session cells — one per source.  A cell
         ;; is `(HANDLER STATE-PLIST...)' when the source has a
         ;; resolved preview handler (from its `:preview' or
         ;; `:category'); nil otherwise.  `fzfa-preview-put' mutates the
         ;; cdr to accumulate state from `:setup' onward, and the
         ;; debouncer's idle-timer callback let-binds
         ;; `fzfa--preview-session' to this cell so dispatches see the
         ;; per-source state.  Built lazily inside the source loop.
         (preview-cells (make-vector n-sources nil))
         (any-preview nil)
         ;; Tracks which source the winning action fired from — for
         ;; broadcasting `:return' on cleanup: the winning source's
         ;; cell receives the RAW CAND (`result-cand'), every other
         ;; cell receives nil ("aborted from its perspective"),
         ;; mirroring `fzfa--multi-build-router' at fzfa.el:~1845.
         ;; `result' (the helm action's return value) may differ from
         ;; the raw cand — multi inject lambdas return whatever the
         ;; inner command returns (e.g. a buffer object from
         ;; `switch-to-buffer'), and feeding that into a `:return'
         ;; handler that expects a string (e.g. `fzfa--file-preview-return')
         ;; signals `Wrong type argument: stringp, #<buffer>'.
         (result-src-idx nil)
         (result-cand nil)
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
                  (cands       (plist-get src :candidates))
                  (directory   (or (plist-get src :directory)
                                   default-directory))
                  (history     (plist-get src :history))
                  (annotate    (plist-get src :annotate))
                  (orig-action (plist-get src :action))
                  ;; Resolve this source's preview handler from its
                  ;; own `:preview' override and `:category'.  When set,
                  ;; store a fresh session cell into the outer
                  ;; `preview-cells' vector so the setup/cleanup loops
                  ;; outside the cl-loop see it.
                  (preview-handler (fzfa--preview-handler
                                    (plist-get src :preview)
                                    (plist-get src :category)))
                  (preview-cell (and preview-handler
                                     (list preview-handler)))
                  (action
                   (lambda (cand)
                     (when (and history (symbolp history)
                                (not (eq history t)))
                       (add-to-history history cand))
                     ;; Record which source the winning action fired
                     ;; from so cleanup can route `:return' correctly,
                     ;; AND the raw cand (separate from the action's
                     ;; return value — see `result-cand' comment above).
                     (setq result-src-idx i
                           result-cand cand
                           result
                           (if orig-action
                               (funcall orig-action cand)
                             cand)))))
             (when preview-cell
               (aset preview-cells i preview-cell)
               (setq any-preview t))
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
                           (fzfa--defer-async-stop handle)))))
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
                                      (fzfa--multi-rank
                                       r (or helm-pattern "") t))
                                r)))))
                        :match-dynamic t
                        :nohighlight t
                        :candidate-number-limit limit
                        :cleanup stop
                        :action action
                        (append
                         (list :filtered-candidate-transformer
                               (fzfa-helm--make-display-transformer
                                annotate))
                         (when preview-cell
                           (list :persistent-action
                                 (fzfa-helm--make-debounced-preview-fn
                                  preview-cell)
                                 :follow 1))))))
              (cands
               ;; Sync source inlined here (rather than via
               ;; `fzfa-helm-make-sync-source') so its `:candidates'
               ;; can update the multi handler's per-source rank slot.
               ;; Same `while-no-input' + `last-result' cache +
               ;; `retry-timer' pattern as the async branch above.
               ;;
               ;; Producer kind is detected once at construction:
               ;; lists and zero-arg fns are static; 2-arg producers
               ;; get a test fire to determine whether the callback
               ;; arrives synchronously (regexp scan etc.) or
               ;; asynchronously (jsonrpc, url-retrieve).  Async-
               ;; firing sources keep their snapshot in source-local
               ;; closure state and trigger `helm-force-update' when
               ;; the callback arrives.
               (let* ((last-filtered nil)
                      (last-total nil)
                      (last-result nil)
                      (retry-timer nil)
                      (prod-snapshot nil)
                      (prod-last-fired :unfired)
                      (prod-token 0)
                      (kind
                       (cond
                        ((listp cands) 'list)
                        ((functionp cands)
                         (if (>= (car (func-arity cands)) 1)
                             (let ((fired nil))
                               (funcall cands ""
                                        (lambda (_x) (setq fired t)))
                               (if fired 'sync 'async))
                           'zero))))
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
                          (let* ((pat (or helm-pattern ""))
                                 ;; #CMD#FILTER split — producer
                                 ;; kinds route CMD to INPUT and
                                 ;; FILTER to fzf scoring.  Static
                                 ;; kinds have no CMD; whole pattern
                                 ;; is the FILTER.
                                 (split (and (memq kind '(sync async))
                                             (fzfa--split-input pat)))
                                 (cmd (and split (car split)))
                                 (filter (if split (cdr split) pat))
                                 (all
                                  (cl-case kind
                                    (list cands)
                                    (zero (funcall cands))
                                    (sync (let (snap)
                                            (funcall cands (or cmd "")
                                                     (lambda (x)
                                                       (setq snap x)))
                                            snap))
                                    (async
                                     ;; Fire producer when CMD changes
                                     ;; since the last fire.  Callback
                                     ;; updates prod-snapshot and
                                     ;; schedules a force-update;
                                     ;; meanwhile we return the current
                                     ;; snapshot (possibly stale for one
                                     ;; tick).
                                     (unless (equal cmd prod-last-fired)
                                       (setq prod-last-fired cmd)
                                       (let ((my-token
                                              (cl-incf prod-token)))
                                         (funcall cands (or cmd "")
                                                  (lambda (cands-result)
                                                    (when (= my-token
                                                             prod-token)
                                                      (setq prod-snapshot
                                                            cands-result)
                                                      (when (and (boundp 'helm-alive-p)
                                                                 helm-alive-p)
                                                        (run-with-idle-timer
                                                         0 nil
                                                         #'helm-force-update)))))))
                                     prod-snapshot)))
                                 (r (while-no-input
                                      (if (string-empty-p filter)
                                          (if history
                                              (fzfa--history-rank all history)
                                            all)
                                        (fzfa--bridge-defcustoms
                                         #'fzf-native-score-all all filter)))))
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
                              (aset ranks i
                                    (fzfa--multi-rank r filter nil))
                              r))))
                        :match-dynamic t
                        :nohighlight t
                        :candidate-number-limit limit
                        :cleanup sync-stop
                        :action action
                        (append
                         (list :filtered-candidate-transformer
                               (fzfa-helm--make-display-transformer
                                annotate))
                         (when preview-cell
                           (list :persistent-action
                                 (fzfa-helm--make-debounced-preview-fn
                                  preview-cell)
                                 :follow 1))))))
              (t
               (error
                "Fzfa helm multi source has neither :command nor :candidates: %S"
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
    ;; Per-source preview `:setup' broadcast.  Each cell captures the
    ;; ORIGIN window/buffer/`default-directory' (the user's selected
    ;; window before helm activated), then dispatches `:setup' under its
    ;; own session binding so per-source state stashed via
    ;; `fzfa-preview-put' lands in this cell's cdr.
    (when any-preview
      (dotimes (i n-sources)
        (when-let* ((cell (aref preview-cells i)))
          (let ((fzfa--preview-session cell))
            (fzfa-preview-put :origin-window (selected-window))
            (fzfa-preview-put :origin-buffer (window-buffer (selected-window)))
            (fzfa-preview-put :default-directory default-directory)
            (fzfa--preview-call :setup)))))
    (unwind-protect
        (let* (;; Narrow-by-source: press `fzfa-multi-narrow-key',
               ;; then the source's `:narrow' key, to filter helm to
               ;; that source only.  Press the prefix again to widen.
               ;; Routes via `helm-set-source-filter' — helm's
               ;; built-in mechanism for showing a subset of
               ;; `helm-sources' without rebuilding them.  Each source
               ;; plist already carries its allocated `:narrow' key
               ;; (assigned by `fzfa--multi-allocate-narrow-keys' in
               ;; `fzfa-multi-read' before the dispatch).
               (narrow-fn
                (lambda ()
                  (interactive)
                  (let* ((sources-v (vconcat sources))
                         ;; KEY:NAME pairs separated by two spaces, with
                         ;; the prefix-widen marker at the end, faced
                         ;; via `fzfa--format-narrow-hint' — same
                         ;; rendering the vertico narrow menu uses.
                         (hint (concat
                                (fzfa--format-narrow-hint
                                 sources-v nil nil fzfa-multi-narrow-key)
                                " "))
                         (source-keys
                          (cl-loop for src in sources
                                   for i from 0
                                   when (plist-get src :narrow)
                                   collect (cons (plist-get src :narrow)
                                                 (aref source-names i))))
                         (c (read-char hint))
                         (key (string c))
                         (target (cdr (assoc key source-keys
                                             #'equal))))
                    (cond
                     (target (helm-set-source-filter (list target)))
                     ((equal key fzfa-multi-narrow-key)
                      (helm-set-source-filter nil))
                     (t (message "fzfa: no source bound to narrow key %S"
                                 key))))))
               ;; Layer the narrow binding onto a fresh COPY of
               ;; `helm-map' so the user's helm-map customizations
               ;; (TAB → persistent-action, etc.) are preserved.
               (helm-map
                (if fzfa-multi-narrow-key
                    (let ((m (copy-keymap helm-map)))
                      (define-key m (kbd fzfa-multi-narrow-key)
                                  narrow-fn)
                      m)
                  helm-map)))
          (add-hook 'helm-after-update-hook jump-fn)
          (helm :sources helm-sources
                :prompt (or prompt "fzf-multi: ")
                :buffer "*helm fzfa multi*"))
      (remove-hook 'helm-after-update-hook jump-fn)
      (when poll-timer (cancel-timer poll-timer))
      ;; Bulk-stop async producers; idempotent — :cleanup may have
      ;; already fired on normal helm exit.
      (mapc #'funcall stops)
      ;; Per-source preview `:exit' + `:return' broadcast.  The winning
      ;; source's cell receives the RAW CAND (`result-cand', not the
      ;; action's return value `result' — they can differ, see comment
      ;; on `result-cand' in the outer let*); every other cell receives
      ;; nil (interpreted as "aborted from this source's perspective").
      ;; Mirrors `fzfa--multi-build-router' broadcast semantics.
      (when any-preview
        (dotimes (i n-sources)
          (when-let* ((cell (aref preview-cells i)))
            (let ((fzfa--preview-session cell))
              (fzfa--preview-call :exit)
              (fzfa--preview-return (if (eql i result-src-idx)
                                        result-cand
                                      nil))))))
      (fzfa-helm--cancel-stranded-follow-timer))
    result))

(provide 'fzfa-helm)
;;; fzfa-helm.el ends here
