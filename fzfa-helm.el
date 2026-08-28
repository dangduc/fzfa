;;; fzfa-helm.el --- Helm frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helm frontend for `fzfa'.  Loaded automatically when `helm' is in
;; `fzfa-extensions' and `fzfa-setup' has been called.  Once loaded,
;; the two internal entry points (`fzfa-helm--completing-read' and
;; `fzfa-helm--read') are picked up by `fzfa.el's dispatch
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
  "Per-source candidate cap inside `fzfa-helm--read'.

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

(defcustom fzfa-helm-want-follow nil
  "Whether helm should auto-fire the persistent action on selection move.

Covers BOTH `:apply' lambdas (side-effecting commands like buffer
kill or compile re-run) AND regular `:persistent-action' previews
\(file content peek, etc.).  When nil (the default), helm only
fires the persistent action when the user presses `C-j' or
`fzfa-preview-key' — matching fzfa's preview-on-demand paradigm
across frontends (vertico/icomplete also wait for the key).

When non-nil, helm sources add `:follow 1', which fires the
persistent action on every arrow-key press.  That's convenient
for live preview but a footgun for `:apply' lambdas with side
effects, hence the conservative default."
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
`fzfa.el' (`fzfa-helm--completing-read', `fzfa-helm--read')
become `fboundp' once the file is loaded and pick up
`helm-mode' automatically.")

(defun fzfa-helm--wrap-apply (apply-fn dir origin-window origin-buffer)
  "Wrap APPLY-FN with a stable origin baseline restored on each fire.

DIR / ORIGIN-WINDOW / ORIGIN-BUFFER are captured at session-start.

Helm fires `:persistent-action' in whatever buffer is currently shown
in its persistent-action-display-window.  When the apply is something
like `find-file', the first fire mutates that window (opens Dired for
a directory candidate, opens an unrelated buffer for a file
candidate, etc.).  Every subsequent fire then inherits the mutated
buffer's `default-directory', and `expand-file-name' on a relative
candidate compounds the corruption.  Under `:follow 1' this cascades
into completely bogus paths within one or two scroll steps
\(e.g. nested `user-lisp-30/user-lisp-30/' duplicated path segments).

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

(defvar fzfa-helm--suppressing-snap nil
  "Dynamically bound to t while fzfa's own snap-to-top runs.

`helm-beginning-of-buffer' fires `helm-move-selection-after-hook' —
the same hook fzfa uses to detect user navigation.  Binding this flag
around our snap lets the marker distinguish our own move from a real
user command.")

(defvar fzfa-helm--user-nav-commands
  '(helm-next-line
    helm-previous-line
    helm-next-page
    helm-previous-page
    helm-next-source
    helm-previous-source
    helm-beginning-of-buffer
    helm-end-of-buffer)
  "Commands that count as user navigation for snap-until-user-moves.

`helm-move-selection-after-hook' fires from many contexts — helm's own
`helm--update-move-first-line' during every update, the initial dispatch
of the entry command (e.g. `fzfa-find-any'), etc.  Marking `user-moved'
only when `this-command' matches one of these explicit navigation
commands avoids treating those internal fires as user intent.")

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

(defun fzfa-helm--cleanup-call (label function &rest args)
  "Call FUNCTION with ARGS during teardown and log an error under LABEL.

Cleanup is best-effort: one failing hook, preview handler, or native stop must
not prevent the remaining session resources from being released."
  ;; `fzfa--log' compiles away unless debugging was enabled at load time.
  (ignore label)
  (condition-case err
      (apply function args)
    ((error quit)
     (ignore err)
     (ignore-errors
       (fzfa--log "Helm cleanup %s failed: %s"
                  label (error-message-string err)))
     nil)))

(defvar helm-alive-p)
(defvar helm-pattern)
(defvar helm-completion-style)
(defvar helm-map)
(defvar helm-source-filter)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")
(declare-function helm-goto-source "helm-core")
(declare-function helm-beginning-of-buffer "helm-core" ())
(declare-function helm-empty-buffer-p "helm-core" (&optional buffer))
(declare-function helm-window "helm-lib" ())
(declare-function helm-mark-current-line "helm-core"
                  (&optional resumep nomouse))
(declare-function helm-get-selection "helm-core"
                  (&optional buffer force-display-part source))
(defvar helm-pattern)
(defvar helm-buffer)
(declare-function helm-set-source-filter "helm-core")
(declare-function helm-get-selection "helm-core")
(declare-function helm-execute-persistent-action "helm-core")
(declare-function fzf-native-async-start "ext:fzf-native-module")
(declare-function fzf-native-async-stop "ext:fzf-native-module")
(declare-function fzf-native-async-generation "ext:fzf-native-module")
(declare-function fzf-native-async-candidates "ext:fzf-native-module")
(declare-function fzf-native-async-stats "ext:fzf-native-module")
(declare-function fzf-native-async-status "ext:fzf-native-module")
(declare-function fzf-native-score-all "ext:fzf-native-module")

(defun fzfa-helm--current-session-buffer ()
  "Return the buffer owned by the currently active Helm session."
  (when (bound-and-true-p helm-alive-p)
    ;; `helm-buffer-get' deliberately returns Helm's action buffer while the
    ;; action pane is visible.  That pane is part of the same session and must
    ;; not revoke an async source's publication rights.  The dynamically bound
    ;; `helm-buffer' is the stable identity of the invocation itself.
    (let ((buffer (and (boundp 'helm-buffer) helm-buffer)))
      (if (bufferp buffer)
          buffer
        (and (stringp buffer) (get-buffer buffer))))))

(defun fzfa-helm--active-minibuffer-buffer ()
  "Return the buffer of the innermost active minibuffer, or nil."
  (let ((window (active-minibuffer-window)))
    (and (window-live-p window) (window-buffer window))))

(defun fzfa-helm--current-session-token ()
  "Return the identity token of the active Helm invocation.

The result buffer alone is not unique: Helm's supported nested mode can reuse
  the same `helm-buffer'.  A recursive minibuffer has a distinct buffer, so pair
the stable result buffer with the active minibuffer buffer."
  (when-let* ((result-buffer (fzfa-helm--current-session-buffer)))
    (cons result-buffer (fzfa-helm--active-minibuffer-buffer))))

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
  (if-let* ((handle)
            (counts
             (if (fzfa--session-api-p)
                 (when-let* ((status (fzf-native-async-status handle)))
                   (cons (or (plist-get status :filtered) 0)
                         (fzfa--async-collected-total status)))
               (fzf-native-async-stats handle))))
      (format " (%s/%s)" (fzfa--commas (car counts))
              (fzfa--commas (cdr counts)))
    ""))

(defun fzfa-helm--sync-stats-suffix (filtered total)
  "Return ` (FILTERED/TOTAL)' suffix string from sync-source counts.

Returns empty string when either count is nil (initial state — `:candidates'
hasn't run yet).  Numbers comma-formatted via `fzfa--commas'."
  (if (and filtered total)
      (format " (%s/%s)" (fzfa--commas filtered) (fzfa--commas total))
    ""))

;;; Live preview wrapper

(defun fzfa-helm--make-debounced-preview-fn (&optional session-cell owner-p)
  "Return a fresh `:persistent-action' closure that debounces preview dispatch.

`helm' fires `:persistent-action' on every selection change when
`:follow 1' is set — no idle-timer debounce of its own.

This wrapper closes over a private `preview-timer' + `preview-last'
pair (per-closure, so each `helm' source in a multi gets its own
debounce state) and:

- Reuses any pending timer rather than cancel-and-reschedule.  The
  candidate passed in is ignored — the timer's callback re-reads the
  CURRENT selection via `helm-get-selection' at fire time, so reuse
  never previews a stale candidate.  Mirrors `fzfa--preview-install'
  (fzfa.el:870) and gives identical idle-respecting semantics across
  the two paths.
- Skips dispatch when the freshly-read candidate equals the
  previously-previewed one (no-op moves under `:follow 1').
- Bypasses the debounce when `fzfa-preview-delay' is 0 or less,
  firing immediately (matches the vertico path's escape hatch).

SESSION-CELL, when non-nil, is bound to `fzfa--preview-session'
inside the dispatch — required for the multi handler where each
source has its own session cell and the ambient
`fzfa--preview-session' may point at a different cell (or be nil)
when the idle timer fires.  When nil, dispatch uses the ambient
binding (sync/async paths bind it themselves at the handler
level for the whole helm session).

OWNER-P, when non-nil, must still return non-nil at dispatch time.  Each
invocation also captures the current Helm session buffer.  Immediate and
deferred dispatches require that exact session to remain current, so a nested
Helm cannot consume an outer session's preview timer or candidate."
  (let ((preview-timer nil)
        (preview-last 'unset))
    (lambda (_cand)
      (when-let* ((owner-token (fzfa-helm--current-session-token))
                  ((or (null owner-p) (funcall owner-p))))
        (cond
         ((<= (or fzfa-preview-delay 0) 0)
          ;; Immediate path — no debounce.  Re-read current selection
          ;; for parity with the idle path, but only from the session that
          ;; invoked this persistent action.
          (when-let* (((equal owner-token
                               (fzfa-helm--current-session-token)))
                      ((or (null owner-p) (funcall owner-p)))
                      (cur (helm-get-selection)))
            (unless (equal cur preview-last)
              (setq preview-last cur)
              (let ((fzfa--preview-session
                     (or session-cell fzfa--preview-session)))
                (fzfa--preview-call :preview nil cur)))))
         ((not (timerp preview-timer))
          (setq preview-timer
                (run-with-idle-timer
                 fzfa-preview-delay nil
                 (lambda ()
                   (setq preview-timer nil)
                   (when-let* (((equal owner-token
                                      (fzfa-helm--current-session-token)))
                               ((or (null owner-p) (funcall owner-p)))
                               (cur (helm-get-selection)))
                     (unless (equal cur preview-last)
                       (setq preview-last cur)
                       (let ((fzfa--preview-session
                              (or session-cell fzfa--preview-session)))
                         (fzfa--preview-call :preview nil cur)))))))))))))

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
`:follow' tracks `fzfa-helm-want-follow' — auto-fire on every
selection move (default) or on `helm-execute-persistent-action'.

Display cycling: `fzfa-display-key' (default `>') is bound in
the source's keymap and cycles `hidden' → `compact' → `full' →
`hidden' using the shared `fzfa--display-{materialize,extract}'
helpers.  Editing the CMD region in compact / full debounce-restarts
the subprocess with the new command.  Same UX as the `completing-read'
path.

Internal — used by both `fzfa-helm-make-async-source' (single-source)
and `fzfa-helm--read' (batch with bulk-stop)."
  (let* ((dir (expand-file-name (or directory default-directory)))
         (initial-char fzfa-separator)
         ;; Per-source state lives on the struct.  `command' slot is
         ;; what we want running; `current-cmd' is what the handle IS
         ;; running.  They differ between user editing and the
         ;; debounced restart firing.
         (source (fzfa-make-source :command command
                                   :directory dir
                                   :display 'hidden))
         (active t)
         (owner-token nil)
         (owner-p
          (lambda ()
            (and active
                 (buffer-live-p (car-safe owner-token))
                 (equal owner-token (fzfa-helm--current-session-token)))))
         (claim-owner
          (lambda ()
            (when-let* ((current (fzfa-helm--current-session-token)))
              (unless owner-token (setq owner-token current))
              (funcall owner-p))))
         (timer nil)
         (refresh-fn
          (lambda ()
            (when (funcall owner-p)
              (helm-force-update)
              (funcall owner-p))))
         (display-cycle
          (lambda ()
            (interactive)
            (fzfa-source--display-cycle source initial-char)
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
              (setq helm-pattern (minibuffer-contents)))))
         (stop
          (lambda ()
            ;; Revoke frontend use on the first call, but retry each physical
            ;; resource in two bounded passes.  A transient timer-cancel error
            ;; must not leave this otherwise unreachable timer running.
            (setq active nil)
            (cl-labels
                ((stop-pass
                  ()
                  (let ((timer-ok
                         (or (null timer)
                             (when (fzfa--cleanup-call
                                    "Helm poll timer" #'cancel-timer timer)
                               (setq timer nil)
                               t)))
                        ;; Evaluate independently even when timer cleanup
                        ;; failed; this is the process-owning resource.
                        (source-ok (fzfa-source--stop source)))
                    (and timer-ok source-ok))))
              (or (stop-pass) (stop-pass))))))
    ;; Pre-arm: start the initial handle and mark `current-cmd' so the
    ;; first `:candidates' tick doesn't trigger a debounced restart.
    (setf (fzfa-source-handle source)
          (fzfa--spawn command dir)
          (fzfa-source-current-cmd source) command)
    (setq timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when (funcall owner-p)
               (let* ((h (fzfa-source-handle source))
                      (gen (and h (fzfa--poll-generation h))))
                 (when (and gen (> gen (fzfa-source-last-gen source)))
                   (helm-force-update)
                   ;; Commit only after Helm published the corresponding
                   ;; candidates, and only while this source still owns the
                   ;; observed handle.  A failed update or an update that
                   ;; replaces the handle must leave the generation edge
                   ;; available to the next poll.
                   (when (and (funcall owner-p)
                              (eq h (fzfa-source-handle source)))
                     (setf (fzfa-source-last-gen source) gen))))))))
    (cons
     (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
            :header-name
            (lambda (n)
              (format "%s [%s]%s" n (abbreviate-file-name dir)
                      (and (fzfa-source-handle source)
                           (fzfa-helm--async-stats-suffix
                            (fzfa-source-handle source)))))
            :keymap (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map helm-map)
                      (when fzfa-preview-key
                        (define-key map (kbd fzfa-preview-key)
                                    #'helm-execute-persistent-action))
                      (when fzfa-display-key
                        (define-key map (kbd fzfa-display-key)
                                    display-cycle))
                      map)
            :candidates
            (lambda ()
              (when (and active (funcall claim-owner))
                (pcase-let* ((`(,cmd . ,filter)
                              (fzfa--split
                               (or helm-pattern "")
                               (fzfa-source-display-state source)
                               (fzfa-source-command source))))
                  (when (not (equal cmd (fzfa-source-current-cmd source)))
                    (fzfa-source--debounce-restart source cmd refresh-fn))
                  (when (fzfa-source-handle source)
                    (let ((r (while-no-input
                               (fzfa--source-async-candidates
                                source filter limit))))
                      (cond
                       ((eq r t)
                        ;; Preserve the last completed list while native
                        ;; scoring catches up.  If input interrupted a final
                        ;; materialization after the generation timer fired,
                        ;; this retry guarantees another render.
                        (when-let* ((tm (fzfa-source-retry-timer source)))
                          (cancel-timer tm))
                        (setf (fzfa-source-retry-timer source)
                              (run-with-idle-timer
                               fzfa-input-debounce nil
                               (lambda ()
                                 (setf (fzfa-source-retry-timer source) nil)
                                 (funcall refresh-fn))))
                        (fzfa-source-last-result source))
                       ((eq (car-safe r) 'failed)
                        ;; Matcher failure is terminal for this request.
                        ;; Keep the last completed list and do not arm the
                        ;; pending-work retry timer; the core adapter already
                        ;; reported the failure once.
                        (when-let* ((tm (fzfa-source-retry-timer source)))
                          (cancel-timer tm)
                          (setf (fzfa-source-retry-timer source) nil))
                        (fzfa-source-last-result source))
                       (t
                        (when-let* ((tm (fzfa-source-retry-timer source)))
                          (cancel-timer tm)
                          (setf (fzfa-source-retry-timer source) nil))
                        (setf (fzfa-source-last-result source) r
                              (fzfa-source-last-query source) filter)
                        r)))))))
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
                       (when fzfa-helm-want-follow '(:follow 1))))
              (persistent-action
               (append (list :persistent-action persistent-action)
                       (when fzfa-helm-want-follow '(:follow 1)))))))
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
`:follow' tracks `fzfa-helm-want-follow' when APPLY is in use, else
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

Producer functions are classified by arity without firing them during
source construction.  Both synchronously and asynchronously firing
producers use the shared fetch protocol; async delivery triggers
`helm-force-update' so helm re-reads the fresh snapshot."
  (fzfa-helm--ensure-loaded)
  (let* ((limit (or candidate-number-limit 10000))
         (kind (fzfa--candidates-kind items))
         (producer-kind-p (eq kind 'producer))
         (initial-char fzfa-separator)
         ;; Per-source state (display-state machinery, producer state,
         ;; result/score, retry-timer) on the struct.  Static kinds
         ;; (list / zero) don't use the display-state or producer
         ;; slots but the source still bundles the per-source state
         ;; uniformly.
         ;; Populate `cands-fn' for producer kinds (sync / async) so
         ;; the shared `fzfa--source-fetch' helper finds the producer
         ;; on the struct rather than via lexical capture.  Static
         ;; kinds (`list' / `zero') keep nil — they short-circuit
         ;; before any fetch dispatch.
         (source (fzfa-make-source :directory default-directory
                                   :display (or display 'hidden)
                                   :candidates (and producer-kind-p
                                                    items)))
         (active t)
         (owner-token nil)
         (owner-p
          (lambda ()
            (and active
                 (buffer-live-p (car-safe owner-token))
                 (equal owner-token (fzfa-helm--current-session-token)))))
         (claim-owner
          (lambda ()
            (when-let* ((current (fzfa-helm--current-session-token)))
              (unless owner-token (setq owner-token current))
              (funcall owner-p))))
         (display-cycle
          (lambda ()
            (interactive)
            (when (funcall owner-p)
              (fzfa-source--display-cycle source initial-char)
              (setq helm-pattern (minibuffer-contents)))))
         (stop
          (lambda ()
            (setq active nil)
            (fzfa-source--stop source))))
    (apply #'helm-make-source (or name "fzfa") 'helm-source-sync
           :header-name
           (lambda (n)
             (format "%s%s" n (fzfa-helm--sync-stats-suffix
                               (fzfa-source-filtered source)
                               (fzfa-source-total source))))
           :keymap (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map helm-map)
                     (when fzfa-preview-key
                       (define-key map (kbd fzfa-preview-key)
                                   #'helm-execute-persistent-action))
                     (when (and producer-kind-p fzfa-display-key)
                       (define-key map (kbd fzfa-display-key)
                                   display-cycle))
                     map)
           :candidates
           (lambda ()
             (when (and active (funcall claim-owner))
               (let* ((pat (or helm-pattern ""))
                    ;; For producer kinds, split CMD from FILTER and
                    ;; route CMD to the producer; for static kinds the
                    ;; whole pattern is the FILTER.
                    (split (and producer-kind-p
                                (fzfa--split pat
                                             (fzfa-source-display-state source)
                                             (fzfa-source-command source))))
                    (cmd (and split (car split)))
                    (filter (if split (cdr split) pat))
                    (all
                     (cl-case kind
                       (list items)
                       (zero (funcall items))
                       (producer
                        ;; Producer protocol + async-refresh dispatch
                        ;; live in the shared `fzfa--source-fetch'
                        ;; helper; the closure adapts its REFRESH-FN
                        ;; contract to helm's `helm-force-update'
                        ;; gated on `helm-alive-p'.
                        (fzfa--source-fetch
                         source cmd
                         (lambda ()
                           (when (funcall owner-p)
                             (helm-force-update)
                             (funcall owner-p))))
                        (fzfa-source-snapshot source))))
                    (r (while-no-input
                         (if (string-empty-p filter)
                             (if history (fzfa--history-rank all history) all)
                           (let ((fzfa-batch-highlight nil))
                             (fzfa--bridge-defcustoms
                              #'fzf-native-score-all all filter))))))
               (cond
                ((eq r t)
                 (when-let* ((tm (fzfa-source-retry-timer source)))
                   (cancel-timer tm))
                 (setf (fzfa-source-retry-timer source)
                       (run-with-idle-timer
                        fzfa-input-debounce nil
                        (lambda ()
                          (setf (fzfa-source-retry-timer source) nil)
                          (when (funcall owner-p)
                            (helm-force-update)
                            (funcall owner-p)))))
                 ;; Return cached candidates only when the filter still
                 ;; matches the query that produced them — otherwise nil so
                 ;; helm doesn't render stale results from a prior query
                 ;; under the new (often empty) header.
                 (if (equal filter (fzfa-source-last-query source))
                     (fzfa-source-last-result source)
                   nil))
                (t
                 (when-let* ((tm (fzfa-source-retry-timer source)))
                   (cancel-timer tm)
                   (setf (fzfa-source-retry-timer source) nil))
                 (setq r (fzfa--rank-and-highlight r filter history))
                 (setf (fzfa-source-total source) (length all)
                       (fzfa-source-filtered source) (length r)
                       (fzfa-source-last-result source) r
                       (fzfa-source-last-query source) filter)
                 r)))))
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
                      (when fzfa-helm-want-follow '(:follow 1))))
             (persistent-action
              (append (list :persistent-action persistent-action)
                      (when fzfa-helm-want-follow '(:follow 1)))))))))

;;; Composition helper — fzfa command -> helm source(s)

(defun fzfa-helm--source-from-plist (plist)
  "Build a helm source from a fzfa source PLIST.

PLIST has the shape produced by fzfa's `:extract' mode (and, for
multi-source commands, by `fzfa-multi-read''s inner per-source
plists).  Dispatches `:command' to `fzfa-helm-make-async-source'
and `:candidates' to `fzfa-helm-make-sync-source'.

The plist's `:action' (typically the `:inject' lambda `fzfa-multi-read'
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
polling timer (no shared timer like `fzfa-helm--read' uses).
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

Shared scaffolding: PROMPT, COMMAND, CANDIDATES, CATEGORY, PREVIEW,
APPLY, HISTORY, DEFAULT, DIRECTORY, DISPLAY, SKIP-EXECUTABLE-CHECK
behave as in `fzfa-completing-read'.  ANNOTATE, AFFIX, GROUP,
REQUIRE-MATCH are accepted for signature parity but unused — helm
sources don't consume completion-read metadata.

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
  ;; Build a 1-source plist and dispatch through
  ;; `fzfa-helm--read'.  N=1 fast paths inside the multi
  ;; entry point (pre-set `narrowed-name', skip the `<' menu,
  ;; single-source buffer name/prompt/default) restore the legacy
  ;; UX while sharing the multi-source plumbing.
  (let ((prompt (or prompt
                    (when command
                      (concat (car (split-string command nil t)) ": "))
                    (when candidates "fzf > "))))
    (fzfa-helm--read
     (list (list :name "fzfa"
                 :prompt prompt
                 :command command
                 :candidates candidates
                 :directory directory
                 :category category
                 :annotate annotate
                 :affix affix
                 :group group
                 :history history
                 :require-match require-match
                 :default default
                 :display display
                 :preview preview
                 :apply apply
                 :action #'identity))
     :prompt prompt)))

;;; Multi handler — dispatched from `fzfa--read'

(cl-defun fzfa-helm--read (sources &key prompt narrow-idx)
  "Helm dispatch for `fzfa--read'.

SOURCES is the same list of plists as the `completing-read' path.
PROMPT is the prompt string shown in the helm session.
NARROW-IDX, when non-nil, seeds the active narrow at session
start to that source index — used by `fzfa-replay' to replay a
session that exited narrowed.  Nil restores the default (N=1
always narrows to 0, N>1 starts widened).
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
  (cl-assert (> (length sources) 0) nil
             "fzfa-helm--read: SOURCES must contain at least one source")
  (fzfa-helm--ensure-loaded)
  (let* ((helm-completion-style 'emacs)
         (n-sources (length sources))
         (multi-p (> n-sources 1))            ; gates narrow menu + tofu
         ;; Per-source render cap.  `min' of the multi cap and the
         ;; single cap — multi cap dominates with defaults (200 < 2000),
         ;; but the `min' guard means a user lowering
         ;; `fzfa-helm-candidate-limit' below the multi cap still wins.
         (limit (if multi-p
                    (min fzfa-helm-candidate-limit
                         fzfa-helm-multi-source-candidate-limit)
                  ;; N=1: full single-source cap.
                  fzfa-helm-candidate-limit))
         (result nil)
         ;; At N=1, source 0's plist holds the session-level keys the
         ;; legacy `fzfa-helm--completing-read' consumed.
         (s0 (car sources))
         ;; Per-source runtime state — handle, snapshot, prod-token,
         ;; prod-input, last-result, rank, total, filtered, last-gen,
         ;; retry-timer — lives on each struct.  Populated inside the
         ;; `cl-loop' body via `fzfa-make-source' + `aset'.  Leader
         ;; detection reads `fzfa-source-rank' across this vector.
         (sources-v    (make-vector n-sources nil))
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
         ;; Mark a cell before its :setup call.  On exceptional unwind this
         ;; lets cleanup close a setup handler that failed after partial work,
         ;; without dispatching :exit to cells that were never entered.
         (preview-active (make-vector n-sources nil))
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
         ;; Flipped to t the first time any source's `:candidates' closure
         ;; returns a non-empty result.  The poll-timer gate skips
         ;; `fzfa-input-throttle' while this is nil, so the empty
         ;; helm-buffer doesn't sit through two throttle windows waiting
         ;; for the producer + scoring round-trip on the cold session.
         (first-cands-shown nil)
         (session-active t)
         (helm-owner-token nil)
         (helm-owner-p
          (lambda ()
            (and session-active
                 (buffer-live-p (car-safe helm-owner-token))
                 (equal helm-owner-token
                        (fzfa-helm--current-session-token)))))
         (helm-claim-owner
          (lambda ()
            (when-let* ((current (fzfa-helm--current-session-token)))
              (unless helm-owner-token (setq helm-owner-token current))
              (funcall helm-owner-p))))
         (helm-refresh
          (lambda ()
            (when (funcall helm-owner-p)
              (helm-force-update)
              (funcall helm-owner-p))))
         ;; Per-source state collected during source construction.
         (handles nil)   ; reversed: list of fzf-native handles (async only)
         (stops nil)     ; reversed: one 0-arg stop closure per source
         poll-timer
         helm-entered
         ;; Most recent user filter, updated by `update-last-query'
         ;; (below) on each `helm-after-update-hook' tick.  Read by
         ;; the replay-snapshot block in the unwind-protect cleanup
         ;; to persist the filter as the narrow target's
         ;; `:initial-input' for the next replay.
         (last-query "")
         ;; Hoisted out of the inner `let*' so they're visible to the
         ;; unwind-protect cleanup (where we `remove-hook' them, and
         ;; where `narrowed-name' is read by the snapshot block to
         ;; derive the session's `:narrow-idx').  Closures bound in
         ;; the inner let* don't survive its end — cleanup runs
         ;; after the inner scope closes.
         update-last-query
         restore-narrow
         narrowed-name
         ;; User-facing entry command — captured at fzfa-helm--read
         ;; entry, stable across the helm session, read by
         ;; `fzfa--sessions-push' for the picker display + dedup key.
         (entry-command this-command)
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
                     ;; Property recovery: the canonical candidate
                     ;; lives on the source's snapshot with all
                     ;; in-band metadata the caller attached.  Helm
                     ;; usually preserves text properties through its
                     ;; buffer-based selection, but recovering via
                     ;; `member' (content equality) keeps the helm
                     ;; and vertico/ivy paths frontend-agnostic and
                     ;; resilient to any frontend that strips props.
                     ;; No-op for shell sources (snapshot is nil).
                     (let* ((snap (fzfa-source-snapshot
                                   (aref sources-v i)))
                            (orig (and snap (car (member cand snap)))))
                       (when orig (setq cand orig)))
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
                      (source (fzfa-make-source :command cmd
                                                :directory dir
                                                :display 'hidden))
                      (active t)
                      (refresh-fn helm-refresh)
                      (stop
                       (lambda ()
                         (setq active nil)
                         (fzfa-source--stop source))))
                 (setf ;; Seed `current-cmd' so the first `:candidates'
                       ;; tick's CMD-change check sees the source as
                       ;; already running its initial cmd — mirrors
                       ;; the single-source helm and multi-source
                       ;; completing-read paths.
                       (fzfa-source-current-cmd source) cmd)
                 (aset sources-v i source)
                 (push stop stops)
                 (apply #'helm-make-source name 'helm-source-sync
                        :keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map helm-map)
                                  (when fzfa-preview-key
                                    (define-key map (kbd fzfa-preview-key)
                                                #'helm-execute-persistent-action))
                                  map)
                        :header-name
                        (lambda (n)
                          (format "%s [%s]%s" n (abbreviate-file-name dir)
                                  (fzfa-helm--async-stats-suffix
                                   (fzfa-source-handle source))))
                        :candidates
                        (lambda ()
                          (when (and active (funcall helm-claim-owner))
                            (pcase-let* ((`(,split-cmd . ,filter)
                                          (fzfa--split
                                           (or helm-pattern "")
                                           (fzfa-source-display-state source)
                                           (fzfa-source-command source))))
                              ;; CMD edited in compact / full →
                              ;; debounce-restart the shell handle.
                              (when (not (equal split-cmd
                                                (fzfa-source-current-cmd
                                                 source)))
                                (fzfa-source--debounce-restart
                                 source split-cmd refresh-fn))
                              (let ((r (while-no-input
                                         (fzfa--source-async-candidates
                                          source filter limit))))
                                (cond
                                 ((eq r t)
                                  (when-let* ((tm (fzfa-source-retry-timer
                                                   source)))
                                    (cancel-timer tm))
                                  (setf (fzfa-source-retry-timer source)
                                        (run-with-idle-timer
                                         fzfa-input-debounce nil
                                         (lambda ()
                                           (setf (fzfa-source-retry-timer
                                                  source) nil)
                                           (funcall helm-refresh))))
                                  ;; Don't update rank — cached
                                  ;; `last-result' is for an earlier query.
                                  (fzfa-source-last-result source))
                                 ((eq (car-safe r) 'failed)
                                  ;; Terminal matcher failure: retain the
                                  ;; last completed candidates and rank, and
                                  ;; do not schedule another pending retry.
                                  (when-let* ((tm (fzfa-source-retry-timer
                                                   source)))
                                    (cancel-timer tm)
                                    (setf (fzfa-source-retry-timer source)
                                          nil))
                                  (fzfa-source-last-result source))
                                 (t
                                  (when (and r (not first-cands-shown))
                                    (setq first-cands-shown t))
                                  (when-let* ((tm (fzfa-source-retry-timer
                                                   source)))
                                    (cancel-timer tm)
                                    (setf (fzfa-source-retry-timer source)
                                          nil))
                                  (setf (fzfa-source-last-result source) r
                                        (fzfa-source-rank source)
                                        (fzfa--multi-rank r filter t))
                                  r))))))
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
                           (append
                            (list :persistent-action
                                  (fzfa-helm--make-debounced-preview-fn
                                   preview-cell helm-owner-p))
                            (when fzfa-helm-want-follow
                              '(:follow 1))))))))
              (cands
               ;; Sync source inlined here (rather than via
               ;; `fzfa-helm-make-sync-source') so its `:candidates'
               ;; can update the multi handler's per-source rank slot.
               ;; Same `while-no-input' + `last-result' cache +
               ;; `retry-timer' pattern as the async branch above.
               ;;
               ;; Producer kind is detected once from its arity.  Do not fire
               ;; a producer during source construction: it can perform I/O
               ;; or have other visible side effects.  Both synchronously and
               ;; asynchronously firing producers use the shared fetch
               ;; protocol and its token-based callback ownership.
               (let* ((kind (fzfa--candidates-kind cands))
                      (source (fzfa-make-source :directory directory
                                                ;; Populate `cands-fn'
                                                ;; on the struct so the
                                                ;; async branch's
                                                ;; `fzfa--source-fetch'
                                                ;; finds the producer.
                                                ;; `--normalize-candidates'
                                                ;; handles all shapes.
                                                :candidates
                                                (and (eq kind 'producer)
                                                     cands)))
                      (sync-stop
                       (lambda () (fzfa-source--stop source))))
                 (aset sources-v i source)
                 (push sync-stop stops)
                 (apply #'helm-make-source name 'helm-source-sync
                        :keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map helm-map)
                                  (when fzfa-preview-key
                                    (define-key map (kbd fzfa-preview-key)
                                                #'helm-execute-persistent-action))
                                  map)
                        :header-name
                        (lambda (n)
                          (format "%s%s" n (fzfa-helm--sync-stats-suffix
                                            (fzfa-source-filtered source)
                                            (fzfa-source-total source))))
                        :candidates
                        (lambda ()
                          (when (funcall helm-claim-owner)
                            (let* ((pat (or helm-pattern ""))
                                 ;; #CMD#FILTER split — producer
                                 ;; kinds route CMD to INPUT and
                                 ;; FILTER to fzf scoring.  Static
                                 ;; kinds have no CMD; whole pattern
                                 ;; is the FILTER.  Use the
                                 ;; display-state-aware `fzfa--split'
                                 ;; \(not the raw `fzfa--split-input')
                                 ;; so non-shell producer sources in
                                 ;; hidden mode (e.g.
                                 ;; `fzfa-replay--file-producer') route
                                 ;; the whole pattern through FILTER
                                 ;; instead of treating it as CMD and
                                 ;; leaving FILTER empty.
                                 (split (and (eq kind 'producer)
                                             (fzfa--split
                                              pat
                                              (fzfa-source-display-state source)
                                              (fzfa-source-command source))))
                                 (cmd (and split (car split)))
                                 (filter (if split (cdr split) pat))
                                 (all
                                  (cl-case kind
                                    (list cands)
                                    (zero (funcall cands))
                                    (producer
                                     ;; Producer protocol + async-refresh
                                     ;; dispatch live in the shared
                                     ;; `fzfa--source-fetch' helper; the
                                     ;; closure adapts its REFRESH-FN
                                     ;; contract to helm's
                                     ;; `helm-force-update' gated on
                                     ;; `helm-alive-p'.  Meanwhile we
                                     ;; return the current snapshot
                                     ;; (possibly stale for one tick).
                                     (fzfa--source-fetch
                                      source cmd
                                      helm-refresh)
                                     (fzfa-source-snapshot source))))
                                 (r (while-no-input
                                      (if (string-empty-p filter)
                                          (if history
                                              (fzfa--history-rank all history)
                                            all)
                                        (let ((fzfa-batch-highlight nil))
                                          (fzfa--bridge-defcustoms
                                           #'fzf-native-score-all all filter))))))
                            (cond
                             ((eq r t)
                              (when-let* ((tm (fzfa-source-retry-timer source)))
                                (cancel-timer tm))
                              (setf (fzfa-source-retry-timer source)
                                    (run-with-idle-timer
                                     fzfa-input-debounce nil
                                     (lambda ()
                                       (setf (fzfa-source-retry-timer source) nil)
                                       (funcall helm-refresh))))
                              ;; Return cached candidates only when the
                              ;; filter still matches the query that
                              ;; produced them — otherwise nil so helm
                              ;; doesn't render stale results from a prior
                              ;; query under the new (often empty) header.
                              (if (equal filter
                                         (fzfa-source-last-query source))
                                  (fzfa-source-last-result source)
                                nil))
                             (t
                              (when-let* ((tm (fzfa-source-retry-timer source)))
                                (cancel-timer tm)
                                (setf (fzfa-source-retry-timer source) nil))
                              (setq r (fzfa--rank-and-highlight
                                       r filter history))
                              (when (and r (not first-cands-shown))
                                (setq first-cands-shown t))
                              (setf (fzfa-source-total source) (length all)
                                    (fzfa-source-filtered source) (length r)
                                    (fzfa-source-last-result source) r
                                    (fzfa-source-last-query source) filter
                                    (fzfa-source-rank source)
                                    (fzfa--multi-rank r filter nil))
                              r)))))
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
                           (append
                            (list :persistent-action
                                  (fzfa-helm--make-debounced-preview-fn
                                   preview-cell helm-owner-p))
                            (when fzfa-helm-want-follow
                              '(:follow 1))))))))
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
            (when (and (funcall helm-owner-p)
                       (not (string-empty-p (or helm-pattern ""))))
              (let ((best-i nil)
                    (best-r 0))
                (dotimes (i n-sources)
                  (when-let* ((src (aref sources-v i))
                              (r (fzfa-source-rank src))
                              ((> r best-r)))
                    (setq best-r r
                          best-i i)))
                (when (and best-i (not (eql best-i last-leader)))
                  (setq last-leader best-i)
                  (helm-goto-source (aref source-names best-i)))))))
         ;; Snap-to-top-until-user-moves.  Fires on `helm-after-update-hook'
         ;; so it covers both the `:command' path (poll-timer refreshes)
         ;; and the `:candidates' path (async callback → helm-force-update).
         (user-moved nil)
         (move-marker
          (lambda ()
            ;; Mark user-moved ONLY when this-command is a known helm
            ;; navigation command.  Excludes:
            ;;   - our own snap (suppress=t),
            ;;   - helm's internal `helm--update-move-first-line' (this-cmd=nil),
            ;;   - the entry command that invoked fzfa (e.g. `fzfa-find-any'),
            ;;   - typing / self-insert-command (changes pattern, not selection),
            ;;   - unknown commands (safer to keep snapping than stop early).
            (when (and (funcall helm-owner-p)
                       (not fzfa-helm--suppressing-snap)
                       (memq this-command fzfa-helm--user-nav-commands))
              (setq user-moved t))))
         (snap-fn
          (lambda ()
            (when (and (funcall helm-owner-p) (not user-moved))
              ;; `helm-after-update-hook' fires INSIDE `helm-update' — but
              ;; `helm-force-update' runs its own `(recenter nil)' AFTER
              ;; `helm-update' returns (helm-core.el:5401), which recenters
              ;; the cursor mid-window and undoes any snap we do here.
              ;; Defer the snap via `run-at-time 0' so it fires on the next
              ;; event-loop tick, after helm's recenter has already run.
              ;;
              ;; Empty pattern: buffer top (first candidate of first source).
              ;; Non-empty pattern in multi-source: leader (highest fzf-scored
              ;; source), matching `jump-fn' — otherwise our snap would drag
              ;; the cursor off the leader back to source 0.
              (run-at-time
               0 nil
               (lambda ()
                 (when-let* (((bound-and-true-p helm-alive-p))
                             ((funcall helm-owner-p))
                             ((not user-moved))
                             (win (helm-window))
                             ((window-live-p win))
                             ((not (helm-empty-buffer-p))))
                   (let ((fzfa-helm--suppressing-snap t))
                     (with-selected-window win
                       (let* ((pat (bound-and-true-p helm-pattern))
                              (multi-nonempty (and multi-p pat
                                                   (not (string-empty-p pat))))
                              (leader
                               (when multi-nonempty
                                 (let ((best-i nil) (best-r 0))
                                   (dotimes (i n-sources)
                                     (when-let* ((src (aref sources-v i))
                                                 (r (fzfa-source-rank src))
                                                 ((> r best-r)))
                                       (setq best-r r best-i i)))
                                   best-i))))
                         (if leader
                             (progn
                               (helm-goto-source (aref source-names leader))
                               ;; `helm-goto-source' lands on the source
                               ;; HEADER (helm-core.el:6367).  The
                               ;; skip-noncandidate logic in
                               ;; `helm-move-selection-common-1' only
                               ;; runs when direction is `next' /
                               ;; `previous', not when direction is a
                               ;; source name.  Advance past the header
                               ;; ourselves and re-mark the candidate
                               ;; line — this mirrors how
                               ;; `helm-preselect' handles source jumps
                               ;; (helm-core.el:6791-6792).
                               (forward-line 1)
                               (helm-mark-current-line))
                           (helm-beginning-of-buffer))
                         ;; `recenter 1' leaves row 0 for the source
                         ;; header and puts the current line (first
                         ;; candidate) on row 1 — otherwise the header
                         ;; gets pushed off-screen and the user has to
                         ;; scroll up to see which source they're on.
                         (recenter 1)))))))))))
    (unwind-protect
        (progn
          ;; Source construction above is deliberately inert.  Start every
          ;; shell handle only after entering the protected region so a later
          ;; spawn, timer, preview setup, or Helm failure cannot leak an
          ;; earlier source.
          (dotimes (i n-sources)
            (let* ((source (aref sources-v i))
                   (cmd (plist-get (nth i sources) :command)))
              (when cmd
                (setf (fzfa-source-handle source)
                      (fzfa--spawn cmd (fzfa-source-directory source)))
                (push (fzfa-source-handle source) handles))))
          ;; Single shared polling timer over all async handles.  Throttled to
          ;; one `helm-force-update' per `fzfa-input-throttle' to amortize the
          ;; cost of recomputing every source's `:candidates'.  Also skipped
          ;; when input is pending — typing always trumps streamed-candidate
          ;; refreshes.
          (when handles
            (setq poll-timer
                  (run-with-timer
                   0 fzfa-refresh-delay
                   (fzfa--make-poll-fn
                    sources-v
                    helm-owner-p
                    helm-refresh
                    (lambda () first-cands-shown)
                    (lambda (work) (funcall work))))))
          ;; Per-source preview `:setup' broadcast.  Each cell captures the
          ;; ORIGIN window/buffer/`default-directory' (the user's selected
          ;; window before helm activated), then dispatches `:setup' under its
          ;; own session binding so per-source state stashed via
          ;; `fzfa-preview-put' lands in this cell's cdr.
          (when any-preview
            (dotimes (i n-sources)
              (when-let* ((cell (aref preview-cells i)))
                (aset preview-active i 'starting)
                (let ((fzfa--preview-session cell)
                      ;; Prefer the source's own `:directory' — that is the
                      ;; root the search command ran under and the base against
                      ;; which its candidate strings are expanded.
                      (src-dir (or (plist-get (nth i sources) :directory)
                                   default-directory)))
                  (fzfa-preview-put :origin-window (selected-window))
                  (fzfa-preview-put :origin-buffer
                                    (window-buffer (selected-window)))
                  (fzfa-preview-put :default-directory src-dir)
                  (fzfa--preview-call :setup nil)
                  (aset preview-active i 'active)))))
          (let* (;; Narrow-by-source: press `fzfa-multi-narrow-key',
               ;; then the source's `:narrow' key, to filter helm to
               ;; that source only.  Press the prefix again to widen.
               ;; Routes via `helm-set-source-filter' — helm's
               ;; built-in mechanism for showing a subset of
               ;; `helm-sources' without rebuilding them.  Each source
               ;; plist already carries its allocated `:narrow' key
               ;; (assigned by `fzfa--multi-allocate-narrow-keys' in
               ;; `fzfa-multi-read' before the dispatch).
               ;;
               ;; `helm-source-filter' is helm-buffer-local (helm sets
               ;; it via `with-helm-buffer'), so we can't read it from
               ;; the minibuffer where `narrow-display-cycle' fires.
               ;; Track the narrowed source name in our own closure
               ;; var; narrow-fn keeps it in sync with the helm-side
               ;; filter.  At N=1 we pre-set it to the lone source's
               ;; name so `>' cycles immediately (no `<' prefix needed
               ;; — there's no other source to switch to).  A
               ;; caller-supplied `:narrow-idx' (`fzfa-replay') wins
               ;; over both — it restores a saved narrow target.
               ;; `narrowed-name' is hoisted to the outer let* so the
               ;; replay-snapshot cleanup can read its final value;
               ;; this is the in-session seed (which `narrow-fn'
               ;; mutates) on top of that outer binding.
               (_narrow-init
                (setq narrowed-name
                      (cond
                       (narrow-idx (aref source-names narrow-idx))
                       ((not multi-p) (or (plist-get s0 :name) "fzfa")))))
               ;; Force any source that's about to leave the narrow
               ;; window back to `hidden' so its `#cmd#filter' buffer
               ;; shape doesn't leak into the new view.  `before' and
               ;; `after' are `helm-source-filter' values (nil =
               ;; widened, or a list of source-name strings).  Sources
               ;; in `before' but not in `after' get extracted.
               (force-hidden-leaving
                (lambda (before after)
                  (dolist (lname (or before '()))
                    (unless (member lname (or after '()))
                      (when-let* ((idx (cl-position lname source-names
                                                    :test #'equal))
                                  (src (aref sources-v idx)))
                        (fzfa-source--display-force-hidden
                         src fzfa-separator))))
                  (when (and (or before after)
                             (not (equal before after))
                             helm-alive-p)
                    ;; Buffer was mutated by `force-hidden' → sync
                    ;; `helm-pattern' so the next `:candidates' tick
                    ;; sees the post-extract text.
                    (setq helm-pattern (minibuffer-contents)))))
               (narrow-fn
                (lambda ()
                  (interactive)
                  (when (funcall helm-owner-p)
                    (let* ((sources-v-local (vconcat sources))
                           ;; KEY:NAME pairs separated by two spaces, with
                           ;; the prefix-widen marker at the end, faced
                           ;; via `fzfa--format-narrow-hint' — same
                           ;; rendering the vertico narrow menu uses.
                           (hint (concat
                                  (fzfa--format-narrow-hint
                                   sources-v-local nil nil
                                   fzfa-multi-narrow-key)
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
                                               #'equal)))
                           (before (and narrowed-name (list narrowed-name))))
                      ;; `read-char' is an event-loop boundary.  A recursive
                      ;; Helm can take ownership while the menu waits.
                      (when (funcall helm-owner-p)
                        (cond
                         (target
                          (helm-set-source-filter (list target))
                          (when (funcall helm-owner-p)
                            (setq narrowed-name target)
                            (funcall force-hidden-leaving
                                     before (list target))))
                         ((equal key fzfa-multi-narrow-key)
                          (helm-set-source-filter nil)
                          (when (funcall helm-owner-p)
                            (setq narrowed-name nil)
                            (funcall force-hidden-leaving before nil)))
                         (t (message "fzfa: no source bound to narrow key %S"
                                     key))))))))
               ;; `>'-cycle handler — fires only when narrowed to a
               ;; single source.  Helm activates a source's `:keymap'
               ;; only while the cursor is on that source's candidate
               ;; row, so a per-source `:keymap' binding wouldn't fire
               ;; when the narrowed source has zero candidates (broken
               ;; or warming cmd).  Bind globally in the helm-map copy
               ;; below and dispatch through `helm-source-filter' so
               ;; `>' works whether or not candidates are showing.
               (narrow-display-cycle
                (lambda ()
                  (interactive)
                  (when (funcall helm-owner-p)
                    (cond
                     (narrowed-name
                      (let* ((idx (cl-position narrowed-name source-names
                                               :test #'equal))
                             (src (and idx (aref sources-v idx))))
                        (when src
                          (fzfa-source--display-cycle src fzfa-separator)
                          (when (and (funcall helm-owner-p) helm-alive-p)
                            (setq helm-pattern (minibuffer-contents))))))
                     (t (call-interactively #'self-insert-command))))))
               ;; Layer the narrow + display bindings onto a fresh
               ;; COPY of `helm-map' so the user's helm-map
               ;; customizations (TAB → persistent-action, etc.) are
               ;; preserved.
               (helm-map
                (let ((m (copy-keymap helm-map)))
                  ;; `<' (narrow-switch) only meaningful when there are
                  ;; multiple sources to switch between.
                  (when (and multi-p fzfa-multi-narrow-key)
                    (define-key m (kbd fzfa-multi-narrow-key) narrow-fn))
                  (when fzfa-display-key
                    (define-key m (kbd fzfa-display-key)
                                narrow-display-cycle))
                  m)))
          ;; Per-tick filter capture for `fzfa-replay'.  Computes the
          ;; FILTER portion of `helm-pattern' against the narrowed
          ;; source's display state + command (so compact / full
          ;; sessions persist just the filter, not the `<sep>CMD<sep>'
          ;; prefix), and stashes it in `last-query'.  Widened reads
          ;; `helm-pattern' verbatim.  `update-last-query' and
          ;; `restore-narrow' are hoisted to the outer let* so they
          ;; survive into the cleanup; setq here installs the closures.
          (setq update-last-query
                (lambda ()
                  (when (funcall helm-owner-p)
                    (let* ((idx (and narrowed-name
                                     (cl-position narrowed-name source-names
                                                  :test #'equal)))
                           (src (and idx (aref sources-v idx)))
                           (display (and src (fzfa-source-display-state src)))
                           (filter
                            (if (and src display (not (eq display 'hidden)))
                                (cdr (fzfa--split (or helm-pattern "")
                                                  display
                                                  (fzfa-source-command src)))
                              (or helm-pattern ""))))
                      (setq last-query (or filter ""))))))
          (when (and multi-p narrow-idx)
            (let ((target (aref source-names narrow-idx)))
              (setq restore-narrow
                    (lambda ()
                      (when (funcall helm-owner-p)
                        (helm-set-source-filter (list target))
                        (remove-hook 'helm-after-update-hook
                                     restore-narrow))))))
          (add-hook 'helm-after-update-hook jump-fn)
          (add-hook 'helm-after-update-hook update-last-query)
          (add-hook 'helm-after-update-hook snap-fn)
          (add-hook 'helm-move-selection-after-hook move-marker)
          (when restore-narrow
            (add-hook 'helm-after-update-hook restore-narrow))
          (setq helm-entered t)
          (helm :sources helm-sources
                :prompt (or prompt
                            (and (not multi-p)
                                 (plist-get s0 :prompt))
                            "fzf-multi: ")
                :default (and (not multi-p) (plist-get s0 :default))
                ;; `:initial-input' lives on the narrow target's spec
                ;; (or source 0 if widened) — read it here so replay
                ;; restores the filter under helm too, matching the
                ;; vertico / ivy paths.
                :input (plist-get (nth (or narrow-idx 0) sources)
                                  :initial-input)
                :buffer (if multi-p "*helm fzfa multi*" "*helm fzfa*"))))
      ;; Release resources first.  Every cleanup call is isolated so one
      ;; broken source or hook cannot strand the remaining handles or timers.
      (setq session-active nil)
      (when poll-timer
        (fzfa-helm--cleanup-call "poll timer" #'cancel-timer poll-timer)
        (setq poll-timer nil))
      (dolist (stop stops)
        (fzfa-helm--cleanup-call "source" stop))
      (fzfa-helm--cleanup-call
       "after-update jump hook" #'remove-hook
       'helm-after-update-hook jump-fn)
      (fzfa-helm--cleanup-call
       "after-update query hook" #'remove-hook
       'helm-after-update-hook update-last-query)
      (fzfa-helm--cleanup-call
       "after-update snap hook" #'remove-hook
       'helm-after-update-hook snap-fn)
      (fzfa-helm--cleanup-call
       "move-selection hook" #'remove-hook
       'helm-move-selection-after-hook move-marker)
      (when restore-narrow
        (fzfa-helm--cleanup-call
         "restore-narrow hook" #'remove-hook
         'helm-after-update-hook restore-narrow))
      ;; `fzfa-source--stop' retains current-cmd and display-state, so replay
      ;; can safely snapshot after teardown-critical resources are released.
      (when helm-entered
        (fzfa-helm--cleanup-call
         "session snapshot" #'fzfa--sessions-push
         sources sources-v
         (or prompt (and (not multi-p) (plist-get s0 :prompt))
             "fzf-multi: ")
         (and narrowed-name
              (cl-position narrowed-name source-names :test #'equal))
         last-query entry-command))
      ;; Per-source preview `:exit' + `:return' broadcast.  The winning
      ;; source's cell receives the raw candidate; every other active cell
      ;; receives nil.  Isolate each dispatch so one handler cannot prevent
      ;; the remaining preview sessions from closing.
      (when any-preview
        (dotimes (i n-sources)
          (when-let* ((state (aref preview-active i))
                      (cell (aref preview-cells i)))
            (let ((fzfa--preview-session cell))
              (fzfa-helm--cleanup-call
               (format "preview %d exit" i) #'fzfa--preview-call :exit nil)
              (when (eq state 'active)
                (fzfa-helm--cleanup-call
                 (format "preview %d return" i) #'fzfa--preview-return
                 (if (eql i result-src-idx) result-cand nil) nil)))
            (aset preview-active i nil))))
      (fzfa-helm--cleanup-call
       "stranded follow timer" #'fzfa-helm--cancel-stranded-follow-timer))
    result))

(provide 'fzfa-helm)
;;; fzfa-helm.el ends here
