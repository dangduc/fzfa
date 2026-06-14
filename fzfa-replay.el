;;; fzfa-replay.el --- Persisted replay for `fzfa' sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Persists `fzfa--sessions' to disk so `fzfa-replay'-class commands
;; can reach past Emacs lifetimes.  Modeled on `recentf':
;;
;;   `fzfa-replay-mode'  Global minor mode.  Enabling it loads the
;;                       on-disk session list, installs an idle
;;                       auto-save timer, and registers a final
;;                       save on `kill-emacs-hook'.
;;
;; The in-memory ring `fzfa--sessions' is captured by `fzfa--read'
;; regardless of this extension — replay-from-memory works without
;; loading anything.  This file adds the disk round-trip.

;;; Code:

(require 'fzfa)

;;; Customization

(defgroup fzfa-replay nil
  "Persisted replay for `fzfa' sessions."
  :group 'fzfa)

(defface fzfa-replay-query
  '((t :underline t))
  "Face for the captured query column in a replay candidate.

Underline by default so the eye finds the query quickly when
scanning a list of otherwise-similar sessions (same command,
same directory, different queries).  Same idea as
`fzfa-regexp-match' on the regexp picker."
  :group 'fzfa-replay)

(defcustom fzfa-replay-file
  (locate-user-emacs-file ".fzfa-replay")
  "File where `fzfa-replay-mode' persists the session list."
  :type 'file
  :group 'fzfa-replay)

(defcustom fzfa-replay-max-saved-items 20
  "Maximum number of sessions kept in `fzfa-replay-file'.

Independent of `fzfa-sessions-max', which caps the in-memory ring.
The on-disk ring is typically larger — it's the retention horizon
across Emacs lifetimes; the in-memory ring is the recent-activity
window."
  :type 'natnum
  :group 'fzfa-replay)

(defcustom fzfa-replay-save-file-modes #o600
  "Permission bits set on `fzfa-replay-file' after writing.
Nil leaves modes alone.  Sessions can contain typed queries and
buffer / file paths — owner-only is the safe default."
  :type '(choice integer (const nil))
  :group 'fzfa-replay)

(defcustom fzfa-replay-auto-save-interval 300
  "Seconds of idle time between background saves of the session list.
A non-positive value disables periodic saving (`kill-emacs-hook'
still runs the final save)."
  :type 'number
  :group 'fzfa-replay)

;;; State

(defvar fzfa-replay-auto-save-timer nil
  "Idle timer installed by `fzfa-replay-mode' for periodic saves.
Nil when the mode is disabled.")

(defvar fzfa-replay--persisted-sessions nil
  "On-disk session list, loaded by `fzfa-replay-load-list'.

Kept separate from `fzfa--sessions' so:
- Disk reads never block the in-memory picker.
- The two rings trim independently (recent activity vs.
  retention horizon).
- The pickers can present them as distinct sources without
  invented merging rules.

`fzfa-replay-from-file' picks over this variable;
`fzfa-replay-any' unions both rings.")

(defvar fzfa-replay--cache-mtime nil
  "Mtime of the last-loaded `fzfa-replay-file'.
Cache key for `fzfa-replay--load-async' — unchanged mtime returns
the cached session list immediately, no re-read.  Invalidated by
`fzfa-replay-save-list'.")

(defvar fzfa-replay--cache-sessions nil
  "Cached value of `fzfa-replay--persisted-sessions' keyed by mtime.")

;;; Serialization helpers

(defun fzfa-replay--scrub-spec (spec snapshot)
  "Return a serialization-safe copy of SPEC.

Walks the plist and:
- Replaces function-shaped `:candidates' with SNAPSHOT (the
  exit-time output captured at session push).  Static-list and
  nil `:candidates' pass through.
- Drops slots whose value is a function — `:action', `:annotate',
  `:affix', `:group', any lambda the caller attached.  These can't
  round-trip through `prin1' / `read' as callables, and replay-
  from-file accepts the loss (the picker still works; the source's
  custom action just falls through to the default identity)."
  (cl-loop for (key value) on spec by #'cddr
           for safe =
           (cond
            ((and (eq key :candidates) (functionp value))
             snapshot)
            ((functionp value) :fzfa-replay--drop)
            (t value))
           unless (eq safe :fzfa-replay--drop)
           collect key and collect safe))

(defun fzfa-replay--scrub-session (session)
  "Return SESSION with each per-source spec scrubbed for disk."
  (let* ((sources (plist-get session :sources))
         (scrubbed
          (cl-map
           'vector
           (lambda (rec)
             (list :spec (fzfa-replay--scrub-spec
                          (plist-get rec :spec)
                          (plist-get rec :snapshot))
                   :command       (plist-get rec :command)
                   :display       (plist-get rec :display)
                   :initial-input (plist-get rec :initial-input)))
           sources)))
    (list :prompt     (plist-get session :prompt)
          :narrow-idx (plist-get session :narrow-idx)
          :timestamp  (plist-get session :timestamp)
          :directory  (plist-get session :directory)
          :command    (plist-get session :command)
          :sources    scrubbed)))

(defun fzfa-replay--readable-p (value)
  "Return non-nil if VALUE survives a `prin1' / `read' round-trip.

The top-level `--scrub-spec' drops function-shaped plist values
but doesn't peer into nested data — text properties on
candidates can still carry markers, byte-compiled functions,
buffers, etc., which print as `#<…>' and won't load back.  This
predicate gates each session before persistence so a single
unreadable candidate doesn't corrupt the whole save file."
  (condition-case nil
      (let ((print-circle t) (print-length nil) (print-level nil))
        (read (prin1-to-string value))
        t)
    (error nil)))

(defconst fzfa-replay--save-file-header
  ";;; -*- mode: emacs-lisp; coding: utf-8-emacs; -*-
;; fzfa-replay session log generated by `fzfa-replay-save-list'.
;; Auto-generated — do not hand-edit.
"
  "Header prepended to `fzfa-replay-file' on each save.")

;;; Save / load

(defun fzfa-replay--merge-sessions (in-memory persisted)
  "Merge IN-MEMORY + PERSISTED session lists, dedup'd by session key.

IN-MEMORY entries win on dedup — re-running a command from the
same directory with the same query inside the current Emacs
session supersedes the stale on-disk entry from a previous run.
Result is sorted by `:timestamp' descending so the most recent
session sits at the head, ready for `cl-subseq' trimming to the
disk cap.

This is the recentf-style accumulation pattern: each save merges
the current ring on top of what's already on disk, so the file
accrues sessions across Emacs lifetimes instead of being
overwritten by whichever ring happened to live longest."
  (let ((seen (make-hash-table :test 'equal))
        merged)
    (dolist (s (append in-memory persisted))
      (let ((key (fzfa--session-dedup-key s)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push s merged))))
    (sort (nreverse merged)
          (lambda (a b)
            (> (or (plist-get a :timestamp) 0)
               (or (plist-get b :timestamp) 0))))))

(defun fzfa-replay-save-list ()
  "Merge `fzfa--sessions' with the on-disk list and write to `fzfa-replay-file'.

Loads the current on-disk sessions (via the mtime-keyed cache
when fresh), merges them with the in-memory ring via
`fzfa-replay--merge-sessions' — newer in-memory entries win on
dedup — sorts by timestamp, trims to
`fzfa-replay-max-saved-items', scrubs each session via
`fzfa-replay--scrub-session' to strip non-readable function
slots, and writes the result.  This is the recentf-style pattern:
the on-disk file accumulates sessions across Emacs lifetimes,
so closing and reopening Emacs preserves replay history.

The in-memory mirror `fzfa-replay--persisted-sessions' is
updated to the just-written list so subsequent
`fzfa-replay-from-file' calls see the fresh state without a
disk reload round-trip."
  (interactive)
  (condition-case err
      (let* ((merged (fzfa-replay--merge-sessions
                      fzfa--sessions fzfa-replay--persisted-sessions))
             (trimmed (cl-subseq merged
                                 0 (min fzfa-replay-max-saved-items
                                        (length merged))))
             (scrubbed (mapcar #'fzfa-replay--scrub-session trimmed))
             (dropped 0)
             (safe (cl-loop for s in scrubbed
                            if (fzfa-replay--readable-p s) collect s
                            else do (cl-incf dropped)))
             (print-length nil)
             (print-level nil)
             (print-quoted t)
             (print-circle t))
        (when (> dropped 0)
          (message "fzfa-replay-save-list: dropped %d unreadable session(s)"
                   dropped))
        (with-temp-buffer
          (insert fzfa-replay--save-file-header)
          (insert "\n(setq fzfa-replay--persisted-sessions\n      '")
          (prin1 safe (current-buffer))
          (insert ")\n")
          (write-region (point-min) (point-max)
                        (expand-file-name fzfa-replay-file))
          (when fzfa-replay-save-file-modes
            (set-file-modes (expand-file-name fzfa-replay-file)
                            fzfa-replay-save-file-modes))
          ;; Sync the in-memory mirror to what we just wrote so the
          ;; next merge / picker sees the accumulated list without a
          ;; disk reload, and invalidate the async cache so any other
          ;; consumer (e.g. a concurrent picker) re-reads fresh.
          (setq fzfa-replay--persisted-sessions safe
                fzfa-replay--cache-mtime nil
                fzfa-replay--cache-sessions nil)))
    (error
     (message "fzfa-replay-save-list: %s" (error-message-string err)))))

(defun fzfa-replay-load-list ()
  "Load saved sessions from `fzfa-replay-file' into
`fzfa-replay--persisted-sessions'.

No-op when the file does not exist (first run).  Errors during
load are logged via `message' — a corrupt or partial file should
not break the in-memory replay path."
  (interactive)
  (let ((file (expand-file-name fzfa-replay-file)))
    (when (file-readable-p file)
      (condition-case err
          (load-file file)
        (error
         (message "fzfa-replay-load-list: %s"
                  (error-message-string err)))))))

;;; Mode

(defun fzfa-replay--start-auto-save-timer ()
  "Install the idle auto-save timer if interval is positive."
  (when (and (numberp fzfa-replay-auto-save-interval)
             (> fzfa-replay-auto-save-interval 0))
    (setq fzfa-replay-auto-save-timer
          (run-with-idle-timer fzfa-replay-auto-save-interval
                               t
                               #'fzfa-replay-save-list))))

(defun fzfa-replay--cancel-auto-save-timer ()
  "Cancel the idle auto-save timer if installed."
  (when fzfa-replay-auto-save-timer
    (cancel-timer fzfa-replay-auto-save-timer)
    (setq fzfa-replay-auto-save-timer nil)))

;;; Async file load

(defun fzfa-replay--load-async (callback)
  "Load persisted sessions asynchronously; CALLBACK receives the list.

Three paths:
- File missing → CALLBACK invoked with nil immediately.
- Cache hit (mtime unchanged) → CALLBACK invoked with cached list
  immediately.
- Cache miss → file read scheduled on `run-with-idle-timer' so the
  caller (typically a `:candidates' producer) returns first; the
  CALLBACK fires from the idle handler with the loaded sessions
  and the cache updates."
  (let* ((file (expand-file-name fzfa-replay-file))
         (mtime (and (file-readable-p file)
                     (file-attribute-modification-time
                      (file-attributes file)))))
    (cond
     ((null mtime) (funcall callback nil))
     ((equal mtime fzfa-replay--cache-mtime)
      (funcall callback fzfa-replay--cache-sessions))
     (t
      (run-with-idle-timer
       0 nil
       (lambda ()
         (fzfa-replay-load-list)
         (setq fzfa-replay--cache-mtime mtime
               fzfa-replay--cache-sessions fzfa-replay--persisted-sessions)
         (funcall callback fzfa-replay--persisted-sessions)))))))

;;; Pickers

(defun fzfa-replay--session-to-candidate (session idx)
  "Build a picker candidate string for SESSION at position IDX.

The displayed text is a short summary — `DATE  COMMAND  QUERY
DIRECTORY' — followed by an invisible per-IDX tofu suffix to
guarantee `string='-uniqueness so two captures with the same
visible columns don't get collapsed by vertico's
`delete-consecutive-dups' (the suffix is the same trick the
multi-source path uses for cross-source disambiguation).  The
full session plist rides on the string as a `fzfa-replay-session'
text property at index 0 — the snapshot-lookup recovery in
`fzfa--read''s post-result block restores it on selection so
`fzfa-replay--action' can dispatch.

QUERY is baked into the candidate string rather than tacked on by
`:annotate' so it surfaces under every frontend (ivy and helm
don't render annotations natively; vertico does but the column
gets lost in vertico-buffer mode trimming).  Inline columns are
the lowest-common-denominator that works everywhere."
  (let* ((ts (or (plist-get session :timestamp) 0))
         (time-str (format-time-string "%a %H:%M" ts))
         (cmd (or (plist-get session :command) "?"))
         (dir (abbreviate-file-name
               (or (plist-get session :directory) "")))
         (sources (plist-get session :sources))
         (target (or (plist-get session :narrow-idx) 0))
         (filter (or (and sources (< target (length sources))
                          (plist-get (aref sources target) :initial-input))
                     ""))
         (filter-col
          (if (string-empty-p filter)
              (propertize "—" 'face 'shadow)
            (propertize filter 'face 'fzfa-replay-query)))
         (display (concat (format "%s  %-24s  %-16s  %s"
                                  time-str cmd filter-col dir)
                          (fzfa--tofu-suffix idx))))
    (propertize display 'fzfa-replay-session session)))

(defun fzfa-replay--sessions-to-candidates (sessions)
  "Convert SESSIONS to picker candidates with aligned columns.

Two-pass column-width computation across the whole batch so the
QUERY and DIRECTORY columns line up vertically even when COMMAND
varies in length across rows (`fzfa-fd' next to
`fzfa-replay-from-memory' otherwise pushes the per-row format's
fixed-width seam off-grid).  Single-shot
`fzfa-replay--session-to-candidate' callers stay on the original
fixed-width format; this helper is for the picker-fills-with-a-
list shape used by `fzfa-replay-from-memory' and
`fzfa-replay-from-file'.

Returns nil for an empty or nil SESSIONS, matching what callers
expect to feed `fzfa-completing-read'."
  (when sessions
    (let* ((rows
            (cl-loop
             for s in sessions for i from 0 collect
             (let* ((ts (or (plist-get s :timestamp) 0))
                    (time-str (format-time-string "%a %H:%M" ts))
                    (cmd (or (plist-get s :command) "?"))
                    (cmd-str (format "%s" cmd))
                    (dir (abbreviate-file-name
                          (or (plist-get s :directory) "")))
                    (sources (plist-get s :sources))
                    (target (or (plist-get s :narrow-idx) 0))
                    (filter (or (and sources
                                     (< target (length sources))
                                     (plist-get (aref sources target)
                                                :initial-input))
                                "")))
               (list s i time-str cmd-str filter dir))))
           (time-w (apply #'max 1
                          (mapcar (lambda (r) (length (nth 2 r))) rows)))
           (cmd-w  (apply #'max 1
                          (mapcar (lambda (r) (length (nth 3 r))) rows)))
           (filter-w
            (apply #'max 1
                   (mapcar
                    (lambda (r)
                      (let ((f (nth 4 r)))
                        (if (string-empty-p f) 1 (length f))))
                    rows)))
           ;; Build the per-row format string once.  Inner format
           ;; produces e.g. "%-7s  %-25s  %-10s  %s" — outer format
           ;; applies that with the row's values.
           (fmt (format "%%-%ds  %%-%ds  %%-%ds  %%s"
                        time-w cmd-w filter-w)))
      (mapcar
       (lambda (r)
         (let* ((session   (nth 0 r))
                (idx       (nth 1 r))
                (time-str  (nth 2 r))
                (cmd-str   (nth 3 r))
                (filter    (nth 4 r))
                (dir       (nth 5 r))
                (filter-col
                 (if (string-empty-p filter)
                     (propertize "—" 'face 'shadow)
                   (propertize filter 'face 'fzfa-replay-query)))
                (display (concat (format fmt time-str cmd-str
                                         filter-col dir)
                                 (fzfa--tofu-suffix idx))))
           (propertize display 'fzfa-replay-session session)))
       rows))))

(defun fzfa-replay--annotate (cand)
  "Annotation function: source count only.

The filter / query is baked into the candidate display string
itself (see `fzfa-replay--session-to-candidate') so it shows on
ivy / helm too; the annotation is just the source-count
afterthought that vertico happens to render."
  (when-let* ((session (get-text-property 0 'fzfa-replay-session cand)))
    (format "  %d src" (length (plist-get session :sources)))))

(defun fzfa-replay--group (cand transform)
  "Group function: bucket CAND by date (Today / Yesterday / Week / Older)."
  (if transform
      cand
    (or (when-let* ((session (get-text-property 0 'fzfa-replay-session cand))
                    (ts (plist-get session :timestamp)))
          (let ((days-ago (/ (- (float-time) ts) 86400)))
            (cond
             ((< days-ago 1) "Today")
             ((< days-ago 2) "Yesterday")
             ((< days-ago 7) "This week")
             (t "Older"))))
        "Unknown")))

(defun fzfa-replay--replay-session (session)
  "Restore SESSION's specs and run `fzfa--read'."
  (let ((specs (cl-map 'list #'fzfa--session-restore-spec
                       (plist-get session :sources))))
    (fzfa--read specs
                :prompt (plist-get session :prompt)
                :narrow-idx (plist-get session :narrow-idx))))

(defun fzfa-replay--action (cand)
  "Picker action: read the session off CAND and replay it."
  (when-let* ((session (and (stringp cand) (> (length cand) 0)
                            (get-text-property 0 'fzfa-replay-session
                                               cand))))
    (fzfa-replay--replay-session session)))

(defvar fzfa-vertico-columns-truncate)

(defun fzfa-replay--picker (sessions prompt)
  "Run a session picker over SESSIONS with PROMPT.

SESSIONS is a list of session plists (most recent first); empty
signals a `user-error'.  Each session renders as a summary
candidate carrying the full plist via text property; selection
calls `fzfa-replay--action'.

Forces left-anchored truncation in `fzfa-vertico' — the
identifying info (DATE / COMMAND / QUERY) lives at the START of
the row, so `fzfa-vertico's default `auto' heuristic (which
treats path-bearing candidates as suffix-anchored) is backwards
here.  Localized via `let' rather than a new per-category
defcustom — we only need this for the two replay pickers, no
need to generalize yet."
  (unless sessions
    (user-error "No fzfa sessions to replay"))
  (let ((fzfa-vertico-columns-truncate 'left))
    (let ((cand (fzfa-completing-read
                 :candidates (fzfa-replay--sessions-to-candidates sessions)
                 :prompt prompt
                 :category 'fzfa-replay-session
                 :annotate #'fzfa-replay--annotate
                 :group #'fzfa-replay--group
                 :require-match t)))
      (fzfa-replay--action cand))))

;;;###autoload
(defun fzfa-replay-from-memory ()
  "Pick from the in-memory session ring (`fzfa--sessions') and replay."
  (interactive)
  (fzfa-replay--picker fzfa--sessions "Replay (memory): "))

(defun fzfa-replay--file-producer (_input cb)
  "2-arg `:candidates' producer that loads `fzfa-replay-file' async.
INPUT is ignored; CB is invoked with the session-candidate list
once the file read finishes (or immediately on cache hit).  Uses
`fzfa-replay--sessions-to-candidates' so column widths are
computed across the loaded batch — no eyeball-misalignment when
commands of varying length share the list."
  (fzfa-replay--load-async
   (lambda (sessions)
     (funcall cb (fzfa-replay--sessions-to-candidates sessions)))))

;;;###autoload
(defun fzfa-replay-from-file ()
  "Pick from the persisted session list and replay.

Loads `fzfa-replay-file' asynchronously — the picker spins up
immediately and candidates stream in once the read completes.
Subsequent invocations hit the mtime-keyed cache.

See `fzfa-replay--picker' for why `fzfa-vertico-columns-truncate'
is forced to `left' here."
  (interactive)
  (let ((fzfa-vertico-columns-truncate 'left))
    (fzfa-replay--action
     (fzfa-completing-read
      :candidates #'fzfa-replay--file-producer
      :prompt "Replay (file): "
      :category 'fzfa-replay-session
      :annotate #'fzfa-replay--annotate
      :group    #'fzfa-replay--group
      :require-match t))))

(defcustom fzfa-replay-any-commands
  '((fzfa-replay-from-memory :narrow m)
    (fzfa-replay-from-file   :narrow f))
  "Commands shown by `fzfa-replay-any'.

Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key.
Users can append their own replay-shaped commands here — anything
that flows through `fzfa-completing-read' and dispatches a
selected candidate via an action of its own (see
`fzfa-replay-from-file' for the producer shape, or
`fzfa-replay-from-memory' for the static-candidates shape)."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

;;;###autoload
(defun fzfa-replay-any ()
  "Pick from in-memory + persisted sessions and replay.

See `fzfa-replay--picker' for why `fzfa-vertico-columns-truncate'
is forced to `left' here.  The let-binding wraps `fzfa-multi-read'
because the actual minibuffer session runs inside it, outside the
inner from-* commands' let-scope."
  (interactive)
  (let ((fzfa-vertico-columns-truncate 'left))
    (fzfa-multi-read fzfa-replay-any-commands :prompt "Replay (any): ")))

;;;###autoload
(defun fzfa-replay-setup ()
  "Enable `fzfa-replay-mode' from `fzfa--ensure-setup'.

Called automatically when `replay' is in `fzfa-extensions'.
Loading this file via the autoload stub registers the mode; the
mode toggle hooks in disk persistence (idle save timer, final
`kill-emacs-hook' save, startup load)."
  (fzfa-replay-mode 1))

;;;###autoload
(define-minor-mode fzfa-replay-mode
  "Persist `fzfa' session history across Emacs lifetimes.

When enabled:
- Loads the on-disk session list from `fzfa-replay-file' into
  `fzfa-replay--persisted-sessions'.
- Installs an idle timer (`fzfa-replay-auto-save-timer') that
  flushes `fzfa--sessions' every `fzfa-replay-auto-save-interval'
  seconds.
- Adds a final-save handler on `kill-emacs-hook'.

The in-memory replay path is unaffected — `fzfa-replay' works
whether or not this mode is on.  This mode only adds the disk
round-trip."
  :global t
  :group 'fzfa-replay
  (cond
   (fzfa-replay-mode
    (fzfa-replay-load-list)
    (fzfa-replay--start-auto-save-timer)
    (add-hook 'kill-emacs-hook #'fzfa-replay-save-list))
   (t
    (fzfa-replay--cancel-auto-save-timer)
    (remove-hook 'kill-emacs-hook #'fzfa-replay-save-list))))

(provide 'fzfa-replay)
;;; fzfa-replay.el ends here
