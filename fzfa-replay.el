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
          :sources    scrubbed)))

(defconst fzfa-replay--save-file-header
  ";;; -*- mode: emacs-lisp; coding: utf-8-emacs; -*-
;; fzfa-replay session log generated by `fzfa-replay-save-list'.
;; Auto-generated — do not hand-edit.
"
  "Header prepended to `fzfa-replay-file' on each save.")

;;; Save / load

(defun fzfa-replay-save-list ()
  "Save `fzfa--sessions' to `fzfa-replay-file'.

Truncates to `fzfa-replay-max-saved-items' before writing.  Each
session is passed through `fzfa-replay--scrub-session' to strip
non-readable function slots and substitute function-shaped
`:candidates' with their captured snapshot.  Writes via
`prin1' with `print-circle' on so propertized strings (e.g.
`fzfa-location' on swiper-class candidates) round-trip."
  (interactive)
  (condition-case err
      (let* ((trimmed (cl-subseq fzfa--sessions
                                 0 (min fzfa-replay-max-saved-items
                                        (length fzfa--sessions))))
             (scrubbed (mapcar #'fzfa-replay--scrub-session trimmed))
             (print-length nil)
             (print-level nil)
             (print-quoted t)
             (print-circle t))
        (with-temp-buffer
          (insert fzfa-replay--save-file-header)
          (insert "\n(setq fzfa-replay--persisted-sessions\n      '")
          (prin1 scrubbed (current-buffer))
          (insert ")\n")
          (write-region (point-min) (point-max)
                        (expand-file-name fzfa-replay-file))
          (when fzfa-replay-save-file-modes
            (set-file-modes (expand-file-name fzfa-replay-file)
                            fzfa-replay-save-file-modes))
          ;; Invalidate the mtime-keyed async cache so the next
          ;; `fzfa-replay--load-async' reads the fresh write.
          (setq fzfa-replay--cache-mtime nil
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

(defun fzfa-replay--session-to-candidate (session)
  "Build a picker candidate string for SESSION.

The displayed text is a short summary (date + directory).  The
full session plist rides on the string as a `fzfa-replay-session'
text property at index 0 — the snapshot-lookup recovery in
`fzfa--read''s post-result block restores it on selection so
`fzfa-replay--action' can dispatch."
  (let* ((ts (or (plist-get session :timestamp) 0))
         (time-str (format-time-string "%a %H:%M" ts))
         (dir (abbreviate-file-name
               (or (plist-get session :directory) ""))))
    (propertize (format "%s  %s" time-str dir)
                'fzfa-replay-session session)))

(defun fzfa-replay--annotate (cand)
  "Annotation function: append filter + source count to CAND."
  (when-let* ((session (get-text-property 0 'fzfa-replay-session cand)))
    (let* ((sources (plist-get session :sources))
           (n (length sources))
           (target (or (plist-get session :narrow-idx) 0))
           (filter (and (< target n)
                        (plist-get (aref sources target) :initial-input))))
      (format "  %s  %d src" (or filter "—") n))))

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

(defun fzfa-replay--picker (sessions prompt)
  "Run a session picker over SESSIONS with PROMPT.

SESSIONS is a list of session plists (most recent first); empty
signals a `user-error'.  Each session renders as a summary
candidate carrying the full plist via text property; selection
calls `fzfa-replay--action'."
  (unless sessions
    (user-error "No fzfa sessions to replay"))
  (let ((cand (fzfa-completing-read
               :candidates (mapcar #'fzfa-replay--session-to-candidate
                                   sessions)
               :prompt prompt
               :category 'fzfa-replay-session
               :annotate #'fzfa-replay--annotate
               :group #'fzfa-replay--group
               :require-match t)))
    (fzfa-replay--action cand)))

;;;###autoload
(defun fzfa-replay-from-memory ()
  "Pick from the in-memory session ring (`fzfa--sessions') and replay."
  (interactive)
  (fzfa-replay--picker fzfa--sessions "Replay (memory): "))

(defun fzfa-replay--file-producer (_input cb)
  "2-arg `:candidates' producer that loads `fzfa-replay-file' async.
INPUT is ignored; CB is invoked with the session-candidate list
once the file read finishes (or immediately on cache hit)."
  (fzfa-replay--load-async
   (lambda (sessions)
     (funcall cb (and sessions
                      (mapcar #'fzfa-replay--session-to-candidate
                              sessions))))))

;;;###autoload
(defun fzfa-replay-from-file ()
  "Pick from the persisted session list and replay.

Loads `fzfa-replay-file' asynchronously — the picker spins up
immediately and candidates stream in once the read completes.
Subsequent invocations hit the mtime-keyed cache."
  (interactive)
  (fzfa-replay--action
   (fzfa-completing-read
    :candidates #'fzfa-replay--file-producer
    :prompt "Replay (file): "
    :category 'fzfa-replay-session
    :annotate #'fzfa-replay--annotate
    :group    #'fzfa-replay--group
    :require-match t)))

;;;###autoload
(defun fzfa-replay-any ()
  "Pick from in-memory + persisted sessions and replay.

Multi-source over both rings as separate sources with narrow keys
`m' (memory) and `f' (file).  The file source loads asynchronously
via `fzfa-replay--file-producer'."
  (interactive)
  (unless (or fzfa--sessions (file-readable-p
                              (expand-file-name fzfa-replay-file)))
    (user-error "No fzfa sessions to replay"))
  (fzfa--read
   (list (list :name "memory" :narrow "m"
               :candidates
               (mapcar #'fzfa-replay--session-to-candidate fzfa--sessions)
               :category 'fzfa-replay-session
               :annotate #'fzfa-replay--annotate
               :group    #'fzfa-replay--group
               :action   #'fzfa-replay--action)
         (list :name "file" :narrow "f"
               :candidates #'fzfa-replay--file-producer
               :category 'fzfa-replay-session
               :annotate #'fzfa-replay--annotate
               :group    #'fzfa-replay--group
               :action   #'fzfa-replay--action))
   :prompt "Replay (any): "))

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
