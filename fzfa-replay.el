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

`fzfa-replay-from-file' (Phase B-3) picks over this variable;
`fzfa-replay-any' (Phase B-3) unions both rings.")

;;; Save / load (stubs — implemented in Phase B-2)

(defun fzfa-replay-save-list ()
  "Save `fzfa--sessions' to `fzfa-replay-file'.
Stub for Phase B-1.  Real implementation lands in B-2."
  (interactive)
  (message "fzfa-replay-save-list: not yet implemented (Phase B-2)"))

(defun fzfa-replay-load-list ()
  "Load the saved session list from `fzfa-replay-file'.
Stub for Phase B-1.  Real implementation lands in B-2."
  (interactive)
  (message "fzfa-replay-load-list: not yet implemented (Phase B-2)"))

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
