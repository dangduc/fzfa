;;; fzfa-locate.el --- Locate integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `locate' integration for fzfa.
;;
;; Loaded automatically when `locate' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-locate'   Find a file system-wide using locate

;;; Code:

(require 'fzfa)

(defcustom fzfa-locate-command "locate ''"
  "Shell command used by `fzfa-locate'.

Stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-locate-external-extensions
  '(;; Video
    "mp4" "mkv" "webm" "mov" "avi" "mpg" "mpeg" "wmv" "flv" "m4v"
    "3gp" "ogv" "ts" "vob" "rmvb"
    ;; Audio
    "mp3" "m4a" "flac" "wav" "ogg" "aac" "wma" "opus" "ape" "alac"
    "aiff" "dsf"
    ;; Other multimedia containers / large binary blobs
    "iso" "dmg")
  "File extensions for which `fzfa-locate' delegates to the OS handler.

Matched case-insensitively against the candidate's
`file-name-extension'.  `mailcap' is intentionally not consulted —
its static MIME table predates modern formats (no `mp4', `mkv',
`webm') and trying to fall back across both sources just produces
inconsistent behavior."
  :type '(repeat string)
  :group 'fzfa)

(defcustom fzfa-locate-external-open-command
  (cond ((eq system-type 'darwin)        "open")
        ((eq system-type 'gnu/linux)     "xdg-open")
        ((memq system-type '(windows-nt cygwin)) "start"))
  "Program used to open files matching `fzfa-locate-external-extensions'.

Nil disables external dispatch — every selection falls back to
`find-file' regardless of extension.  The program is invoked with
the absolute file path as its single argument and detached from
Emacs (`call-process' with PROC=0), so Emacs doesn't block on the
external viewer."
  :type '(choice string (const :tag "Disable external dispatch" nil))
  :group 'fzfa)

(defun fzfa-locate--external-p (file)
  "Non-nil if FILE's extension is in `fzfa-locate-external-extensions'."
  (when-let* ((ext (file-name-extension file)))
    (member (downcase ext) fzfa-locate-external-extensions)))

;;;###autoload
(defun fzfa-locate ()
  "Find a file system-wide using locate.

The command is configurable via `fzfa-locate-command'.  Files
whose extension matches `fzfa-locate-external-extensions' are
opened via `fzfa-locate-external-open-command' (the OS handler:
`open' on macOS, `xdg-open' on Linux, `start' on Windows) rather
than `find-file' — so picking an MKV or FLAC from locate launches
the system player instead of loading binary into a buffer."
  (interactive)
  (when-let* ((result (fzfa-completing-read
                       :command fzfa-locate-command)))
    (cond
     ((and fzfa-locate-external-open-command
           (fzfa-locate--external-p result))
      (call-process fzfa-locate-external-open-command
                    nil 0 nil
                    (expand-file-name result)))
     (t
      (fzfa-with-visit (find-file result))))))

(provide 'fzfa-locate)
;;; fzfa-locate.el ends here
