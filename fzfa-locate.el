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

;;;###autoload
(defun fzfa-locate ()
  "Find a file system-wide using locate.

The command is configurable via `fzfa-locate-command'.  Selection
is routed through `fzfa-visit-file', so files whose extension is
in `fzfa-external-extensions' are dispatched to the OS handler
\(`open' on macOS, `xdg-open' on Linux, `start' on Windows) and
everything else goes through `fzfa-find-file-function' — same
strategy every other fzfa file-picker uses."
  (interactive)
  (when-let* ((result (fzfa-completing-read
                       :command fzfa-locate-command)))
    (fzfa-visit-file result)))

(provide 'fzfa-locate)
;;; fzfa-locate.el ends here
