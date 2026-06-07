;;; fzfa-grep.el --- POSIX grep integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; POSIX `grep' integration for fzfa.
;;
;; Loaded automatically when `grep' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-grep'                Search file contents under `default-directory'
;;   `fzfa-grep-current-file'   Search the current buffer's file with grep

;;; Code:

(require 'fzfa)

(defcustom fzfa-grep-command "grep -Rn ''"
  "Shell command used by `fzfa-grep' for content search.
Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-grep-current-file-command "grep -vnH '^[[:space:]]*$' %s"
  "Shell command used by `fzfa-grep-current-file'.
A `%s' placeholder is filled with the shell-quoted current file path.
Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-grep ()
  "Search file contents under `default-directory' with grep.
Streams all file contents as FILE:LINE:CONTENT; type
 to fuzzy-filter across them.
Selecting a candidate opens the file at that line.
The command is configurable via `fzfa-grep-command'."
  (interactive)
  (when-let* ((r (fzfa-async-completing-read
                  :command fzfa-grep-command
                  :category 'fzfa-grep
                  :group #'fzfa--grep-group)))
    (fzfa--grep-jump r)))

;;;###autoload
(defun fzfa-grep-current-file ()
  "Search the current buffer's file with grep.
Streams non-blank lines as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate jumps to that line in the file.
The command is configurable via `fzfa-grep-current-file-command'."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when-let* ((r (fzfa-async-completing-read
                  :command (format fzfa-grep-current-file-command
                                   (shell-quote-argument buffer-file-name))
                  :category 'fzfa-grep)))
    (fzfa--grep-jump r)))

(when (memq 'fzfa-grep-2p fzfa-2p-functions)
  (fzfa-2p-define 'fzfa-grep))

(provide 'fzfa-grep)
;;; fzfa-grep.el ends here
