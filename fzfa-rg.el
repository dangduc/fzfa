;;; fzfa-rg.el --- Ripgrep integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ripgrep' (https://github.com/BurntSushi/ripgrep) integration for fzfa.
;;
;; Loaded automatically when `rg' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-rg-files'   Find a file under `default-directory' using rg --files
;;   `fzfa-rg'         Search file contents under `default-directory' with rg

;;; Code:

(require 'fzfa)

(defcustom fzfa-rg-files-command "rg --files"
  "Shell command used by `fzfa-rg-files'.

Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-rg-command
  "rg --line-number --no-heading --with-filename %s ''"
  "Shell command used by `fzfa-rg' for content search.

A `%s' placeholder is filled with the max-columns flag derived from
`fzfa-max-line-length'.  Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-rg-files ()
  "Find a file under `default-directory' using rg --files.

The command is configurable via `fzfa-rg-files-command'."
  (interactive)
  (when-let* ((result (fzfa-completing-read
                       :prompt "rg files: " :command fzfa-rg-files-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-rg ()
  "Search file contents under `default-directory' with rg.

Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line.
The command is configurable via `fzfa-rg-command'."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :command (format fzfa-rg-command
                                   (fzfa--max-columns-flag 'rg))
                  :category 'fzfa-grep
                  :group #'fzfa--grep-group)))
    (fzfa-with-visit (fzfa--grep-jump r))))

(provide 'fzfa-rg)
;;; fzfa-rg.el ends here
