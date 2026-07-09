;;; fzfa-ugrep.el --- Ugrep integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ugrep' (https://github.com/Genivia/ugrep) integration for fzfa.
;;
;; Loaded automatically when `ugrep' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-ugrep'   Search file contents under `default-directory' with ugrep

;;; Code:

(require 'fzfa)

(defcustom fzfa-ugrep-command
  "ugrep -RIn --no-heading %s ''"
  "Shell command used by `fzfa-ugrep' for content search.

A `%s' placeholder is filled with the max-columns flag derived from
`fzfa-max-line-length'.  Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-ugrep ()
  "Search file contents under `default-directory' with ugrep.

Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.

Selecting a candidate opens the file at that line.
The command is configurable via `fzfa-ugrep-command'."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :command (format fzfa-ugrep-command
                                   (fzfa--max-columns-flag 'ugrep))
                  :category 'fzfa-grep
                  :group #'fzfa--grep-group)))
    (fzfa-grep-jump r)))

(provide 'fzfa-ugrep)
;;; fzfa-ugrep.el ends here
