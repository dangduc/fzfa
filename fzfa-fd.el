;;; fzfa-fd.el --- Fd (find alternative) integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `fd' (https://github.com/sharkdp/fd) integration for fzfa.
;;
;; Loaded automatically when `fd' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-fd'   Find a file under `default-directory' using fd

;;; Code:

(require 'fzfa)

(defcustom fzfa-fd-command "fd --no-ignore"
  "Shell command used by `fzfa-fd'.

Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-fd ()
  "Find a file under `default-directory' using fd.

The command is configurable via `fzfa-fd-command'."
  (interactive)
  (when-let* ((result (fzfa-completing-read :command fzfa-fd-command)))
    (fzfa-with-visit (find-file result))))

(provide 'fzfa-fd)
;;; fzfa-fd.el ends here
