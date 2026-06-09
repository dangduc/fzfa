;;; fzfa-find.el --- Find integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; POSIX `find' integration for fzfa.
;;
;; Loaded automatically when `find' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-find'   Find a file under `default-directory' using find

;;; Code:

(require 'fzfa)

(defcustom fzfa-find-command "find ."
  "Shell command used by `fzfa-find'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-find ()
  "Find a file under `default-directory' using find.
The command is configurable via `fzfa-find-command'."
  (interactive)
  (when-let* ((result (fzfa-async-completing-read :command fzfa-find-command)))
    (fzfa-with-visit (find-file result))))

(provide 'fzfa-find)
;;; fzfa-find.el ends here
