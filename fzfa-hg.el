;;; fzfa-hg.el --- Mercurial integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files, matching, vc
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Mercurial (`hg') integration for fzfa.
;;
;; Loaded automatically when `hg' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-hg-files'   Find a tracked file in the current Mercurial repo

;;; Code:

(require 'fzfa)

(defcustom fzfa-hg-files-command "hg files"
  "Shell command used by `fzfa-hg-files'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-hg-files ()
  "Find a tracked file in the current Mercurial repo using hg files.
The command is configurable via `fzfa-hg-files-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "hg files: "
                       :command fzfa-hg-files-command)))
    (find-file result)))

(provide 'fzfa-hg)
;;; fzfa-hg.el ends here
