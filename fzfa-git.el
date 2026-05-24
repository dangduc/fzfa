;;; fzfa-git.el --- Git integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files, matching, vc
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Git integration for fzfa.
;;
;; Loaded automatically when `git' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-git-grep'      Search file contents in the current Git repo
;;   `fzfa-git-ls-files'  Find a tracked file in the current Git repo

;;; Code:

(require 'fzfa)

(defcustom fzfa-git-grep-command "git --no-pager grep -n \"\""
  "Shell command used by `fzfa-git-grep' for content search.
Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-ls-files-command "git ls-files"
  "Shell command used by `fzfa-git-ls-files'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-git-grep ()
  "Search file contents under `default-directory' with git grep.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line.
The command is configurable via `fzfa-git-grep-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((r (fzfa-async-completing-read
                  :command fzfa-git-grep-command
                  :category 'fzfa-grep
                  :group #'fzfa--grep-group)))
    (fzfa--grep-jump r)))

;;;###autoload
(defun fzfa-git-ls-files ()
  "Find a tracked file in the current Git repo using git ls-files.
The command is configurable via `fzfa-git-ls-files-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git ls files: "
                       :command fzfa-git-ls-files-command)))
    (find-file result)))

(provide 'fzfa-git)
;;; fzfa-git.el ends here
