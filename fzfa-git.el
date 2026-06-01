;;; fzfa-git.el --- Git integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
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
;;   `fzfa-git-log-grep'  Fuzzy-filter the commit log of the current repo

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

(defcustom fzfa-git-log-grep-command
  "git --no-pager log --pretty=format:'%h  %ad  %<(20,trunc)%aN  %s' --date=format:'%Y-%m-%d %H:%M'"
  "Shell command used by `fzfa-git-log-grep'.
Each output line must begin with the commit's short SHA followed by
display columns; the leading hex token is parsed as the SHA when a
candidate is selected."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-log-grep-action #'fzfa-git-log-grep-show-commit
  "Function called with the selected commit SHA in `fzfa-git-log-grep'."
  :type 'function
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

(declare-function magit-show-commit "magit-diff")

(defun fzfa-git-log-grep-show-commit (sha)
  "Show commit SHA.
Dispatches to `magit-show-commit' when Magit is loaded, otherwise falls
back to `fzfa-git-log-grep-show-commit-plain'.  Magit is preferred but
not required."
  (if (fboundp 'magit-show-commit)
      (magit-show-commit sha)
    (fzfa-git-log-grep-show-commit-plain sha)))

(defun fzfa-git-log-grep-show-commit-plain (sha)
  "Display \\='git show SHA\\=' in a buffer in `diff-mode'."
  (let* ((short-sha (substring sha 0 (min 8 (length sha))))
         (buf (get-buffer-create (format "*fzfa-git-show %s*" short-sha))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process "git" nil buf nil "--no-pager" "show" sha))
      (goto-char (point-min))
      (diff-mode))
    (display-buffer buf)))

;;;###autoload
(defun fzfa-git-log-grep ()
  "Fuzzy-filter the git log of the current repo.
Streams `git log' output as candidates; type to fuzzy-filter across
commit SHAs, dates, authors, and subjects.  Selecting a candidate calls
`fzfa-git-log-grep-action' with the commit SHA (default: show the commit
in a buffer).
The command is configurable via `fzfa-git-log-grep-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git log: "
                       :command fzfa-git-log-grep-command
                       :category 'fzfa-misc
                       :resolve-paths nil))
              ((string-match "\\`\\([a-f0-9]+\\)" result)))
    (funcall fzfa-git-log-grep-action (match-string 1 result))))

(when (memq 'fzfa-git-grep-2p fzfa-2p-functions)
  (fzfa-2p-define 'fzfa-git-grep))

(provide 'fzfa-git)
;;; fzfa-git.el ends here
