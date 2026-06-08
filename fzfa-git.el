;;; fzfa-git.el --- Git integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
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
;;   `fzfa-git-grep'  Search file contents in the current Git repo
;;   `fzfa-git-ls-files'  Find a tracked file in the current Git repo
;;   `fzfa-git-log-grep'  Fuzzy-filter the commit log of the current repo
;;   `fzfa-git-modified-locally'  Pick a locally-modified tracked file
;;   `fzfa-git-added-files'  Pick an untracked file
;;   `fzfa-git-staged-for-commit'  Pick a file staged for the next commit
;;   `fzfa-git-modified-in-head'  Pick a file modified by the HEAD commit

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

(defcustom fzfa-git-modified-locally-command "git ls-files -m"
  "Shell command used by `fzfa-git-modified-locally'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-added-files-command "git ls-files -o --exclude-standard"
  "Shell command used by `fzfa-git-added-files'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-staged-for-commit-command "git diff --cached --name-only"
  "Shell command used by `fzfa-git-staged-for-commit'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-modified-in-head-command
  "git diff-tree --no-commit-id --name-only -r HEAD"
  "Shell command used by `fzfa-git-modified-in-head'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-git-log-grep-command
  (concat "git --no-pager log"
          " --pretty=format:'%h  %ad  %<(20,trunc)%aN  %s'"
          " --date=format:'%Y-%m-%d %H:%M'")
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
    (fzfa-with-visit (fzfa--grep-jump r))))

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
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-git-modified-locally ()
  "Pick a locally-modified tracked file in the current Git repo.
The command is configurable via `fzfa-git-modified-locally-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git modified: "
                       :command fzfa-git-modified-locally-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-git-added-files ()
  "Pick an untracked file in the current Git repo.
The command is configurable via `fzfa-git-added-files-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git added: "
                       :command fzfa-git-added-files-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-git-staged-for-commit ()
  "Pick a file staged for the next commit in the current Git repo.
The command is configurable via `fzfa-git-staged-for-commit-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git staged: "
                       :command fzfa-git-staged-for-commit-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-git-modified-in-head ()
  "Pick a file modified by the HEAD commit of the current Git repo.
The command is configurable via `fzfa-git-modified-in-head-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "git HEAD: "
                       :command fzfa-git-modified-in-head-command)))
    (fzfa-with-visit (find-file result))))

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
