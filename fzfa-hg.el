;;; fzfa-hg.el --- Mercurial integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
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
;;   `fzfa-hg-files'  Find a tracked file in the current Mercurial repo
;;   `fzfa-hg-modified-locally'  Pick a locally-modified tracked file
;;   `fzfa-hg-added-files'  Pick an unknown (untracked) file
;;   `fzfa-hg-modified-in-head'  Pick a file modified by the parent revision

;;; Code:

(require 'fzfa)

(defcustom fzfa-hg-files-command "hg files"
  "Shell command used by `fzfa-hg-files'.

Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-hg-modified-locally-command "hg status -n -m"
  "Shell command used by `fzfa-hg-modified-locally'.

Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-hg-added-files-command "hg status -n -u"
  "Shell command used by `fzfa-hg-added-files'.

Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-hg-modified-in-head-command "hg status -n --change ."
  "Shell command used by `fzfa-hg-modified-in-head'.

`.' resolves to the working directory's parent revision — i.e., the
most recent commit on the current branch.  Run from `default-directory';
stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-hg-files ()
  "Find a tracked file in the current Mercurial repo using hg files.

The command is configurable via `fzfa-hg-files-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzfa-completing-read
                       :prompt "hg files: "
                       :command fzfa-hg-files-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-hg-modified-locally ()
  "Pick a locally-modified tracked file in the current Mercurial repo.

The command is configurable via `fzfa-hg-modified-locally-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzfa-completing-read
                       :prompt "hg modified: "
                       :command fzfa-hg-modified-locally-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-hg-added-files ()
  "Pick an unknown (untracked) file in the current Mercurial repo.

The command is configurable via `fzfa-hg-added-files-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzfa-completing-read
                       :prompt "hg added: "
                       :command fzfa-hg-added-files-command)))
    (fzfa-with-visit (find-file result))))

;;;###autoload
(defun fzfa-hg-modified-in-head ()
  "Pick a file modified by the parent revision of the current Mercurial repo.

The command is configurable via `fzfa-hg-modified-in-head-command'."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzfa-completing-read
                       :prompt "hg HEAD: "
                       :command fzfa-hg-modified-in-head-command)))
    (fzfa-with-visit (find-file result))))

(provide 'fzfa-hg)
;;; fzfa-hg.el ends here
