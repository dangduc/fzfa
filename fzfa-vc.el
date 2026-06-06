;;; fzfa-vc.el --- VC dispatcher for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files, matching, vc
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Multi-source modified-files picker that dispatches to backend-
;; specific commands from vc extensions `fzfa-git' and `fzfa-hg'
;; `vc-responsible-backend' is used for detection
;; only — it is cheap (walks parents looking for `.git' / `.hg' / etc.).
;;
;; Loaded automatically when `vc' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the command is usable immediately.
;;
;; Commands:
;;   `fzfa-vc-modified-files'      Multi-source picker over the current
;;                                 repo's modified/added/staged/HEAD files
;;   `fzfa-vc-modified-locally'    Single-source: locally modified files
;;   `fzfa-vc-added-files'         Single-source: added (untracked) files
;;   `fzfa-vc-staged-for-commit'   Single-source: staged-for-commit files
;;   `fzfa-vc-modified-in-head'    Single-source: files modified in HEAD
;;
;; The single-source commands dispatch to the appropriate backend
;; entry in `fzfa-vc-modified-files-sources'; the per-source commands
;; themselves live in the backend extensions
;; (`fzfa-git-modified-locally', `fzfa-hg-modified-locally', ...) and
;; can be called directly, bypassing this dispatcher entirely.
;;
;; Supported backends out of the box: Git, Hg.  Extend by adding entries
;; to `fzfa-vc-modified-files-sources'.

;;; Code:

(require 'fzfa)

(declare-function vc-responsible-backend "vc")

(defcustom fzfa-vc-modified-files-sources
  '((Git
     (modified-locally  . fzfa-git-modified-locally)
     (added-files       . fzfa-git-added-files)
     (staged-for-commit . fzfa-git-staged-for-commit)
     (modified-in-head  . fzfa-git-modified-in-head))
    (Hg
     (modified-locally . fzfa-hg-modified-locally)
     (added-files      . fzfa-hg-added-files)
     ;; Vanilla hg has no staging area; the entry is omitted.
     (modified-in-head . fzfa-hg-modified-in-head)))
  "Per-backend source list for `fzfa-vc-modified-files'.

Each entry is (BACKEND . SOURCES) where BACKEND is the symbol returned
by `vc-responsible-backend' (e.g. `Git', `Hg') and SOURCES is an alist
of (ID . COMMAND).  ID is a short symbol naming the source (e.g.
`modified-locally', `added-files'); COMMAND is the interactive fzfa
command in the corresponding backend extension that enumerates and
opens a candidate file."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'fzfa)

(defun fzfa-vc--backend ()
  "Return the VC backend symbol for `default-directory'.
Signal a `user-error' when no backend is responsible."
  (or (vc-responsible-backend default-directory t)
      (user-error "No VC backend responsible for %s" default-directory)))

;;;###autoload
(defun fzfa-vc-modified-files ()
  "Multi-source picker over the current VC repository's modified files.
Streams every source configured for the current backend (per
`fzfa-vc-modified-files-sources') into a single fzf session with
group headers; selection invokes the source's underlying command."
  (interactive)
  (require 'vc-hooks)
  (let* ((backend (fzfa-vc--backend))
         (sources
          (or (alist-get backend fzfa-vc-modified-files-sources)
              (user-error
               "No `fzfa-vc-modified-files-sources' entry for backend %s"
               backend))))
    (fzfa-multi-read (mapcar #'cdr sources)
                     :prompt (format "vc files [%s]: " backend))))

(defun fzfa-vc--dispatch (id)
  "Invoke the backend-specific command bound to ID in the current repo.
ID is a key in the per-backend alists of
`fzfa-vc-modified-files-sources' (e.g. `modified-locally')."
  (require 'vc-hooks)
  (let* ((backend (fzfa-vc--backend))
         (sources (alist-get backend fzfa-vc-modified-files-sources))
         (cmd (alist-get id sources)))
    (unless cmd
      (user-error "Backend %s has no `%s' source" backend id))
    (call-interactively cmd)))

;;;###autoload
(defun fzfa-vc-modified-locally ()
  "Pick a locally modified file from the current VC repository.
Dispatches to the backend's `modified-locally' source in
`fzfa-vc-modified-files-sources'."
  (interactive)
  (fzfa-vc--dispatch 'modified-locally))

;;;###autoload
(defun fzfa-vc-added-files ()
  "Pick an added (untracked) file from the current VC repository.
Dispatches to the backend's `added-files' source in
`fzfa-vc-modified-files-sources'."
  (interactive)
  (fzfa-vc--dispatch 'added-files))

;;;###autoload
(defun fzfa-vc-staged-for-commit ()
  "Pick a staged-for-commit file from the current VC repository.
Dispatches to the backend's `staged-for-commit' source in
`fzfa-vc-modified-files-sources'."
  (interactive)
  (fzfa-vc--dispatch 'staged-for-commit))

;;;###autoload
(defun fzfa-vc-modified-in-head ()
  "Pick a file modified in HEAD from the current VC repository.
Dispatches to the backend's `modified-in-head' source in
`fzfa-vc-modified-files-sources'."
  (interactive)
  (fzfa-vc--dispatch 'modified-in-head))

;;; Multi-source VCS

(defcustom fzfa-vcs-any-commands
  '((fzfa-vc-modified-locally  :narrow m)
    (fzfa-vc-added-files       :narrow a)
    (fzfa-vc-staged-for-commit :narrow s)
    (fzfa-vc-modified-in-head  :narrow h))
  "Commands shown by `fzfa-vcs-any'.
Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key.
The defaults dispatch via `vc-responsible-backend' so the active
VCS backend is picked per project."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

;;;###autoload
(defun fzfa-vcs-any ()
  "Multi-source fuzzy completion over `fzfa-vcs-any-commands'."
  (interactive)
  (fzfa-multi-read fzfa-vcs-any-commands :prompt "vcs?: "))

(provide 'fzfa-vc)
;;; fzfa-vc.el ends here
