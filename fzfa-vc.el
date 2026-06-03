;;; fzfa-vc.el --- VC dispatcher for `fzfa' -*- lexical-binding: t; -*-

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
;;   `fzfa-vc-modified-files'  Multi-source picker over the current
;;                             repo's modified/added/staged/HEAD files
;;
;; The per-source commands themselves live in the backend extensions
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

(provide 'fzfa-vc)
;;; fzfa-vc.el ends here
