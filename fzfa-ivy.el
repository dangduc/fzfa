;;; fzfa-ivy.el --- Ivy frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: matching, completion, ivy
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ivy frontend for `fzfa'.  Loaded automatically when `ivy' is in
;; `fzfa-extensions' and `fzfa-setup' has been called.  Loading this
;; file alone is a no-op; `fzfa-ivy-setup' — called from `fzfa-setup'
;; — defers its work via `with-eval-after-load' on `ivy' so the
;; registrations kick in only when ivy is actually loaded.
;;
;; Single-source fzfa-async and fzfa-sync already handle ivy
;; correctly in fzfa.el itself — they bind
;; `ivy-completing-read-dynamic-collection t' so ivy skips its
;; re-builder filter and trusts the fzf-scored output.  This
;; extension is for the multi-source path, which needs:
;;
;;   - A display transformer so the user can tell which source each
;;     candidate came from (ivy ignores the `group-function'
;;     completion-metadata key that vertico/icomplete consult).
;;
;;   - A push closure that re-scores async sources against `ivy-text'
;;     on each pattern change.  ivy's push model means typing doesn't
;;     re-call the collection function, so without this async sources
;;     stay stuck on the pattern they were first invoked with.
;;

;;; Code:

(require 'cl-lib)
(require 'fzfa)
(require 'ivy nil t)

(defvar ivy-text)
(defvar ivy--all-candidates)
(defvar ivy-re-builders-alist)
(defvar ivy-last)
(declare-function ivy-state-caller "ivy")
(declare-function ivy-set-display-transformer "ivy")
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")

(defun fzfa-ivy-setup ()
  "Wire fzfa's ivy integration into the current session."
  (with-eval-after-load 'ivy
    nil))

(provide 'fzfa-ivy)
;;; fzfa-ivy.el ends here
