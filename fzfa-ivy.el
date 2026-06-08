;;; fzfa-ivy.el --- Ivy frontend for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
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
(defvar ivy--old-cands)
(defvar ivy-re-builders-alist)
(defvar ivy-last)
(declare-function ivy-state-caller "ivy")
(declare-function ivy-state-current "ivy")
(declare-function ivy-state-dynamic-collection "ivy")
(declare-function ivy-configure "ivy")
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")
(declare-function ivy-call "ivy")

(defface fzfa-ivy-multi-source-label
  '((t :inherit font-lock-comment-face))
  "Face for source labels prepended to multi-source ivy candidates."
  :group 'fzfa)

(defun fzfa-ivy--multi-display-transformer (cand)
  "Prepend `[source-name]' to CAND when it carries a fzfa tofu suffix.

Self-gates on `fzfa--multi-active-sources' (bound only inside a
`fzfa--multi-read' session) so non-fzfa `ivy-completing-read'
calls pass through unchanged."
  (let* ((n (length cand))
         (last (and (> n 0) (aref cand (1- n)))))
    (if (and fzfa--multi-active-sources
             last
             (>= last fzfa--tofu-base)
             (< last (+ fzfa--tofu-base (length fzfa--multi-active-sources))))
        (let* ((idx (- last fzfa--tofu-base))
               (name (plist-get (aref fzfa--multi-active-sources idx) :name)))
          (concat (propertize (format "[%s] " (or name "?"))
                              'face 'fzfa-ivy-multi-source-label)
                  cand))
      cand)))

(defun fzfa-ivy--session-p ()
  "Non-nil when the active ivy session belongs to a `fzfa'.

Single-source: `fzfa--session-apply' is let-bound by every `fzfa'
constructor.

Multi-source: `fzfa--multi-active-sources' is let-bound by
`fzfa--multi-read'."
  (or (bound-and-true-p fzfa--session-apply)
      (bound-and-true-p fzfa--multi-active-sources)))

(defun fzfa-ivy--call-advice (orig &rest args)
  "`:around' advice on `ivy-call' for `fzfa' sessions.

In `fzfa' sessions, replace `ivy''s identity action with our `:apply'
dispatch — the source plist's (or constructor's) `:apply' is invoked
on the current candidate without exiting.  All other sessions pass
through unchanged via (apply ORIG ARGS)."
  (if (fzfa-ivy--session-p)
      (fzfa-apply-current)
    (apply orig args)))

(defun fzfa-ivy--restrict-to-matches-backfill (&rest _)
  "Backfill `ivy--old-cands' from `ivy--all-candidates' in dynamic sessions.

`ivy''s dynamic-collection update path maintains `ivy--all-candidates' but
not `ivy--old-cands'; `ivy-restrict-to-matches' reads the latter and
pins everything to it.  Without this backfill, S-SPC
in a dynamic session restricts to a stale `ivy--old-cands' — empty for
fresh async sessions (→ 0 candidates), the initial full list for sync (→
no-op)."
  (when (and (bound-and-true-p ivy-last)
             (ivy-state-dynamic-collection ivy-last)
             ivy--all-candidates
             (not (equal ivy--old-cands ivy--all-candidates)))
    (setq ivy--old-cands ivy--all-candidates)))

(defun fzfa-ivy-setup ()
  "Wire fzfa's ivy integration into the current session."
  (with-eval-after-load 'ivy
    ;; Register under the `t' fallback caller — `ivy-completing-read'
    ;; uses `this-command' as the actual caller (ivy.el:2672), so
    ;; per-fzfa-command registration would be brittle.  The
    ;; transformer self-gates on `fzfa--multi-active-sources' so
    ;; non-fzfa-multi sessions pass through unchanged.
    (ivy-configure t
      :display-transformer-fn #'fzfa-ivy--multi-display-transformer)
    (advice-add 'ivy-restrict-to-matches :before
                #'fzfa-ivy--restrict-to-matches-backfill)
    (advice-add 'ivy-call :around #'fzfa-ivy--call-advice)))

(provide 'fzfa-ivy)
;;; fzfa-ivy.el ends here
