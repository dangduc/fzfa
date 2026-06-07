;;; fzfa-info.el --- Info manual integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa pickers over Info manual indices.  Each manual contributes its
;; index entries as candidates; selection opens the Info viewer at the
;; entry's node.
;;
;; Loaded automatically when `info' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-info-emacs'      Pick from the Emacs manual
;;   `fzfa-info-elisp'      Pick from the Emacs Lisp manual
;;   `fzfa-info-org'        Pick from the Org manual
;;   `fzfa-info-cl'         Pick from the Common Lisp manual
;;   `fzfa-info-eieio'      Pick from the EIEIO manual
;;   `fzfa-info-magit'      Pick from the Magit manual (if installed)
;;   `fzfa-info'            Multi-source pick across `fzfa-info-commands'
;;   `fzfa-info-at-point'   Delegate to `info-lookup-symbol' for the
;;                           symbol at point

;;; Code:

(require 'fzfa)
(require 'info)
(require 'cl-lib)

(declare-function info-lookup-symbol "info-look"
                  (symbol &optional mode same-window))

(defcustom fzfa-info-manuals
  '("emacs" "elisp" "org" "cl" "eieio")
  "Info manuals whose index entries `fzfa-info-*' commands can pick.
Each entry is a manual name as accepted by `Info-find-node'.
Manuals that aren't installed are reported with `user-error' when
their command runs; missing manuals don't prevent other manuals
from working in the multi-source `fzfa-info'."
  :type '(repeat string)
  :group 'fzfa)

(defcustom fzfa-info-commands
  '(fzfa-info-emacs
    fzfa-info-elisp
    fzfa-info-org
    fzfa-info-cl
    fzfa-info-eieio)
  "Commands shown by the multi-source `fzfa-info'.
Each must be a `fzfa-info-MANUAL'-style command that pulls index
entries from a single manual via `fzfa-info--read'."
  :type '(repeat function)
  :group 'fzfa)

(defvar fzfa-info--cache (make-hash-table :test 'equal)
  "Per-manual cache of (ENTRY . NODE) index pairs.
Key is the manual name (string).  Cleared by
`fzfa-info-clear-cache'.")

(defun fzfa-info-clear-cache ()
  "Forget cached index entries; the next pick re-walks each manual.
Useful after installing a new package whose Info manual you want
indexed, or when a manual has changed on disk."
  (interactive)
  (clrhash fzfa-info--cache))

(defun fzfa-info--manual-entries (manual)
  "Return a list of (ENTRY . NODE) index pairs for MANUAL.
Walks every node in `Info-index-nodes' for the manual, parsing
each menu entry.  Result is memoised in `fzfa-info--cache'."
  (or (gethash manual fzfa-info--cache)
      (let (entries err)
        (condition-case e
            (with-temp-buffer
              (Info-mode)
              (Info-find-node manual "Top")
              (dolist (node (Info-index-nodes))
                (Info-goto-node node)
                (goto-char (point-min))
                ;; Info menu line: "* ENTRY: NODE.    description"
                ;; Be permissive about whitespace; reject entries that
                ;; cross newlines so a torn line doesn't fold two
                ;; entries together.
                (while (re-search-forward
                        "^\\* \\([^:\n]+\\): +\\([^.\n]+\\)\\."
                        nil t)
                  (push (cons (string-trim (match-string 1))
                              (string-trim (match-string 2)))
                        entries))))
          (error (setq err e)))
        (when err
          (user-error "Failed to read Info manual %s: %s"
                      manual (error-message-string err)))
        (puthash manual (nreverse entries) fzfa-info--cache))))

(defun fzfa-info--read (manual prompt)
  "Pick an index entry from MANUAL with PROMPT and visit it.
Each candidate is rendered \"ENTRY — NODE\"; on selection
the Info viewer opens at \"(MANUAL)NODE\"."
  (let* ((entries (fzfa-info--manual-entries manual))
         (lookup  (make-hash-table :test 'equal))
         (used    (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for (entry . node) in entries
           for display = (format "%s — %s" entry node)
           collect
           (progn
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display node lookup)
             display))))
    (unless candidates
      (user-error "No index entries in manual %s" manual))
    (when-let* ((r    (fzfa-sync-completing-read
                       :candidates candidates
                       :prompt prompt
                       :category 'fzfa-info))
                (node (gethash r lookup)))
      (fzfa-with-visit (Info-goto-node (format "(%s)%s" manual node))))))

;;;###autoload
(defun fzfa-info-emacs ()
  "Pick an index entry from the Emacs manual using fzf."
  (interactive)
  (fzfa-info--read "emacs" "info-emacs: "))

;;;###autoload
(defun fzfa-info-elisp ()
  "Pick an index entry from the Emacs Lisp manual using fzf."
  (interactive)
  (fzfa-info--read "elisp" "info-elisp: "))

;;;###autoload
(defun fzfa-info-org ()
  "Pick an index entry from the Org manual using fzf."
  (interactive)
  (fzfa-info--read "org" "info-org: "))

;;;###autoload
(defun fzfa-info-cl ()
  "Pick an index entry from the Common Lisp manual using fzf."
  (interactive)
  (fzfa-info--read "cl" "info-cl: "))

;;;###autoload
(defun fzfa-info-eieio ()
  "Pick an index entry from the EIEIO manual using fzf."
  (interactive)
  (fzfa-info--read "eieio" "info-eieio: "))

;;;###autoload
(defun fzfa-info-magit ()
  "Pick an index entry from the Magit manual using fzf."
  (interactive)
  (fzfa-info--read "magit" "info-magit: "))

;;;###autoload
(defun fzfa-info ()
  "Multi-source Info picker across `fzfa-info-commands'.
Each command in the list contributes its manual as a group;
ranking is per-group's top fzf score, recomputed per keystroke."
  (interactive)
  (fzfa-multi-read fzfa-info-commands :prompt "info: "))

;;;###autoload
(defun fzfa-info-at-point ()
  "Look up the symbol at point in its matching Info manual.
Thin wrapper around `info-lookup-symbol' (`C-h S'): the symbol
prompt routes through whatever `completion-styles' the user has
configured globally — fzfa style does not apply, since
`info-lookup-symbol' calls `completing-read' with a sorted list
table and a normal completion category."
  (interactive)
  (require 'info-look)
  (call-interactively #'info-lookup-symbol))

(provide 'fzfa-info)
;;; fzfa-info.el ends here
