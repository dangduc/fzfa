;;; fzfa-org.el --- Org-mode integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.1
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa pickers for `org-mode' — heading navigation, agenda jump,
;; TODO list, tags-view, link insertion, plus a multi-source picker
;; over the first four for a single-prompt "show me everything in my
;; org universe" experience.
;;
;; Loaded automatically when `org' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Requires the Emacs built-in `org'
;; package; no external soft dependency.
;;
;; Commands:
;;   `fzfa-org-heading'      Jump to a heading in the current org buffer
;;   `fzfa-org-heading-all'  Jump to a heading across all open org buffers
;;   `fzfa-org-agenda'       Jump to a heading in `org-agenda-files'
;;   `fzfa-org-todo'         Jump to a TODO-state heading in agenda files
;;   `fzfa-org-tags-view'    Pick a tag, then pick an entry with that tag
;;   `fzfa-org-insert-link'  Pick a heading; insert an org-link at point
;;   `fzfa-org-any'          Multi-source over `fzfa-org-any-commands'

;;; Code:

(require 'fzfa)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))   ; `when-let*' macro expansion only

(defvar org-done-keywords)

(declare-function org-map-entries "org"
                  (func &optional match scope &rest skip))
(declare-function org-get-heading "org"
                  (&optional no-tags no-todo no-priority no-comment))
(declare-function org-current-level "org")
(declare-function org-get-todo-state "org")
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-agenda-files "org" (&optional unrestricted archives))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-show-context "org" (&optional key))

(defcustom fzfa-org-any-commands
  '(fzfa-org-heading
    fzfa-org-heading-all
    fzfa-org-agenda
    fzfa-org-todo)
  "Commands shown by the multi-source `fzfa-org-any'.
Each entry must be an interactive command that funnels through
`fzfa-org--read'.  Two-step commands (`fzfa-org-tags-view') and
non-jump commands (`fzfa-org-insert-link') are deliberately
excluded — they don't compose cleanly under the jump-oriented
multi flow."
  :type '(repeat function)
  :group 'fzfa)

(defun fzfa-org--format-display ()
  "Format the current heading as a `SOURCE:LINE:CONTENT' display string.
CONTENT carries the heading at its native depth (leading stars), the
TODO state when set, the heading text itself, and `:tag1:tag2:'-style
tags appended.  Called from inside `org-map-entries' on a heading line."
  (let* ((source  (or (buffer-file-name) (buffer-name)))
         (line    (line-number-at-pos))
         (heading (substring-no-properties
                   (org-get-heading nil nil t t)))
         (state   (or (org-get-todo-state) ""))
         (tags    (org-get-tags))
         (level   (or (org-current-level) 1))
         (stars   (make-string level ?*))
         (state-s (if (string= state "") "" (concat state " ")))
         (tags-s  (if tags
                      (format " :%s:" (mapconcat #'identity tags ":"))
                    "")))
    (format "%s:%d:%s %s%s%s" source line stars state-s heading tags-s)))

(defun fzfa-org--reveal ()
  "Reveal the entry around point if folded.
Tolerates org-fold renames across Emacs versions."
  (cond
   ((fboundp 'org-fold-show-context) (org-fold-show-context))
   ((fboundp 'org-show-context)      (org-show-context))))

(defun fzfa-org--org-buffers ()
  "Return all live buffers whose major mode derives from `org-mode'."
  (cl-remove-if-not
   (lambda (b) (with-current-buffer b (derived-mode-p 'org-mode)))
   (buffer-list)))

(defun fzfa-org--collect (scope &optional match predicate)
  "Walk SCOPE collecting (DISPLAY . MARKER) pairs for org headings.

SCOPE is one of:
  a list of buffers — walked one at a time via `with-current-buffer';
  the symbol `agenda' — walks every file in `org-agenda-files'.

MATCH, when non-nil, is forwarded as the second argument to
`org-map-entries' (agenda match syntax: tags, properties, TODO
filters).

PREDICATE, when non-nil, is called at each heading; the heading is
included only when PREDICATE returns non-nil.  Stacks with MATCH —
PREDICATE filters from within the callback after MATCH has narrowed."
  (let (entries)
    (cl-flet ((capture ()
                (when (or (null predicate) (funcall predicate))
                  (push (cons (fzfa-org--format-display) (point-marker))
                        entries))))
      (cond
       ((listp scope)
        (dolist (buf scope)
          (with-current-buffer buf
            (org-map-entries #'capture match nil))))
       ((eq scope 'agenda)
        (require 'org-agenda)
        (org-map-entries #'capture match 'agenda))))
    (nreverse entries)))

(defun fzfa-org--all-tags (scope)
  "Return a sorted unique list of tags across SCOPE.
SCOPE accepts the same values as `fzfa-org--collect'."
  (let ((tags (make-hash-table :test 'equal)))
    (cl-flet ((capture ()
                (dolist (tag (org-get-tags nil t))
                  (puthash tag t tags))))
      (cond
       ((listp scope)
        (dolist (buf scope)
          (with-current-buffer buf
            (org-map-entries #'capture nil nil))))
       ((eq scope 'agenda)
        (require 'org-agenda)
        (org-map-entries #'capture nil 'agenda))))
    (let (keys)
      (maphash (lambda (k _v) (push k keys)) tags)
      (sort keys #'string<))))

(defun fzfa-org--jump (marker)
  "Switch to MARKER's buffer, move point, push the prior point, reveal."
  (push-mark nil t)
  (let ((buf (marker-buffer marker)))
    (when (buffer-live-p buf)
      (fzfa-with-visit
        (switch-to-buffer buf)
        (goto-char marker)
        (fzfa-org--reveal)))))

(defun fzfa-org--read (entries prompt &optional action)
  "Present ENTRIES via fzf with PROMPT; ACTION on the chosen marker.
ENTRIES is a list of (DISPLAY . MARKER) pairs.  ACTION is a function
of one argument (the marker); defaults to `fzfa-org--jump'."
  (let* ((lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for (display . marker) in entries
           collect
           (progn
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display marker lookup)
             display))))
    (unless candidates
      (user-error "No matching org headings"))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt prompt
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group))
                (m (gethash r lookup))
                ((markerp m)))
      (funcall (or action #'fzfa-org--jump) m))))

(defun fzfa-org--ensure-agenda-files ()
  "Signal a user-error when no `org-agenda-files' are configured."
  (require 'org-agenda)
  (unless (org-agenda-files t t)
    (user-error "`org-agenda-files' is empty")))

;;;###autoload
(defun fzfa-org-heading ()
  "Jump to a heading in the current `org-mode' buffer using fzf.
Candidates show SOURCE:LINE:STARS [TODO] HEADING :tags:.  Selection
moves point to the heading, pushing the prior position onto the mark
ring; the entry is revealed if folded."
  (interactive)
  (require 'org)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (fzfa-org--read (fzfa-org--collect (list (current-buffer)))
                  "org-heading: "))

;;;###autoload
(defun fzfa-org-heading-all ()
  "Jump to a heading across all live `org-mode' buffers using fzf.
Walks every buffer where `derived-mode-p' reports `org-mode'.  Buffers
without an associated file are walked too — the candidate's SOURCE
falls back to the buffer name and the jump still resolves via the
captured marker."
  (interactive)
  (require 'org)
  (let ((bufs (fzfa-org--org-buffers)))
    (unless bufs (user-error "No `org-mode' buffers open"))
    (fzfa-org--read (fzfa-org--collect bufs) "org-heading-all: ")))

;;;###autoload
(defun fzfa-org-agenda ()
  "Jump to a heading across `org-agenda-files' using fzf.
Files not currently visited are loaded by `org-map-entries' as needed."
  (interactive)
  (require 'org)
  (fzfa-org--ensure-agenda-files)
  (fzfa-org--read (fzfa-org--collect 'agenda) "org-agenda: "))

;;;###autoload
(defun fzfa-org-todo ()
  "Jump to a TODO-state heading across `org-agenda-files' using fzf.
The PREDICATE step in `fzfa-org--collect' filters to headings with
a non-nil `org-get-todo-state', covering any keyword the user has
configured in `org-todo-keywords' (TODO, NEXT, WAITING, …) but
excluding DONE-class keywords."
  (interactive)
  (require 'org)
  (fzfa-org--ensure-agenda-files)
  (fzfa-org--read (fzfa-org--collect
                   'agenda nil
                   (lambda ()
                     (let ((s (org-get-todo-state)))
                       (and s (not (member s org-done-keywords))))))
                  "org-todo: "))

;;;###autoload
(defun fzfa-org-tags-view ()
  "Pick a tag, then pick a heading carrying that tag, via fzf.
Tags and headings are sourced from `org-agenda-files'.  Two-step
flow: the first prompt picks one tag from the deduplicated tag set;
the second prompt picks an entry filtered to that tag via an
`+TAG' match string passed to `org-map-entries'."
  (interactive)
  (require 'org)
  (fzfa-org--ensure-agenda-files)
  (let ((tags (fzfa-org--all-tags 'agenda)))
    (unless tags
      (user-error "No tags found across agenda files"))
    (when-let* ((tag (fzfa-sync-completing-read
                      :candidates tags
                      :prompt "tag: "
                      :category 'fzfa-misc)))
      (fzfa-org--read
       (fzfa-org--collect 'agenda (concat "+" tag))
       (format "org-tag[%s]: " tag)))))

(defun fzfa-org--insert-link (marker)
  "Insert an org link to MARKER's heading at point in the current buffer."
  (let* ((buf (marker-buffer marker))
         (data
          (and buf (buffer-live-p buf)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char marker)
                   (cons (substring-no-properties
                          (org-get-heading t t t t))
                         (buffer-file-name)))))))
    (when data
      (let ((heading (car data))
            (file    (cdr data)))
        (insert
         (if file
             (format "[[file:%s::*%s][%s]]" file heading heading)
           (format "[[*%s][%s]]" heading heading)))))))

;;;###autoload
(defun fzfa-org-insert-link ()
  "Pick an org heading and insert an org-link to it at point.
Walks all live `org-mode' buffers (`fzfa-org-heading-all'-style) to
gather candidates so links can target unsaved scratch org buffers
as well as file-backed ones.  Link target uses `file:PATH::*HEADING'
when the source has a file, `*HEADING' otherwise."
  (interactive)
  (require 'org)
  (let ((bufs (fzfa-org--org-buffers)))
    (unless bufs (user-error "No `org-mode' buffers open"))
    (fzfa-org--read (fzfa-org--collect bufs)
                    "org-insert-link: "
                    #'fzfa-org--insert-link)))

;;;###autoload
(defun fzfa-org-any ()
  "Multi-source pick across `fzfa-org-any-commands'.
Each command in the list contributes one group; per-group rank is
recomputed on every keystroke per the standard `fzfa-multi-read'
algorithm.  Commands whose buffer/file scope is empty are silently
skipped (e.g. `fzfa-org-heading' drops when not in an org buffer)."
  (interactive)
  (require 'org)
  (fzfa-multi-read fzfa-org-any-commands :prompt "org-any: "))

(provide 'fzfa-org)
;;; fzfa-org.el ends here
