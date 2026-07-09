;;; fzfa-org.el --- Org-mode integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.1
;; Package-Requires: ((emacs "29.1"))
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
;;   `fzfa-org-grep'         Grep `.org' content across `fzfa-org-directories'
;;   `fzfa-org-files'        Find `.org' files across `fzfa-org-directories'
;;   `fzfa-org-mdfind-files' Find `.org' files via macOS Spotlight (mdfind)
;;   `fzfa-org-mdfind-grep'  Grep across `.org' files discovered via mdfind
;;   `fzfa-org-any'          Multi-source over `fzfa-org-any-commands'

;;; Code:

(require 'fzfa)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))   ; `when-let*' macro expansion only

(defvar org-done-keywords)
(defvar org-directory)

(declare-function org-map-entries "org"
                  (func &optional match scope &rest skip))
(declare-function org-get-heading "org"
                  (&optional no-tags no-todo no-priority no-comment))
(declare-function org-current-level "org")
(declare-function org-get-todo-state "org")
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-agenda-files "org" (&optional unrestricted archives))
(declare-function org-fold-show-context "org-fold" (&optional key))

(defcustom fzfa-org-directories nil
  "Directories `fzfa-org-grep' and `fzfa-org-files' search.

Each entry is expanded and shell-quoted, then appended after the
base command produced by `fzfa-org-grep-command-function' or
`fzfa-org-files-command-function'.  When nil, falls back to a
one-element list containing `org-directory'."
  :type '(repeat directory)
  :group 'fzfa)

(defcustom fzfa-org-grep-command-function
  (lambda ()
    (cond
     ((executable-find "rg")
      (format
       "rg --line-number --no-heading --with-filename -g '*.org' %s ''"
       (fzfa--max-columns-flag 'rg)))
     ((executable-find "ag")
      (format
       "ag --nocolor --nogroup --line-number -G '\\.org$' %s \".\""
       (fzfa--max-columns-flag 'ag)))
     ((executable-find "ugrep")
      (format "ugrep -RIn --no-heading --include='*.org' %s ''"
              (fzfa--max-columns-flag 'ugrep)))
     (t "grep -Rn --include='*.org' ''")))
  "Function returning the base grep command for `fzfa-org-grep'.

Called with no args; must return a shell command string that streams
FILE:LINE:CONTENT when appended with one or more shell-quoted
directory paths.  Called at command time so `executable-find' picks
up whichever backend is installed today, not at load."
  :type 'function
  :group 'fzfa)

(defcustom fzfa-org-files-command-function
  (lambda ()
    (cond
     ((executable-find "rg") "rg --files -g '*.org'")
     ((executable-find "fd") "fd --type f --extension org --color=never .")
     ((executable-find "ag") "ag -g '\\.org$'")
     (t (user-error
         (concat "No `rg', `fd', or `ag' found; "
                 "customize `fzfa-org-files-command-function'")))))
  "Function returning the base find-files command for `fzfa-org-files'.

Called with no args; must return a shell command string that emits
one file path per line when appended with one or more shell-quoted
directory paths.  Called at command time so `executable-find' picks
up whichever backend is installed today, not at load.

`find' is not tried by default because it wants paths BEFORE
predicates; users who need it can substitute a lambda that
composes the paths in the correct position."
  :type 'function
  :group 'fzfa)

(defcustom fzfa-org-mdfind-directories nil
  "Directories `fzfa-org-mdfind-files' scopes via `mdfind -onlyin'.

Nil (the default) searches the whole Spotlight index.  Distinct from
`fzfa-org-directories' because the Spotlight command is most useful
when it can see files OUTSIDE the user's known org roots — that's the
whole point of running it instead of `fzfa-org-files'."
  :type '(repeat directory)
  :group 'fzfa)

(defcustom fzfa-org-mdfind-query "kMDItemFSName == \"*.org\"c"
  "Spotlight query used by `fzfa-org-mdfind-files'.

Default matches any file whose name ends in `.org' (case-insensitive
via the trailing `c' flag).  Passed verbatim to `mdfind' — the caller
shell-quotes it."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-org-any-commands
  (append
   '(fzfa-org-heading
     fzfa-org-heading-all
     fzfa-org-agenda
     fzfa-org-todo)
   (if (eq system-type 'darwin)
       '(fzfa-org-mdfind-files
         fzfa-org-mdfind-grep)
     '(fzfa-org-files
       fzfa-org-grep)))
  "Commands shown by the multi-source `fzfa-org-any'.

Each entry must be a jump-oriented interactive command that reaches
`fzfa-completing-read' — either via `fzfa-org--read' (heading pickers)
or via a `:command' shell source (`fzfa-org-grep', `fzfa-org-files',
`fzfa-org-mdfind-files', `fzfa-org-mdfind-grep').  Two-step commands
\(`fzfa-org-tags-view') and non-jump commands (`fzfa-org-insert-link')
are deliberately excluded — they don't compose cleanly under the
jump-oriented multi flow.

The default picks Spotlight-backed sources on macOS (system-wide,
indexed, near-instant) and directory-walking sources everywhere else.
Users can override to include both flavors."
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
   ((fboundp 'org-show-context)      (funcall 'org-show-context))))

(defun fzfa-org--org-buffers ()
  "Return all live buffers whose major mode derives from `org-mode'."
  (cl-remove-if-not
   (lambda (b) (with-current-buffer b (derived-mode-p 'org-mode)))
   (buffer-list)))

(defun fzfa-org--collect (scope &optional match predicate)
  "Walk SCOPE collecting (DISPLAY . MARKER) pairs for org headings.

SCOPE is one of:
  a list of buffers — walked one at a time via `with-current-buffer';
  the symbol `agenda' — walks every file in variable `org-agenda-files'.

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
    (when-let* ((r (fzfa-completing-read
                    :candidates candidates
                    :prompt prompt
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group))
                (m (gethash r lookup))
                ((markerp m)))
      (funcall (or action #'fzfa-org--jump) m))))

(defun fzfa-org--ensure-agenda-files ()
  "Signal a user-error when no variable `org-agenda-files' are configured."
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
  "Jump to a heading across variable `org-agenda-files' using fzf.

Files not currently visited are loaded by `org-map-entries' as needed."
  (interactive)
  (require 'org)
  (fzfa-org--ensure-agenda-files)
  (fzfa-org--read (fzfa-org--collect 'agenda) "org-agenda: "))

;;;###autoload
(defun fzfa-org-todo ()
  "Jump to a TODO-state heading across variable `org-agenda-files'.

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

Tags and headings are sourced from variable `org-agenda-files'.  Two-step
flow: the first prompt picks one tag from the deduplicated tag set;
the second prompt picks an entry filtered to that tag via an
`+TAG' match string passed to `org-map-entries'."
  (interactive)
  (require 'org)
  (fzfa-org--ensure-agenda-files)
  (let ((tags (fzfa-org--all-tags 'agenda)))
    (unless tags
      (user-error "No tags found across agenda files"))
    (when-let* ((tag (fzfa-completing-read
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

(defun fzfa-org--directories ()
  "Return the effective directory list for `fzfa-org-grep'/`fzfa-org-files'.

`fzfa-org-directories' when non-nil; otherwise `org' is loaded so
`org-directory' is bound, and its value is returned as a one-element
list.  Signals when neither is set."
  (or fzfa-org-directories
      (progn (require 'org)
             (and (bound-and-true-p org-directory)
                  (list org-directory)))
      (user-error
       "Set `fzfa-org-directories' or `org-directory'")))

(defun fzfa-org--append-dirs (base dirs)
  "Return BASE followed by shell-quoted, tilde-expanded DIRS.

`expand-file-name' runs before `shell-quote-argument' — single-quoted
`~/org' is NOT tilde-expanded by the shell, so the subprocess would
otherwise receive a literal `~/org'."
  (concat base " "
          (mapconcat (lambda (d)
                       (shell-quote-argument (expand-file-name d)))
                     dirs " ")))

;;;###autoload
(defun fzfa-org-grep ()
  "Grep across `fzfa-org-directories' for `.org' content.

Base command comes from `fzfa-org-grep-command-function' (picks the
first available of rg/ag/ugrep/grep and applies an `.org' glob
filter); directories are shell-quoted and appended.  Output is
FILE:LINE:CONTENT; selecting a candidate opens the file at that line."
  (interactive)
  (let ((cmd (fzfa-org--append-dirs
              (funcall fzfa-org-grep-command-function)
              (fzfa-org--directories))))
    (when-let* ((r (fzfa-completing-read
                    :command cmd
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group)))
      (fzfa-visit-grep r))))

;;;###autoload
(defun fzfa-org-files ()
  "Find `.org' files across `fzfa-org-directories'.

Base command comes from `fzfa-org-files-command-function' (picks the
first available of rg/fd/ag in files-only mode with an `.org' filter);
directories are shell-quoted and appended.  Selecting a candidate opens
the file."
  (interactive)
  (let ((cmd (fzfa-org--append-dirs
              (funcall fzfa-org-files-command-function)
              (fzfa-org--directories))))
    (when-let* ((r (fzfa-completing-read
                    :command cmd
                    :prompt "org-files: ")))
      (fzfa-visit-file r))))

(defun fzfa-org--mdfind-command ()
  "Return the shell command string that streams `.org' paths via `mdfind'.

Chains one `mdfind -onlyin' per `fzfa-org-mdfind-directories', joined
with `;', or a single unrestricted `mdfind' when the list is nil."
  (let ((query (shell-quote-argument fzfa-org-mdfind-query)))
    (if fzfa-org-mdfind-directories
        (mapconcat
         (lambda (dir)
           (format "mdfind -onlyin %s %s"
                   (shell-quote-argument (expand-file-name dir))
                   query))
         fzfa-org-mdfind-directories
         "; ")
      (concat "mdfind " query))))

;;;###autoload
(defun fzfa-org-mdfind-files ()
  "Find `.org' files via macOS Spotlight (`mdfind').

Query comes from `fzfa-org-mdfind-query'.  When
`fzfa-org-mdfind-directories' is non-nil, one `mdfind -onlyin' call is
chained per directory; otherwise the whole Spotlight index is searched.

Non-macOS systems have no `mdfind' — the executable check inside
`fzfa-completing-read' surfaces that as a `user-error'."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :command (fzfa-org--mdfind-command)
                  :prompt "org-mdfind: ")))
    (fzfa-visit-file r)))

;;;###autoload
(defun fzfa-org-mdfind-grep ()
  "Grep across `.org' files discovered via macOS Spotlight (`mdfind').

Pipeline: `mdfind' produces the `.org' file list; `tr' NUL-terminates
each path so filenames containing spaces or newlines survive; `xargs
-0' hands the batch to the base grep command produced by
`fzfa-org-grep-command-function'.  macOS `xargs' does not invoke the
utility on empty input, so an empty mdfind result yields an empty
candidate list rather than a hung `rg' waiting on stdin.

The base grep's `-g '*.org'' filter still applies harmlessly — every
mdfind hit already has that extension."
  (interactive)
  (let ((cmd (format "{ %s; } | tr '\\n' '\\0' | xargs -0 %s"
                     (fzfa-org--mdfind-command)
                     (funcall fzfa-org-grep-command-function))))
    (when-let* ((r (fzfa-completing-read
                    :command cmd
                    :prompt "org-mdfind-grep: "
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group
                    ;; First token of the pipeline is `{', not a real
                    ;; executable.  `mdfind' failures (e.g. off macOS)
                    ;; surface as empty output, which `xargs' handles
                    ;; by not invoking the grep tool.
                    :skip-executable-check t)))
      (fzfa-visit-grep r))))

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
