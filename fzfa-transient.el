;;; fzfa-transient.el --- Transient menus for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Transient menus for `fzfa'.
;;
;; Loaded automatically when `transient' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Entry points:
;;
;;   M-x fzfa-transient
;;     Top-level menu that adapts to frame width (wide / narrow layout).
;;
;;   M-x fzfa-transient-find, -grep, -vcs, -project, ...
;;     Sub-prefix menus bindable directly.  Each is autoloaded.
;;
;; Layout is data-driven: each menu column is a `defvar' holding a
;; transient suffix-spec vector; the public commands rebuild the
;; underlying prefix on every call so column-vector edits are picked
;; up live.  `transient' is required lazily on first invocation.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar fzfa-directory)
(declare-function transient-args "transient" (prefix))
(declare-function transient-arg-value "transient" (arg args))
;; ARGLIST is `t' (unspecified) — `transient-scope's signature changed
;; between 30.x and the snapshot; check-declare would trip on the
;; mismatch otherwise.
(declare-function transient-scope "transient" t t)
(declare-function transient-prefix-object "transient" ())
(declare-function transient-setup "transient"
                  (&optional name layout edit &rest params))
(declare-function project-current "project")
(declare-function project-root "project")

(defun fzfa-transient--current-scope ()
  "Return the prefix arg to thread through the transient's scope.

If invoked while another transient is active (sub-transient
navigation), inherit its scope.  Otherwise capture the current
`current-prefix-arg' fresh so it propagates to suffix commands via
`transient-scope'."
  (or (and (transient-prefix-object) (transient-scope))
      current-prefix-arg))

;;; Boilerplate-reducing macro

(defmacro fzfa-transient-define-prefix (name docstring &rest groups)
  "Define interactive command NAME opening a transient with DOCSTRING.

Each element of GROUPS is a row in the transient layout — either a list
of column-vector defvar symbols, or a bare symbol (shorthand for a
single-column group).  Each named defvar must hold a vector in
transient's suffix-spec form.

The underlying prefix is interned as NAME--def and re-defined on
every call, so editing a column defvar is reflected on the next
invocation.  `transient' is required lazily inside the body.

The entering `current-prefix-arg' is passed to `transient-setup' as
the `:scope', so suffix commands can retrieve it via `transient-scope'
and thread it through to their target fzfa command."
  (declare (indent 2))
  (let ((def (intern (format "%s--def" name))))
    `(defun ,name ()
       ,docstring
       (interactive)
       (require 'transient)
       (eval
        (list 'transient-define-prefix ',def '() ,docstring
              ,@(mapcar (lambda (g)
                          (if (symbolp g)
                              `(vector ,g)
                            `(vector ,@g)))
                        groups)))
       (transient-setup ',def nil nil :scope (fzfa-transient--current-scope)))))

;;; Dispatch helpers
;;
;; Sub-prefixes (find / grep / vcs) expose `--dir=' / `--backend='
;; infixes; their suffix commands consult the active prefix's
;; `transient-args' so the directory/backend selection propagates.

(defun fzfa-transient--with-dir (dir-spec fn)
  "Invoke FN with `fzfa-directory' bound per DIR-SPEC.
DIR-SPEC is \"current\", \"project\", or anything else (no override)."
  (pcase dir-spec
    ("current"
     (let ((fzfa-directory default-directory))
       (funcall fn)))
    ("project"
     (require 'project)
     (let ((fzfa-directory (when-let* ((pr (project-current)))
                             (project-root pr))))
       (funcall fn)))
    (_ (funcall fn))))

(defun fzfa-transient--invoke (base prefix)
  "Invoke BASE under PREFIX's --dir= infix env.

Rebinds `current-prefix-arg' from `transient-scope' so a prefix arg
supplied when entering the transient reaches BASE through
`call-interactively'."
  (let* ((args (transient-args prefix))
         (dir  (transient-arg-value "--dir=" args))
         (current-prefix-arg (transient-scope)))
    (fzfa-transient--with-dir dir (lambda () (call-interactively base)))))

(defun fzfa-transient--vcs-cmd (op args)
  "Return the fzfa command implementing VCS OP for --backend= in ARGS.
OP is a string like \"modified-locally\".  Backend defaults to \"smart\",
which dispatches via `fzfa-vc-*' (uses `vc-responsible-backend')."
  (let* ((backend (or (transient-arg-value "--backend=" args) "smart"))
         (sym
          (pcase (cons op backend)
            (`("ls-files" . "hg")    'fzfa-hg-files)
            (`("ls-files" . "smart") 'fzfa-vc-modified-files)
            (`(,_ . "smart")         (intern (concat "fzfa-vc-" op)))
            (`(,_ . ,b)              (intern (format "fzfa-%s-%s" b op))))))
    (unless (fboundp sym)
      (user-error "fzfa-transient: %s has no `%s' implementation" backend op))
    sym))

(defun fzfa-transient--vcs-invoke (op)
  "Invoke the VCS command for OP under --backend= / --dir= infixes.

Rebinds `current-prefix-arg' from `transient-scope' so a prefix arg
supplied when entering the transient reaches the target command
through `call-interactively'."
  (let* ((args (transient-args 'fzfa-transient-vcs--def))
         (cmd  (fzfa-transient--vcs-cmd op args))
         (dir  (transient-arg-value "--dir=" args))
         (current-prefix-arg (transient-scope)))
    (fzfa-transient--with-dir dir (lambda () (call-interactively cmd)))))

(defmacro fzfa-transient--def-dispatch (name base prefix)
  "Define NAME as a command invoking BASE under PREFIX's infix env."
  `(defun ,name ()
     (interactive)
     (fzfa-transient--invoke ',base ',prefix)))

(defmacro fzfa-transient--def-vcs (name op)
  "Define NAME as a command invoking VCS OP under active backend."
  `(defun ,name ()
     (interactive)
     (fzfa-transient--vcs-invoke ,op)))

;; Find suffix dispatchers (read fzfa-transient-find--def's infixes).
(fzfa-transient--def-dispatch fzfa-transient--find-smart  fzfa-smart-find  fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-fd     fzfa-fd          fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-rg     fzfa-rg-files    fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-ag     fzfa-ag-files    fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-hg     fzfa-hg-files    fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-find   fzfa-find        fzfa-transient-find--def)
(fzfa-transient--def-dispatch fzfa-transient--find-hungry fzfa-hungry-find fzfa-transient-find--def)

;; Grep suffix dispatchers.
(fzfa-transient--def-dispatch fzfa-transient--grep-smart fzfa-smart-grep fzfa-transient-grep--def)
(fzfa-transient--def-dispatch fzfa-transient--grep-rg    fzfa-rg         fzfa-transient-grep--def)
(fzfa-transient--def-dispatch fzfa-transient--grep-ugrep fzfa-ugrep      fzfa-transient-grep--def)
(fzfa-transient--def-dispatch fzfa-transient--grep-ag    fzfa-ag         fzfa-transient-grep--def)
(fzfa-transient--def-dispatch fzfa-transient--grep-grep  fzfa-grep       fzfa-transient-grep--def)
(fzfa-transient--def-dispatch fzfa-transient--grep-git   fzfa-git-grep   fzfa-transient-grep--def)

;; VCS suffix dispatchers (read fzfa-transient-vcs--def's infixes).
(fzfa-transient--def-vcs fzfa-transient--vcs-ls       "ls-files")
(fzfa-transient--def-vcs fzfa-transient--vcs-modified "modified-locally")
(fzfa-transient--def-vcs fzfa-transient--vcs-added    "added-files")
(fzfa-transient--def-vcs fzfa-transient--vcs-staged   "staged-for-commit")
(fzfa-transient--def-vcs fzfa-transient--vcs-in-head  "modified-in-head")

;;; Sub-prefix column vectors

(defvar fzfa-transient---find-backends nil)
(defvar fzfa-transient---find-options nil)
(defvar fzfa-transient---grep-backends nil)
(defvar fzfa-transient---grep-options nil)
(defvar fzfa-transient---vcs-operations nil)
(defvar fzfa-transient---vcs-options nil)
(defvar fzfa-transient---project nil)
(defvar fzfa-transient---search nil)
(defvar fzfa-transient---shell nil)
(defvar fzfa-transient---marks-marks nil)
(defvar fzfa-transient---marks-evil nil)
(defvar fzfa-transient---code-imenu nil)
(defvar fzfa-transient---code-misc nil)
(defvar fzfa-transient---org nil)
(defvar fzfa-transient---mail nil)
(defvar fzfa-transient---music nil)
(defvar fzfa-transient---evil nil)
(defvar fzfa-transient---chrome nil)
(defvar fzfa-transient---firefox nil)
(defvar fzfa-transient---safari nil)
(defvar fzfa-transient---passwords-pass nil)
(defvar fzfa-transient---passwords-chrome nil)
(defvar fzfa-transient---passwords-multi nil)
(defvar fzfa-transient---info nil)
(defvar fzfa-transient---replay nil)

;;; Top-level column vectors

(defvar fzfa-transient---multi nil)
(defvar fzfa-transient---find-files nil)
(defvar fzfa-transient---grep nil)
(defvar fzfa-transient---sources nil)
(defvar fzfa-transient---system nil)
(defvar fzfa-transient---emacs nil)
(defvar fzfa-transient---swiper nil)
(defvar fzfa-transient---code nil)
(defvar fzfa-transient---web nil)
(defvar fzfa-transient---flymake nil)
(defvar fzfa-transient---apps nil)

;;; Sub-prefix column contents

(setq fzfa-transient---find-backends
      ["Backends"
       ("f"   "Smart"             fzfa-transient--find-smart)
       ("d"   "Fd"                fzfa-transient--find-fd)
       ("r"   "Rg files"          fzfa-transient--find-rg)
       ("a"   "Ag files"          fzfa-transient--find-ag)
       ("h"   "Hg files"          fzfa-transient--find-hg)
       ("F"   "Find (POSIX)"      fzfa-transient--find-find)
       ("n"   "Hungry Find"       fzfa-transient--find-hungry)
       ("P"   "Project Find File" fzfa-project-find-file)
       ("SPC" "Find Any"          fzfa-find-any)])

(setq fzfa-transient---find-options
      ["Options"
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq fzfa-transient---grep-backends
      ["Backends"
       ("g" "Smart"             fzfa-transient--grep-smart)
       ("R" "Rg"                fzfa-transient--grep-rg)
       ("u" "Ugrep"             fzfa-transient--grep-ugrep)
       ("A" "Ag"                fzfa-transient--grep-ag)
       ("j" "Grep"              fzfa-transient--grep-grep)
       ("G" "Git Grep"          fzfa-transient--grep-git)
       ("F" "Grep Current File" fzfa-grep-current-file)])

(setq fzfa-transient---grep-options
      ["Options"
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq fzfa-transient---vcs-operations
      ["Operations"
       ("l" "Ls / files"       fzfa-transient--vcs-ls)
       ("m" "Modified locally" fzfa-transient--vcs-modified)
       ("a" "Added"            fzfa-transient--vcs-added)
       ("s" "Staged"           fzfa-transient--vcs-staged)
       ("h" "In HEAD"          fzfa-transient--vcs-in-head)
       ("G" "Git Grep"         fzfa-git-grep)
       ("L" "Git Log Grep"     fzfa-git-log-grep)
       ("v" "VCS Any"          fzfa-vc-any)])

(setq fzfa-transient---vcs-options
      ["Options"
       ("-b" "Backend"   "--backend="
        :choices ("smart" "git" "hg"))
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq fzfa-transient---project
      ["Project"
       ("f" "Find File"      fzfa-project-find-file)
       ("d" "Find Dir"       fzfa-project-find-dir)
       ("b" "Buffer"         fzfa-project-buffer)
       ("e" "Recent File"    fzfa-project-recentf)
       ("p" "Switch Project" fzfa-project-switch-project)])

(setq fzfa-transient---search
      ["Search"
       ("l" "Locate"          fzfa-locate)
       ("s" "Spotlight"       fzfa-spotlight)
       ("S" "Spotlight Apps"  fzfa-spotlight-apps)
       ("m" "Spotlight Audio" fzfa-spotlight-audio)
       ("t" "Tramp"           fzfa-tramp)
       ("T" "SSH"             fzfa-ssh)])

(setq fzfa-transient---shell
      ["Shell / Make"
       ("c" "Shell Command"         fzfa-shell-command)
       ("C" "Project Shell Command" fzfa-shell-project-command)
       ("H" "Shell History"         fzfa-shell-history)
       ("M" "Make"                  fzfa-make)
       ("R" "Make: Reset Cache"     fzfa-make-reset-cache)])

(setq fzfa-transient---marks-marks
      ["Marks"
       ("m" "Mark"        fzfa-mark)
       ("M" "Global Mark" fzfa-global-mark)
       ("r" "Register"    fzfa-register)])

(setq fzfa-transient---marks-evil
      ["Evil"
       ("e" "Evil Marks"     fzfa-evil-marks)
       ("E" "Evil Registers" fzfa-evil-registers)
       ("j" "Evil Jumps"     fzfa-evil-jumps)])

(setq fzfa-transient---code-imenu
      ["Imenu"
       ("i" "Imenu"         fzfa-imenu)
       ("I" "Imenu (All)"   fzfa-imenu-all)
       ("o" "Imenu Others"  fzfa-imenu-all-but-current)
       ("e" "Eglot Symbols" fzfa-eglot-symbols)])

(setq fzfa-transient---code-misc
      ["Outline / Errors / Company"
       ("u" "Outline"        fzfa-outline)
       ("c" "Compile Errors" fzfa-compile-error)
       ("C" "Company"        fzfa-company)])

(setq fzfa-transient---org
      ["Org"
       ("h"   "Heading"       fzfa-org-heading)
       ("H"   "Heading (All)" fzfa-org-heading-all)
       ("a"   "Agenda"        fzfa-org-agenda)
       ("t"   "Todo"          fzfa-org-todo)
       ("f"   "Files"         fzfa-org-files)
       ("g"   "Tags View"     fzfa-org-tags-view)
       ("G"   "Grep"          fzfa-org-grep)
       ("l"   "Insert Link"   fzfa-org-insert-link)
       ("m"   "Files (mdfind)" fzfa-org-mdfind-files)
       ("M"   "Grep (mdfind)"  fzfa-org-mdfind-grep)
       ("SPC" "Org Any"       fzfa-org-any)])

(setq fzfa-transient---mail
      ["Mail"
       ("m" "Mail"           fzfa-mail)
       ("r" "Refresh Mail"   fzfa-mail-refresh)
       ("n" "Notmuch"        fzfa-notmuch)
       ("N" "Notmuch (Tree)" fzfa-notmuch-tree)])

(setq fzfa-transient---music
      ["Music"
       ("m" "Music"              fzfa-music)
       ("a" "By Artist"          fzfa-music-by-artist)
       ("g" "By Genre"           fzfa-music-by-genre)
       ("p" "Playlist"           fzfa-music-playlist)
       ("P" "Playlist (Shuffle)" fzfa-music-playlist-shuffle)
       ("r" "Refresh"            fzfa-music-refresh)])

(setq fzfa-transient---evil
      ["Evil"
       ("m"   "Marks"          fzfa-evil-marks)
       ("r"   "Registers"      fzfa-evil-registers)
       ("j"   "Jumps"          fzfa-evil-jumps)
       (":"   "Ex History"     fzfa-evil-ex-history)
       ("/"   "Search History" fzfa-evil-search-history)
       ("q"   "Command Window" fzfa-evil-command-window)
       ("SPC" "Evil Any"       fzfa-evil-any)])

(setq fzfa-transient---chrome
      ["Chrome"
       ("b" "Bookmarks"          fzfa-chrome-bookmarks)
       ("e" "Edit Bookmark"      fzfa-chrome-edit)
       ("c" "Copy Bookmark URL"  fzfa-chrome-bookmark-copy-url)
       ("h" "History"            fzfa-chrome-history)
       ("H" "Copy History URL"   fzfa-chrome-history-copy-url)
       ("R" "Refresh"            fzfa-chrome-refresh)])

(setq fzfa-transient---firefox
      ["Firefox"
       ("b" "Bookmarks"          fzfa-firefox-bookmarks)
       ("c" "Copy Bookmark URL"  fzfa-firefox-bookmark-copy-url)
       ("h" "History"            fzfa-firefox-history)
       ("H" "Copy History URL"   fzfa-firefox-history-copy-url)
       ("R" "Refresh"            fzfa-firefox-refresh)])

(setq fzfa-transient---safari
      ["Safari"
       ("b" "Bookmarks"          fzfa-safari-bookmarks)
       ("c" "Copy Bookmark URL"  fzfa-safari-bookmark-copy-url)
       ("h" "History"            fzfa-safari-history)
       ("H" "Copy History URL"   fzfa-safari-history-copy-url)
       ("R" "Refresh"            fzfa-safari-refresh)])

(setq fzfa-transient---passwords-pass
      ["Pass"
       ("c" "Copy"     fzfa-pass-copy)
       ("e" "Edit"     fzfa-pass-edit)
       ("a" "Add"      fzfa-pass-add)
       ("g" "Generate" fzfa-pass-generate)
       ("u" "URL"      fzfa-pass-url)
       ("r" "Rename"   fzfa-pass-rename)
       ("D" "Delete"   fzfa-pass-delete)])

(setq fzfa-transient---passwords-chrome
      ["Chrome Pass"
       ("C" "Copy"     fzfa-chrome-pass-copy)
       ("U" "Username" fzfa-chrome-pass-copy-username)
       ("l" "URL"      fzfa-chrome-pass-url)])

(setq fzfa-transient---passwords-multi
      ["Multi"
       ("M-p" "All Passwords" fzfa-passwords)])

(setq fzfa-transient---info
      ["Info"
       ("i" "Any node"     fzfa-info)
       ("?" "At point"     fzfa-info-at-point)
       ("m" "Man page"     fzfa-man)
       ("E" "Elisp manual" fzfa-info-elisp)
       ("M" "Emacs manual" fzfa-info-emacs)
       ("c" "CL"           fzfa-info-cl)
       ("e" "EIEIO"        fzfa-info-eieio)
       ("o" "Org"          fzfa-info-org)
       ("g" "Magit"        fzfa-info-magit)])

(setq fzfa-transient---replay
      ["Replay"
       ("," "Last session"     fzfa-replay)
       ("." "Any session"      fzfa-replay-any)
       ("/" "From persisted"   fzfa-replay-from-file)])

;;; Top-level columns

(setq fzfa-transient---multi
      ["Multi"
       ("SPC" "Find Any"      fzfa-find-any)
       ("/"   "Find Some"     fzfa-find-some)
       ("v"   "VCS Any"       fzfa-vc-any)
       ("e"   "Evil Any"      fzfa-evil-any)
       ("E"   "Evil »"        fzfa-transient-evil)
       ("'"   "Replay »"      fzfa-transient-replay)
       ("M-p" "All Passwords" fzfa-passwords)])

(setq fzfa-transient---find-files
      ["Find Files"
       ("f" "Find (smart)" fzfa-smart-find)
       ("F" "Find »"      fzfa-transient-find)
       ("b" "Buffer"       fzfa-buffer)
       ("r" "Recent File"  fzfa-recent-file)
       ("a" "FFAP menu"    fzfa-ffap-menu)])

(setq fzfa-transient---grep
      ["Grep"
       ("g" "Grep (smart)" fzfa-smart-grep)
       ("G" "Grep »"      fzfa-transient-grep)])

(setq fzfa-transient---sources
      ["Sources"
       ("p" "Project »" fzfa-transient-project)
       ("V" "VCS »"     fzfa-transient-vcs)])

(setq fzfa-transient---system
      ["System"
       ("!" "Shell/Make »" fzfa-transient-shell)
       ("L" "Locate/etc »" fzfa-transient-search)])

(setq fzfa-transient---emacs
      ["Emacs"
       ("B" "Bookmark"          fzfa-bookmark)
       ("y" "Yank Pop"          fzfa-yank-pop)
       ("T" "Theme"             fzfa-theme)
       ("t" "Font"              fzfa-font)
       ("x" "M-x"               fzfa-M-x)
       ("X" "M-x (mode)"        fzfa-M-x-for-buffer)
       ("s" "Symbol (apropos)"  fzfa-apropos)
       ("k" "Key (descbinds)"   fzfa-descbinds)
       (";" "Complex Command"   fzfa-complex-command)
       ("h" "History"           fzfa-history)
       ("u" "Unicode char"      fzfa-unicode-char)])

(setq fzfa-transient---swiper
      ["Swiper"
       ("w" "Swiper"        fzfa-swiper)
       ("W" "Swiper All"    fzfa-swiper-all)
       ("n" "Hungry Swiper" fzfa-hungry-swiper)
       ("R" "Regexp"        fzfa-regexp)])

(setq fzfa-transient---code
      ["Code"
       ("i" "Imenu/Outline »" fzfa-transient-code)
       ("m" "Marks/Reg »"     fzfa-transient-marks)])

(setq fzfa-transient---web
      ["Web"
       ("C"   "Chrome »"    fzfa-transient-chrome)
       ("M-f" "Firefox »"   fzfa-transient-firefox)
       ("M-s" "Safari »"    fzfa-transient-safari)
       ("P"   "Passwords »" fzfa-transient-passwords)])

(setq fzfa-transient---flymake
      ["Flymake / Info"
       ("z" "Flymake"         fzfa-flymake)
       ("Z" "Flymake Project" fzfa-flymake-project)
       ("I" "Info »"          fzfa-transient-info)
       ("?" "Info at Point"   fzfa-info-at-point)])

(setq fzfa-transient---apps
      ["Apps"
       ("o" "Org »"   fzfa-transient-org)
       ("M" "Mail »"  fzfa-transient-mail)
       ("N" "Music »" fzfa-transient-music)])

;;; Sub-prefix entry points

;;;###autoload (autoload 'fzfa-transient-find "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-find
    "Fuzzy find files."
  (fzfa-transient---find-backends fzfa-transient---find-options))

;;;###autoload (autoload 'fzfa-transient-grep "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-grep
    "Fuzzy content search."
  (fzfa-transient---grep-backends fzfa-transient---grep-options))

;;;###autoload (autoload 'fzfa-transient-vcs "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-vcs
    "VCS operations.  Backend defaults to smart auto-dispatch."
  (fzfa-transient---vcs-operations fzfa-transient---vcs-options))

;;;###autoload (autoload 'fzfa-transient-project "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-project
    "Project commands."
  fzfa-transient---project)

;;;###autoload (autoload 'fzfa-transient-search "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-search
    "System search."
  fzfa-transient---search)

;;;###autoload (autoload 'fzfa-transient-shell "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-shell
    "Shell / Make."
  fzfa-transient---shell)

;;;###autoload (autoload 'fzfa-transient-marks "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-marks
    "Marks & Registers."
  (fzfa-transient---marks-marks fzfa-transient---marks-evil))

;;;###autoload (autoload 'fzfa-transient-code "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-code
    "Code navigation."
  (fzfa-transient---code-imenu fzfa-transient---code-misc))

;;;###autoload (autoload 'fzfa-transient-org "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-org
    "Org commands."
  fzfa-transient---org)

;;;###autoload (autoload 'fzfa-transient-mail "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-mail
    "Mail."
  fzfa-transient---mail)

;;;###autoload (autoload 'fzfa-transient-music "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-music
    "Apple Music."
  fzfa-transient---music)

;;;###autoload (autoload 'fzfa-transient-evil "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-evil
    "Evil."
  fzfa-transient---evil)

;;;###autoload (autoload 'fzfa-transient-chrome "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-chrome
    "Chrome."
  fzfa-transient---chrome)

;;;###autoload (autoload 'fzfa-transient-firefox "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-firefox
    "Firefox."
  fzfa-transient---firefox)

;;;###autoload (autoload 'fzfa-transient-safari "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-safari
    "Safari."
  fzfa-transient---safari)

;;;###autoload (autoload 'fzfa-transient-passwords "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-passwords
    "Passwords."
  (fzfa-transient---passwords-pass
   fzfa-transient---passwords-chrome
   fzfa-transient---passwords-multi))

;;;###autoload (autoload 'fzfa-transient-info "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-info
    "Info manuals."
  fzfa-transient---info)

;;;###autoload (autoload 'fzfa-transient-replay "fzfa-transient" nil t)
(fzfa-transient-define-prefix fzfa-transient-replay
    "Replay saved sessions."
  fzfa-transient---replay)

;;; Top-level entry points

(fzfa-transient-define-prefix fzfa-transient--wide
    "fzfa"
  (fzfa-transient---multi
   fzfa-transient---find-files
   fzfa-transient---grep
   fzfa-transient---sources
   fzfa-transient---system
   fzfa-transient---emacs)
  (fzfa-transient---swiper
   fzfa-transient---code
   fzfa-transient---web
   fzfa-transient---flymake
   fzfa-transient---apps))

(fzfa-transient-define-prefix fzfa-transient--narrow
    "fzfa"
  (fzfa-transient---multi
   fzfa-transient---find-files
   fzfa-transient---grep
   fzfa-transient---sources)
  (fzfa-transient---system
   fzfa-transient---emacs
   fzfa-transient---swiper
   fzfa-transient---code)
  (fzfa-transient---web
   fzfa-transient---flymake
   fzfa-transient---apps))

;;;###autoload
(defun fzfa-transient ()
  "Open the fzfa transient menu.

Picks the wide or narrow layout based on the current frame's
share of the display width — wide on a maximised / full-width
frame, narrow on a half-screen frame."
  (interactive)
  (if (> (/ (float (frame-pixel-width)) (display-pixel-width)) 0.5)
      (fzfa-transient--wide)
    (fzfa-transient--narrow)))

(provide 'fzfa-transient)
;;; fzfa-transient.el ends here
