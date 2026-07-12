;;; fzfa-embark.el --- Embark integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Embark integration for fzfa.
;;
;; Loaded automatically when `embark' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  The wiring (sub-keymap on
;; `embark-general-map' under the `Z' prefix plus a handful of
;; target-specific bindings) only runs after the `embark' feature is
;; loaded.
;;
;; Global prefix: `Z' on `embark-general-map' opens the full fzfa
;; action palette regardless of target type.
;;
;; Per-target direct bindings.  Keys are chosen to avoid clobbering
;; embark defaults — notably `S' (`embark-collect'), `A'
;; (`embark-act-all' / `align-regexp'), and `R' (`repunctuate-sentences'
;; on region, `byte-recompile-directory' on file).  The
;; backend-specific bindings (rg/grep/ag/ugrep) collapse into a single
;; `G' = `fzfa-smart-grep' that dispatches to whichever backend is on
;; PATH; the full per-backend palette stays reachable via `Z'.
;;
;;   region / identifier maps (plain command; target flows into the fzf
;;   filter via embark's minibuffer-injection mechanism + the
;;   `embark--allow-edit' / `embark--unmark-target' pre-action hooks
;;   installed by `fzfa-embark-setup'):
;;     G smart-grep
;;   file / buffer / email maps (wrappers that switch context — buffer
;;   or rewrite the search query — based on the target type):
;;     file:    G grep-this-file
;;     buffer:  I imenu-in-buffer, J swiper-in-buffer
;;     email:   N notmuch, T notmuch-tree
;;   flymake map (plain commands; act as re-picks):
;;     F flymake, P flymake-project

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar embark-general-map)
(defvar embark-region-map)
(defvar embark-identifier-map)
(defvar embark-file-map)
(defvar embark-buffer-map)
(defvar embark-flymake-map)
(defvar embark-email-map)
(defvar embark-become-match-map)
(defvar embark-become-file+buffer-map)
(defvar embark-pre-action-hooks)
(defvar embark-target-injection-hooks)
(defvar embark-exporters-alist)
(defvar embark-transformer-alist)
(declare-function embark--unmark-target "embark")
(declare-function embark--allow-edit "embark")
(declare-function grep-mode "grep" t t)

;; The wrappers and keymaps reference commands defined in sibling
;; fzfa extension files.  Those files autoload their commands, so the
;; references resolve when invoked; declare them here only to keep
;; byte-compile quiet.
(declare-function fzfa-rg "fzfa-rg")
(declare-function fzfa-grep "fzfa-grep")
(declare-function fzfa-grep-current-file "fzfa-grep")
(declare-function fzfa-ag "fzfa-ag")
(declare-function fzfa-ugrep "fzfa-ugrep")
(declare-function fzfa-git-grep "fzfa-git")
(declare-function fzfa-find "fzfa-find")
(declare-function fzfa-fd "fzfa-fd")
(declare-function fzfa-locate "fzfa-locate")
(declare-function fzfa-spotlight "fzfa-spotlight")
(declare-function fzfa-swiper "fzfa-emacs")
(declare-function fzfa-swiper-all "fzfa-emacs")
(declare-function fzfa-imenu "fzfa-imenu")
(declare-function fzfa-imenu-all "fzfa-imenu")
(declare-function fzfa-outline "fzfa-emacs")
(declare-function fzfa-buffer "fzfa-emacs")
(declare-function fzfa-bookmark "fzfa-emacs")
(declare-function fzfa-M-x "fzfa-emacs")
(declare-function fzfa-M-x-for-buffer "fzfa-emacs")
(declare-function fzfa-recent-file "fzfa-emacs")
(declare-function fzfa-project-buffer "fzfa-project")
(declare-function fzfa-flymake "fzfa-flymake")
(declare-function fzfa-flymake-project "fzfa-flymake")
(declare-function fzfa-notmuch "fzfa-notmuch")
(declare-function fzfa-notmuch-tree "fzfa-notmuch")

;;; Target-aware wrappers.
;; These are for targets where the embark target is NOT a filter query —
;; it's a buffer, file path, or email address — and the wrapper has to
;; switch context (current buffer, `default-directory', notmuch query
;; syntax) before invoking the underlying fzfa command.  For region /
;; identifier targets where the target IS the desired fzf filter, no
;; wrapper is needed; embark's pre-action hooks (installed below) inject
;; the target directly into the fzf minibuffer.

(defun fzfa--embark-resolve (type target)
  "Embark transformer: return (TYPE . ABSOLUTE-PATH) for TARGET."
  (cons type (fzfa-resolve-candidate target (fzfa--current-session))))

(defun fzfa-embark-grep-this-file (file)
  "Grep within FILE via `fzfa-grep-current-file'.

FILE is opened with `find-file-noselect' so the command sees a real
variable `buffer-file-name'."
  (interactive "fFile: ")
  (with-current-buffer (find-file-noselect file)
    (fzfa-grep-current-file)))

(defun fzfa-embark-imenu-in-buffer (buffer)
  "Run `fzfa-imenu' in BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer (get-buffer buffer)
    (fzfa-imenu)))

(defun fzfa-embark-swiper-in-buffer (buffer)
  "Run `fzfa-swiper' in BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer (get-buffer buffer)
    (fzfa-swiper)))

(defun fzfa-embark-notmuch (email)
  "Run `fzfa-notmuch' for messages from or to EMAIL."
  (interactive "sEmail: ")
  (fzfa-notmuch (format "from:%s OR to:%s" email email)))

(defun fzfa-embark-notmuch-tree (email)
  "Run `fzfa-notmuch-tree' for messages from or to EMAIL."
  (interactive "sEmail: ")
  (fzfa-notmuch-tree (format "from:%s OR to:%s" email email)))

;;; Exporters.

(defun fzfa-embark-export-location (cands)
  "Embark exporter for `fzfa-location' candidates CANDS.

Emit SOURCE:LINE:CAND lines in a fresh `grep-mode' buffer so RET jumps
to the hit and `wgrep' can edit hits in place.  SOURCE comes from each
element's `fzfa-location' text property; file paths navigate, buffer-only
sources (swiper on a non-file buffer) render but don't click through."
  (require 'grep)
  (let ((buf (generate-new-buffer "*Embark Export Location*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (dolist (c cands)
          (when-let* ((loc (and (stringp c) (> (length c) 0)
                                (get-text-property 0 'fzfa-location c))))
            (insert (format "%s:%d:%s\n" (car loc) (cdr loc) c)))))
      (goto-char (point-min))
      (grep-mode))
    (pop-to-buffer buf)))

;;; Sub-keymaps.

(defvar-keymap fzfa-embark-sync-search-map
  :doc "Sync (in-Emacs) fzfa pickers.
Reused as the `Z' sub-map on `embark-become-match-map'."
  "l" #'fzfa-swiper
  "L" #'fzfa-swiper-all
  "i" #'fzfa-imenu
  "I" #'fzfa-imenu-all
  "o" #'fzfa-outline)

(defvar-keymap fzfa-embark-async-search-map
  :doc "Async (shell-backed) fzfa pickers."
  "g" #'fzfa-grep
  "r" #'fzfa-rg
  "a" #'fzfa-ag
  "u" #'fzfa-ugrep
  "G" #'fzfa-git-grep
  "f" #'fzfa-find
  "d" #'fzfa-fd
  "F" #'fzfa-locate
  "s" #'fzfa-spotlight)

(defvar-keymap fzfa-embark-search-map
  :doc "Top-level fzfa palette.
Bound to `Z' on `embark-general-map' so every embark target gets a
prefix into the full set.  Inherits `fzfa-embark-sync-search-map' and
`fzfa-embark-async-search-map' via a composed parent.

Only commands whose candidate set can plausibly contain the embark
target live here — the target is injected as the initial filter via
`embark--allow-edit'.  Launcher commands that ignore the target
(marks, yank-pop, themes, narrow multi-source dispatchers like
`fzfa-org-any', …) are intentionally absent; reach them via M-x or a
global keybinding instead."
  :parent (make-composed-keymap (list fzfa-embark-sync-search-map
                                      fzfa-embark-async-search-map))
  "b" #'fzfa-buffer
  "B" #'fzfa-project-buffer
  "k" #'fzfa-bookmark
  "R" #'fzfa-recent-file
  "x" #'fzfa-M-x
  "X" #'fzfa-M-x-for-buffer
  ;; General-purpose multi-source pickers — target pre-fills as the
  ;; cross-source filter query.
  "." #'fzfa-find-any
  "/" #'fzfa-find-some)

;; Make the keymap symbols `commandp' so embark's default prompter
;; renders them as e.g. "fzfa-embark-search-map" instead of the
;; placeholder "<keymap>".
(fset 'fzfa-embark-sync-search-map  fzfa-embark-sync-search-map)
(fset 'fzfa-embark-async-search-map fzfa-embark-async-search-map)
(fset 'fzfa-embark-search-map       fzfa-embark-search-map)

;;; Setup

;;;###autoload
(defun fzfa-embark-setup ()
  "Install fzfa actions on embark keymaps.

Idempotent — safe to call more than once."
  (with-eval-after-load 'embark
    ;; Bind by quoted symbol (not keymap value) so embark's default
    ;; prompter shows "fzfa-embark-search-map" instead of "<keymap>".
    ;; The keymap symbols are fset above to their keymap values, which
    ;; makes them `commandp' for embark's purposes.

    ;; Global prefix: `Z' on `embark-general-map' inherits to every target.
    (keymap-set embark-general-map "Z" 'fzfa-embark-search-map)

    ;; Become contexts: sync sub-map on match-become, file/buffer commands
    ;; on file+buffer-become.
    (keymap-set embark-become-match-map "Z" 'fzfa-embark-sync-search-map)
    (keymap-set embark-become-file+buffer-map "Z b" #'fzfa-buffer)
    (keymap-set embark-become-file+buffer-map "Z B" #'fzfa-project-buffer)
    (keymap-set embark-become-file+buffer-map "Z f" #'fzfa-find)

    ;; Per-target direct bindings.  Region / identifier bind a single
    ;; smart-grep dispatcher — embark's pre-action hooks (installed
    ;; below) inject the target into the fzf minibuffer.  File / buffer
    ;; / email use wrappers because the target drives context (buffer,
    ;; notmuch query syntax) rather than the filter.  Keys are chosen
    ;; to avoid colliding with embark defaults (S=collect, A=act-all,
    ;; R=repunctuate/byte-recompile).
    (keymap-set embark-region-map     "G" #'fzfa-smart-grep)
    (keymap-set embark-identifier-map "G" #'fzfa-smart-grep)

    (keymap-set embark-file-map       "G" #'fzfa-embark-grep-this-file)

    (keymap-set embark-buffer-map     "I" #'fzfa-embark-imenu-in-buffer)
    (keymap-set embark-buffer-map     "J" #'fzfa-embark-swiper-in-buffer)

    (keymap-set embark-flymake-map    "F" #'fzfa-flymake)
    (keymap-set embark-flymake-map    "P" #'fzfa-flymake-project)

    (keymap-set embark-email-map      "N" #'fzfa-embark-notmuch)
    (keymap-set embark-email-map      "T" #'fzfa-embark-notmuch-tree)

    ;; Exporter for `fzfa-location' so `embark-export' produces a
    ;; grep-mode buffer (wgrep-compatible) instead of falling through
    ;; to `embark-collect's plain list.
    (setf (alist-get 'fzfa-location embark-exporters-alist)
          #'fzfa-embark-export-location)

    ;; Transformer: resolve every fzfa candidate to an absolute path
    ;; before the action fires.  Built-in (`find-file',
    ;; `embark-dired-jump', `delete-file'), third-party, and unknown
    ;; actions all receive the absolute path with no per-action wiring.
    (dolist (cat '(fzfa-file fzfa-multi fzfa-grep fzfa-location))
      (setf (alist-get cat embark-transformer-alist)
            #'fzfa--embark-resolve))

    ;; Install target-injection hooks on the palette commands.  Two
    ;; flavors:
    ;;
    ;;  - Search sub-maps (rg/grep/swiper/imenu/…) treat the target as
    ;;    an editable fzf query → `embark--allow-edit' cancels the
    ;;    queued `exit-minibuffer' so the user can refine it.  Also
    ;;    applied to the smart dispatchers since they're bound
    ;;    directly on per-target maps.
    ;;
    ;;  - Top-level pickers (buffer, bookmark, recent-file, M-x, the
    ;;    multi-source `find-any' / `find-some') treat the target as
    ;;    the initial filter input — `embark--allow-edit' cancels the
    ;;    queued `exit-minibuffer' so the user can either RET on a
    ;;    pre-filtered match or backspace to widen.  Without a hook,
    ;;    embark auto-submits the injected target, and when it doesn't
    ;;    match a candidate the picker exits before `:apply' /
    ;;    `:return' can fire — the command appears to do nothing.
    ;;
    ;; `map-keymap' does not recurse into composed parents, so iterate
    ;; each sub-map directly.
    (dolist (cmd (let (cmds)
                   (dolist (m (list fzfa-embark-sync-search-map
                                    fzfa-embark-async-search-map))
                     (map-keymap
                      (lambda (_key c) (when (symbolp c) (push c cmds)))
                      m))
                   (append '(fzfa-smart-grep fzfa-smart-find) cmds)))
      (cl-pushnew #'embark--unmark-target
                  (alist-get cmd embark-pre-action-hooks))
      (cl-pushnew #'embark--allow-edit
                  (alist-get cmd embark-target-injection-hooks)))
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (cl-pushnew #'embark--unmark-target
                     (alist-get cmd embark-pre-action-hooks))
         (cl-pushnew #'embark--allow-edit
                     (alist-get cmd embark-target-injection-hooks))))
     fzfa-embark-search-map)))

(provide 'fzfa-embark)
;;; fzfa-embark.el ends here
