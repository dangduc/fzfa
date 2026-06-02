;;; fzfa-embark.el --- Embark integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching
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
;; Per-target direct bindings:
;;   region / identifier maps (plain commands; target flows into the fzf
;;   filter via embark's minibuffer-injection mechanism + the
;;   `embark--allow-edit' / `embark--unmark-target' pre-action hooks
;;   installed by `fzfa-embark-setup'):
;;     R rg, G grep, A ag, U ugrep, S swiper
;;   file / buffer / email maps (wrappers that switch context — buffer,
;;   directory, or rewrite the search query — based on the target type):
;;     file:    G grep-this-file, R rg-in-file-dir
;;     buffer:  I imenu-in-buffer, S swiper-in-buffer
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
(declare-function embark--unmark-target "embark")
(declare-function embark--allow-edit "embark")

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
(declare-function fzfa-imenu "fzfa-emacs")
(declare-function fzfa-imenu-all "fzfa-emacs")
(declare-function fzfa-outline "fzfa-emacs")
(declare-function fzfa-buffer "fzfa-emacs")
(declare-function fzfa-bookmark "fzfa-emacs")
(declare-function fzfa-mark "fzfa-emacs")
(declare-function fzfa-global-mark "fzfa-emacs")
(declare-function fzfa-yank-pop "fzfa-emacs")
(declare-function fzfa-M-x "fzfa-emacs")
(declare-function fzfa-M-x-for-buffer "fzfa-emacs")
(declare-function fzfa-theme "fzfa-emacs")
(declare-function fzfa-tramp "fzfa-emacs")
(declare-function fzfa-compile-error "fzfa-emacs")
(declare-function fzfa-recent-file "fzfa-emacs")
(declare-function fzfa-project-buffer "fzfa-project")
(declare-function fzfa-project-find-file "fzfa-project")
(declare-function fzfa-info "fzfa-info")
(declare-function fzfa-info-at-point "fzfa-info")
(declare-function fzfa-flymake "fzfa-flymake")
(declare-function fzfa-flymake-project "fzfa-flymake")
(declare-function fzfa-notmuch "fzfa-notmuch")
(declare-function fzfa-notmuch-tree "fzfa-notmuch")
(declare-function fzfa-evil-any "fzfa-evil")
(declare-function fzfa-org-any "fzfa-org")
(declare-function fzfa-passwords "fzfa")
(declare-function fzfa-find-any "fzfa")
(declare-function fzfa-find-some "fzfa")

;;; Target-aware wrappers.
;; These are for targets where the embark target is NOT a filter query —
;; it's a buffer, file path, or email address — and the wrapper has to
;; switch context (current buffer, `default-directory', notmuch query
;; syntax) before invoking the underlying fzfa command.  For region /
;; identifier targets where the target IS the desired fzf filter, no
;; wrapper is needed; embark's pre-action hooks (installed below) inject
;; the target directly into the fzf minibuffer.

(defun fzfa-embark-grep-this-file (file)
  "Grep within FILE via `fzfa-grep-current-file'.
FILE is opened with `find-file-noselect' so the command sees a real
variable `buffer-file-name'."
  (interactive "fFile: ")
  (with-current-buffer (find-file-noselect file)
    (fzfa-grep-current-file)))

(defun fzfa-embark-rg-in-file-dir (file)
  "Run `fzfa-rg' rooted at FILE's parent directory."
  (interactive "fFile: ")
  (let ((default-directory (or (file-name-directory (expand-file-name file))
                               default-directory)))
    (fzfa-rg)))

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
`fzfa-embark-async-search-map' via a composed parent."
  :parent (make-composed-keymap (list fzfa-embark-sync-search-map
                                      fzfa-embark-async-search-map))
  "b" #'fzfa-buffer
  "B" #'fzfa-project-buffer
  "m" #'fzfa-mark
  "M" #'fzfa-global-mark
  "y" #'fzfa-yank-pop
  "h" #'fzfa-info
  "H" #'fzfa-info-at-point
  "e" #'fzfa-compile-error
  "x" #'fzfa-M-x
  "X" #'fzfa-M-x-for-buffer
  "t" #'fzfa-theme
  "k" #'fzfa-bookmark
  "T" #'fzfa-tramp
  "R" #'fzfa-recent-file
  ;; -any / -some multi-source pickers — useful from any target.
  "." #'fzfa-find-any
  "/" #'fzfa-find-some
  "O" #'fzfa-org-any
  "E" #'fzfa-evil-any
  "P" #'fzfa-passwords)

;; Make the keymap symbols `commandp' so embark's default prompter
;; renders them as e.g. "fzfa-embark-search-map" instead of the
;; placeholder "<keymap>".  Same trick embark-consult uses at
;; embark-consult.el:399 / 404.
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

    ;; Per-target direct bindings.  Region / identifier bind the plain
    ;; commands — embark's pre-action hooks (installed below) inject the
    ;; target into the fzf minibuffer.  File / buffer / email use
    ;; wrappers because the target drives context (cwd, current buffer,
    ;; notmuch query syntax) rather than the filter.
    (keymap-set embark-region-map     "R" #'fzfa-rg)
    (keymap-set embark-region-map     "G" #'fzfa-grep)
    (keymap-set embark-region-map     "A" #'fzfa-ag)
    (keymap-set embark-region-map     "U" #'fzfa-ugrep)
    (keymap-set embark-region-map     "S" #'fzfa-swiper)

    (keymap-set embark-identifier-map "R" #'fzfa-rg)
    (keymap-set embark-identifier-map "G" #'fzfa-grep)
    (keymap-set embark-identifier-map "A" #'fzfa-ag)
    (keymap-set embark-identifier-map "U" #'fzfa-ugrep)
    (keymap-set embark-identifier-map "S" #'fzfa-swiper)

    (keymap-set embark-file-map       "G" #'fzfa-embark-grep-this-file)
    (keymap-set embark-file-map       "R" #'fzfa-embark-rg-in-file-dir)

    (keymap-set embark-buffer-map     "I" #'fzfa-embark-imenu-in-buffer)
    (keymap-set embark-buffer-map     "S" #'fzfa-embark-swiper-in-buffer)

    (keymap-set embark-flymake-map    "F" #'fzfa-flymake)
    (keymap-set embark-flymake-map    "P" #'fzfa-flymake-project)

    (keymap-set embark-email-map      "N" #'fzfa-embark-notmuch)
    (keymap-set embark-email-map      "T" #'fzfa-embark-notmuch-tree)

    ;; Hooks on every command reachable from the `Z' prefix — including
    ;; those inherited from the sync and async sub-maps.  `map-keymap'
    ;; does NOT recurse into composed parents, so iterate the three maps
    ;; explicitly.
    ;;
    ;; `embark--unmark-target' goes on `embark-pre-action-hooks' (fires
    ;; before the action runs, in the originating buffer — deactivates
    ;; the region target).  `embark--allow-edit' goes on
    ;; `embark-target-injection-hooks' (fires AFTER `embark''s inject
    ;; closure has inserted the target into the action's minibuffer and
    ;; queued `exit-minibuffer' on `post-command-hook'; this hook
    ;; removes that queued exit so the user can actually see and edit
    ;; the pre-filled filter).  Mirrors embark-consult's split at
    ;; embark-consult.el:407-413.
    (dolist (m (list fzfa-embark-search-map
                     fzfa-embark-sync-search-map
                     fzfa-embark-async-search-map))
      (map-keymap
       (lambda (_key cmd)
         (when (symbolp cmd)
           (cl-pushnew #'embark--unmark-target
                       (alist-get cmd embark-pre-action-hooks))
           (cl-pushnew #'embark--allow-edit
                       (alist-get cmd embark-target-injection-hooks))))
       m))))

(provide 'fzfa-embark)
;;; fzfa-embark.el ends here
