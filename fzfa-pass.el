;;; fzfa-pass.el --- Pass interface to `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: pass, password, convenience
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa interface to `password-store' (pass), modeled on `ivy-pass'.
;;
;; Loaded automatically when `pass' is in `fzfa-extensions' (the
;; default) and `fzfa-setup' has been called.  Requires the
;; `password-store' package to be installed and available on `load-path';
;; it is loaded lazily on first use.
;;
;; M-x fzfa-pass copies the selected entry's password to the kill ring.
;; With embark configured, these actions are available on a candidate:
;;
;;   c  copy password         (`fzfa-pass-copy')
;;   e  edit                  (`fzfa-pass-edit')
;;   d  delete                (`fzfa-pass-delete')
;;   a  add (using as seed)   (`fzfa-pass-add')
;;   r  rename                (`fzfa-pass-rename')
;;   g  generate              (`fzfa-pass-generate')
;;   u  open url field        (`fzfa-pass-url')

;;; Code:

(require 'fzfa)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(declare-function password-store-list      "password-store" (&optional subdir))
(declare-function password-store-dir       "password-store" ())
(declare-function password-store-copy      "password-store" (entry))
(declare-function password-store-edit      "password-store" (entry))
(declare-function password-store-rename    "password-store" (entry new-entry))
(declare-function password-store-remove    "password-store" (entry))
(declare-function password-store-generate  "password-store" (entry &optional password-length))
(declare-function password-store-url       "password-store" (entry))

(defun fzfa-pass--read (prompt)
  "Fuzzy-select a password-store entry with PROMPT."
  (require 'password-store)
  (let ((entries (password-store-list (password-store-dir))))
    (unless entries
      (user-error "No password-store entries found"))
    (fzfa-sync-completing-read
     :candidates entries
     :prompt prompt
     :category 'fzfa-pass)))

;;;###autoload
(defun fzfa-pass-copy (&optional key)
  "Copy the password for KEY to the kill ring.
When KEY is nil (e.g. called interactively), prompt for one."
  (interactive)
  (when-let* ((key (or key (fzfa-pass--read "Copy password: "))))
    (password-store-copy key)))

;;;###autoload
(defalias 'fzfa-pass #'fzfa-pass-copy
  "Default `fzfa-pass' action: copy the password to the kill ring.")

;;;###autoload
(defun fzfa-pass-edit (key)
  "Edit password-store entry KEY."
  (interactive (list (fzfa-pass--read "Edit entry: ")))
  (password-store-edit key))

;;;###autoload
(defun fzfa-pass-rename (key)
  "Rename password-store entry KEY."
  (interactive (list (fzfa-pass--read "Rename entry: ")))
  (password-store-rename
   key (read-string (format "Rename `%s' to: " key) key)))

;;;###autoload
(defun fzfa-pass-delete (key)
  "Delete password-store entry KEY, after confirmation."
  (interactive (list (fzfa-pass--read "Delete entry: ")))
  (when (yes-or-no-p (format "Really delete the entry `%s'? " key))
    (password-store-remove key)))

;;;###autoload
(defun fzfa-pass-add (&optional seed)
  "Add a new password-store entry, optionally seeded by SEED."
  (interactive)
  (require 'password-store)
  (password-store-edit (read-string "New entry: " seed)))

;;;###autoload
(defun fzfa-pass-generate (&optional seed)
  "Generate a new password-store entry, optionally seeded by SEED."
  (interactive)
  (require 'password-store)
  (let ((new (read-string "Generate password for new entry: " seed)))
    (password-store-generate new)
    (password-store-edit new)))

;;;###autoload
(defun fzfa-pass-url (key)
  "Open the url field of password-store entry KEY."
  (interactive (list (fzfa-pass--read "URL of entry: ")))
  (password-store-url key))

(defvar-keymap fzfa-pass-map
  :doc "Embark keymap for `fzfa-pass' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "c" #'fzfa-pass-copy
  "e" #'fzfa-pass-edit
  "d" #'fzfa-pass-delete
  "a" #'fzfa-pass-add
  "r" #'fzfa-pass-rename
  "g" #'fzfa-pass-generate
  "u" #'fzfa-pass-url)

;;;###autoload
(defun fzfa-pass-setup ()
  "Register the `fzfa-pass' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzfa-pass (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzfa-pass fzfa-pass-map embark-general-map))))

(provide 'fzfa-pass)
;;; fzfa-pass.el ends here
