;;; fzfa-imenu.el --- Imenu interface to `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `fzfa' pickers over imenu indices, single-buffer and multi-buffer.
;;
;; Loaded automatically when `imenu' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-imenu'                  Jump to an imenu entry in this buffer
;;   `fzfa-imenu-all'              Jump to an imenu entry across buffers
;;   `fzfa-imenu-all-but-current'  Like `fzfa-imenu-all' but skip current

;;; Code:

(require 'fzfa)
(require 'imenu)
(eval-when-compile (require 'cl-lib))

(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")

(defun fzfa--imenu (scope)
  "Implementation of `fzfa-imenu' / `fzfa-imenu-all'.

SCOPE selects which buffers to walk:
  nil / `current'  — just the current buffer.
  `all'            — every live non-internal buffer.
  `others'         — every live non-internal buffer except the current one.

Display differences:
- Single buffer: display = NAME (with \"(CATEGORY)\" appended on
  cross-category name collision); group header = imenu category.
- Multi buffer:  display = \"[CATEGORY] NAME\" (no collision possible —
  entries are already partitioned by buffer); group header = buffer name."
  (let* ((multi (memq scope '(all others)))
         (buf-vec (vconcat
                   (pcase scope
                     ((or 'all 'others)
                      (cl-remove-if
                       (lambda (b)
                         (or (minibufferp b)
                             (string-prefix-p " " (buffer-name b))
                             (and (eq scope 'others)
                                  (eq b (current-buffer)))))
                       (buffer-list)))
                     (_ (list (current-buffer))))))
         (entries nil)
         (lookup (make-hash-table :test 'equal))
         (groups (make-hash-table :test 'equal)))
    (cl-loop
     for buf across buf-vec
     for i from 0
     for index = (with-current-buffer buf
                   (ignore-errors (imenu--make-index-alist t)))
     when index do
     (cl-labels
         ((walk (alist category)
            (dolist (entry alist)
              (cond
               ((or (null entry) (equal (car entry) "*Rescan*")))
               ((imenu--subalist-p entry)
                (walk (cdr entry) (car entry)))
               (t
                (let* ((name (car entry))
                       (display
                        (if multi
                            (format "%d:%s%s"
                                    i
                                    (if category (format "[%s] " category) "")
                                    name)
                          ;; Disambiguate cross-category name collisions
                          ;; (e.g. an elisp function and variable named foo).
                          (if (and category (gethash name lookup))
                              (format "%s (%s)" name category)
                            name))))
                  (push display entries)
                  (puthash display (cons i entry) lookup)
                  (when (and (not multi) category)
                    (puthash display category groups))))))))
       (walk index nil)))
    (unless entries
      (user-error "No imenu entries%s" (if multi " in any buffer" "")))
    (when-let* ((result
                 (fzfa-sync-completing-read
                  :candidates (nreverse entries)
                  :prompt (pcase scope
                            ('all    "imenu-all: ")
                            ('others "imenu-others: ")
                            (_       "imenu: "))
                  :category 'fzfa-imenu
                  :preview
                  (lambda (cand)
                    (when-let* ((hit (gethash cand lookup))
                                (idx (car hit))
                                ((< idx (length buf-vec)))
                                (buf (aref buf-vec idx))
                                (entry (cdr hit))
                                (val (cdr entry))
                                (pos (cond
                                      ((markerp val) val)
                                      ((numberp val) val)
                                      ((overlayp val) (overlay-start val)))))
                      (fzfa-preview-show buf pos)))
                  :group
                  (lambda (cand transform)
                    (cond
                     ((not multi)
                      (if transform cand (or (gethash cand groups) "")))
                     (transform
                      ;; Strip "IDX:" prefix for display.
                      (when (string-match "^[0-9]+:\\(.*\\)$" cand)
                        (match-string 1 cand)))
                     (t
                      ;; Header: reverse-map IDX → buffer name.
                      (when (string-match "^\\([0-9]+\\):" cand)
                        (let ((i (string-to-number (match-string 1 cand))))
                          (when (< i (length buf-vec))
                            (buffer-name (aref buf-vec i))))))))))
                (hit (gethash result lookup))
                (idx (car hit))
                ((< idx (length buf-vec)))
                (buffer (aref buf-vec idx))
                ((buffer-live-p buffer)))
      (fzfa-with-visit
        (unless (eq buffer (current-buffer))
          (switch-to-buffer buffer))
        (push-mark nil t)
        (imenu (cdr hit))))))

;;;###autoload
(defun fzfa-imenu ()
  "Jump to an imenu entry in the current buffer using fzf."
  (interactive)
  (fzfa--imenu 'current))

;;;###autoload
(defun fzfa-imenu-all ()
  "Jump to an imenu entry across all open buffers using fzf.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzfa--imenu 'all))

;;;###autoload
(defun fzfa-imenu-all-but-current ()
  "Jump to an imenu entry across all open buffers except the current one.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzfa--imenu 'others))

(provide 'fzfa-imenu)
;;; fzfa-imenu.el ends here
