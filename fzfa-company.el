;;; fzfa-company.el --- Company-mode interface to `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa interface to `company-mode'.
;;
;; Loaded automatically when `company' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Requires the `company' package
;; to be installed and available on `load-path'; it is loaded lazily
;; on first use.
;;
;; In modal-editing states where point sits on a character rather than
;; past it (any evil state besides insert/emacs), the prefix at point
;; is empty and no backend matches.  We move forward by one before
;; starting the session — the prefix `company-complete' would see right
;; after evil's `a' (append) command — and reverse the move on abort.
;;
;; With embark configured, these actions are available on a candidate:
;;
;;   d  show documentation   (`fzfa-company-show-doc')
;;   l  show source location (`fzfa-company-show-location')

;;; Code:

(require 'fzfa)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(defvar company-candidates)
(defvar company-backend)
(defvar evil-state)

(declare-function company-mode         "company")
(declare-function company-manual-begin "company")
(declare-function company-finish       "company")
(declare-function company-abort        "company")
(declare-function company-call-backend "company" (command &rest args))
(declare-function evil-insert-state    "evil-states")
(declare-function evil-change-state    "evil-core")

(defvar fzfa-company--source-buffer nil
  "Buffer that originated the current `fzfa-company' session.

Let-bound during the read so the annotation function and embark
actions can call `company-call-backend' in the buffer where the
session is alive — `company-backend' is buffer-local and is nil
inside the minibuffer.")

(defun fzfa-company--annotate (cand)
  "Return the annotation string company would show for CAND, or nil."
  (when-let* ((buf (or fzfa-company--source-buffer (current-buffer)))
              ((buffer-live-p buf))
              (ann (with-current-buffer buf
                     (ignore-errors
                       (company-call-backend 'annotation cand)))))
    (concat " " (propertize ann 'face 'completions-annotations))))

;;;###autoload
(defun fzfa-company ()
  "Fuzzy-select from `company-mode' candidates and complete the selection.

In modal-editing states where point sits on a character (e.g. evil
normal state), both point position *and* the buffer's
`completion-at-point-functions' context need to look like insert
state for backends to fire — capf in particular returns a nil prefix
from non-insert states even after a manual `forward-char'.  So when
invoked from an evil non-insert state, switch into evil insert state
the way `evil-append' would (advance point by one, then enter insert
state), populate candidates, and run the read.  The originating evil
state is always restored on exit (success or abort); on abort, the
original point is restored as well."
  (interactive)
  (require 'company)
  (unless (bound-and-true-p company-mode)
    (company-mode 1))
  (let* ((origin (point))
         (orig-state (and (boundp 'evil-state) evil-state))
         (in-modal (and orig-state
                        (not (memq orig-state '(insert emacs)))))
         (moved nil)
         (state-changed nil)
         (started nil)
         (finished nil))
    (unwind-protect
        (progn
          (when in-modal
            (when (and (not (eobp))
                       (or (looking-at-p "\\sw") (looking-at-p "\\s_")))
              (forward-char 1)
              (setq moved t))
            (when (fboundp 'evil-insert-state)
              (evil-insert-state)
              (setq state-changed t)))
          (unless company-candidates
            (setq started t)
            (let ((this-command this-command))
              (company-manual-begin)))
          (unless company-candidates
            (user-error "No company candidates available"))
          (let ((fzfa-company--source-buffer (current-buffer)))
            (let ((selection (fzfa-completing-read
                              :candidates company-candidates
                              :prompt "Company: "
                              :category 'fzfa-company
                              :annotate #'fzfa-company--annotate)))
              (when (and selection (not (string-empty-p selection)))
                (company-finish (substring-no-properties selection))
                (setq finished t)))))
      (unless finished
        (when (and started company-candidates)
          (ignore-errors (company-abort)))
        (when moved (goto-char origin)))
      (when (and state-changed (fboundp 'evil-change-state))
        (evil-change-state orig-state)))))

;;;###autoload
(defun fzfa-company-show-doc (cand)
  "Show the documentation buffer for company candidate CAND."
  (interactive "sCandidate: ")
  (let* ((buf (or fzfa-company--source-buffer (current-buffer)))
         (doc (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((company-candidates (list cand)))
                    (ignore-errors
                      (company-call-backend 'doc-buffer cand)))))))
    (unless doc (user-error "No doc-buffer for `%s'" cand))
    (let ((b (if (consp doc) (car doc) doc)))
      (with-current-buffer b (goto-char (point-min)))
      (display-buffer b))))

;;;###autoload
(defun fzfa-company-show-location (cand)
  "Pop to the source location of company candidate CAND."
  (interactive "sCandidate: ")
  (let* ((buf (or fzfa-company--source-buffer (current-buffer)))
         (loc (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((company-candidates (list cand)))
                    (ignore-errors
                      (company-call-backend 'location cand)))))))
    (unless loc (user-error "No location available for `%s'" cand))
    (let ((target-buf (cond
                       ((bufferp (car loc)) (car loc))
                       ((stringp (car loc)) (find-file-noselect (car loc)))))
          (target (cdr loc)))
      (unless target-buf (user-error "Cannot resolve location for `%s'" cand))
      (pop-to-buffer target-buf)
      (cond ((numberp target)
             (goto-char (point-min))
             (forward-line (1- target)))
            ((markerp target) (goto-char target))))))

(defvar-keymap fzfa-company-map
  :doc "Embark keymap for `fzfa-company' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "d" #'fzfa-company-show-doc
  "l" #'fzfa-company-show-location)

;;;###autoload
(defun fzfa-company-setup ()
  "Register the `fzfa-company' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzfa-company (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzfa-company
                   fzfa-company-map embark-general-map))))

(provide 'fzfa-company)
;;; fzfa-company.el ends here
