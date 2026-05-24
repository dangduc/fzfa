;;; fzf-async-company.el --- Company-mode interface to `fzf-async' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: company, completion, convenience
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; fzf-async interface to `company-mode'.
;;
;; Loaded automatically when `company' is in `fzf-async-extensions' and
;; `fzf-async-setup' has been called.  Requires the `company' package
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
;;   d  show documentation   (`fzf-async-company-show-doc')
;;   l  show source location (`fzf-async-company-show-location')

;;; Code:

(require 'fzf-async)

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

(defvar fzf-async-company--source-buffer nil
  "Buffer that originated the current `fzf-async-company' session.
Let-bound during the read so the annotation function and embark
actions can call `company-call-backend' in the buffer where the
session is alive — `company-backend' is buffer-local and is nil
inside the minibuffer.")

(defun fzf-async-company--annotate (cand)
  "Return the annotation string company would show for CAND, or nil."
  (when-let* ((buf (or fzf-async-company--source-buffer (current-buffer)))
              ((buffer-live-p buf))
              (ann (with-current-buffer buf
                     (ignore-errors
                       (company-call-backend 'annotation cand)))))
    (concat " " (propertize ann 'face 'completions-annotations))))

;;;###autoload
(defun fzf-async-company ()
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
          (let ((fzf-async-company--source-buffer (current-buffer)))
            (let ((selection (fzf-sync-completing-read
                              :candidates company-candidates
                              :prompt "Company: "
                              :category 'fzf-async-company
                              :annotate #'fzf-async-company--annotate)))
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
(defun fzf-async-company-show-doc (cand)
  "Show the documentation buffer for company candidate CAND."
  (interactive "sCandidate: ")
  (let* ((buf (or fzf-async-company--source-buffer (current-buffer)))
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
(defun fzf-async-company-show-location (cand)
  "Pop to the source location of company candidate CAND."
  (interactive "sCandidate: ")
  (let* ((buf (or fzf-async-company--source-buffer (current-buffer)))
         (loc (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((company-candidates (list cand)))
                    (ignore-errors
                      (company-call-backend 'location cand)))))))
    (unless loc (user-error "No location available for `%s'" cand))
    (let ((target-buf (cond ((bufferp (car loc)) (car loc))
                            ((stringp (car loc)) (find-file-noselect (car loc)))))
          (target (cdr loc)))
      (unless target-buf (user-error "Cannot resolve location for `%s'" cand))
      (pop-to-buffer target-buf)
      (cond ((numberp target)
             (goto-char (point-min))
             (forward-line (1- target)))
            ((markerp target) (goto-char target))))))

(defvar-keymap fzf-async-company-map
  :doc "Embark keymap for `fzf-async-company' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "d" #'fzf-async-company-show-doc
  "l" #'fzf-async-company-show-location)

;;;###autoload
(defun fzf-async-company-setup ()
  "Register the `fzf-async-company' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzf-async-company (styles fzf-async)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzf-async-company
                   fzf-async-company-map embark-general-map))))

(provide 'fzf-async-company)
;;; fzf-async-company.el ends here
