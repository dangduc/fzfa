;;; fzfa-flymake.el --- Flymake interface to `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa interface to Flymake diagnostics.
;;
;; Loaded automatically when `flymake' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.
;;
;; Commands:
;;   `fzfa-flymake'          Jump to a diagnostic in the current buffer.
;;   `fzfa-flymake-project'  Jump to a diagnostic in any buffer of the
;;                           current project.

;;; Code:

(require 'fzfa)
(eval-when-compile (require 'cl-lib))

(declare-function project-current "project")
(declare-function flymake--project-diagnostics "flymake")
(declare-function flymake--severity "flymake")
(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake-diagnostic-buffer "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function flymake-reporting-backends "flymake")

(defun fzfa-flymake--collect (diags)
  "Walk DIAGS and return a list of (BUFFER LINE TYPE TEXT MARKER) tuples.

Diagnostics whose buffer has been killed are dropped."
  (delq nil
        (mapcar
         (lambda (diag)
           (let ((buffer (flymake-diagnostic-buffer diag))
                 (type (flymake-diagnostic-type diag)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (flymake-diagnostic-beg diag))
                     (list (buffer-name buffer)
                           (line-number-at-pos)
                           type
                           (flymake-diagnostic-text diag)
                           (point-marker))))))))
         diags)))

(defun fzfa-flymake--candidates (diags)
  "Return (CANDIDATES . LOOKUP) for DIAGS.

CANDIDATES is a list of pre-formatted display strings sorted by buffer,
severity (descending), then position.  LOOKUP is a hash mapping each
display string to its source-buffer marker."
  (let ((items (fzfa-flymake--collect diags)))
    (unless items
      (user-error "No flymake diagnostics (Status: %s)"
                  (if (seq-difference (flymake-running-backends)
                                      (flymake-reporting-backends))
                      'running 'finished)))
    (let* ((sorted
            (sort items
                  (pcase-lambda (`(,b1 _ ,t1 _ ,m1) `(,b2 _ ,t2 _ ,m2))
                    (let ((s1 (flymake--severity t1))
                          (s2 (flymake--severity t2)))
                      (or (string-lessp b1 b2)
                          (and (string-equal b1 b2)
                               (or (> s1 s2)
                                   (and (= s1 s2)
                                        (< (marker-position m1)
                                           (marker-position m2))))))))))
           (buffer-width (cl-loop for x in sorted maximize (length (nth 0 x))))
           (line-width (cl-loop for x in sorted
                                maximize (length (number-to-string (nth 1 x)))))
           (fmt (format "%%-%ds %%-%dd %%-7s %%s" buffer-width line-width))
           (lookup (make-hash-table :test 'equal))
           (candidates
            (mapcar
             (pcase-lambda (`(,buffer ,line ,type ,text ,marker))
               (let* ((type-name (format "%s"
                                         (flymake--lookup-type-property
                                          type 'flymake-type-name type)))
                      (face (flymake--lookup-type-property
                             type 'mode-line-face 'flymake-error))
                      (display (format fmt buffer line
                                       (propertize type-name 'face face)
                                       text)))
                 (while (gethash display lookup)
                   (setq display (concat display " ")))
                 (puthash display marker lookup)
                 display))
             sorted)))
      (cons candidates lookup))))

(defun fzfa-flymake--group (lookup)
  "Return a group function partitioning candidates by source buffer.

LOOKUP is the display→marker hash returned by `fzfa-flymake--candidates'."
  (lambda (cand transform)
    (if transform
        cand
      (when-let* ((m (gethash cand lookup))
                  ((markerp m))
                  (buf (marker-buffer m)))
        (buffer-name buf)))))

(defun fzfa-flymake--read (diags prompt)
  "Prompt for one of DIAGS via fzf and jump to it.

PROMPT is the minibuffer prompt string."
  (let* ((pair (fzfa-flymake--candidates diags))
         (candidates (car pair))
         (lookup (cdr pair)))
    (when-let* ((result (fzfa-completing-read
                         :candidates candidates
                         :prompt prompt
                         :category 'fzfa-flymake
                         :group (fzfa-flymake--group lookup)
                         :preview
                         (lambda (cand)
                           (when-let* ((m (gethash cand lookup))
                                       ((markerp m))
                                       (buf (marker-buffer m)))
                             (fzfa-preview-show buf m)))))
                (marker (gethash result lookup))
                ((markerp marker))
                (buffer (marker-buffer marker))
                ((buffer-live-p buffer)))
      (fzfa-with-visit
        (unless (eq buffer (current-buffer))
          (switch-to-buffer buffer))
        (push-mark nil t)
        (goto-char (marker-position marker))))))

;;;###autoload
(defun fzfa-flymake ()
  "Jump to a flymake diagnostic in the current buffer."
  (interactive)
  (require 'flymake)
  (fzfa-flymake--read (flymake-diagnostics) "flymake: "))

;;;###autoload
(defun fzfa-flymake-project ()
  "Jump to a flymake diagnostic from any buffer in the current project."
  (interactive)
  (require 'flymake)
  (let ((pr (or (project-current)
                (user-error "No current project"))))
    (fzfa-flymake--read (flymake--project-diagnostics pr)
                        "flymake [project]: ")))

;;;###autoload
(defun fzfa-flymake-setup ()
  "Register the `fzfa-flymake' completion category."
  (add-to-list 'completion-category-overrides
               '(fzfa-flymake (styles fzfa))))

(provide 'fzfa-flymake)
;;; fzfa-flymake.el ends here
