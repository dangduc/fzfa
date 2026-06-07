;;; fzfa-project.el --- project.el integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files, matching
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; project.el integration for fzfa.  Mirrors the project-scoped commands
;; commonly found in `counsel-projectile', `helm-projectile', and
;; `consult-project-buffer' / `project' built-ins.
;;
;; Loaded automatically when `project' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-project-find-file'      Find a file in the current project
;;   `fzfa-project-find-dir'       Open a directory in the current project
;;   `fzfa-project-buffer'         Switch to a buffer of the current project
;;   `fzfa-project-recentf'        Open a recent file from the current project
;;   `fzfa-project-switch-project' Switch to a known project root
;;
;; File-search and grep commands shipped in sibling extensions
;; (e.g. `fzfa-rg', `fzfa-fd', `fzfa-find', `fzfa-git-ls-files') already
;; honor the project root via `fzfa-project-backend' / `fzfa--default-dir'.
;; This extension covers operations that are project-aware in a way the
;; shell-driven commands are not: candidate sets derived from
;; `project-files', `project-buffers', and `project-known-project-roots'.

;;; Code:

(require 'fzfa)
(require 'project)
(require 'cl-lib)

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-files "project")
(declare-function project-buffers "project")
(declare-function project-known-project-roots "project")
(declare-function project-switch-project "project")
(defvar recentf-list)

(defun fzfa-project--current ()
  "Return the current project, or signal a user-error."
  (or (project-current)
      (user-error "Not in a project")))

(defun fzfa-project--label (root)
  "Return a short label for project ROOT for use in a prompt."
  (file-name-nondirectory (directory-file-name root)))

;;;###autoload
(defun fzfa-project-find-file ()
  "Find a file in the current project.
Candidate set comes from `project-files', so membership respects
`project-vc-*' and `project-find-functions'.  Modeled on the built-in
`project-find-file', `counsel-projectile-find-file', and
`helm-projectile-find-file'."
  (interactive)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (files (project-files pr)))
    (unless files
      (user-error "No files in current project"))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates (mapcar (lambda (f) (file-relative-name f root))
                                          files)
                      :prompt (format "project file [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (fzfa-with-visit (find-file (expand-file-name sel root))))))

;;;###autoload
(defun fzfa-project-find-dir ()
  "Open a directory contained in the current project, in dired.
Candidates are the unique parent directories of `project-files', plus
the project root itself.  Modeled on the built-in `project-find-dir',
`counsel-projectile-find-dir', and `helm-projectile-find-dir'."
  (interactive)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (seen (make-hash-table :test 'equal))
         (dirs nil))
    (dolist (f (project-files pr))
      (let ((d (file-name-directory f)))
        (unless (gethash d seen)
          (puthash d t seen)
          (push (file-relative-name d root) dirs))))
    (unless (member "./" dirs) (push "./" dirs))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates (sort dirs #'string<)
                      :prompt (format "project dir [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (dired (expand-file-name sel root)))))

;;;###autoload
(defun fzfa-project-buffer ()
  "Switch to a buffer of the current project.
Modeled on the built-in `project-switch-to-buffer', `consult-project-buffer',
`counsel-projectile-switch-to-buffer', and
`helm-projectile-switch-to-buffer'."
  (interactive)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (names (cl-loop for b in (project-buffers pr)
                         for name = (buffer-name b)
                         unless (or (minibufferp b)
                                    (string-prefix-p " " name))
                         collect name)))
    (unless names
      (user-error "No buffers in current project"))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates names
                      :prompt (format "project buffer [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-buffer)))
      (fzfa-with-visit (switch-to-buffer sel)))))

;;;###autoload
(defun fzfa-project-recentf ()
  "Open a recently visited file under the current project.
Filters `recentf-list' to entries whose expanded path is under the
current project's root.  Modeled on `counsel-projectile-recentf' and
`helm-projectile-recentf'."
  (interactive)
  (require 'recentf)
  (recentf-mode 1)
  (let* ((pr (fzfa-project--current))
         (root (file-name-as-directory (expand-file-name (project-root pr))))
         (files (cl-loop for f in recentf-list
                         for ef = (expand-file-name f)
                         when (string-prefix-p root ef)
                         collect (file-relative-name ef root))))
    (unless files
      (user-error "No recent files under %s" (abbreviate-file-name root)))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates files
                      :prompt (format "project recentf [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (fzfa-with-visit (find-file (expand-file-name sel root))))))

;;;###autoload
(defun fzfa-project-switch-project ()
  "Switch to a known project root via fzf.
After selection, dispatches through `project-switch-project' so the
user's `project-switch-commands' menu kicks in.  Modeled on the
built-in `project-switch-project', `counsel-projectile-switch-project',
and `helm-projectile-switch-project'."
  (interactive)
  (let ((roots (project-known-project-roots)))
    (unless roots
      (user-error "No known projects"))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates (mapcar #'abbreviate-file-name roots)
                      :prompt "switch project: "
                      :category 'fzfa-file)))
      (project-switch-project (expand-file-name sel)))))

(provide 'fzfa-project)
;;; fzfa-project.el ends here
