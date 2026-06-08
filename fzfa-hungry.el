;;; fzfa-hungry.el --- Buffer-derived multi-directory sources for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Hungry variants: derive the search scope from every file-visiting
;; buffer's parent directory (deduplicated so a shallower parent
;; subsumes its descendants), then stream a single command across all
;; of them.
;;
;; Loaded automatically when `hungry' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-hungry-find'     Find files across derived directories (fd / find)
;;   `fzfa-hungry-swiper'   Grep file contents across derived directories
;;                          (rg / grep)

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defun fzfa-hungry--deduplicate-dirs (dirs)
  "Remove duplicates and subdirectory entries from DIRS.
If directory A is a prefix of directory B, B is dropped — A's recursive
search already covers it.  Exception: B is kept when it is itself a git
root (contains a .git entry), so rg's gitignore stack starts at B rather
than inheriting A's.  Git-specific — rg only honors .gitignore, so .hg
/ .jj / etc. would just add duplicate hits."
  (let ((unique (cl-delete-duplicates dirs :test #'string=)))
    (cl-loop for dir in unique
             unless (and (not (file-exists-p (expand-file-name ".git" dir)))
                         (cl-some (lambda (other)
                                    (and (not (string= dir other))
                                         (string-prefix-p other dir)))
                                  unique))
             collect dir)))

;;;###autoload
(defun fzfa-hungry-swiper ()
  "Grep across the parent directories of all file-visiting buffers.
Collects unique parent directories, drops any that are subdirectories of
another in the set, then streams rg (or grep) output through fzf.
Selecting a match opens the file and jumps to the line."
  (interactive)
  (let* ((raw-dirs (cl-loop for buf in (buffer-list)
                            for file = (buffer-file-name buf)
                            when file
                            collect (file-name-directory
                                     (expand-file-name file))))
         (dirs (fzfa-hungry--deduplicate-dirs raw-dirs)))
    (unless dirs
      (user-error "No file-visiting buffers found"))
    (let* ((rg   (executable-find "rg"))
           (grep (executable-find "grep"))
           (dir-args (mapconcat #'shell-quote-argument dirs " "))
           (command
            (cond
             (rg   (concat (shell-quote-argument rg)
                           " --line-number --no-heading --with-filename '' "
                           dir-args))
             (grep (concat (shell-quote-argument grep)
                           " -Rn '' "
                           dir-args))
             (t (user-error "Neither rg nor grep found in exec-path")))))
      (when-let* ((r (fzfa-async-completing-read
                      :prompt "hungry swiper: "
                      :command command
                      :directory default-directory
                      :category 'fzfa-grep
                      :group #'fzfa--grep-group)))
        (fzfa-with-visit (fzfa--grep-jump r))))))

;;;###autoload
(defun fzfa-hungry-find ()
  "Find files across the parent directories of all file-visiting buffers.
Collects unique parent directories, drops subdirectories already covered
by a shallower parent, then streams fd (or find) output through fzf."
  (interactive)
  (let* ((raw-dirs (cl-loop for buf in (buffer-list)
                            for file = (buffer-file-name buf)
                            when file
                            collect (file-name-directory
                                     (expand-file-name file))))
         (dirs (fzfa-hungry--deduplicate-dirs raw-dirs)))
    (unless dirs
      (user-error "No file-visiting buffers found"))
    (let* ((fd   (executable-find "fd"))
           (find (executable-find "find"))
           (dir-args (mapconcat #'shell-quote-argument dirs " "))
           (command
            (cond
             (fd   (concat (shell-quote-argument fd)
                           " --no-ignore . "
                           dir-args))
             (find (concat (shell-quote-argument find)
                           " "
                           dir-args
                           " -type f"))
             (t (user-error "Neither fd nor find found in exec-path")))))
      (when-let* ((result (fzfa-async-completing-read
                           :prompt "hungry find: "
                           :command command
                           :directory default-directory)))
        (fzfa-with-visit (find-file result))))))

(provide 'fzfa-hungry)
;;; fzfa-hungry.el ends here
