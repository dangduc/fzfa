;;; fzfa-spotlight.el --- Spotlight (mdfind) integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; macOS Spotlight (mdfind) commands for fzfa.
;;
;; Loaded automatically when `spotlight' is in `fzfa-extensions'
;; (the default) and `fzfa-setup' has been called.  No setup
;; function is registered — the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-spotlight'        Find any indexed file or .app bundle
;;   `fzfa-spotlight-apps'   Find an installed application
;;   `fzfa-spotlight-audio'  Find audio and play it with the default app

;;; Code:

(require 'fzfa)

(defcustom fzfa-spotlight-audio-directories
  '("~/Music" "~/Downloads" "~/Desktop")
  "Directories searched by `fzfa-spotlight-audio'.
Each directory is passed to `mdfind -onlyin'; results are concatenated.
Set to nil to search the whole index."
  :type '(repeat directory)
  :group 'fzfa)

;;;###autoload
(defun fzfa-spotlight ()
  "Find a file system-wide using Spotlight (mdfind).
.app bundles are opened with `open'; all other results open with `find-file'."
  (interactive)
  (when-let* ((result (fzfa-completing-read
                       :prompt "spotlight: "
                       :command "mdfind 'kMDItemFSName != \"\"'")))
    (if (string-suffix-p ".app" result)
        (start-process "default-app" nil "open" result)
      (fzfa-with-visit (find-file result)))))

;;;###autoload
(defun fzfa-spotlight-apps ()
  "Find an installed application using Spotlight.
Opens the selection with `open'."
  (interactive)
  (when-let*
      ((result
        (fzfa-completing-read
         :prompt "spotlight: "
         :command
         (concat "mdfind 'kMDItemContentTypeTree"
                 " == \"com.apple.application-bundle\"'"))))
    (start-process "default-app" nil "open" result)))

;;;###autoload
(defun fzfa-spotlight-audio ()
  "Find audio and play it using Spotlight.
Constrained to `fzfa-spotlight-audio-directories'."
  (interactive)
  (let* ((query "'kMDItemContentTypeTree == \"public.audio\"'")
         (command
          (if fzfa-spotlight-audio-directories
              (mapconcat
               (lambda (dir)
                 (format "mdfind -onlyin %s %s"
                         (shell-quote-argument (expand-file-name dir))
                         query))
               fzfa-spotlight-audio-directories
               "; ")
            (concat "mdfind " query))))
    (when-let* ((result (fzfa-completing-read
                         :prompt "spotlight: "
                         :command command)))
      (start-process "default-app" nil "open" result))))

(provide 'fzfa-spotlight)
;;; fzfa-spotlight.el ends here
