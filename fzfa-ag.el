;;; fzfa-ag.el --- Ag (the_silver_searcher) integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files, matching
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ag' (https://github.com/ggreer/the_silver_searcher) integration for fzfa.
;;
;; Loaded automatically when `ag' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-ag-files'   Find a file under `default-directory' using ag
;;   `fzfa-ag'         Search file contents under `default-directory' with ag

;;; Code:

(require 'fzfa)

(defcustom fzfa-ag-files-command "ag -g ."
  "Shell command used by `fzfa-ag-files'.
Run from `default-directory'; stdout lines become file candidates."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-ag-command
  "ag --nocolor --nogroup --line-number %s \".\""
  "Shell command used by `fzfa-ag' for content search.
A `%s' placeholder is filled with the max-columns flag derived from
`fzfa-max-line-length'.  Output must be FILE:LINE:CONTENT."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-ag-files ()
  "Find a file under `default-directory' using ag.
The command is configurable via `fzfa-ag-files-command'."
  (interactive)
  (when-let* ((result (fzfa-async-completing-read
                       :prompt "ag files: " :command fzfa-ag-files-command)))
    (find-file result)))

;;;###autoload
(defun fzfa-ag ()
  "Search file contents under `default-directory' with ag.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line.
The command is configurable via `fzfa-ag-command'."
  (interactive)
  (when-let* ((r (fzfa-async-completing-read
                  :command (format fzfa-ag-command
                                   (fzfa--max-columns-flag 'ag))
                  :category 'fzfa-grep
                  :group #'fzfa--grep-group)))
    (fzfa--grep-jump r)))

(when (memq 'fzfa-ag-2p fzfa-2p-functions)
  (fzfa-2p-define 'fzfa-ag))
(when (memq 'fzfa-ag-files-2p fzfa-2p-functions)
  (fzfa-2p-define 'fzfa-ag-files))

(provide 'fzfa-ag)
;;; fzfa-ag.el ends here
