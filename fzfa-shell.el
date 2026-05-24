;;; fzfa-shell.el --- Shell command sources for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching, shell
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shell-oriented sources for fzfa: ad-hoc shell commands and shell
;; history lookup.
;;
;; Loaded automatically when `shell' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-shell-command'           Fuzzy-search the output of a shell command
;;   `fzfa-project-shell-command'   Same, but run from the project root
;;   `fzfa-shell-history'           Insert / copy a line from shell history

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar fzfa-shell-command-history nil
  "Minibuffer history for `fzfa-shell-command'.")

;;;###autoload
(defun fzfa-shell-command (command &optional directory)
  "Fuzzy-search the output of a user-provided shell COMMAND.
Runs in DIRECTORY, defaulting to `default-directory'.
COMMAND is passed verbatim to `shell-file-name', so pipes,
redirections, and shell quoting all work as expected.  The selected
candidate is opened as a file if it exists relative to the working
directory; otherwise it is placed in the kill ring."
  (interactive
   (list (read-shell-command "Shell command: " nil
                             'fzfa-shell-command-history)))
  (let* ((cmd (string-trim command))
         (dir (or directory default-directory)))
    (when (string-empty-p cmd)
      (user-error "Command cannot be empty"))
    (when-let* ((result (fzfa-async-completing-read
                         :prompt (format "%s » " cmd)
                         :command cmd
                         :directory dir
                         :category 'fzfa-misc
                         :resolve-paths nil
                         :skip-executable-check t)))
      (let ((path (expand-file-name result dir)))
        (if (file-exists-p path)
            (find-file path)
          (kill-new result)
          (message "%s" result))))))

;;;###autoload
(defun fzfa-project-shell-command (command)
  "Fuzzy-search the output of a user-provided shell COMMAND.
Like `fzfa-shell-command' but runs in the project root."
  (interactive
   (list (read-shell-command "Shell command: "
                             nil 'fzfa-shell-command-history)))
  (fzfa-shell-command command (fzfa--default-dir)))

(defcustom fzfa-shell-history-file nil
  "Path to a shell history file (bash or zsh).
When nil, defaults to `$HISTFILE' if set, otherwise `~/.zsh_history'."
  :type '(choice (const :tag "Auto ($HISTFILE or ~/.zsh_history)" nil)
                 file)
  :group 'fzfa)

;;;###autoload
(defun fzfa-shell-history ()
  "Select a command from the shell history file and insert it at point.
Supports bash and zsh history file formats (including zsh
`EXTENDED_HISTORY' and bash `HISTTIMEFORMAT' timestamp comments).
If the current buffer is read-only the selection is copied to the
kill ring instead.  Override the location via
`fzfa-shell-history-file'."
  (interactive)
  (cl-labels
      ((parse-entry (raw)
         (cond
          ((string-match "\\`: [0-9]+:[0-9]+;\\(\\(?:.\\|\n\\)*\\)\\'" raw)
           (match-string 1 raw))
          ((string-match-p "\\`#[0-9]+\\'" raw) nil)
          (t raw)))
       (read-entries (file)
         (let ((seen (make-hash-table :test 'equal)) results)
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8-auto))
               (insert-file-contents file))
             (while (not (eobp))
               (let ((start (point)))
                 (end-of-line)
                 ;; Continuations: trailing backslash escapes the newline.
                 (while (and (eq (char-before) ?\\) (not (eobp)))
                   (forward-char 1) (end-of-line))
                 (when-let* ((cmd (parse-entry
                                   (buffer-substring-no-properties
                                    start (point))))
                             (cmd (string-trim cmd))
                             ((not (string-empty-p cmd)))
                             ((not (gethash cmd seen))))
                   (puthash cmd t seen)
                   (push cmd results))
                 (unless (eobp) (forward-char 1)))))
           results)))
    (let* ((file (expand-file-name
                  (or fzfa-shell-history-file
                      (getenv "HISTFILE")
                      "~/.zsh_history")))
           (cmds (and (or (file-readable-p file)
                          (user-error "Cannot read shell history: %s" file))
                      (or (read-entries file)
                          (user-error "Shell history is empty")))))
      (when-let* ((result (fzfa-sync-completing-read
                           :candidates cmds :prompt "shell-history: ")))
        (if buffer-read-only
            (progn (kill-new result) (message "Copied: %s" result))
          (insert result))))))

(provide 'fzfa-shell)
;;; fzfa-shell.el ends here
