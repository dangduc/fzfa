;;; fzfa-notmuch.el --- Notmuch interface to `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; fzfa interface to notmuch.
;;
;; Loaded automatically when `notmuch' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Requires the `notmuch' Emacs
;; package and the `notmuch' CLI on `exec-path'; both are loaded lazily
;; on first use.
;;
;; Commands:
;;   `fzfa-notmuch'        Search threads, open in `notmuch-show'
;;   `fzfa-notmuch-tree'   Search threads, open in `notmuch-tree'
;;
;; With embark configured, these actions are available on a candidate:
;;
;;   s  show thread          (`fzfa-notmuch-show-thread')
;;   t  open in tree view    (`fzfa-notmuch-tree-thread')

;;; Code:

(require 'fzfa)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(declare-function notmuch-show "notmuch-show" (&optional thread-id))
(declare-function notmuch-tree "notmuch-tree" (&optional query))

(defcustom fzfa-notmuch-default-query "tag:inbox"
  "Default notmuch query offered at the `fzfa-notmuch' prompt.
Anything notmuch's CLI accepts is valid (e.g. \"tag:unread\",
\"from:alice and date:1week..\")."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-notmuch-search-args
  '("--format=text" "--output=summary" "--sort=newest-first")
  "Arguments inserted between `notmuch search' and the user query.
Each element is shell-quoted before being joined into the command."
  :type '(repeat string)
  :group 'fzfa)

(defvar fzfa-notmuch--history nil
  "Minibuffer history for notmuch queries.")

(defconst fzfa-notmuch--thread-regexp
  "^\\(thread:[0-9a-f]+\\)"
  "Regex matching the leading `thread:ID' token in `notmuch search' output.")

(defun fzfa-notmuch--thread-id (cand)
  "Return the `thread:ID' prefix of CAND, or nil."
  (when (and cand (string-match fzfa-notmuch--thread-regexp cand))
    (match-string 1 cand)))

(defun fzfa-notmuch--query-candidates ()
  "Candidates for the query completing-read: tags plus saved-search queries.
Tags are formatted as `tag:NAME'.  Saved searches come from
`notmuch-saved-searches' as-is."
  (let ((tags (ignore-errors
                (process-lines (or (bound-and-true-p notmuch-command) "notmuch")
                               "search" "--output=tags" "*")))
        (saved (mapcar (lambda (s) (plist-get s :query))
                       (and (boundp 'notmuch-saved-searches)
                            notmuch-saved-searches))))
    (delete-dups
     (append (mapcar (lambda (tag) (concat "tag:" tag)) tags)
             saved))))

(defun fzfa-notmuch--read-query (prompt)
  "Read a notmuch query with PROMPT.

Completes over notmuch tags (as `tag:NAME') and saved-search queries.
Free-form input is accepted.  Defaults to `fzfa-notmuch-default-query'."
  (fzfa-completing-read
   :candidates (fzfa-notmuch--query-candidates)
   :prompt prompt
   :history 'fzfa-notmuch--history
   :require-match nil
   :default fzfa-notmuch-default-query))

(defun fzfa-notmuch--command (query)
  "Build the shell command for `notmuch search' over QUERY."
  (mapconcat #'identity
             (cons "notmuch search"
                   (append (mapcar #'shell-quote-argument
                                   fzfa-notmuch-search-args)
                           (list (shell-quote-argument query))))
             " "))

(defun fzfa-notmuch--select (query prompt)
  "Run notmuch search QUERY and read a selection with PROMPT."
  (fzfa-completing-read
   :command (fzfa-notmuch--command query)
   :prompt prompt
   :category 'fzfa-notmuch
   :resolve-paths nil))

;;;###autoload
(defun fzfa-notmuch (query)
  "Run notmuch search QUERY, fuzzy-select a thread, open in `notmuch-show'."
  (interactive (list (fzfa-notmuch--read-query "Notmuch search: ")))
  (when-let* ((sel (fzfa-notmuch--select
                    query (format "notmuch[%s]: " query)))
              (tid (fzfa-notmuch--thread-id sel)))
    (require 'notmuch-show)
    (fzfa-with-visit (notmuch-show tid))))

;;;###autoload
(defun fzfa-notmuch-tree (query)
  "Run notmuch search QUERY, fuzzy-select a thread, open in `notmuch-tree'."
  (interactive (list (fzfa-notmuch--read-query "Notmuch tree: ")))
  (when-let* ((sel (fzfa-notmuch--select
                    query (format "notmuch-tree[%s]: " query)))
              (tid (fzfa-notmuch--thread-id sel)))
    (require 'notmuch-tree)
    (fzfa-with-visit (notmuch-tree tid))))

;;;###autoload
(defun fzfa-notmuch-show-thread (cand)
  "Open the thread referenced by candidate CAND in `notmuch-show'."
  (interactive "sThread: ")
  (when-let* ((tid (fzfa-notmuch--thread-id cand)))
    (require 'notmuch-show)
    (fzfa-with-visit (notmuch-show tid))))

;;;###autoload
(defun fzfa-notmuch-tree-thread (cand)
  "Open the thread referenced by candidate CAND in `notmuch-tree'."
  (interactive "sThread: ")
  (when-let* ((tid (fzfa-notmuch--thread-id cand)))
    (require 'notmuch-tree)
    (fzfa-with-visit (notmuch-tree tid))))

(defvar-keymap fzfa-notmuch-map
  :doc "Embark keymap for `fzfa-notmuch' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "s" #'fzfa-notmuch-show-thread
  "t" #'fzfa-notmuch-tree-thread)

;;;###autoload
(defun fzfa-notmuch-setup ()
  "Register the `fzfa-notmuch' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzfa-notmuch (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzfa-notmuch fzfa-notmuch-map embark-general-map))))

(provide 'fzfa-notmuch)
;;; fzfa-notmuch.el ends here
