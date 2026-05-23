;;; fzf-async-notmuch.el --- fzf-async interface for notmuch -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: mail, notmuch, matching, fzf
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; fzf-async interface to notmuch, modeled on `helm-notmuch',
;; `counsel-notmuch', and `consult-notmuch'.
;;
;; Loaded automatically when `notmuch' is in `fzf-async-extensions' and
;; `fzf-async-setup' has been called.  Requires the `notmuch' Emacs
;; package and the `notmuch' CLI on `exec-path'; both are loaded lazily
;; on first use.
;;
;; Commands:
;;   `fzf-async-notmuch'        Search threads, open in `notmuch-show'
;;   `fzf-async-notmuch-tree'   Search threads, open in `notmuch-tree'
;;
;; With embark configured, these actions are available on a candidate:
;;
;;   s  show thread          (`fzf-async-notmuch-show-thread')
;;   t  open in tree view    (`fzf-async-notmuch-tree-thread')

;;; Code:

(require 'fzf-async)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(declare-function notmuch-show "notmuch-show" (&optional thread-id))
(declare-function notmuch-tree "notmuch-tree" (&optional query))

(defcustom fzf-async-notmuch-default-query "tag:inbox"
  "Default notmuch query offered at the `fzf-async-notmuch' prompt.
Anything notmuch's CLI accepts is valid (e.g. \"tag:unread\",
\"from:alice and date:1week..\")."
  :type 'string
  :group 'fzf-async)

(defcustom fzf-async-notmuch-search-args
  '("--format=text" "--output=summary" "--sort=newest-first")
  "Arguments inserted between `notmuch search' and the user query.
Each element is shell-quoted before being joined into the command."
  :type '(repeat string)
  :group 'fzf-async)

(defvar fzf-async-notmuch--history nil
  "Minibuffer history for notmuch queries.")

(defconst fzf-async-notmuch--thread-regexp
  "^\\(thread:[0-9a-f]+\\)"
  "Regex matching the leading `thread:ID' token in `notmuch search' output.")

(defun fzf-async-notmuch--thread-id (cand)
  "Return the `thread:ID' prefix of CAND, or nil."
  (when (and cand (string-match fzf-async-notmuch--thread-regexp cand))
    (match-string 1 cand)))

(defun fzf-async-notmuch--query-candidates ()
  "Candidates for the query completing-read: tags plus saved-search queries.
Tags are formatted as `tag:NAME'. Saved searches come from
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

(defun fzf-async-notmuch--read-query (prompt)
  "Read a notmuch query with PROMPT.
Completes over notmuch tags (as `tag:NAME') and saved-search queries.
Free-form input is accepted. Defaults to `fzf-async-notmuch-default-query'."
  (completing-read prompt
                   (fzf-async-notmuch--query-candidates)
                   nil nil nil
                   'fzf-async-notmuch--history
                   fzf-async-notmuch-default-query))

(defun fzf-async-notmuch--command (query)
  "Build the shell command for `notmuch search' over QUERY."
  (mapconcat #'identity
             (cons "notmuch search"
                   (append (mapcar #'shell-quote-argument
                                   fzf-async-notmuch-search-args)
                           (list (shell-quote-argument query))))
             " "))

(defun fzf-async-notmuch--select (query prompt)
  "Run notmuch search QUERY and read a selection with PROMPT."
  (fzf-async-completing-read
   :command (fzf-async-notmuch--command query)
   :prompt prompt
   :category 'fzf-async-notmuch
   :resolve-paths nil))

;;;###autoload
(defun fzf-async-notmuch (query)
  "Run notmuch search QUERY, fuzzy-select a thread, open in `notmuch-show'."
  (interactive (list (fzf-async-notmuch--read-query "Notmuch search: ")))
  (when-let* ((sel (fzf-async-notmuch--select
                    query (format "notmuch[%s]: " query)))
              (tid (fzf-async-notmuch--thread-id sel)))
    (require 'notmuch-show)
    (notmuch-show tid)))

;;;###autoload
(defun fzf-async-notmuch-tree (query)
  "Run notmuch search QUERY, fuzzy-select a thread, open in `notmuch-tree'."
  (interactive (list (fzf-async-notmuch--read-query "Notmuch tree: ")))
  (when-let* ((sel (fzf-async-notmuch--select
                    query (format "notmuch-tree[%s]: " query)))
              (tid (fzf-async-notmuch--thread-id sel)))
    (require 'notmuch-tree)
    (notmuch-tree tid)))

;;;###autoload
(defun fzf-async-notmuch-show-thread (cand)
  "Open the thread referenced by candidate CAND in `notmuch-show'."
  (interactive "sThread: ")
  (when-let* ((tid (fzf-async-notmuch--thread-id cand)))
    (require 'notmuch-show)
    (notmuch-show tid)))

;;;###autoload
(defun fzf-async-notmuch-tree-thread (cand)
  "Open the thread referenced by candidate CAND in `notmuch-tree'."
  (interactive "sThread: ")
  (when-let* ((tid (fzf-async-notmuch--thread-id cand)))
    (require 'notmuch-tree)
    (notmuch-tree tid)))

(defvar-keymap fzf-async-notmuch-map
  :doc "Embark keymap for `fzf-async-notmuch' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "s" #'fzf-async-notmuch-show-thread
  "t" #'fzf-async-notmuch-tree-thread)

;;;###autoload
(defun fzf-async-notmuch-setup ()
  "Register the `fzf-async-notmuch' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzf-async-notmuch (styles fzf-async)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzf-async-notmuch fzf-async-notmuch-map embark-general-map))))

(provide 'fzf-async-notmuch)
;;; fzf-async-notmuch.el ends here
