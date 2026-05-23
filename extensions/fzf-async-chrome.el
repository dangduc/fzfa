;;; fzf-async-chrome.el --- Chrome bookmark search via fzf-async -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: convenience, matching, fzf
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; Fuzzy-find Google Chrome bookmarks and open the selection.
;;
;; Loaded automatically when `chrome' is in `fzf-async-extensions'.
;; Reads Chrome's `Bookmarks' JSON file directly — no Chrome process,
;; shell commands, or external tools required.  Works for any
;; Chromium-derived browser by pointing `fzf-async-chrome-bookmarks-file'
;; at its Bookmarks file (Brave, Edge, Vivaldi, Arc, Chromium itself).
;;
;; Commands (also exposed as embark actions on the `fzf-async-chrome'
;; category):
;;
;;   `fzf-async-chrome-bookmarks'  Open URL with `browse-url' (default)
;;   `fzf-async-chrome-edit'       Open the bookmark in Chrome's editor
;;                                 (chrome://bookmarks/?id=N) — the
;;                                 supported way to rename or delete a
;;                                 bookmark without risking corruption
;;                                 of Chrome's JSON file
;;   `fzf-async-chrome-copy-url'   Copy the URL to the kill ring
;;   `fzf-async-chrome-refresh'    Drop the cached bookmark list

;;; Code:

(require 'fzf-async)
(require 'cl-lib)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(defcustom fzf-async-chrome-bookmarks-file
  (pcase system-type
    ('darwin    "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
    ('gnu/linux "~/.config/google-chrome/Default/Bookmarks")
    ('windows-nt
     (when-let* ((appdata (getenv "LOCALAPPDATA")))
       (concat appdata "/Google/Chrome/User Data/Default/Bookmarks"))))
  "Path to Chrome's Bookmarks JSON file.
Override to point at a non-Default profile or a different Chromium
browser (Brave, Edge, Vivaldi, Arc)."
  :type '(choice (file :tag "Bookmarks file") (const :tag "Auto/Unsupported" nil))
  :group 'fzf-async)

(defvar fzf-async-chrome--cache nil
  "Cached bookmark candidates (tab-encoded strings).")

(defun fzf-async-chrome--walk (node folder-path)
  "Collect tab-encoded candidate strings for url nodes under NODE.
FOLDER-PATH accumulates the breadcrumb of containing folders.  Each
emitted line has fields: FOLDER\\tNAME\\tURL\\tID.  Folder nodes recurse;
url nodes emit one row."
  (let ((type (gethash "type" node))
        (name (gethash "name" node)))
    (pcase type
      ("folder"
       (let ((sub (if folder-path (concat folder-path "/" name) name)))
         (cl-loop for c in (gethash "children" node)
                  append (fzf-async-chrome--walk c sub))))
      ("url"
       (list (format "%s\t%s\t%s\t%s"
                     (or folder-path "")
                     (or name "")
                     (gethash "url" node)
                     (or (gethash "id" node) "")))))))

(defun fzf-async-chrome--load ()
  "Parse Chrome's Bookmarks JSON; return list of tab-encoded strings."
  (unless fzf-async-chrome-bookmarks-file
    (user-error
     "Fzf-async-chrome: no default bookmarks path for `%s'; set `fzf-async-chrome-bookmarks-file'"
     system-type))
  (let ((file (expand-file-name fzf-async-chrome-bookmarks-file)))
    (unless (file-readable-p file)
      (user-error "Fzf-async-chrome: cannot read %s" file))
    (let* ((data (with-temp-buffer
                   (insert-file-contents file)
                   (json-parse-buffer :object-type 'hash-table
                                      :array-type  'list)))
           (roots (gethash "roots" data)))
      (cl-loop for root being the hash-values of roots
               append (fzf-async-chrome--walk root nil)))))

(defun fzf-async-chrome--bookmarks ()
  "Return cached bookmark candidates, loading from disk on first use."
  (or fzf-async-chrome--cache
      (setq fzf-async-chrome--cache (fzf-async-chrome--load))))

(defun fzf-async-chrome--group (cand transform)
  "Group fn for `fzf-async-chrome' candidate CAND.
TRANSFORM nil returns the constant group key (suppresses headers
beyond the first); TRANSFORM t returns the cleaned per-row display
without the trailing ID field."
  (let ((fields (split-string cand "\t")))
    (if transform
        (format "%s — %s — %s"
                (or (nth 0 fields) "")
                (or (nth 1 fields) "")
                (or (nth 2 fields) ""))
      "")))

(defun fzf-async-chrome--pick (prompt)
  "Fuzzy-select a bookmark with PROMPT; return the raw tab-encoded candidate."
  (fzf-sync-completing-read
   :candidates (fzf-async-chrome--bookmarks)
   :prompt    prompt
   :category  'fzf-async-chrome
   :group     #'fzf-async-chrome--group))

;;;###autoload
(defun fzf-async-chrome-refresh ()
  "Invalidate the cached bookmark list so the next call re-reads from disk."
  (interactive)
  (setq fzf-async-chrome--cache nil)
  (message "Chrome bookmarks cache cleared"))

;;;###autoload
(defun fzf-async-chrome-bookmarks (cand)
  "Open the Chrome bookmark CAND with `browse-url'."
  (interactive (list (fzf-async-chrome--pick "chrome: ")))
  (when cand
    (browse-url (nth 2 (split-string cand "\t")))))

;;;###autoload
(defun fzf-async-chrome-edit (cand)
  "Open Chrome's bookmark editor on CAND.
Navigates to `chrome://bookmarks/?id=N' (the URL scheme only Chrome
understands), so the request is dispatched to Chrome explicitly
rather than via `browse-url' — which might pick Safari/Firefox.
Chrome's UI handles renames and deletions safely, avoiding direct
edits to the Bookmarks JSON file."
  (interactive (list (fzf-async-chrome--pick "edit bookmark: ")))
  (when cand
    (let ((id (nth 3 (split-string cand "\t"))))
      (when (and id (not (string-empty-p id)))
        (let ((url (format "chrome://bookmarks/?id=%s" id)))
          (pcase system-type
            ('darwin    (call-process "open" nil 0 nil "-a" "Google Chrome" url))
            ('gnu/linux (call-process "google-chrome" nil 0 nil url))
            (_          (browse-url url))))))))

;;;###autoload
(defun fzf-async-chrome-copy-url (cand)
  "Copy the URL of bookmark CAND to the kill ring."
  (interactive (list (fzf-async-chrome--pick "copy url: ")))
  (when cand
    (let ((url (nth 2 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzf-async-chrome-map
  :doc "Embark keymap for `fzf-async-chrome' bookmarks.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzf-async-chrome-bookmarks
  "e" #'fzf-async-chrome-edit
  "w" #'fzf-async-chrome-copy-url)

;;;###autoload
(defun fzf-async-chrome-setup ()
  "Register the `fzf-async-chrome' category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzf-async-chrome (styles fzf-async)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzf-async-chrome fzf-async-chrome-map embark-general-map))))

(provide 'fzf-async-chrome)
;;; fzf-async-chrome.el ends here
