;;; fzfa-safari.el --- Safari via `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fuzzy-find Safari bookmarks and history (macOS only).
;; Loaded automatically when `safari' is in `fzfa-extensions'.
;;
;; macOS gates access to `~/Library/Safari/' through TCC (Full Disk
;; Access).  The Emacs process — and any helper process it spawns —
;; must be granted FDA in System Settings ▸ Privacy & Security ▸ Full
;; Disk Access before `fzfa-safari-bookmarks' or `fzfa-safari-history'
;; can read Safari's files.  Without it the loaders fail with a clear
;; permission-denied user-error pointing at this caveat.
;;
;; Bookmarks: reads `~/Library/Safari/Bookmarks.plist' via `plutil
;; -convert json -o -' (system-provided on macOS) and walks the
;; resulting tree.  Safari's special root folders are remapped to
;; their UI-visible names — `BookmarksBar' → "Favorites",
;; `BookmarksMenu' → "Bookmarks Menu", `com.apple.ReadingList' →
;; "Reading List".
;;
;; History: streams from `~/Library/Safari/History.db' via an embedded
;; Python helper — same async shape as `fzfa-chrome-history' and
;; `fzfa-firefox-history' so a future unified picker over all three
;; browsers can fan out via `fzfa''s multi-source machinery.  Safari
;; holds an exclusive lock on the live DB, so the helper copies it to
;; a tempfile before querying.  Requires `python3' on PATH.
;;
;; URLs open via `fzfa-safari-browser-function' (defaults to `open -a
;; Safari') rather than the OS default browser, so picks from these
;; commands land in Safari even when the system default is something
;; else.
;;
;; Passwords are intentionally out of scope: Safari delegates
;; credential storage to the macOS Keychain, which has no bulk-export
;; analog to Chrome's `Login Data' DB — each `security
;; find-internet-password' call either requires per-item "Always
;; Allow" or surfaces an interactive auth prompt.
;;
;; Bookmark commands (embark category `fzfa-safari-bookmark'):
;;
;;   `fzfa-safari-bookmarks'           Open URL in Safari (default)
;;   `fzfa-safari-bookmark-copy-url'   Copy the bookmark URL to the kill ring
;;   `fzfa-safari-refresh'             Drop the cached bookmark list
;;
;; History commands (embark category `fzfa-safari-history'):
;;
;;   `fzfa-safari-history'           Open URL in Safari (default)
;;   `fzfa-safari-history-copy-url'  Copy the URL to the kill ring

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar embark-keymap-alist)
(defvar embark-general-map)


;;; Browser

(defcustom fzfa-safari-browser-function
  (if (eq system-type 'darwin)
      (lambda (url) (call-process "open" nil 0 nil "-a" "Safari" url))
    #'browse-url)
  "Function used to open URLs from `fzfa-safari-*' commands.
Dispatches bookmark and history entries to Safari explicitly rather
than via `browse-url' — which on a non-Safari default would send them
to Chrome/Firefox.  Override to point at Safari Technology Preview or
a wrapper that adds flags."
  :type 'function
  :group 'fzfa)


;;; Bookmarks

(defcustom fzfa-safari-bookmarks-file
  (when (eq system-type 'darwin)
    "~/Library/Safari/Bookmarks.plist")
  "Path to Safari's Bookmarks.plist (binary property list)."
  :type '(choice (file :tag "Bookmarks.plist")
                 (const :tag "Auto/Unsupported" nil))
  :group 'fzfa)

(defvar fzfa-safari--cache nil
  "Cached bookmark candidates (tab-encoded FOLDER\\tNAME\\tURL\\tUUID strings).")

(defun fzfa-safari--decode-plist (file)
  "Run `plutil -convert json' on FILE; return parsed hash-table tree."
  (unless (executable-find "plutil")
    (user-error "Fzfa-safari: plutil not found on PATH"))
  (with-temp-buffer
    (let ((exit (call-process "plutil" nil t nil
                              "-convert" "json" "-o" "-" file)))
      (unless (zerop exit)
        (user-error
         (concat "Fzfa-safari: plutil failed reading %s — likely "
                 "missing Full Disk Access (grant Emacs FDA in "
                 "System Settings ▸ Privacy & Security): %s")
         file (string-trim (buffer-string)))))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'hash-table :array-type 'list)))

(defun fzfa-safari--folder-display-name (raw)
  "Remap Safari's internal folder titles to UI-visible names."
  (pcase raw
    ("BookmarksBar"          "Favorites")
    ("BookmarksMenu"         "Bookmarks Menu")
    ("com.apple.ReadingList" "Reading List")
    (_                       (or raw ""))))

(defun fzfa-safari--walk (node folder-path)
  "Collect tab-encoded candidate strings for leaf nodes under NODE.
FOLDER-PATH accumulates the breadcrumb of containing folders.  Each
emitted line has fields: FOLDER\\tNAME\\tURL\\tUUID.  Folder nodes
recurse; leaf nodes emit one row; proxy nodes (e.g. cloud tabs) are
ignored."
  (let ((type (gethash "WebBookmarkType" node)))
    (pcase type
      ("WebBookmarkTypeList"
       (let* ((raw   (gethash "Title" node))
              (title (fzfa-safari--folder-display-name raw))
              (sub   (cond ((null folder-path) title)
                           ((string-empty-p title) folder-path)
                           (t (concat folder-path "/" title)))))
         (cl-loop for c in (gethash "Children" node)
                  append (fzfa-safari--walk c sub))))
      ("WebBookmarkTypeLeaf"
       (let* ((dict (gethash "URIDictionary" node))
              (name (and (hash-table-p dict) (gethash "title" dict)))
              (url  (gethash "URLString" node))
              (uuid (gethash "WebBookmarkUUID" node)))
         (when (and url (not (string-empty-p url)))
           (list (format "%s\t%s\t%s\t%s"
                         (or folder-path "")
                         (or name "")
                         url
                         (or uuid "")))))))))

(defun fzfa-safari--load ()
  "Parse Safari's Bookmarks.plist; return list of tab-encoded strings."
  (unless fzfa-safari-bookmarks-file
    (user-error "Fzfa-safari: macOS-only — no bookmarks path for `%s'"
                system-type))
  (let ((file (expand-file-name fzfa-safari-bookmarks-file)))
    (unless (file-readable-p file)
      (user-error
       (concat "Fzfa-safari: cannot read %s — likely missing Full "
               "Disk Access (grant Emacs FDA in System Settings ▸ "
               "Privacy & Security)")
       file))
    (let* ((root     (fzfa-safari--decode-plist file))
           (children (gethash "Children" root)))
      (cl-loop for c in children
               append (fzfa-safari--walk c nil)))))

(defun fzfa-safari--bookmarks ()
  "Return cached bookmark candidates, loading from disk on first use."
  (or fzfa-safari--cache
      (setq fzfa-safari--cache (fzfa-safari--load))))

(defun fzfa-safari--group (cand transform)
  "Group fn for `fzfa-safari-bookmark' candidate CAND.
TRANSFORM nil returns the constant group key (suppresses headers
beyond the first); TRANSFORM t returns the cleaned per-row display
without the trailing UUID field."
  (let ((fields (split-string cand "\t")))
    (if transform
        (format "%s — %s — %s"
                (or (nth 0 fields) "")
                (or (nth 1 fields) "")
                (or (nth 2 fields) ""))
      "")))

(defun fzfa-safari--pick (prompt)
  "Fuzzy-select a bookmark with PROMPT; return the raw tab-encoded candidate."
  (fzfa-completing-read
   :candidates (fzfa-safari--bookmarks)
   :prompt    prompt
   :category  'fzfa-safari-bookmark
   :group     #'fzfa-safari--group))

;;;###autoload
(defun fzfa-safari-refresh ()
  "Invalidate the cached bookmark list so the next call re-reads from disk."
  (interactive)
  (setq fzfa-safari--cache nil)
  (message "Safari bookmarks cache cleared"))

;;;###autoload
(defun fzfa-safari-bookmarks (cand)
  "Open the Safari bookmark CAND via `fzfa-safari-browser-function'."
  (interactive (list (fzfa-safari--pick "safari: ")))
  (when cand
    (funcall fzfa-safari-browser-function
             (nth 2 (split-string cand "\t")))))

;;;###autoload
(defun fzfa-safari-bookmark-copy-url (cand)
  "Copy the URL of bookmark CAND to the kill ring."
  (interactive (list (fzfa-safari--pick "copy url: ")))
  (when cand
    (let ((url (nth 2 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzfa-safari-map
  :doc "Embark keymap for `fzfa-safari-bookmark' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzfa-safari-bookmarks
  "w" #'fzfa-safari-bookmark-copy-url)


;;; History

(defcustom fzfa-safari-history-file
  (when (eq system-type 'darwin)
    "~/Library/Safari/History.db")
  "Path to Safari's History.db SQLite database."
  :type '(choice (file :tag "History.db")
                 (const :tag "Auto/Unsupported" nil))
  :group 'fzfa)

(defcustom fzfa-safari-history-limit 5000
  "Maximum number of history rows streamed, ordered by most recent visit."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-safari-history-python (executable-find "python3")
  "Path to `python3', used to run the streaming history helper."
  :type '(choice (file :tag "python3 executable") (const nil))
  :group 'fzfa)

(defconst fzfa-safari-history--py
  "import sys, os, shutil, tempfile, sqlite3
src = sys.argv[1]
limit = int(sys.argv[2]) if len(sys.argv) > 2 else 5000
fd, tmp = tempfile.mkstemp(prefix='fzfa-safari-history-', suffix='.db')
os.close(fd)
try:
    shutil.copyfile(src, tmp)
    con = sqlite3.connect(tmp)
    cur = con.execute(
        \"SELECT REPLACE(REPLACE(REPLACE(\"
        \"COALESCE(v.title,''),char(9),' '),\"
        \"char(10),' '),char(13),' '), i.url \"
        \"FROM history_visits v \"
        \"JOIN history_items i ON v.history_item = i.id \"
        \"WHERE i.url IS NOT NULL AND i.url <> '' \"
        \"GROUP BY i.id \"
        \"ORDER BY MAX(v.visit_time) DESC LIMIT ?\",
        (limit,))
    out = sys.stdout
    for title, url in cur:
        out.write(title + '\\t' + url + '\\n')
    out.flush()
    con.close()
finally:
    try: os.remove(tmp)
    except OSError: pass
"
  "Python helper that streams Safari history rows.
Reads the History.db path from argv[1] and the row limit from
argv[2]; writes TAB-separated TITLE\\tURL lines to stdout.  Joins
`history_visits' onto `history_items', grouped by URL — SQLite's
bare-column-with-MAX rule yields the title from each URL's latest
visit.  Safari locks the live DB, so the helper copies it to a
tempfile first.")

(defvar fzfa-safari-history--py-path nil
  "Cached path to the materialized Python helper script.")

(defun fzfa-safari-history--py-script ()
  "Write the Python helper to a tempfile (if not cached) and return its path."
  (unless (and fzfa-safari-history--py-path
               (file-readable-p fzfa-safari-history--py-path))
    (let ((path (make-temp-file "fzfa-safari-history-" nil ".py")))
      (with-temp-file path
        (insert fzfa-safari-history--py))
      (setq fzfa-safari-history--py-path path)))
  fzfa-safari-history--py-path)

(defun fzfa-safari-history--command ()
  "Return the shell command string that streams TITLE\\tURL lines on stdout."
  (unless fzfa-safari-history-file
    (user-error "Fzfa-safari-history: macOS-only — no DB path for `%s'"
                system-type))
  (unless fzfa-safari-history-python
    (user-error
     (concat "Fzfa-safari-history: python3 not found; "
             "set `fzfa-safari-history-python'")))
  (let ((src (expand-file-name fzfa-safari-history-file)))
    (unless (file-readable-p src)
      (user-error
       (concat "Fzfa-safari-history: cannot read %s — likely missing "
               "Full Disk Access (grant Emacs FDA in System Settings ▸ "
               "Privacy & Security)")
       src))
    (format "%s %s %s %d"
            (shell-quote-argument fzfa-safari-history-python)
            (shell-quote-argument (fzfa-safari-history--py-script))
            (shell-quote-argument src)
            fzfa-safari-history-limit)))

(defun fzfa-safari-history--group (cand transform)
  "Group fn for `fzfa-safari-history' candidate CAND.
TRANSFORM nil returns the constant group key (suppresses headers
beyond the first); TRANSFORM t returns the per-row display."
  (let ((fields (split-string cand "\t")))
    (if transform
        (format "%s — %s"
                (or (nth 0 fields) "")
                (or (nth 1 fields) ""))
      "")))

(defun fzfa-safari-history--pick (prompt)
  "Fuzzy-select a history entry with PROMPT; return raw tab-encoded candidate.
Streams candidates asynchronously from the Python helper so the picker
stays responsive even on large History.db DBs."
  (fzfa-completing-read
   :command       (fzfa-safari-history--command)
   :prompt        prompt
   :category      'fzfa-safari-history
   :group         #'fzfa-safari-history--group
   :resolve-paths nil))

;;;###autoload
(defun fzfa-safari-history (cand)
  "Open the Safari history entry CAND via `fzfa-safari-browser-function'."
  (interactive (list (fzfa-safari-history--pick "safari-history: ")))
  (when cand
    (funcall fzfa-safari-browser-function
             (nth 1 (split-string cand "\t")))))

;;;###autoload
(defun fzfa-safari-history-copy-url (cand)
  "Copy the URL of history entry CAND to the kill ring."
  (interactive (list (fzfa-safari-history--pick "copy url: ")))
  (when cand
    (let ((url (nth 1 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzfa-safari-history-map
  :doc "Embark keymap for `fzfa-safari-history' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzfa-safari-history
  "w" #'fzfa-safari-history-copy-url)


;;; Setup

;;;###autoload
(defun fzfa-safari-setup ()
  "Register `fzfa-safari-bookmark' and `fzfa-safari-history' categories."
  (add-to-list 'completion-category-overrides
               '(fzfa-safari-bookmark (styles fzfa)))
  (add-to-list 'completion-category-overrides
               '(fzfa-safari-history (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list
     'embark-keymap-alist
     '(fzfa-safari-bookmark fzfa-safari-map embark-general-map))
    (add-to-list
     'embark-keymap-alist
     '(fzfa-safari-history fzfa-safari-history-map embark-general-map))))

(provide 'fzfa-safari)
;;; fzfa-safari.el ends here
