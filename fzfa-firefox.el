;;; fzfa-firefox.el --- Firefox via `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fuzzy-find Firefox bookmarks.
;; Loaded automatically when `firefox' is in `fzfa-extensions'.
;;
;; Bookmarks live in `places.sqlite' (history + bookmarks share the
;; database).  Firefox holds an exclusive lock on the file while
;; running, so this package copies it to a tempfile before querying via
;; `sqlite3'.  No Python or third-party tooling required.
;;
;; Profile auto-detection: looks under the platform Firefox directory
;; for a `Profiles/*.default-release' folder first; if none is found,
;; falls back to parsing `profiles.ini' (preferring the `[Install*]'
;; section's `Default=' entry, then any `[Profile*]' with `Default=1').
;; Override with `fzfa-firefox-profile-dir' to pin a specific profile.
;;
;; Passwords are intentionally out of scope: Firefox encrypts
;; `logins.json' via NSS / `key4.db', which has no clean shell pipeline
;; analogous to Chrome's PBKDF2/AES flow.  Reading them would require
;; either linking `libnss3' or a third-party tool like
;; `firefox_decrypt' — neither of which is system-provided.
;;
;; Bookmark and history URLs open in Firefox via
;; `fzfa-firefox-browser-function' rather than the OS default browser,
;; so picks from these commands land in Firefox even on a system whose
;; default is Chrome or Safari.  Override to point at a specific
;; Firefox binary (e.g. Developer Edition) or a wrapper that adds
;; flags (profile, `--private-window').
;;
;; Bookmark commands (embark category `fzfa-firefox-bookmark'):
;;
;;   `fzfa-firefox-bookmarks'           Open URL in Firefox (default)
;;   `fzfa-firefox-bookmark-copy-url'   Copy the bookmark URL to the kill ring
;;   `fzfa-firefox-refresh'             Drop the cached bookmark list
;;
;; History commands (embark category `fzfa-firefox-history'):
;;
;;   `fzfa-firefox-history'           Open URL in Firefox (default)
;;   `fzfa-firefox-history-copy-url'  Copy the URL to the kill ring
;;
;; History streams from `moz_places' via an embedded Python helper
;; spawned as a subprocess — same async shape as `fzfa-chrome-history',
;; so a future unified picker over Chrome/Firefox/Safari/etc. can fan
;; out via `fzfa''s existing multi-source machinery.  The helper copies
;; `places.sqlite' to a tempfile before querying so it works while
;; Firefox is open.  Requires `python3' on PATH.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar embark-keymap-alist)
(defvar embark-general-map)


;;; Browser

(defcustom fzfa-firefox-browser-function
  (pcase system-type
    ('darwin    (lambda (url)
                  (call-process "open" nil 0 nil "-a" "Firefox" url)))
    ('gnu/linux (lambda (url) (call-process "firefox" nil 0 nil url)))
    (_          #'browse-url))
  "Function used to open URLs from `fzfa-firefox-*' commands.
Dispatches bookmark and history entries to Firefox explicitly
rather than via `browse-url' — which on a non-Firefox default would
send them to Chrome/Safari.  Override to point at a specific Firefox
binary (e.g. Firefox Developer Edition) or a wrapper that adds flags
(e.g. profile selection or `--private-window')."
  :type 'function
  :group 'fzfa)


;;; Profile detection

(defconst fzfa-firefox--profiles-root
  (pcase system-type
    ('darwin    "~/Library/Application Support/Firefox/")
    ('gnu/linux "~/.mozilla/firefox/")
    ('windows-nt
     (when-let* ((appdata (getenv "APPDATA")))
       (concat appdata "/Mozilla/Firefox/"))))
  "Per-OS Firefox application support directory.
Contains `profiles.ini' and the `Profiles' subdirectory.")

(defcustom fzfa-firefox-profile-dir nil
  "Path to the Firefox profile directory to use.
When nil, auto-detected at first use: glob for
`Profiles/*.default-release' under `fzfa-firefox--profiles-root',
falling back to parsing `profiles.ini'."
  :type '(choice (directory :tag "Profile directory")
                 (const :tag "Auto-detect" nil))
  :group 'fzfa)

(defvar fzfa-firefox--detected-profile nil
  "Cached result of profile auto-detection.")

(defun fzfa-firefox--glob-default-release ()
  "Return the first `Profiles/*.default-release' directory, or nil."
  (when fzfa-firefox--profiles-root
    (let* ((root (expand-file-name
                  "Profiles" fzfa-firefox--profiles-root))
           (matches (and (file-directory-p root)
                         (file-expand-wildcards
                          (concat root "/*.default-release")))))
      (car matches))))

(defun fzfa-firefox--profiles-ini-default ()
  "Parse `profiles.ini' and return the active profile's absolute path.
Prefers the `[Install*]' section's `Default=' (the modern,
Firefox 67+ active-profile marker) over the legacy `[Profile*]
Default=1' flag.  Returns nil when nothing parses."
  (when fzfa-firefox--profiles-root
    (let ((ini (expand-file-name
                "profiles.ini" fzfa-firefox--profiles-root)))
      (when (file-readable-p ini)
        (with-temp-buffer
          (insert-file-contents ini)
          (let (install-default profile-default)
            (goto-char (point-min))
            (when (re-search-forward "^\\[Install[^]]*\\]" nil t)
              (let ((end (or (save-excursion
                               (re-search-forward "^\\[" nil t))
                             (point-max))))
                (when (re-search-forward "^Default=\\(.+\\)$" end t)
                  (setq install-default (match-string 1)))))
            (goto-char (point-min))
            (while (and (not profile-default)
                        (re-search-forward "^\\[Profile[0-9]+\\]" nil t))
              (let ((end (or (save-excursion
                               (re-search-forward "^\\[" nil t))
                             (point-max)))
                    path is-default)
                (save-excursion
                  (when (re-search-forward "^Path=\\(.+\\)$" end t)
                    (setq path (match-string 1))))
                (save-excursion
                  (when (re-search-forward "^Default=1\\b" end t)
                    (setq is-default t)))
                (when (and is-default path)
                  (setq profile-default path))))
            (let ((rel (or install-default profile-default)))
              (when rel
                (expand-file-name rel fzfa-firefox--profiles-root)))))))))

(defun fzfa-firefox--detect-profile ()
  "Auto-detect the active Firefox profile directory."
  (or fzfa-firefox--detected-profile
      (setq fzfa-firefox--detected-profile
            (or (fzfa-firefox--glob-default-release)
                (fzfa-firefox--profiles-ini-default)))))

(defun fzfa-firefox--profile ()
  "Return the Firefox profile dir to use (configured or auto-detected)."
  (or fzfa-firefox-profile-dir
      (fzfa-firefox--detect-profile)
      (user-error
       (concat "Fzfa-firefox: cannot detect a Firefox profile under `%s'; "
               "set `fzfa-firefox-profile-dir'")
       (or fzfa-firefox--profiles-root "<unsupported OS>"))))


;;; Bookmarks

(defcustom fzfa-firefox-places-file nil
  "Path to Firefox's `places.sqlite' (history + bookmarks share the DB).
When nil, derived from `fzfa-firefox--profile' at first use."
  :type '(choice (file :tag "places.sqlite")
                 (const :tag "Auto-detect from profile" nil))
  :group 'fzfa)

(defun fzfa-firefox--places-file ()
  "Return the resolved path to `places.sqlite'."
  (or fzfa-firefox-places-file
      (expand-file-name "places.sqlite" (fzfa-firefox--profile))))

(defvar fzfa-firefox--cache nil
  "Cached bookmark candidates (tab-encoded FOLDER\\tNAME\\tURL\\tID strings).")

(defun fzfa-firefox--copy-db ()
  "Copy `places.sqlite' to a tempfile, returning its path.
Firefox holds an exclusive lock while running, so queries operate on a copy."
  (let ((src (fzfa-firefox--places-file))
        (dst (make-temp-file "fzfa-firefox-" nil ".sqlite")))
    (unless (file-readable-p src)
      (user-error "Fzfa-firefox: cannot read %s" src))
    (copy-file src dst t)
    dst))

(defconst fzfa-firefox--bookmarks-sql
  "WITH RECURSIVE fp(id, path) AS (
   SELECT id, title FROM moz_bookmarks
   WHERE guid IN ('menu________','toolbar_____','unfiled_____','mobile______')
   UNION ALL
   SELECT b.id, fp.path || '/' || b.title
   FROM moz_bookmarks b JOIN fp ON b.parent = fp.id
   WHERE b.type = 2)
SELECT
   REPLACE(REPLACE(REPLACE(COALESCE(fp.path,''),char(9),' '),char(10),' '),char(13),' '),
   REPLACE(REPLACE(REPLACE(COALESCE(b.title,pl.title,''),char(9),' '),char(10),' '),char(13),' '),
   pl.url,
   b.id
FROM moz_bookmarks b
JOIN moz_places pl ON b.fk = pl.id
JOIN fp ON b.parent = fp.id
WHERE b.type = 1 AND pl.url NOT LIKE 'place:%'
ORDER BY fp.path, b.position;"
  "SQL producing FOLDER\\tNAME\\tURL\\tID rows from `places.sqlite'.
The recursive CTE seeds at the four user-visible roots
(menu/toolbar/unfiled/mobile), so the `tags' subtree — Firefox's
fake folders that hold tagged-URL backrefs — is naturally excluded.
Tabs/newlines in folder names and titles are scrubbed in SQL so the
split-on-tab decoding on the Lisp side stays sound.")

(defun fzfa-firefox--load ()
  "Run `sqlite3' against a copy of `places.sqlite'; return candidate strings."
  (unless (executable-find "sqlite3")
    (user-error "Fzfa-firefox: sqlite3 not found on PATH"))
  (let ((tmp (fzfa-firefox--copy-db))
        (rows '()))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (call-process
                       "sqlite3" nil t nil "-separator" "\t" tmp
                       fzfa-firefox--bookmarks-sql)))
            (unless (zerop exit)
              (user-error "Fzfa-firefox: sqlite3 failed: %s"
                          (string-trim (buffer-string)))))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-empty-p line)
                (push line rows)))
            (forward-line 1)))
      (ignore-errors (delete-file tmp)))
    (nreverse rows)))

(defun fzfa-firefox--bookmarks ()
  "Return cached bookmark candidates, loading from disk on first use."
  (or fzfa-firefox--cache
      (setq fzfa-firefox--cache (fzfa-firefox--load))))

(defun fzfa-firefox--group (cand transform)
  "Group fn for `fzfa-firefox-bookmark' candidate CAND.
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

(defun fzfa-firefox--pick (prompt)
  "Fuzzy-select a bookmark with PROMPT; return the raw tab-encoded candidate."
  (fzfa-completing-read
   :candidates (fzfa-firefox--bookmarks)
   :prompt    prompt
   :category  'fzfa-firefox-bookmark
   :group     #'fzfa-firefox--group))

;;;###autoload
(defun fzfa-firefox-refresh ()
  "Invalidate the cached bookmark list so the next call re-reads from disk."
  (interactive)
  (setq fzfa-firefox--cache nil)
  (message "Firefox bookmarks cache cleared"))

;;;###autoload
(defun fzfa-firefox-bookmarks (cand)
  "Open the Firefox bookmark CAND via `fzfa-firefox-browser-function'."
  (interactive (list (fzfa-firefox--pick "firefox: ")))
  (when cand
    (funcall fzfa-firefox-browser-function
             (nth 2 (split-string cand "\t")))))

;;;###autoload
(defun fzfa-firefox-bookmark-copy-url (cand)
  "Copy the URL of bookmark CAND to the kill ring."
  (interactive (list (fzfa-firefox--pick "copy url: ")))
  (when cand
    (let ((url (nth 2 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzfa-firefox-map
  :doc "Embark keymap for `fzfa-firefox-bookmark' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzfa-firefox-bookmarks
  "w" #'fzfa-firefox-bookmark-copy-url)


;;; History

(defcustom fzfa-firefox-history-limit 5000
  "Maximum number of history rows streamed, ordered by most recent visit."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-firefox-history-python (executable-find "python3")
  "Path to `python3', used to run the streaming history helper."
  :type '(choice (file :tag "python3 executable") (const nil))
  :group 'fzfa)

(defconst fzfa-firefox-history--py
  "import sys, os, shutil, tempfile, sqlite3
src = sys.argv[1]
limit = int(sys.argv[2]) if len(sys.argv) > 2 else 5000
fd, tmp = tempfile.mkstemp(prefix='fzfa-firefox-history-', suffix='.sqlite')
os.close(fd)
try:
    shutil.copyfile(src, tmp)
    con = sqlite3.connect(tmp)
    cur = con.execute(
        \"SELECT REPLACE(REPLACE(REPLACE(\"
        \"COALESCE(title,''),char(9),' '),\"
        \"char(10),' '),char(13),' '), url FROM moz_places \"
        \"WHERE url IS NOT NULL AND url <> '' \"
        \"AND url NOT LIKE 'place:%' \"
        \"AND hidden = 0 AND last_visit_date IS NOT NULL \"
        \"ORDER BY last_visit_date DESC LIMIT ?\",
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
  "Python helper that streams Firefox history rows.
Reads the `places.sqlite' path from argv[1] and the row limit from
argv[2]; writes TAB-separated TITLE\\tURL lines to stdout.  Firefox
locks the live DB, so the helper copies it to a tempfile first.")

(defvar fzfa-firefox-history--py-path nil
  "Cached path to the materialized Python helper script.")

(defun fzfa-firefox-history--py-script ()
  "Write the Python helper to a tempfile (if not cached) and return its path."
  (unless (and fzfa-firefox-history--py-path
               (file-readable-p fzfa-firefox-history--py-path))
    (let ((path (make-temp-file "fzfa-firefox-history-" nil ".py")))
      (with-temp-file path
        (insert fzfa-firefox-history--py))
      (setq fzfa-firefox-history--py-path path)))
  fzfa-firefox-history--py-path)

(defun fzfa-firefox-history--command ()
  "Return the shell command string that streams TITLE\\tURL lines on stdout."
  (unless fzfa-firefox-history-python
    (user-error
     (concat "Fzfa-firefox-history: python3 not found; "
             "set `fzfa-firefox-history-python'")))
  (let ((src (fzfa-firefox--places-file)))
    (unless (file-readable-p src)
      (user-error "Fzfa-firefox-history: cannot read %s" src))
    (format "%s %s %s %d"
            (shell-quote-argument fzfa-firefox-history-python)
            (shell-quote-argument (fzfa-firefox-history--py-script))
            (shell-quote-argument src)
            fzfa-firefox-history-limit)))

(defun fzfa-firefox-history--group (cand transform)
  "Group fn for `fzfa-firefox-history' candidate CAND.
TRANSFORM nil returns the constant group key (suppresses headers
beyond the first); TRANSFORM t returns the per-row display."
  (let ((fields (split-string cand "\t")))
    (if transform
        (format "%s — %s"
                (or (nth 0 fields) "")
                (or (nth 1 fields) ""))
      "")))

(defun fzfa-firefox-history--pick (prompt)
  "Fuzzy-select a history entry with PROMPT; return raw tab-encoded candidate.
Streams candidates asynchronously from the Python helper so the picker
stays responsive even on large `places.sqlite' DBs."
  (fzfa-completing-read
   :command       (fzfa-firefox-history--command)
   :prompt        prompt
   :category      'fzfa-firefox-history
   :group         #'fzfa-firefox-history--group
   :resolve-paths nil))

;;;###autoload
(defun fzfa-firefox-history (cand)
  "Open the Firefox history entry CAND via `fzfa-firefox-browser-function'."
  (interactive (list (fzfa-firefox-history--pick "firefox-history: ")))
  (when cand
    (funcall fzfa-firefox-browser-function
             (nth 1 (split-string cand "\t")))))

;;;###autoload
(defun fzfa-firefox-history-copy-url (cand)
  "Copy the URL of history entry CAND to the kill ring."
  (interactive (list (fzfa-firefox-history--pick "copy url: ")))
  (when cand
    (let ((url (nth 1 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzfa-firefox-history-map
  :doc "Embark keymap for `fzfa-firefox-history' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzfa-firefox-history
  "w" #'fzfa-firefox-history-copy-url)


;;; Setup

;;;###autoload
(defun fzfa-firefox-setup ()
  "Register `fzfa-firefox-bookmark' and `fzfa-firefox-history' categories."
  (add-to-list 'completion-category-overrides
               '(fzfa-firefox-bookmark (styles fzfa)))
  (add-to-list 'completion-category-overrides
               '(fzfa-firefox-history (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list
     'embark-keymap-alist
     '(fzfa-firefox-bookmark fzfa-firefox-map embark-general-map))
    (add-to-list
     'embark-keymap-alist
     '(fzfa-firefox-history fzfa-firefox-history-map embark-general-map))))

(provide 'fzfa-firefox)
;;; fzfa-firefox.el ends here
