;;; fzfa-chrome.el --- Chrome via `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching, password, chrome, fzf
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fuzzy-find Google Chrome bookmarks and saved passwords.
;; Loaded automatically when `chrome' is in `fzfa-extensions'.
;;
;; Bookmarks: reads Chrome's `Bookmarks' JSON file directly — no Chrome
;; process, shell commands, or external tools required.  Works for any
;; Chromium-derived browser by pointing `fzfa-chrome-bookmarks-file'
;; at its Bookmarks file (Brave, Edge, Vivaldi, Arc, Chromium itself).
;;
;; Passwords (macOS only): Chrome stores credentials in a SQLite
;; database (`Login Data') and encrypts each `password_value' blob with
;; AES-128-CBC, using a key derived (PBKDF2-HMAC-SHA1, 1003 iterations,
;; salt `saltysalt') from the keychain entry `Chrome Safe Storage'.
;; Requires `sqlite3', `security', `openssl', and `python3' on PATH —
;; all system-provided on macOS, no third-party Python packages needed.
;; The first decrypt of a session may trigger a macOS keychain prompt
;; for `/usr/bin/security'.  Chrome holds an exclusive lock on `Login
;; Data' while running, so this package copies it to a tempfile before
;; querying.  Chrome 127+ may app-bound-encrypt some newer entries;
;; those will fail to decrypt with the classic keychain key.
;;
;; Bookmark commands (embark category `fzfa-chrome-bookmark'):
;;
;;   `fzfa-chrome-bookmarks'  Open URL with `browse-url' (default)
;;   `fzfa-chrome-edit'       Open the bookmark in Chrome's editor
;;                                 (chrome://bookmarks/?id=N) — the
;;                                 supported way to rename or delete a
;;                                 bookmark without risking corruption
;;                                 of Chrome's JSON file
;;   `fzfa-chrome-copy-url'   Copy the URL to the kill ring
;;   `fzfa-chrome-refresh'    Drop the cached bookmark list
;;
;; Password commands (embark category `fzfa-chrome-pass'):
;;
;;   `fzfa-chrome-pass-copy'           Copy password (default)
;;   `fzfa-chrome-pass-copy-username'  Copy username
;;   `fzfa-chrome-pass-url'            Open URL with `browse-url'
;;   `fzfa-chrome-pass-refresh'        Drop the cached entry list

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defvar embark-keymap-alist)
(defvar embark-general-map)


;;; Bookmarks

(defcustom fzfa-chrome-bookmarks-file
  (pcase system-type
    ('darwin    "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
    ('gnu/linux "~/.config/google-chrome/Default/Bookmarks")
    ('windows-nt
     (when-let* ((appdata (getenv "LOCALAPPDATA")))
       (concat appdata "/Google/Chrome/User Data/Default/Bookmarks"))))
  "Path to Chrome's Bookmarks JSON file.
Override to point at a non-Default profile or a different Chromium
browser (Brave, Edge, Vivaldi, Arc)."
  :type '(choice (file :tag "Bookmarks file")
                 (const :tag "Auto/Unsupported" nil))
  :group 'fzfa)

(defvar fzfa-chrome--cache nil
  "Cached bookmark candidates (tab-encoded strings).")

(defun fzfa-chrome--walk (node folder-path)
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
                  append (fzfa-chrome--walk c sub))))
      ("url"
       (list (format "%s\t%s\t%s\t%s"
                     (or folder-path "")
                     (or name "")
                     (gethash "url" node)
                     (or (gethash "id" node) "")))))))

(defun fzfa-chrome--load ()
  "Parse Chrome's Bookmarks JSON; return list of tab-encoded strings."
  (unless fzfa-chrome-bookmarks-file
    (user-error
     (concat "Fzfa-chrome: no default bookmarks path for `%s'; "
             "set `fzfa-chrome-bookmarks-file'")
     system-type))
  (let ((file (expand-file-name fzfa-chrome-bookmarks-file)))
    (unless (file-readable-p file)
      (user-error "Fzfa-chrome: cannot read %s" file))
    (let* ((data (with-temp-buffer
                   (insert-file-contents file)
                   (json-parse-buffer :object-type 'hash-table
                                      :array-type  'list)))
           (roots (gethash "roots" data)))
      (cl-loop for root being the hash-values of roots
               append (fzfa-chrome--walk root nil)))))

(defun fzfa-chrome--bookmarks ()
  "Return cached bookmark candidates, loading from disk on first use."
  (or fzfa-chrome--cache
      (setq fzfa-chrome--cache (fzfa-chrome--load))))

(defun fzfa-chrome--group (cand transform)
  "Group fn for `fzfa-chrome-bookmark' candidate CAND.
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

(defun fzfa-chrome--pick (prompt)
  "Fuzzy-select a bookmark with PROMPT; return the raw tab-encoded candidate."
  (fzfa-sync-completing-read
   :candidates (fzfa-chrome--bookmarks)
   :prompt    prompt
   :category  'fzfa-chrome-bookmark
   :group     #'fzfa-chrome--group))

;;;###autoload
(defun fzfa-chrome-refresh ()
  "Invalidate the cached bookmark list so the next call re-reads from disk."
  (interactive)
  (setq fzfa-chrome--cache nil)
  (message "Chrome bookmarks cache cleared"))

;;;###autoload
(defun fzfa-chrome-bookmarks (cand)
  "Open the Chrome bookmark CAND with `browse-url'."
  (interactive (list (fzfa-chrome--pick "chrome: ")))
  (when cand
    (browse-url (nth 2 (split-string cand "\t")))))

;;;###autoload
(defun fzfa-chrome-edit (cand)
  "Open Chrome's bookmark editor on CAND.
Navigates to `chrome://bookmarks/?id=N' (the URL scheme only Chrome
understands), so the request is dispatched to Chrome explicitly
rather than via `browse-url' — which might pick Safari/Firefox.
Chrome's UI handles renames and deletions safely, avoiding direct
edits to the Bookmarks JSON file."
  (interactive (list (fzfa-chrome--pick "edit bookmark: ")))
  (when cand
    (let ((id (nth 3 (split-string cand "\t"))))
      (when (and id (not (string-empty-p id)))
        (let ((url (format "chrome://bookmarks/?id=%s" id)))
          (pcase system-type
            ('darwin    (call-process "open" nil 0 nil
                                      "-a" "Google Chrome" url))
            ('gnu/linux (call-process "google-chrome" nil 0 nil url))
            (_          (browse-url url))))))))

;;;###autoload
(defun fzfa-chrome-copy-url (cand)
  "Copy the URL of bookmark CAND to the kill ring."
  (interactive (list (fzfa-chrome--pick "copy url: ")))
  (when cand
    (let ((url (nth 2 (split-string cand "\t"))))
      (kill-new url)
      (message "Copied: %s" url))))

(defvar-keymap fzfa-chrome-map
  :doc "Embark keymap for `fzfa-chrome-bookmark' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "b" #'fzfa-chrome-bookmarks
  "e" #'fzfa-chrome-edit
  "w" #'fzfa-chrome-copy-url)


;;; Password manager

(defcustom fzfa-chrome-pass-database
  (pcase system-type
    ('darwin
     "~/Library/Application Support/Google/Chrome/Default/Login Data")
    ('gnu/linux "~/.config/google-chrome/Default/Login Data")
    ('windows-nt
     (when-let* ((appdata (getenv "LOCALAPPDATA")))
       (concat appdata "/Google/Chrome/User Data/Default/Login Data"))))
  "Path to Chrome's Login Data SQLite database.
Override to point at a non-Default profile or another Chromium browser."
  :type '(choice (file :tag "Login Data file")
                 (const :tag "Auto/Unsupported" nil))
  :group 'fzfa)

(defcustom fzfa-chrome-pass-keychain-service "Chrome Safe Storage"
  "Keychain service name used by Chrome to store its encryption key (macOS)."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-chrome-pass-keychain-account "Chrome"
  "Keychain account name for the Chrome Safe Storage entry (macOS)."
  :type 'string
  :group 'fzfa)

(defcustom fzfa-chrome-pass-python (executable-find "python3")
  "Path to `python3', used for PBKDF2 key derivation and ciphertext piping."
  :type '(choice (file :tag "python3 executable") (const nil))
  :group 'fzfa)

(defcustom fzfa-chrome-pass-timeout 45
  "Seconds before a copied password is cleared from the kill ring.
Set to 0 to disable auto-clearing."
  :type 'integer
  :group 'fzfa)

(defvar fzfa-chrome-pass--cache nil
  "Cached entry candidates (tab-encoded URL\\tUSER\\tHEX-PWD strings).")

(defconst fzfa-chrome-pass--py
  "import os, sys, hashlib, subprocess
hex_blob = sys.argv[1]
pwd = os.environ['CHROME_PWD']
key = hashlib.pbkdf2_hmac('sha1', pwd.encode(), b'saltysalt', 1003, 16)
data = bytes.fromhex(hex_blob)
if data[:3] in (b'v10', b'v11'):
    data = data[3:]
iv = b' ' * 16
r = subprocess.run(
    ['openssl', 'enc', '-d', '-aes-128-cbc',
     '-K', key.hex(), '-iv', iv.hex()],
    input=data, capture_output=True, check=True)
sys.stdout.buffer.write(r.stdout)
"
  "Python helper that decrypts a Chrome password blob.
Reads the hex-encoded ciphertext from argv[1] and the keychain
password from $CHROME_PWD; writes plaintext bytes to stdout.")

(defun fzfa-chrome-pass--keychain-password ()
  "Return the Chrome Safe Storage password from the macOS keychain."
  (unless (eq system-type 'darwin)
    (user-error "Fzfa-chrome-pass: keychain lookup is macOS-only"))
  (with-temp-buffer
    (let ((exit (call-process
                 "security" nil t nil "find-generic-password"
                 "-w"
                 "-s" fzfa-chrome-pass-keychain-service
                 "-a" fzfa-chrome-pass-keychain-account)))
      (unless (zerop exit)
        (user-error
         "Fzfa-chrome-pass: cannot read keychain entry %s/%s: %s"
         fzfa-chrome-pass-keychain-service
         fzfa-chrome-pass-keychain-account
         (string-trim (buffer-string)))))
    (string-trim (buffer-string))))

(defun fzfa-chrome-pass--copy-db ()
  "Copy the Login Data DB to a tempfile, returning its path.
Chrome holds an exclusive lock while running, so queries operate on a copy."
  (unless fzfa-chrome-pass-database
    (user-error
     (concat "Fzfa-chrome-pass: no default DB path for `%s'; "
             "set `fzfa-chrome-pass-database'")
     system-type))
  (let ((src (expand-file-name fzfa-chrome-pass-database))
        (dst (make-temp-file "fzfa-chrome-pass-" nil ".sqlite")))
    (unless (file-readable-p src)
      (user-error "Fzfa-chrome-pass: cannot read %s" src))
    (copy-file src dst t)
    dst))

(defun fzfa-chrome-pass--load ()
  "Return list of tab-encoded URL\\tUSER\\tHEX-PWD strings for all logins."
  (unless (executable-find "sqlite3")
    (user-error "Fzfa-chrome-pass: sqlite3 not found on PATH"))
  (let ((tmp (fzfa-chrome-pass--copy-db))
        (rows '()))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (call-process
                       "sqlite3" nil t nil "-separator" "\t" tmp
                       (concat "SELECT origin_url, username_value, "
                               "hex(password_value) FROM logins"))))
            (unless (zerop exit)
              (user-error "Fzfa-chrome-pass: sqlite3 failed: %s"
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

(defun fzfa-chrome-pass--candidates ()
  "Return cached entry candidates, loading from disk on first use."
  (or fzfa-chrome-pass--cache
      (setq fzfa-chrome-pass--cache (fzfa-chrome-pass--load))))

(defun fzfa-chrome-pass--group (cand transform)
  "Group fn for `fzfa-chrome-pass' candidate CAND.
TRANSFORM nil suppresses headers; TRANSFORM t formats the row for
display without revealing the encrypted blob."
  (let ((fields (split-string cand "\t")))
    (if transform
        (format "%s — %s"
                (or (nth 1 fields) "")
                (or (nth 0 fields) ""))
      "")))

(defun fzfa-chrome-pass--decrypt (hex)
  "Decrypt HEX (hex-encoded password blob); return the plaintext string."
  (unless fzfa-chrome-pass-python
    (user-error
     (concat "Fzfa-chrome-pass: python3 not found; "
             "set `fzfa-chrome-pass-python'")))
  (unless (executable-find "openssl")
    (user-error "Fzfa-chrome-pass: openssl not found on PATH"))
  (let* ((key (fzfa-chrome-pass--keychain-password))
         (process-environment (cons (concat "CHROME_PWD=" key)
                                    process-environment)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((exit (call-process fzfa-chrome-pass-python nil t nil
                                "-c" fzfa-chrome-pass--py hex)))
        (unless (zerop exit)
          (user-error "Fzfa-chrome-pass: decrypt failed: %s"
                      (string-trim (buffer-string)))))
      (decode-coding-string (buffer-string) 'utf-8))))

(defun fzfa-chrome-pass--pick (prompt)
  "Fuzzy-select a Chrome login with PROMPT; return raw tab-encoded candidate."
  (fzfa-sync-completing-read
   :candidates (fzfa-chrome-pass--candidates)
   :prompt    prompt
   :category  'fzfa-chrome-pass
   :group     #'fzfa-chrome-pass--group))

(defun fzfa-chrome-pass--schedule-clear (secret)
  "Schedule clearing SECRET from the kill ring after the configured timeout."
  (when (and (> fzfa-chrome-pass-timeout 0)
             (stringp secret) (not (string-empty-p secret)))
    (run-at-time
     fzfa-chrome-pass-timeout nil
     (lambda ()
       (when (equal (ignore-errors (current-kill 0 t)) secret)
         (kill-new "" t)
         (when (fboundp 'gui-set-selection)
           (ignore-errors (gui-set-selection 'CLIPBOARD ""))))))))

;;;###autoload
(defun fzfa-chrome-pass-refresh ()
  "Invalidate the cached entry list so the next call re-reads from disk."
  (interactive)
  (setq fzfa-chrome-pass--cache nil)
  (message "Chrome password cache cleared"))

;;;###autoload
(defun fzfa-chrome-pass-copy (&optional cand)
  "Copy the password of Chrome login CAND to the kill ring.
When CAND is nil (e.g. called interactively), prompt for one."
  (interactive)
  (when-let* ((cand (or cand
                        (fzfa-chrome-pass--pick "chrome-pass: "))))
    (let* ((fields (split-string cand "\t"))
           (pwd (fzfa-chrome-pass--decrypt (nth 2 fields))))
      (kill-new pwd)
      (fzfa-chrome-pass--schedule-clear pwd)
      (message "Copied password for %s @ %s"
               (nth 1 fields) (nth 0 fields)))))

;;;###autoload
(defalias 'fzfa-chrome-pass #'fzfa-chrome-pass-copy
  "Default `fzfa-chrome-pass' action: copy password to the kill ring.")

;;;###autoload
(defun fzfa-chrome-pass-copy-username (cand)
  "Copy the username of Chrome login CAND to the kill ring."
  (interactive (list (fzfa-chrome-pass--pick "chrome-pass user: ")))
  (when cand
    (let ((user (nth 1 (split-string cand "\t"))))
      (kill-new user)
      (message "Copied username: %s" user))))

;;;###autoload
(defun fzfa-chrome-pass-url (cand)
  "Open the URL of Chrome login CAND with `browse-url'."
  (interactive (list (fzfa-chrome-pass--pick "chrome-pass url: ")))
  (when cand
    (browse-url (nth 0 (split-string cand "\t")))))

(defvar-keymap fzfa-chrome-pass-map
  :doc "Embark keymap for `fzfa-chrome-pass' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "c" #'fzfa-chrome-pass-copy
  "u" #'fzfa-chrome-pass-copy-username
  "b" #'fzfa-chrome-pass-url)


;;; Setup

;;;###autoload
(defun fzfa-chrome-setup ()
  "Register `fzfa-chrome-bookmark' and `fzfa-chrome-pass' categories."
  (add-to-list 'completion-category-overrides
               '(fzfa-chrome-bookmark (styles fzfa)))
  (add-to-list 'completion-category-overrides
               '(fzfa-chrome-pass (styles fzfa)))
  (with-eval-after-load 'embark
    (add-to-list
     'embark-keymap-alist
     '(fzfa-chrome-bookmark fzfa-chrome-map embark-general-map))
    (add-to-list
     'embark-keymap-alist
     '(fzfa-chrome-pass fzfa-chrome-pass-map embark-general-map))))

(provide 'fzfa-chrome)
;;; fzfa-chrome.el ends here
