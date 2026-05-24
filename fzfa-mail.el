;;; fzfa-mail.el --- MacOS Mail.app integration for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail, matching, fzf
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Browse and open macOS Mail.app inbox messages via fzfa.
;;
;; Loaded automatically when `mail' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Requires macOS — uses
;; `osascript' (JXA) to enumerate messages and to open the selection
;; in Mail.app.
;;
;; Strategy: bulk-fetch every inbox message's date/sender/subject and
;; message-id via JXA into one cached list, present via
;; `fzfa-sync-completing-read', open the selection in Mail.app by
;; `message id'.  The initial dump is slow for large inboxes (10–30s
;; depending on size), so it is cached for the session.  Run
;; `fzfa-mail-refresh' after new mail arrives.
;;
;; Why not stream via `mdfind' or `find'?  Spotlight does not index
;; `~/Library/Mail/' on most machines (depends on the user's Spotlight
;; settings) and direct filesystem access is blocked by macOS TCC
;; unless Emacs has Full Disk Access.  The Mail.app IPC route works in
;; every default install at the cost of an upfront wait.
;;
;; Commands:
;;   `fzfa-mail'           Fuzzy-select and open an inbox message
;;   `fzfa-mail-refresh'   Drop the cached message list

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defcustom fzfa-mail-dump-timeout 120
  "Seconds to wait for the Mail.app dump before giving up.
Large inboxes (10k+ messages) can take 30s+ to enumerate."
  :type 'number
  :group 'fzfa)

(defconst fzfa-mail--dump-script
  "var Mail = Application('Mail');
   var msgs = Mail.inbox.messages;
   var ids = msgs.messageId();
   var dates = msgs.dateReceived();
   var senders = msgs.sender();
   var subjects = msgs.subject();
   var out = [];
   for (var i = 0; i < ids.length; i++) {
     var d = dates[i];
     var ds = (d instanceof Date) ? d.toISOString().slice(0, 10) : String(d);
     out.push(ids[i] + '\\t' + ds + '\\t' + senders[i] + '\\t' + subjects[i]);
   }
   out.join('\\n');"
  "JXA snippet returning tab-separated id/date/sender/subject lines.")

(defvar fzfa-mail--cache nil
  "Cached messages.
Each entry is a plist with `:id', `:date', `:from', and `:subject' keys.")

(defun fzfa-mail--osascript-lines (script)
  "Run JXA SCRIPT via `osascript', return non-empty stdout lines."
  (unless (eq system-type 'darwin)
    (user-error "Fzfa-mail requires macOS"))
  (with-temp-buffer
    (let ((rc (with-timeout (fzfa-mail-dump-timeout
                             (user-error "Mail.app query timed out after %ss"
                                         fzfa-mail-dump-timeout))
                (call-process "osascript" nil t nil
                              "-l" "JavaScript" "-e" script))))
      (unless (zerop rc)
        (user-error "Osascript failed (exit %s): %s" rc (buffer-string)))
      (split-string (buffer-string) "\n" t))))

(defun fzfa-mail--dump ()
  "Dump Mail.app's inbox into a list of plists."
  (cl-loop for line in (fzfa-mail--osascript-lines
                        fzfa-mail--dump-script)
           for parts = (split-string line "\t")
           when (>= (length parts) 4)
           collect (list :id      (nth 0 parts)
                         :date    (nth 1 parts)
                         :from    (nth 2 parts)
                         :subject (nth 3 parts))))

(defun fzfa-mail--messages ()
  "Return cached message list, dumping Mail.app on first use."
  (or fzfa-mail--cache
      (setq fzfa-mail--cache
            (with-temp-message "Loading Mail.app inbox (first call is slow)..."
              (fzfa-mail--dump)))))

;;;###autoload
(defun fzfa-mail-refresh ()
  "Invalidate the cached Mail.app inbox so the next call re-dumps."
  (interactive)
  (setq fzfa-mail--cache nil)
  (message "Mail.app cache cleared"))

;;;###autoload
(defun fzfa-mail ()
  "Fuzzy-select and open a Mail.app inbox message."
  (interactive)
  (let* ((msgs (fzfa-mail--messages))
         (map (make-hash-table :test #'equal))
         (cands (mapcar (lambda (m)
                          (let ((d (format "%s — %s — %s"
                                           (plist-get m :date)
                                           (plist-get m :from)
                                           (plist-get m :subject))))
                            (puthash d m map) d))
                        msgs)))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates cands
                      :prompt "mail: "
                      :category 'fzfa-mail))
                (item (gethash sel map)))
      ;; `open -a Mail' reliably brings Mail to the foreground (macOS
      ;; treats it as a user-initiated launch), then the AppleScript
      ;; navigates to the message.
      (call-process "open" nil 0 nil "-a" "Mail")
      (call-process
       "osascript" nil 0 nil "-e"
       (format
        "tell application \"Mail\" to open (first message of inbox whose message id is %S)"
        (plist-get item :id))))))

;;;###autoload
(defun fzfa-mail-setup ()
  "Register the `fzfa-mail' completion category."
  (add-to-list 'completion-category-overrides
               '(fzfa-mail (styles fzfa))))

(provide 'fzfa-mail)
;;; fzfa-mail.el ends here
