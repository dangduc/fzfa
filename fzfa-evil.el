;;; fzfa-evil.el --- Evil-mode sources for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil-mode front-ends for `fzfa'.  Soft-depends on `evil' (loaded lazily).
;;
;; Loaded automatically when `evil' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.
;;
;; Commands:
;;   `fzfa-evil-marks'           Jump to an evil mark (buffer-local or global)
;;   `fzfa-evil-registers'       Paste from / execute an evil register
;;   `fzfa-evil-jumps'           Jump to an entry in the evil jump list
;;   `fzfa-evil-ex-history'      Re-run an ex command from history
;;   `fzfa-evil-search-history'  Re-run a search from `/' history
;;   `fzfa-evil-command-window'  Unified ex + search history picker
;;   `fzfa-evil-any'        Multi-source picker over all of the above

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function evil-get-marker "evil-common"
                  (char &optional raw))
(declare-function evil-goto-mark "evil-commands"
                  (char &optional noerror))
(declare-function evil-register-list          "evil-common")
(declare-function evil-paste-from-register    "evil-commands" (register))
(declare-function evil-execute-macro          "evil-macros" (count macro))
(declare-function ring-elements                    "ring" (ring))
(declare-function evil--jumps-get-window-jump-list "evil-jumps")
(declare-function evil-ex-execute             "evil-ex" (string))
(declare-function evil-ex-make-search-pattern "evil-search" (regexp))
(declare-function evil-ex-search-next         "evil-commands" (&optional count))
(defvar evil-markers-alist)
(defvar evil-ex-history)
(defvar evil-ex-search-history)
(defvar evil-ex-search-pattern)
(defvar evil-ex-search-direction)

(defun fzfa-evil--line-at (pos)
  "Return the (trimmed) text of the line containing POS in the current buffer."
  (save-excursion
    (goto-char pos)
    (string-trim
     (buffer-substring-no-properties
      (line-beginning-position) (line-end-position)))))

(defun fzfa-evil--mark-location (val)
  "Return a \"BUFFER:LINE: CONTENT\" string describing evil mark value VAL.
VAL is whatever `evil-get-marker' returns: a marker, an integer
position in the current buffer, or a (FILE . POS) cons for an
unloaded global mark.  Returns nil when VAL is unrecognized."
  (cl-flet ((fmt (buf line content)
              (if (and content (not (string-empty-p content)))
                  (format "%s:%d: %s" buf line content)
                (format "%s:%d" buf line))))
    (cond
     ((markerp val)
      (when-let* ((buf (marker-buffer val))
                  (pos (marker-position val)))
        (with-current-buffer buf
          (fmt (buffer-name) (line-number-at-pos pos)
               (fzfa-evil--line-at pos)))))
     ((integerp val)
      (fmt (buffer-name) (line-number-at-pos val)
           (fzfa-evil--line-at val)))
     ((and (consp val) (stringp (car val)))
      (format "%s:%s" (car val) (cdr val))))))

(defun fzfa-evil--mark-entries ()
  "Return alist of (CHAR-STR . LOCATION) for evil marks that are set."
  (let (out)
    (cl-flet ((collect
                (char)
                (when-let* ((val (ignore-errors (evil-get-marker char)))
                            (loc (fzfa-evil--mark-location val)))
                  (push (cons (char-to-string char) loc) out))))
      (dolist (c (number-sequence ?a ?z)) (collect c))
      (dolist (c (number-sequence ?A ?Z)) (collect c))
      (dolist (c (number-sequence ?0 ?9)) (collect c)))
    (nreverse out)))

;;;###autoload
(defun fzfa-evil-marks ()
  "Jump to an evil mark, fuzzy-selected from the set of evil marks.
The candidate string includes the mark's location and line content
so fzf scores against the preview too — type a snippet of the line
to filter."
  (interactive)
  (require 'evil)
  (let ((entries (fzfa-evil--mark-entries)))
    (unless entries
      (user-error "No evil marks set"))
    (let* ((map (make-hash-table :test 'equal))
           (cands
            (mapcar (lambda (e)
                      (let* ((char (car e))
                             (loc (cdr e))
                             (display (format "%s  %s" char
                                              (propertize
                                               loc 'face
                                               'completions-annotations))))
                        (puthash display char map)
                        display))
                    entries)))
      (when-let* ((sel (fzfa-sync-completing-read
                        :candidates cands
                        :prompt "evil mark: "
                        :category 'fzfa-evil-mark
                        :preview
                        (lambda (cand)
                          (when-let* ((char (gethash cand map))
                                      (val (ignore-errors
                                             (evil-get-marker (aref char 0)))))
                            (cond
                             ((markerp val)
                              (fzfa-preview-show (marker-buffer val) val))
                             ((integerp val)
                              (fzfa-preview-show (current-buffer) val)))))))
                  (char (gethash sel map)))
        (fzfa-with-visit (evil-goto-mark (aref char 0)))))))

(defun fzfa-evil--register-preview (val)
  "Return a one-line preview string for register value VAL."
  (let ((s (cond
            ((stringp val) val)
            ((vectorp val) (key-description val))
            ((numberp val) (number-to-string val))
            ((markerp val)
             (if-let* ((buf (marker-buffer val)))
                 (format "<marker %s:%d>"
                         (buffer-name buf) (marker-position val))
               "<dead marker>"))
            ((consp val) (format "%S" val))
            ((functionp val) "<function>")
            (t (format "%S" val)))))
    (setq s (replace-regexp-in-string "\n" "⏎" (substring-no-properties s)))
    (if (> (length s) 200) (concat (substring s 0 200) "…") s)))

;;;###autoload
(defun fzfa-evil-registers ()
  "Fuzzy-select an evil register; paste text or execute a macro.
Vector / string-of-key-events values are executed as keyboard macros;
other values are inserted via `evil-paste-from-register'."
  (interactive)
  (require 'evil)
  (let ((entries (evil-register-list)))
    (unless entries
      (user-error "No evil registers set"))
    (let* ((map (make-hash-table :test 'equal))
           (cands
            (cl-loop for (char . val) in entries
                     for key = (char-to-string char)
                     for preview = (fzfa-evil--register-preview val)
                     do (puthash key (cons val preview) map)
                     collect key))
           (annotate (lambda (cand)
                       (concat "  "
                               (propertize (cdr (gethash cand map))
                                           'face 'completions-annotations))))
           (sel (fzfa-sync-completing-read
                 :candidates cands
                 :prompt "evil register: "
                 :category 'fzfa-evil-register
                 :annotate annotate)))
      (when sel
        (let ((val (car (gethash sel map)))
              (char (aref sel 0)))
          (if (vectorp val)
              (evil-execute-macro 1 val)
            (evil-paste-from-register char)))))))

(defun fzfa-evil--jump-format (entry)
  "Return (DISPLAY . ACTION-PLIST) for jump-list ENTRY, or nil to skip.
ENTRY is `(MARK FILE-NAME)' as stored in `evil--jumps-get-window-jump-list'.
MARK is a marker for in-session jumps and an integer for savehist-restored
jumps.  DISPLAY includes the line content (when the buffer is loaded) so
the fzf scorer can match against the preview text."
  (let* ((mark (car entry))
         (file (cadr entry))
         (pos  (if (markerp mark) (marker-position mark) mark))
         (buf  (or (and (markerp mark) (marker-buffer mark))
                   (and file (get-file-buffer file))))
         (line (and buf
                    (with-current-buffer buf (line-number-at-pos pos))))
         (content (and buf
                       (with-current-buffer buf
                         (fzfa-evil--line-at pos))))
         (label (cond
                 (buf (buffer-name buf))
                 (file (abbreviate-file-name file))
                 (t nil))))
    (when (and label pos)
      (cons (cond
             ((and content (not (string-empty-p content)))
              (format "%s:%s: %s" label (or line pos) content))
             (t
              (format "%s:%s" label (or line pos))))
            (list :file file :buffer buf :pos pos)))))

;;;###autoload
(defun fzfa-evil-jumps ()
  "Fuzzy-select an entry from the evil jump list and jump to it."
  (interactive)
  (require 'evil)
  (require 'evil-jumps)
  (require 'ring)
  (let* ((ring (evil--jumps-get-window-jump-list))
         (entries (ring-elements ring))
         (map (make-hash-table :test 'equal))
         (cands
          (cl-loop for e in entries
                   for formatted = (fzfa-evil--jump-format e)
                   when formatted
                   do (let ((d (car formatted)))
                        (while (gethash d map)
                          (setq d (concat d " ")))
                        (puthash d (cdr formatted) map))
                   and collect (car formatted))))
    (unless cands
      (user-error "Evil jump list is empty"))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates cands
                      :prompt "evil jump: "
                      :category 'fzfa-evil-jump
                      :preview
                      (lambda (cand)
                        (when-let* ((plist (gethash cand map))
                                    (buf (plist-get plist :buffer))
                                    ((buffer-live-p buf))
                                    (pos (plist-get plist :pos)))
                          (fzfa-preview-show buf pos)))))
                (plist (gethash sel map)))
      (let ((buf (plist-get plist :buffer))
            (file (plist-get plist :file))
            (pos (plist-get plist :pos)))
        (fzfa-with-visit
          (cond
           ((buffer-live-p buf) (switch-to-buffer buf))
           ((and file (file-exists-p file)) (find-file file))
           (t (user-error "Jump target unavailable")))
          (goto-char pos))))))

;;;###autoload
(defun fzfa-evil-ex-history ()
  "Fuzzy-select an entry from `evil-ex-history' and re-execute it."
  (interactive)
  (require 'evil)
  (unless evil-ex-history
    (user-error "Evil ex history is empty"))
  (when-let* ((sel (fzfa-sync-completing-read
                    :candidates (delete-dups (copy-sequence evil-ex-history))
                    :prompt ": "
                    :category 'fzfa-evil-ex-history)))
    (evil-ex-execute sel)))

(defun fzfa-evil--run-search (pattern)
  "Activate evil search for PATTERN going forward from point."
  (setq evil-ex-search-direction 'forward
        evil-ex-search-pattern (evil-ex-make-search-pattern pattern))
  (unless (equal pattern (car evil-ex-search-history))
    (push pattern evil-ex-search-history))
  (evil-ex-search-next 1))

;;;###autoload
(defun fzfa-evil-search-history ()
  "Fuzzy-select an entry from `evil-ex-search-history' and re-run the search."
  (interactive)
  (require 'evil)
  (unless evil-ex-search-history
    (user-error "Evil search history is empty"))
  (when-let* ((sel (fzfa-sync-completing-read
                    :candidates (delete-dups
                                 (copy-sequence evil-ex-search-history))
                    :prompt "/"
                    :category 'fzfa-evil-search-history)))
    (fzfa-evil--run-search sel)))

;;;###autoload
(defun fzfa-evil-command-window ()
  "Fuzzy-select from unified ex + search history.
Ex commands display with a `:' prefix, search patterns with `/'.
The prefix is stripped before dispatching to `evil-ex-execute' or
the evil search."
  (interactive)
  (require 'evil)
  (let* ((ex (mapcar (lambda (s) (concat ":" s))
                     (delete-dups (copy-sequence (or evil-ex-history '())))))
         (sr (mapcar (lambda (s) (concat "/" s))
                     (delete-dups
                      (copy-sequence (or evil-ex-search-history '())))))
         (cands (append ex sr))
         (group (lambda (cand transform)
                  (if transform
                      (substring cand 1)
                    (pcase (aref cand 0)
                      (?: "Ex")
                      (?/ "Search"))))))
    (unless cands
      (user-error "No evil ex or search history"))
    (when-let* ((sel (fzfa-sync-completing-read
                      :candidates cands
                      :prompt "evil: "
                      :category 'fzfa-evil-command-window
                      :group group)))
      (pcase (aref sel 0)
        (?: (evil-ex-execute (substring sel 1)))
        (?/ (fzfa-evil--run-search (substring sel 1)))))))

(defcustom fzfa-evil-any-commands
  '(fzfa-evil-marks
    fzfa-evil-jumps
    fzfa-evil-registers
    fzfa-evil-ex-history
    fzfa-evil-search-history)
  "Commands shown by `fzfa-evil-any'."
  :type '(repeat function)
  :group 'fzfa)

;;;###autoload
(defun fzfa-evil-any ()
  "Multi-source fuzzy completion over `fzfa-evil-any-commands'."
  (interactive)
  (require 'evil)
  (fzfa-multi-read fzfa-evil-any-commands :prompt "evil?: "))

;;;###autoload
(defun fzfa-evil-setup ()
  "Register completion categories for fzfa-evil commands."
  (dolist (cat '(fzfa-evil-mark
                 fzfa-evil-register
                 fzfa-evil-jump
                 fzfa-evil-ex-history
                 fzfa-evil-search-history
                 fzfa-evil-command-window))
    (add-to-list 'completion-category-overrides
                 `(,cat (styles fzfa)))))

(provide 'fzfa-evil)
;;; fzfa-evil.el ends here
