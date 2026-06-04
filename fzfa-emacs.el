;;; fzfa-emacs.el --- Built-in completion sources for `fzfa' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Keywords: convenience, matching
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs-internal sources for fzfa: recent files, buffers, the kill
;; ring, bookmarks, themes, TRAMP hosts, current-buffer / all-buffer
;; line search, and imenu.
;;
;; Loaded automatically when `emacs' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-recent-file'              Find a recently visited file
;;   `fzfa-buffer'                   Switch to an open buffer
;;   `fzfa-yank-pop'                 Yank from `kill-ring' with fzf
;;   `fzfa-bookmark'                 Jump to a bookmark
;;   `fzfa-theme'                    Enable a theme with live preview
;;   `fzfa-tramp'                    Connect to a host from ~/.ssh/config
;;   `fzfa-swiper'                   Search lines of the current buffer
;;   `fzfa-swiper-all'               Search lines across all open buffers
;;   `fzfa-imenu'                    Jump to an imenu entry in this buffer
;;   `fzfa-imenu-all'                Jump to an imenu entry across buffers
;;   `fzfa-imenu-all-but-current'    Like `fzfa-imenu-all' but skip current
;;   `fzfa-M-x'                      Run an extended command (like \\[execute-extended-command])
;;   `fzfa-M-x-for-buffer'           Run an extended command applicable to the current mode
;;   `fzfa-mark'                     Jump to a position in this buffer's `mark-ring'
;;   `fzfa-global-mark'              Jump to a position in `global-mark-ring'
;;   `fzfa-register'                 Use a register (jump-to or insert based on type)
;;   `fzfa-outline'                  Jump to an outline heading in this buffer
;;   `fzfa-compile-error'            Jump to an error from a compilation buffer

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(declare-function outline-next-heading "outline")
(declare-function compilation-next-error "compile" (n &optional types start))
(declare-function compile-goto-error "compile" (&optional event))
(defvar recentf-list)

;;;###autoload
(defun fzfa-recent-file ()
  "Find a recently visited file using `recentf'."
  (interactive)
  (require 'recentf)
  (recentf-mode 1)
  (unless recentf-list
    (user-error "No recent files"))
  (when-let* ((result (fzfa-sync-completing-read :candidates recentf-list
                                                :prompt "recent: "
                                                :category 'fzfa-file)))
    (find-file result)))

;;;###autoload
(defun fzfa-buffer ()
  "Switch to an open buffer."
  (interactive)
  (let* ((names (cl-loop for b in (buffer-list)
                         unless (or (minibufferp b)
                                    (string-prefix-p " " (buffer-name b)))
                         collect (buffer-name b))))
    (when-let* ((result (fzfa-sync-completing-read
                         :candidates names :prompt "buffer: "
                         :category 'fzfa-buffer)))
      (switch-to-buffer result))))

;;;###autoload
(defun fzfa-yank-pop ()
  "Yank from `kill-ring' using fzf for selection.
When invoked immediately after a yank command, replaces the previously
yanked text with the selection (mirroring `yank-pop' / `consult-yank-pop')."
  (interactive "*")
  (unless kill-ring
    (user-error "Kill ring is empty"))
  (let* ((seen (make-hash-table :test 'equal))
         (lookup (make-hash-table :test 'equal))
         (entries
          (cl-loop
           for s in kill-ring
           for clean = (substring-no-properties s)
           unless (or (string-empty-p clean) (gethash clean seen))
           do (puthash clean t seen)
           and collect
           (let ((display
                  (replace-regexp-in-string
                   "\n" (propertize "⏎" 'face 'shadow)
                   (if (> (length clean) 200)
                       (concat (substring clean 0 200) "…")
                     clean))))
             ;; Disambiguate displays collapsed to the same string
             ;; (e.g. entries differing only in stripped properties).
             (while (gethash display lookup)
               (setq display (concat display " ")))
             (puthash display s lookup)
             display))))
    (when-let* ((result (fzfa-sync-completing-read
                         :candidates entries
                         :prompt "yank-pop: "))
                (text (gethash result lookup)))
      (cond
       ((eq last-command 'yank)
        (let ((inhibit-read-only t)
              (pt (point))
              (mk (mark t)))
          (funcall (or yank-undo-function #'delete-region)
                   (min pt mk) (max pt mk))
          (setq yank-undo-function nil)
          (set-marker (mark-marker) pt (current-buffer))
          (insert-for-yank text)))
       (t
        (push-mark)
        (insert-for-yank text)))
      (setq this-command 'yank))))

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))

;;;###autoload
(defun fzfa-bookmark ()
  "Jump to a bookmark."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let ((names (bookmark-all-names)))
    (unless names
      (user-error "No bookmarks defined"))
    (when-let* ((result
                 (fzfa-sync-completing-read
                  :candidates names :prompt "bookmark: "
                  :category 'fzfa-bookmark
                  :preview
                  `(:setup
                    ,(lambda ()
                       (fzfa-preview-put :opener (fzfa--temporary-files)))
                    :preview
                    ,(lambda (cand)
                       (when-let* ((file (ignore-errors
                                           (bookmark-get-filename cand)))
                                   ((stringp file))
                                   (path (expand-file-name file))
                                   ((file-readable-p path))
                                   ((not (file-directory-p path)))
                                   (attrs (file-attributes path))
                                   (size (file-attribute-size attrs))
                                   (limit fzfa-preview-file-size-limit)
                                   ((or (null limit) (zerop limit)
                                        (< size limit)))
                                   (opener (fzfa-preview-get :opener))
                                   (buf (funcall opener path))
                                   (pos (or (ignore-errors
                                              (bookmark-get-position cand))
                                            1)))
                         (fzfa-preview-show buf pos)))
                    :return
                    ,(lambda (cand)
                       (when-let* ((opener (fzfa-preview-get :opener)))
                         (when cand
                           (when-let* ((file (ignore-errors
                                               (bookmark-get-filename cand)))
                                       ((stringp file))
                                       (buf (get-file-buffer
                                             (expand-file-name file))))
                             (funcall opener buf)))
                         (funcall opener)))))))
      (bookmark-jump result))))

(defun fzfa--theme-switch (sym)
  "Disable currently enabled themes (except SYM) and enable SYM, if any.
SYM nil means leave nothing enabled."
  (dolist (th custom-enabled-themes)
    (unless (eq th sym) (disable-theme th)))
  (when (and sym (not (memq sym custom-enabled-themes)))
    (if (custom-theme-p sym)
        (enable-theme sym)
      (load-theme sym :no-confirm))))

(defun fzfa--theme-symbol (cand)
  "Return the theme symbol for CAND, or nil for the \"default\" sentinel."
  (and cand (not (equal cand "default")) (intern cand)))

;;;###autoload
(defun fzfa-theme ()
  "Prompt for a theme to enable, previewing each candidate live.
Aborting (e.g. \\[keyboard-quit]) restores the themes that were enabled
when the command was invoked.  Selecting \"default\" disables all themes."
  (interactive)
  (fzfa-sync-completing-read
   :candidates (cons "default"
                     (mapcar #'symbol-name (custom-available-themes)))
   :prompt "theme: "
   :category 'fzfa-theme
   :preview `(:setup
              ,(lambda ()
                 (fzfa-preview-put :saved
                                   (copy-sequence custom-enabled-themes)))
              :preview
              ,(lambda (cand)
                 (when cand
                   (fzfa--theme-switch (fzfa--theme-symbol cand))))
              :return
              ,(lambda (cand)
                 (if cand
                     (fzfa--theme-switch (fzfa--theme-symbol cand))
                   (mapc #'disable-theme custom-enabled-themes)
                   (mapc #'enable-theme
                         (reverse (fzfa-preview-get :saved))))))))

;;;###autoload
(defun fzfa-tramp ()
  "Connect to a remote host via TRAMP, with hosts from ~/.ssh/config."
  (interactive)
  (cl-labels ((ssh-hosts ()
                (let ((config (expand-file-name "~/.ssh/config"))
                      hosts)
                  (when (file-readable-p config)
                    (with-temp-buffer
                      (insert-file-contents config)
                      (while (re-search-forward
                              "^[Hh]ost[[:space:]]+\\(.+\\)" nil t)
                        (dolist (host (split-string (match-string 1)))
                          (unless (string-match-p "[*?!]" host)
                            (push host hosts))))))
                  (nreverse hosts))))
    (when-let* ((hosts (or (ssh-hosts)
                           (user-error "No SSH hosts in ~/.ssh/config")))
                (host (fzfa-sync-completing-read
                       :candidates hosts :prompt "ssh: ")))
      (find-file (concat "/ssh:" host ":")))))

;;;###autoload
(defun fzfa-swiper ()
  "Search lines of the current buffer using fzf."
  (interactive)
  (let* ((source (or (buffer-file-name) (buffer-name)))
         (candidates
          (let (lines)
            (save-excursion
              (goto-char (point-min))
              (let ((i 1))
                (while (not (eobp))
                  (let ((content (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
                    (unless (string-empty-p content)
                      (push (format "%s:%d:%s" source i content) lines)))
                  (forward-line 1)
                  (cl-incf i))))
            (nreverse lines))))
    (when-let* ((r (fzfa-sync-completing-read :candidates candidates :prompt "swiper: "
                                             :category 'fzfa-grep)))
      (fzfa--grep-jump r))))

;;;###autoload
(defun fzfa-swiper-all ()
  "Search lines across all open buffers using fzf.
Candidates are formatted as SOURCE:LINE:CONTENT where SOURCE is the
buffer's file path when file-backed, else its buffer name.  Buffer
names containing `:DIGITS:' substrings are not encoded specially and
may parse ambiguously — a rare-enough hazard to accept."
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (b)
                     (or (minibufferp b)
                         (string-prefix-p " " (buffer-name b))))
                   (buffer-list)))
         (candidates
          (cl-loop
           for buf in buffers
           for source = (or (buffer-file-name buf) (buffer-name buf))
           append (with-current-buffer buf
                    (let (lines)
                      (save-excursion
                        (goto-char (point-min))
                        (let ((j 1))
                          (while (not (eobp))
                            (let ((content (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
                              (unless (string-empty-p content)
                                (push (format "%s:%d:%s" source j content)
                                      lines)))
                            (forward-line 1)
                            (cl-incf j))))
                      (nreverse lines))))))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt "swiper-all: "
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group)))
      (fzfa--grep-jump r))))

(defun fzfa--imenu (scope)
  "Implementation of `fzfa-imenu' / `fzfa-imenu-all'.
SCOPE selects which buffers to walk:
  nil / `current'  — just the current buffer.
  `all'            — every live non-internal buffer.
  `others'         — every live non-internal buffer except the current one.
Display differences:
- Single buffer: display = NAME (with \"(CATEGORY)\" appended on
  cross-category name collision); group header = imenu category.
- Multi buffer:  display = \"[CATEGORY] NAME\" (no collision possible —
  entries are already partitioned by buffer); group header = buffer name."
  (require 'imenu)
  (let* ((multi (memq scope '(all others)))
         (buf-vec (vconcat
                   (pcase scope
                     ((or 'all 'others)
                      (cl-remove-if
                       (lambda (b)
                         (or (minibufferp b)
                             (string-prefix-p " " (buffer-name b))
                             (and (eq scope 'others)
                                  (eq b (current-buffer)))))
                       (buffer-list)))
                     (_ (list (current-buffer))))))
         (entries nil)
         (lookup (make-hash-table :test 'equal))
         (groups (make-hash-table :test 'equal)))
    (cl-loop
     for buf across buf-vec
     for i from 0
     for index = (with-current-buffer buf
                   (ignore-errors (imenu--make-index-alist t)))
     when index do
     (cl-labels
         ((walk (alist category)
            (dolist (entry alist)
              (cond
               ((or (null entry) (equal (car entry) "*Rescan*")))
               ((imenu--subalist-p entry)
                (walk (cdr entry) (car entry)))
               (t
                (let* ((name (car entry))
                       (display
                        (if multi
                            (format "%d:%s%s"
                                    i
                                    (if category (format "[%s] " category) "")
                                    name)
                          ;; Disambiguate cross-category name collisions
                          ;; (e.g. an elisp function and variable named foo).
                          (if (and category (gethash name lookup))
                              (format "%s (%s)" name category)
                            name))))
                  (push display entries)
                  (puthash display (cons i entry) lookup)
                  (when (and (not multi) category)
                    (puthash display category groups))))))))
       (walk index nil)))
    (unless entries
      (user-error "No imenu entries%s" (if multi " in any buffer" "")))
    (when-let* ((result
                 (fzfa-sync-completing-read
                  :candidates (nreverse entries)
                  :prompt (pcase scope
                            ('all    "imenu-all: ")
                            ('others "imenu-others: ")
                            (_       "imenu: "))
                  :category 'fzfa-imenu
                  :preview
                  (lambda (cand)
                    (when-let* ((hit (gethash cand lookup))
                                (idx (car hit))
                                ((< idx (length buf-vec)))
                                (buf (aref buf-vec idx))
                                (entry (cdr hit))
                                (val (cdr entry))
                                (pos (cond
                                      ((markerp val) val)
                                      ((numberp val) val)
                                      ((overlayp val) (overlay-start val)))))
                      (fzfa-preview-show buf pos)))
                  :group
                  (lambda (cand transform)
                    (cond
                     ((not multi)
                      (if transform cand (or (gethash cand groups) "")))
                     (transform
                      ;; Strip "IDX:" prefix for display.
                      (when (string-match "^[0-9]+:\\(.*\\)$" cand)
                        (match-string 1 cand)))
                     (t
                      ;; Header: reverse-map IDX → buffer name.
                      (when (string-match "^\\([0-9]+\\):" cand)
                        (let ((i (string-to-number (match-string 1 cand))))
                          (when (< i (length buf-vec))
                            (buffer-name (aref buf-vec i))))))))))
                (hit (gethash result lookup))
                (idx (car hit))
                ((< idx (length buf-vec)))
                (buffer (aref buf-vec idx))
                ((buffer-live-p buffer)))
      (unless (eq buffer (current-buffer))
        (switch-to-buffer buffer))
      (push-mark nil t)
      (imenu (cdr hit)))))

;;;###autoload
(defun fzfa-imenu ()
  "Jump to an imenu entry in the current buffer using fzf."
  (interactive)
  (fzfa--imenu 'current))

;;;###autoload
(defun fzfa-imenu-all ()
  "Jump to an imenu entry across all open buffers using fzf.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzfa--imenu 'all))

;;;###autoload
(defun fzfa-imenu-all-but-current ()
  "Jump to an imenu entry across all open buffers except the current one.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzfa--imenu 'others))

(defun fzfa--commands (&optional predicate)
  "Return a sorted list of command names as strings.
When PREDICATE is non-nil, only include commands for which
\(funcall PREDICATE SYMBOL) returns non-nil."
  (let (commands)
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (or (not predicate) (funcall predicate sym)))
         (push (symbol-name sym) commands))))
    (sort commands #'string<)))

(defun fzfa--run-command (name)
  "Execute the command named NAME, recording it like \\[execute-extended-command]."
  (let ((cmd (intern name)))
    (setq this-command cmd
          real-this-command cmd)
    (command-execute cmd 'record)))

;;;###autoload
(defun fzfa-M-x ()
  "Run an extended command using fzf, like \\[execute-extended-command]."
  (interactive)
  (when-let* ((result (fzfa-sync-completing-read
                       :candidates (fzfa--commands)
                       :prompt "M-x: "
                       :category 'command
                       :history 'extended-command-history)))
    (fzfa--run-command result)))

;;;###autoload
(defun fzfa-M-x-for-buffer ()
  "Run an extended command applicable to the current buffer's mode.
Filters using `command-completion-default-include-p' when available,
mirroring `execute-extended-command-for-buffer'."
  (interactive)
  (let* ((buffer (current-buffer))
         (predicate
          (when (fboundp 'command-completion-default-include-p)
            (lambda (sym)
              (command-completion-default-include-p sym buffer)))))
    (when-let* ((result (fzfa-sync-completing-read
                         :candidates (fzfa--commands predicate)
                         :prompt (format "M-x [%s]: " major-mode)
                         :category 'command
                         :history 'extended-command-history)))
      (fzfa--run-command result))))

(defun fzfa--mark-candidates (markers)
  "Build FILE:LINE:CONTENT candidates from MARKERS.
Each marker contributes one candidate showing the buffer (or
file path) it points into, the line number, and the line text.
Disambiguates duplicate display strings with a trailing space so
fzf-native's distinct-candidate guarantee holds."
  (let ((used (make-hash-table :test 'equal)))
    (cl-loop
     for m in markers
     for buf = (and (markerp m) (marker-buffer m))
     for pos = (and (markerp m) (marker-position m))
     when (and buf (buffer-live-p buf) pos)
     collect
     (with-current-buffer buf
       (save-excursion
         (when (and (<= (point-min) pos) (<= pos (point-max)))
           (goto-char pos)
           (let* ((source (or (buffer-file-name) (buffer-name)))
                  (line (line-number-at-pos))
                  (content (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                  (display (format "%s:%d:%s" source line content)))
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             display)))))))

;;;###autoload
(defun fzfa-mark ()
  "Jump to a position in the current buffer's `mark-ring' using fzf.
Includes the buffer's current `mark-marker' as the first candidate.
Selection pushes point onto the mark ring before moving so the prior
position is recoverable with \\[set-mark-command] \\[set-mark-command]."
  (interactive)
  (let* ((markers (cl-remove-if-not
                   (lambda (m) (eq (and (markerp m) (marker-buffer m))
                                   (current-buffer)))
                   (cons (mark-marker) mark-ring)))
         (candidates (cl-remove-if-not #'identity
                                       (fzfa--mark-candidates markers))))
    (unless candidates
      (user-error "No marks in this buffer"))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt "mark: "
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group)))
      (push-mark nil t)
      (fzfa--grep-jump r))))

;;;###autoload
(defun fzfa-global-mark ()
  "Jump to a position in `global-mark-ring' using fzf.
Selection switches buffers when needed and moves point to the marked
line.  Dead markers (buffer killed or position out of range) are
silently skipped."
  (interactive)
  (unless global-mark-ring
    (user-error "Global mark ring is empty"))
  (let ((candidates (cl-remove-if-not #'identity
                                      (fzfa--mark-candidates
                                       global-mark-ring))))
    (unless candidates
      (user-error "No live global marks"))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt "global-mark: "
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group)))
      (push-mark nil t)
      (fzfa--grep-jump r))))

;;;###autoload
(defun fzfa-register ()
  "Select and use a register from `register-alist' via fzf.
Dispatches by type: position-class registers (markers, frame and
window configurations, framesets, file/file-query, bookmark) jump;
remaining types (text, number, rectangle, …) insert at point.
Falls back to `insert-register' when `jump-to-register' signals."
  (interactive)
  (unless register-alist
    (user-error "No registers set"))
  (let* ((lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for (name . val) in register-alist
           for label = (cond
                        ((characterp name) (single-key-description name))
                        ((symbolp name)    (symbol-name name))
                        (t                  (format "%S" name)))
           for preview = (or (and (fboundp 'register-describe-oneline)
                                  (ignore-errors
                                    (substring-no-properties
                                     (register-describe-oneline name))))
                             (replace-regexp-in-string
                              "\n" (propertize "⏎" 'face 'shadow)
                              (format "%S" val)))
           for display = (format "[%s] %s" label preview)
           collect
           (progn
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display name lookup)
             display))))
    (when-let* ((r    (fzfa-sync-completing-read
                       :candidates candidates
                       :prompt "register: "
                       :category 'fzfa-misc))
                (name (gethash r lookup)))
      (condition-case _
          (jump-to-register name)
        (error (insert-register name))))))

;;;###autoload
(defun fzfa-outline ()
  "Jump to an outline heading in the current buffer using fzf.
Walks every line matching `outline-regexp' (set by `outline-mode',
`outline-minor-mode', or the major mode's own definition — most
programming modes set one).  Selection pushes point onto the mark
ring and moves to the heading line.

Candidates are formatted as SOURCE:LINE:CONTENT so the `fzfa-grep'
category's group function and embark map apply automatically."
  (interactive)
  (require 'outline)
  (unless (and (boundp 'outline-regexp) outline-regexp)
    (user-error "No `outline-regexp' set in this buffer"))
  (let* ((source (or (buffer-file-name) (buffer-name)))
         (used (make-hash-table :test 'equal))
         (candidates
          (save-excursion
            (goto-char (point-min))
            (let (out)
              (while (outline-next-heading)
                (let* ((line    (line-number-at-pos))
                       (content (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                       (display (format "%s:%d:%s" source line content)))
                  (while (gethash display used)
                    (setq display (concat display " ")))
                  (puthash display t used)
                  (push display out)))
              (nreverse out)))))
    (unless candidates
      (user-error "No outline headings in buffer"))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt "outline: "
                    :category 'fzfa-grep
                    :group #'fzfa--grep-group)))
      (push-mark nil t)
      (fzfa--grep-jump r))))

;;;###autoload
(defun fzfa-compile-error ()
  "Jump to an error from a compilation buffer using fzf.
Walks the current buffer when it derives from `compilation-mode',
otherwise the buffer returned by `next-error-find-buffer' (typically
the most recent compile/grep buffer).  Each line carrying a
`compilation-message' text property contributes a candidate showing
that line's text verbatim.

Selection pops to the compile buffer at the chosen error and
invokes `compile-goto-error', which resolves the source location
via the compile buffer's own `default-directory' and error
regexp — so relative paths and unconventional formats work as long
as `compile' itself can navigate them."
  (interactive)
  (require 'compile)
  (let ((buffer (or (and (derived-mode-p 'compilation-mode) (current-buffer))
                    (next-error-find-buffer))))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No compilation buffer"))
    (let* ((lookup (make-hash-table :test 'equal))
           (used   (make-hash-table :test 'equal))
           (candidates
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (let (out)
                  (while (condition-case nil
                             (progn (compilation-next-error 1) t)
                           (error nil))
                    (let* ((content (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                           (display (if (string-empty-p content)
                                        (format "<line %d>"
                                                (line-number-at-pos))
                                      content)))
                      (while (gethash display used)
                        (setq display (concat display " ")))
                      (puthash display t used)
                      (puthash display (point) lookup)
                      (push display out)))
                  (nreverse out))))))
      (unless candidates
        (user-error "No errors in %s" (buffer-name buffer)))
      (when-let* ((r   (fzfa-sync-completing-read
                        :candidates candidates
                        :prompt (format "compile-error[%s]: "
                                        (buffer-name buffer))
                        :category 'fzfa-misc))
                  (pos (gethash r lookup)))
        (pop-to-buffer buffer)
        (goto-char pos)
        (compile-goto-error)))))

(provide 'fzfa-emacs)

;; Local Variables:
;; package-lint-main-file: "fzfa.el"
;; End:

;;; fzfa-emacs.el ends here
