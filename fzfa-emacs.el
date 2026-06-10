;;; fzfa-emacs.el --- Built-in completion sources for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs-internal sources for fzfa: recent files, buffers, the kill
;; ring, bookmarks, themes, TRAMP hosts, and current-buffer /
;; all-buffer line search.
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
;;   `fzfa-M-x'                      Run an extended command
;;   `fzfa-M-x-for-buffer'           Run an extended command for current mode
;;   `fzfa-minor-mode-menu'          Toggle a minor mode (on/off annotated)
;;   `fzfa-mark'                     Jump to a position in this buffer's marks
;;   `fzfa-global-mark'              Jump to a position in `global-mark-ring'
;;   `fzfa-register'                 Use a register (jump-to or insert)
;;   `fzfa-outline'                  Jump to an outline heading in this buffer
;;   `fzfa-compile-error'            Jump to an error from a compilation buffer
;;   `fzfa-frames'                   Switch focus to another live frame
;;   `fzfa-tabs'                     Switch to a tab on the current frame

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
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
    (fzfa-with-visit (find-file result))))

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
      (fzfa-with-visit (switch-to-buffer result)))))

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
      (fzfa-with-visit (bookmark-jump result)))))

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
   :apply (lambda (cand)
            (fzfa--theme-switch (fzfa--theme-symbol cand)))
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
      (fzfa-with-visit (find-file (concat "/ssh:" host ":"))))))

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
                      (push (fzfa--location-candidate
                             (format "%d:%s" i content) source i)
                            lines)))
                  (forward-line 1)
                  (cl-incf i))))
            (nreverse lines))))
    (fzfa-with-visit
      (fzfa--location-jump
       (fzfa-sync-completing-read :candidates candidates
                                  :prompt "swiper: "
                                  :category 'fzfa-location)))))

;;;###autoload
(defun fzfa-swiper-all ()
  "Search lines across all open buffers using fzf.
Candidates display LINE:CONTENT; the originating buffer (file path or
buffer name) is carried on each candidate as an `fzfa-location' text
property and shown as the group header.  fzf scores only against
LINE:CONTENT — buffer names never enter the search input."
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (b)
                     (or (minibufferp b)
                         (string-prefix-p " " (buffer-name b))))
                   (buffer-list)))
         (used (make-hash-table :test 'equal))
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
                                (let ((display (format "%d:%s" j content)))
                                  (while (gethash display used)
                                    (setq display (concat display " ")))
                                  (puthash display t used)
                                  (push (fzfa--location-candidate
                                         display source j)
                                        lines))))
                            (forward-line 1)
                            (cl-incf j))))
                      (nreverse lines))))))
    (fzfa-with-visit
      (fzfa--location-jump
       (fzfa-sync-completing-read
        :candidates candidates
        :prompt "swiper-all: "
        :category 'fzfa-location
        :group #'fzfa--location-group)))))

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
  "Execute the command named NAME.
Records it like \\[execute-extended-command]."
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

;;;###autoload
(defun fzfa-minor-mode-menu ()
  "Toggle a minor mode via fzf.
Candidates are every command-bound symbol in `minor-mode-list'.
Enabled modes sort to the top with an `[on]' prefix; disabled modes
follow with `[off]'.  Selecting calls the mode function via
`call-interactively' — toggling it for buffer-local modes, flipping
the global state for global modes."
  (interactive)
  (cl-labels ((active-p (m) (and (boundp m) (symbol-value m))))
    (let* ((modes (cl-remove-if-not
                   (lambda (m) (and (boundp m) (commandp m)))
                   minor-mode-list))
           (sorted (sort (copy-sequence modes)
                         (lambda (a b)
                           (let ((av (active-p a)) (bv (active-p b)))
                             (cond ((and av (not bv)) t)
                                   ((and bv (not av)) nil)
                                   (t (string< (symbol-name a)
                                               (symbol-name b))))))))
           (cands (mapcar #'symbol-name sorted))
           (affix
            (lambda (cs)
              (mapcar (lambda (c)
                        (let* ((sym (intern-soft c))
                               (on  (and sym (active-p sym))))
                          (list c
                                (propertize (if on "[on]  " "[off] ")
                                            'face (if on 'success 'shadow))
                                "")))
                      cs))))
      (unless cands
        (user-error "No minor modes available"))
      (when-let* ((sel (fzfa-sync-completing-read
                        :candidates cands
                        :prompt "minor mode: "
                        :category 'fzfa-minor-mode
                        :affix affix)))
        (call-interactively (intern sel))))))

(defun fzfa--mark-candidates (markers)
  "Build `fzfa-location' candidates from MARKERS.
Each marker contributes one candidate showing LINE:CONTENT of the
target line, with the originating buffer (file path or buffer name)
and the line number carried as an `fzfa-location' text property.
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
                  (display (format "%d:%s" line content)))
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (fzfa--location-candidate display source line))))))))

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
                    :category 'fzfa-location
                    :group #'fzfa--location-group)))
      (push-mark nil t)
      (fzfa-with-visit (fzfa--location-jump r)))))

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
                    :category 'fzfa-location
                    :group #'fzfa--location-group)))
      (push-mark nil t)
      (fzfa-with-visit (fzfa--location-jump r)))))

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
      (fzfa-with-visit
        (condition-case _
            (jump-to-register name)
          (error (insert-register name)))))))

;;;###autoload
(defun fzfa-outline ()
  "Jump to an outline heading in the current buffer using fzf.
Walks every line matching `outline-regexp' (set by `outline-mode',
`outline-minor-mode', or the major mode's own definition — most
programming modes set one).  Selection pushes point onto the mark
ring and moves to the heading line.

Candidates display LINE:CONTENT; the source buffer/file is carried as
an `fzfa-location' text property so fzf scores only the heading text
and line number."
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
                       (display (format "%d:%s" line content)))
                  (while (gethash display used)
                    (setq display (concat display " ")))
                  (puthash display t used)
                  (push (fzfa--location-candidate display source line)
                        out)))
              (nreverse out)))))
    (unless candidates
      (user-error "No outline headings in buffer"))
    (when-let* ((r (fzfa-sync-completing-read
                    :candidates candidates
                    :prompt "outline: "
                    :category 'fzfa-location)))
      (push-mark nil t)
      (fzfa-with-visit (fzfa--location-jump r)))))

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
        (fzfa-with-visit
          (pop-to-buffer buffer)
          (goto-char pos)
          (compile-goto-error))))))

;;;###autoload
(defun fzfa-frames ()
  "Pick a live Emacs frame other than the current one and switch focus to it.

Candidates are visible and iconified frames excluding the
currently-selected frame.  Selecting via RET calls
`select-frame-set-input-focus' (deiconifying first when needed).
The apply key (`fzfa-apply-key', default \\[fzfa-apply-current])
focuses without exiting the picker — useful for visually
confirming which frame the candidate refers to before committing.

Previews show the candidate frame's selected-window buffer in the
originating window, so the picker keeps focus while you browse."
  (interactive)
  (let* ((current (selected-frame))
         (all (frame-list))
         (frames (cl-remove-if
                  (lambda (f)
                    (or (eq f current)
                        (frame-parameter f 'no-accept-focus)
                        (not (memq (frame-visible-p f) '(t icon)))))
                  all))
         (lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for f in frames
           for name = (or (frame-parameter f 'name)
                          (format "Frame-%d"
                                  (1+ (cl-position f all))))
           for buf  = (buffer-name (window-buffer (frame-selected-window f)))
           for vis  = (if (eq (frame-visible-p f) 'icon)
                          "iconified"
                        "visible")
           for display = (format "[%s]  %s  %dx%d  %s"
                                 name buf
                                 (frame-width f) (frame-height f)
                                 vis)
           do (progn
                (while (gethash display used)
                  (setq display (concat display " ")))
                (puthash display t used)
                (puthash display f lookup))
           collect display)))
    (unless candidates
      (user-error "No other frames"))
    (cl-labels
        ((focus-frame (cand)
           (when-let* ((f (gethash cand lookup)) ((frame-live-p f)))
             (when (eq (frame-visible-p f) 'icon)
               (make-frame-visible f))
             (fzfa-with-visit (select-frame-set-input-focus f))))
         (preview-frame (cand)
           (when-let* ((f (gethash cand lookup))
                       ((frame-live-p f))
                       (win (frame-selected-window f))
                       ((window-live-p win))
                       (buf (window-buffer win))
                       ((buffer-live-p buf)))
             (fzfa-preview-show buf (window-point win)))))
      (when-let* ((result (fzfa-sync-completing-read
                           :candidates candidates
                           :prompt "frame: "
                           :category 'fzfa-frame
                           :apply #'focus-frame
                           :preview #'preview-frame)))
        (focus-frame result)))))

;;;###autoload
(defun fzfa-tabs ()
  "Pick a tab on the current frame and switch to it.

Candidates include every tab on the selected frame, marking the
active one.  Selecting via RET calls `tab-bar-select-tab' with
the chosen tab's 1-based index.  The apply key (`fzfa-apply-key',
default \\[fzfa-apply-current]) switches without exiting the picker,
so you can hop between tabs visually before committing.

Previews show a representative buffer for the candidate tab — the
current buffer for the active tab, or the head of the tab's saved
buffer list for the others."
  (interactive)
  (require 'tab-bar)
  (let* ((tabs (tab-bar-tabs))
         (lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for tab in tabs
           for i from 1
           unless (eq (car tab) 'current-tab)
           collect
           (let* ((name (or (alist-get 'name tab) (format "Tab-%d" i)))
                  (b (car (alist-get 'wc-bl tab)))
                  (buf (and (buffer-live-p b) (buffer-name b)))
                  (display (format "[%d]  %s%s"
                                   i name
                                   (if (and buf (not (equal buf name)))
                                       (format "  %s" buf)
                                     ""))))
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display i lookup)
             display))))
    (unless candidates
      (user-error "No other tabs"))
    (cl-labels
        ((tab-buffer (i)
           (car (alist-get 'wc-bl (nth (1- i) tabs))))
         (switch-tab (cand)
           (when-let* ((i (gethash cand lookup)))
             (fzfa-with-visit (tab-bar-select-tab i))))
         (preview-tab (cand)
           (when-let* ((i (gethash cand lookup))
                       (buf (tab-buffer i))
                       ((buffer-live-p buf)))
             (fzfa-preview-show buf))))
      (when-let* ((result (fzfa-sync-completing-read
                           :candidates candidates
                           :prompt "tab: "
                           :category 'fzfa-tab
                           :apply #'switch-tab
                           :preview #'preview-tab)))
        (switch-tab result)))))

(provide 'fzfa-emacs)

;; Local Variables:
;; package-lint-main-file: "fzfa.el"
;; End:

;;; fzfa-emacs.el ends here
