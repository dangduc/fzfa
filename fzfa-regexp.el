;;; fzfa-regexp.el --- Regexp picker for fzfa  -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/fzfa
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching
;; Version: 0.1

;;; Commentary:

;; Pick a line from the current buffer by Elisp regexp + fzf-native
;; fuzzy filter.  Two-phase semantics; capture groups get distinct
;; theme-aware highlight colours; remap them via
;; `fzfa-regexp-group-faces'.
;;
;; See the project README for the full tutorial and example regexps.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defface fzfa-regexp-match
  '((t :underline t))
  "Face for the regexp match within an `fzfa-regexp' candidate line."
  :group 'fzfa)

;; Capture-group faces inherit from font-lock faces every theme styles
;; with distinct, well-contrasted colours.  Inheriting (rather than
;; hard-coding hex) means theme switches keep the highlights readable
;; and on-palette without any per-theme tuning here.

(defface fzfa-regexp-match-1
  '((t :inherit (fzfa-regexp-match font-lock-keyword-face)))
  "Face for capture group 1 in an `fzfa-regexp' match.

Inherits from `font-lock-keyword-face' — themes commonly style this
in a distinct hue (often red/blue/purple) with strong contrast."
  :group 'fzfa)

(defface fzfa-regexp-match-2
  '((t :inherit (fzfa-regexp-match font-lock-function-name-face)))
  "Face for capture group 2 in an `fzfa-regexp' match.

Inherits from `font-lock-function-name-face' — themes commonly style
this in a blue/teal/yellow distinct from keyword and string faces."
  :group 'fzfa)

(defface fzfa-regexp-match-3
  '((t :inherit (fzfa-regexp-match font-lock-string-face)))
  "Face for capture group 3 in an `fzfa-regexp' match.

Inherits from `font-lock-string-face' — themes commonly style this
in green/orange, visibly distinct from the keyword/function colours."
  :group 'fzfa)

(defface fzfa-regexp-match-4
  '((t :inherit (fzfa-regexp-match font-lock-type-face)))
  "Face for capture group 4 in an `fzfa-regexp' match.

Inherits from `font-lock-type-face' — themes commonly style this
in purple/teal, distinct from the keyword/function/string hues."
  :group 'fzfa)

(defcustom fzfa-regexp-group-faces
  '(fzfa-regexp-match-1
    fzfa-regexp-match-2
    fzfa-regexp-match-3
    fzfa-regexp-match-4)
  "Faces layered on top of `fzfa-regexp-match' for each capture group.

The Nth element is applied to capture group (N+1) of the user's regexp.
A nil entry suppresses highlighting for that group.  Groups beyond the
list's length are not highlighted."
  :type '(repeat (choice face (const :tag "Suppress" nil)))
  :group 'fzfa)

(defun fzfa-regexp--format-line (buf line text)
  "Build candidate for line LINE of BUF; no match face.

TEXT is the line text."
  (propertize (format "%d: %s" line text)
              'fzfa-regexp-buffer buf
              'fzfa-regexp-line line))

(defun fzfa-regexp--format-match (buf line md)
  "Build candidate for a regexp match in BUF.

LINE is the line number.  MD is the match-data list
\(BEG0 END0 BEG1 END1 …): index 0 bounds the whole match, indices
1, 2, … bound capture groups.  The matched region within the
candidate's line text carries `fzfa-regexp-match'; each captured
group additionally gets the corresponding face from
`fzfa-regexp-group-faces' layered on top via `add-face-text-property',
which the default fzf-native highlight handler preserves."
  (with-current-buffer buf
    (let* ((beg      (nth 0 md))
           (end      (nth 1 md))
           (line-beg (save-excursion (goto-char beg) (pos-bol)))
           (line-end (save-excursion (goto-char beg) (pos-eol)))
           (text     (buffer-substring-no-properties line-beg line-end))
           (col      (- beg line-beg))
           ;; Offset from CAND's index 0 to the buffer's BEG.
           (off      (+ (length (format "%d:%d: " line col)) col)))
      (let ((cand (format "%d:%d: %s" line col text)))
        (add-face-text-property off (+ off (- end beg))
                                'fzfa-regexp-match nil cand)
        ;; Per-group overlays on top of the base match face.
        (let ((g 1)
              (rest (nthcdr 2 md)))
          (while rest
            (let ((gbeg (car rest))
                  (gend (cadr rest)))
              (when (and gbeg gend)
                (let ((face (nth (1- g) fzfa-regexp-group-faces)))
                  (when face
                    (add-face-text-property (+ off (- gbeg beg))
                                            (+ off (- gend beg))
                                            face nil cand))))
              (setq g (1+ g) rest (cddr rest)))))
        (propertize cand
                    'fzfa-regexp-buffer buf
                    'fzfa-regexp-position beg)))))

(defun fzfa-regexp--producer ()
  "Build a producer scanning the originating buffer for an Elisp regexp.

Empty INPUT emits every line of the buffer."
  (let ((buf (current-buffer)))
    (lambda (input callback)
      (with-current-buffer buf
        (let (matches)
          (save-excursion
            (goto-char (point-min))
            (if (or (null input) (string-empty-p input))
                (let ((line 1))
                  (while (and (not (input-pending-p))
                              (not (eobp)))
                    (push (fzfa-regexp--format-line
                           buf line
                           (buffer-substring-no-properties (pos-bol) (pos-eol)))
                          matches)
                    (forward-line 1)
                    (cl-incf line)))
              (condition-case nil
                  (while (and (not (input-pending-p))
                              (re-search-forward input nil t))
                    (push (fzfa-regexp--format-match
                           buf (line-number-at-pos)
                           (match-data))
                          matches))
                ;; Invalid regexp — surface as empty result for this tick.
                (invalid-regexp nil))))
          (funcall callback (nreverse matches)))))))

(defun fzfa-regexp--jump (cand)
  "Visit the buffer/line CAND points back to."
  (when-let* ((buf (get-text-property 0 'fzfa-regexp-buffer cand)))
    (switch-to-buffer buf)
    (cond
     ((get-text-property 0 'fzfa-regexp-position cand)
      (goto-char (get-text-property 0 'fzfa-regexp-position cand)))
     ((get-text-property 0 'fzfa-regexp-line cand)
      (goto-char (point-min))
      (forward-line (1- (get-text-property 0 'fzfa-regexp-line cand)))))))

;;;###autoload
(defun fzfa-regexp ()
  "Pick a line from the current buffer by Elisp regexp.

Input splits on `fzfa-separator': the CMD half is the Elisp
regexp evaluated against the buffer, the FILTER half re-scores the
matches via fzf-native.  Empty CMD lists every line."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :prompt "regexp: "
                  :candidates (fzfa-regexp--producer)
                  :category 'fzfa-regexp
                  :display 'compact
                  :resolve-paths nil
                  :apply #'fzfa-regexp--jump)))
    (fzfa-regexp--jump r)))

(provide 'fzfa-regexp)
;;; fzfa-regexp.el ends here
