;;; fzfa-vertico.el --- Vertico extensions for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Per-group column layout for `vertico'.  Each completion group
;; renders as its own vertical column instead of the default stacked
;; layout.  Primarily intended for `fzfa-multi-read' sessions, where
;; one column equals one source.
;;
;; Loaded automatically when `vertico' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  By default `fzfa-vertico-setup'
;; registers the columns mode with `vertico-multiform-categories'
;; for each category in `fzfa-vertico-multiform-categories'
;; (defaults to `fzfa-multi') and turns `vertico-multiform-mode'
;; on if not already — so columns activate inside multi-source
;; pickers and tear down on exit, without touching unrelated
;; minibuffer sessions.  Set `fzfa-vertico-columns-auto' to nil
;; to suppress all auto-wiring.
;;
;; Manual enable (e.g. globally for testing):
;;   (fzfa-vertico-columns-mode 1)
;;
;; Or scope a different category yourself:
;;   (add-to-list 'vertico-multiform-categories
;;                '(imenu fzfa-vertico-columns-mode))
;;
;; When the active completion produces more groups than
;; `fzfa-vertico-columns-max', the overflow groups wrap into
;; additional bands stacked below.  For example, with the default
;; max of 3 and 7 groups the layout is three bands of 3, 3, 1
;; columns.
;;
;; Navigation while the mode is active.  Vim-style M-hjkl jumps
;; between groups (sources); arrow keys move between candidates
;; within the current source:
;;   M-l                       Next source in reading order; at a
;;                             band's right edge, wraps to the
;;                             leftmost source of the next band.
;;   M-h                       Previous source (symmetric inverse).
;;   M-j                       Source in the next band, same
;;                             column-in-band (jumps a whole band down).
;;   M-k                       Source in the previous band, same
;;                             column-in-band.
;;   next-line / previous-line Move within the current source;
;;                             at the source's last row, jumps to
;;                             the source visually below in the
;;                             next band (and vice versa).
;;
;; The mode falls back to vertico's default arrangement when the
;; active completion has no `group-function' or only produces a
;; single group, so it is safe to enable globally.

;;; Code:

(require 'cl-lib)

;; Soft dependency.  The file is loaded by `fzfa-setup' whenever
;; `vertico' is in `fzfa-extensions', regardless of whether
;; `vertico' itself is installed — keeping the require soft lets
;; the extension's customs and the minor-mode shell load cleanly
;; in vertico-less environments.  All vertico-dependent
;; behaviour (the cl-defmethod, navigation, auto-activation) is
;; gated on `(featurep 'vertico)' below.
(require 'vertico nil t)

(defvar vertico-mode)
(defvar vertico-multiform-mode)
(defvar vertico-multiform-categories)
(declare-function vertico-multiform-mode "vertico-multiform" (&optional arg))
(defvar vertico-group-format)
(defvar vertico-count)
(defvar vertico--candidates)
(defvar vertico--metadata)
(defvar vertico--index)
(defvar vertico--input)
(declare-function vertico--metadata-get "vertico" (prop))
(declare-function vertico--window-width "vertico" ())
(declare-function vertico--hilit "vertico" (cand))
(declare-function vertico--format-candidate "vertico"
                  (cand prefix suffix index start))
(declare-function vertico--goto "vertico" (index))
(declare-function vertico-next "vertico" (&optional n))
(declare-function vertico-previous "vertico" (&optional n))

(defvar-local fzfa-vertico--band-offset 0
  "Per-minibuffer first visible band index, for pagination.

When `fzfa-vertico-columns-page-size' caps the number of bands
rendered, this offset slides as the selection moves between
bands so the band containing the selection always stays in
view.  Recomputed from `vertico--index' on every arrange, so
nothing else needs to mutate it directly.")

(defvar-local fzfa-vertico--initial-snap-done nil
  "Per-minibuffer one-shot flag for the initial selection snap.

`vertico--update' resets `vertico--index' to 0 on entry, which —
once partitioned by `fzfa-vertico-columns-source-sort' — points
at whichever candidate `fzfa--sort-by-history' promoted to the
top, not necessarily column 0 row 0.  After the first render in
each minibuffer session we move the selection to the partition's
top-left cell and set this flag so subsequent renders honour the
user's explicit navigation.")

(defgroup fzfa-vertico nil
  "Vertico extensions for fzfa."
  :group 'fzfa
  :group 'vertico)

(defcustom fzfa-vertico-columns-max 3
  "Maximum number of columns rendered per band.

When the active completion produces more groups than this, the
overflow groups wrap into additional bands stacked below.  For
example, with `fzfa-vertico-columns-max' = 3 and 7 groups, the
layout is three bands of (3 3 1) columns."
  :type 'natnum
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-page-size 6
  "Maximum sources displayed simultaneously; pagination kicks in beyond.

When the active completion produces more groups than this, the
layout paginates: only a window containing at most PAGE-SIZE
sources is visible at any time.  The window scrolls
automatically as the selection moves between bands — moving
past the last visible band's bottom row brings the next band
into view; moving past the top scrolls back.

Counted as `(ceil PAGE-SIZE / fzfa-vertico-columns-max)' bands.
With the default of 6 and `fzfa-vertico-columns-max' = 3 you see
2 bands of 3 columns at a time.  Each visible band gets a larger
share of `vertico-count' rows than it would if every band were
on-screen, which is the main UX win for many-source pickers.

Set to 0 or nil to disable pagination (all bands always visible)."
  :type '(choice (const :tag "Disable pagination" nil)
                 (natnum :tag "Page size (sources)"))
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-min-width 12
  "Minimum width per column, in characters."
  :type 'natnum
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-max-width 60
  "Maximum width per column, in characters."
  :type 'natnum
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-separator
  #("  |  " 2 3 (display (space :width (1))
                 face (:inherit window-divider :inverse-video t)))
  "Separator string between adjacent columns.

The middle character carries a `display' property that renders it
as a 1-pixel-wide vertical line in the `window-divider' face, so
the divider looks like a window separator on GUI frames.  On a
TTY the display property is ignored and the literal `|' shows
through as a readable fallback."
  :type 'string
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-headers t
  "When non-nil, render group names as a header row above the candidates.

The header consumes one slot of `vertico-count'."
  :type 'boolean
  :group 'fzfa-vertico)

(defface fzfa-vertico-columns-header
  '((t :inherit minibuffer-prompt))
  "Face for source-name header text in `fzfa-vertico-columns-mode'.

The overline above and underline beneath each header are layered
on at render time using `window-divider's foreground, so the
framing rules track theme changes alongside the column-separator
hairline.  Customize this face to change the header text's own
foreground / weight."
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-header-face 'fzfa-vertico-columns-header
  "Face applied to the column header row.

Defaults to `fzfa-vertico-columns-header', which inherits from
`minibuffer-prompt' and adds an underline."
  :type 'face
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-auto t
  "When non-nil, `fzfa-vertico-setup' wires up per-category activation.

The hook fires from `fzfa-setup' when `vertico' is in
`fzfa-extensions'.  Each entry in
`fzfa-vertico-multiform-categories' is registered with
`vertico-multiform-categories' so the columns layout activates
automatically inside the listed categories' minibuffer
sessions, and `vertico-multiform-mode' is enabled if not already
on.  Set to nil to load this extension's commands and face
definitions without touching multiform — you can then enable
`fzfa-vertico-columns-mode' manually or scope it yourself."
  :type 'boolean
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-multiform-categories '(fzfa-multi)
  "Completion categories that should auto-activate columns mode.

Each symbol is registered with `vertico-multiform-categories'
as (CATEGORY fzfa-vertico-columns-mode), so opening a
`completing-read' under one of these categories turns the
columns layout on for that session and tears it down on exit.
Defaults to `fzfa-multi' — the category used by
`fzfa-multi-read', `fzfa-find-any', and `fzfa-find-some' — since
multi-source pickers are where the per-group column layout pays
off.  Add other categories (e.g. `imenu', `consult-buffer') if
you also want columns there."
  :type '(repeat symbol)
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-truncate 'auto
  "How to truncate candidates that exceed the column width.

  left      Keep the leading characters; replace the tail with
            `fzfa-vertico-columns-ellipsis'.  Matches Emacs's
            default `truncate-string-to-width' behaviour.
  right     Keep the trailing characters; replace the head with
            the ellipsis.  Best for file paths and grep-style
            FILE:LINE:CONTENT candidates where the suffix
            (basename, match content) is the identifying part.
  auto      Per-candidate heuristic — `right' when the candidate
            looks path-like (contains `/' or starts with `~'),
            `left' otherwise.  Default.
  function  Called with (CAND WIDTH) and must return a string of
            visible width WIDTH (pad with spaces if shorter).

Truncation affects only the display: fzf scoring and the
`completing-read' return value always operate on the full
untouched candidate string."
  :type '(choice (const :tag "Left-anchored (keep prefix)" left)
                 (const :tag "Right-anchored (keep suffix)" right)
                 (const :tag "Auto-detect path-like" auto)
                 (function :tag "Custom function"))
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-ellipsis
  (if (char-displayable-p ?…) "…" "...")
  "String used to mark truncated candidates in the columns layout."
  :type 'string
  :group 'fzfa-vertico)

(defcustom fzfa-vertico-columns-source-sort 'scored
  "How to order group columns.

  declared     Sort by declared order.
  scored       Groups are sorted by their first candidates score.
               Default.
  alphabetical Sort group names lexicographically.
  function     A function called with the partition list
               ((GROUP . CANDS) ...) returning the reordered list."
  :type '(choice (const :tag "By declared source order (alias)" declared)
                 (const :tag "By per-source fzf score (rank-sorted)" scored)
                 (const :tag "Alphabetical" alphabetical)
                 (function :tag "Custom function"))
  :group 'fzfa-vertico)

(defvar-keymap fzfa-vertico-columns-map
  :doc "Additional keymap activated in `fzfa-vertico-columns-mode'.
Arrow-key remaps move between candidates within a column
(the default within-source navigation), while vim-style M-hjkl
shortcuts jump between groups: M-h/M-l cycle through sources in
reading order, M-j/M-k jump to the source in the next or
previous band (same column-in-band)."
  "<remap> <left-char>"      #'fzfa-vertico-columns-left
  "<remap> <right-char>"     #'fzfa-vertico-columns-right
  "<remap> <next-line>"      #'fzfa-vertico-columns-next
  "<remap> <previous-line>"  #'fzfa-vertico-columns-previous
  "M-h"                      #'fzfa-vertico-columns-left
  "M-l"                      #'fzfa-vertico-columns-right
  "M-j"                      #'fzfa-vertico-columns-band-down
  "M-k"                      #'fzfa-vertico-columns-band-up)

;;;###autoload
(define-minor-mode fzfa-vertico-columns-mode
  "Render each completion group as a column in `vertico'.

The active completion's `group-function' partitions candidates;
each unique group becomes one column.  Falls back to the default
stacked layout when there is no `group-function' or only one
group is produced."
  :global t :group 'fzfa-vertico
  (when-let* ((win (active-minibuffer-window)))
    (unless (frame-root-window-p win)
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise)))
  (cl-callf2 rassq-delete-all fzfa-vertico-columns-map minor-mode-map-alist)
  (when fzfa-vertico-columns-mode
    (push `(vertico--input . ,fzfa-vertico-columns-map)
          minor-mode-map-alist)))

;;; Partitioning

(defun fzfa-vertico--group-function ()
  "Return the active `group-function', or nil when grouping is disabled."
  (and vertico-group-format (vertico--metadata-get 'group-function)))

(defun fzfa-vertico--src-idx-of (part)
  "Return PART's first candidate `fzfa-src-idx', or `most-positive-fixnum'.

Used as the sort key for the `source-idx' ordering mode."
  (let ((c (cadr part)))
    (or (and (stringp c)
             (> (length c) 0)
             (get-text-property 0 'fzfa-src-idx c))
        most-positive-fixnum)))

(defun fzfa-vertico--empty-query-p ()
  "Return non-nil when the active minibuffer has no user query.

Used by the `scored' column-sort mode to lock declared order
while sources stream in — without a query there is no rank to
follow, and async arrival order would otherwise shuffle columns."
  (when-let* ((win (active-minibuffer-window)))
    (with-current-buffer (window-buffer win)
      (= (minibuffer-prompt-end) (point-max)))))

(defun fzfa-vertico--sort-parts (parts)
  "Order PARTS according to `fzfa-vertico-columns-source-sort'.

`sort' is stable, so groups with equal sort keys (e.g., no
`fzfa-src-idx' property) retain their discovery order."
  (pcase fzfa-vertico-columns-source-sort
    ('scored
     ;; Empty query → lock declared order so async streaming doesn't
     ;; shuffle columns as sources arrive at different times.  With a
     ;; query, follow `vertico--candidates' discovery order, which
     ;; `fzfa--multi-read' merges in rank order (strongest source first).
     (if (fzfa-vertico--empty-query-p)
         (sort parts (lambda (a b) (< (fzfa-vertico--src-idx-of a)
                                      (fzfa-vertico--src-idx-of b))))
       parts))
    ('alphabetical
     (sort parts (lambda (a b) (string< (car a) (car b)))))
    ((or 'source-idx 'declared)
     (sort parts (lambda (a b) (< (fzfa-vertico--src-idx-of a)
                                  (fzfa-vertico--src-idx-of b)))))
    ((pred functionp)
     (funcall fzfa-vertico-columns-source-sort parts))
    (_ parts)))

(defun fzfa-vertico--partition (group-fun)
  "Partition `vertico--candidates' by GROUP-FUN.

Returns ((GROUP . (CAND ...)) ...) ordered according to
`fzfa-vertico-columns-source-sort'; per-group candidate order
follows the original `vertico--candidates' order."
  (let ((table (make-hash-table :test 'equal))
        (order nil))
    (dolist (c vertico--candidates)
      (let ((g (or (funcall group-fun c nil) "")))
        (unless (gethash g table)
          (puthash g (cons nil nil) table)
          (push g order))
        (let ((cell (gethash g table)))
          (setcar cell (cons c (car cell))))))
    (fzfa-vertico--sort-parts
     (mapcar (lambda (g) (cons g (nreverse (car (gethash g table)))))
             (nreverse order)))))

;;; Navigation

(defun fzfa-vertico--locate (parts idx)
  "Find IDX (into `vertico--candidates') in PARTS as (COL . ROW), or nil."
  (when-let* ((cand (and (>= idx 0)
                         (< idx (length vertico--candidates))
                         (nth idx vertico--candidates))))
    (let ((col 0) found)
      (cl-dolist (part parts)
        (let ((row (cl-position cand (cdr part) :test #'eq)))
          (when row
            (setq found (cons col row))
            (cl-return)))
        (cl-incf col))
      found)))

(defun fzfa-vertico--flat-index (parts col row)
  "Return the `vertico--candidates' index of (COL ROW) in PARTS, or nil."
  (when-let* ((cands (cdr (nth col parts)))
              (cand (nth row cands)))
    (cl-position cand vertico--candidates :test #'eq)))

(defun fzfa-vertico--move-source (dsrc)
  "Move DSRC sources horizontally in the linear source order.

Row index is preserved (clamped to the destination source's row
count).  Crossing a band boundary wraps to the adjacent band's
edge source on the same data row, matching the visual reading
order left-to-right, top-to-bottom."
  (when-let* ((gf (fzfa-vertico--group-function))
              (parts (fzfa-vertico--partition gf))
              ((> (length parts) 1))
              (pos (fzfa-vertico--locate parts vertico--index)))
    (let* ((n (length parts))
           (src (car pos)) (row (cdr pos))
           (new-src (max 0 (min (1- n) (+ src dsrc))))
           (rows (length (cdr (nth new-src parts))))
           (new-row (min (1- rows) row)))
      (when-let* ((idx (fzfa-vertico--flat-index parts new-src new-row)))
        (vertico--goto idx)))))

(defun fzfa-vertico--move-row (drow)
  "Move DROW rows vertically within the current source's column.

At the bottom of a source, DOWN jumps to the source visually
below in the next band (same column-in-band).  At the top, UP
jumps to the corresponding source in the previous band."
  (when-let* ((gf (fzfa-vertico--group-function))
              (parts (fzfa-vertico--partition gf))
              ((> (length parts) 1))
              (pos (fzfa-vertico--locate parts vertico--index)))
    (let* ((src (car pos)) (row (cdr pos))
           (src-rows (length (cdr (nth src parts))))
           (new-row (+ row drow))
           (max-cols (max 1 fzfa-vertico-columns-max)))
      (cond
       ((and (>= new-row 0) (< new-row src-rows))
        (when-let* ((idx (fzfa-vertico--flat-index parts src new-row)))
          (vertico--goto idx)))
       ((> drow 0)
        (let ((next-src (+ src max-cols)))
          (when (< next-src (length parts))
            (let* ((excess (- new-row src-rows))
                   (next-rows (length (cdr (nth next-src parts))))
                   (target (min excess (1- next-rows))))
              (when-let* ((idx (fzfa-vertico--flat-index
                                parts next-src (max 0 target))))
                (vertico--goto idx))))))
       (t
        (let ((prev-src (- src max-cols)))
          (when (>= prev-src 0)
            (let* ((prev-rows (length (cdr (nth prev-src parts))))
                   (target (+ prev-rows new-row)))
              (when-let* ((idx (fzfa-vertico--flat-index
                                parts prev-src
                                (max 0 (min (1- prev-rows) target)))))
                (vertico--goto idx))))))))))

(defun fzfa-vertico--multi-columns-p ()
  "Non-nil when more than one group is currently rendered as columns.

This is the condition under which our custom columnar navigation
is meaningful.  When the active completion has no group-function,
or when narrowing collapses the layout to a single column,
returns nil so the navigation wrappers fall through to standard
vertico / cursor commands."
  (when-let* ((gf (fzfa-vertico--group-function)))
    (> (length (fzfa-vertico--partition gf)) 1)))

(defun fzfa-vertico-columns-right (&optional n)
  "Move N sources to the right in reading order (default 1).

At a band's right edge, wraps to the next band's leftmost source
on the same data row.  Falls back to `right-char' when the layout
is single-column (no group-function, or narrowed to one group)."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-source (or n 1))
    (call-interactively #'right-char)))

(defun fzfa-vertico-columns-left (&optional n)
  "Move N sources to the left in reading order (default 1).

At a band's left edge, wraps to the previous band's rightmost
source on the same data row.  Falls back to `left-char' when the
layout is single-column."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-source (- (or n 1)))
    (call-interactively #'left-char)))

(defun fzfa-vertico-columns-next (&optional n)
  "Move N rows down within the current source's column (default 1).

At the source's last row, jumps to the source visually below in
the next band.  Falls back to `vertico-next' when the layout is
single-column — so vertico's normal scrolling still works after
narrowing to one source."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-row (or n 1))
    (vertico-next (or n 1))))

(defun fzfa-vertico-columns-previous (&optional n)
  "Move N rows up within the current source's column (default 1).

At the source's first row, jumps to the source visually above in
the previous band.  Falls back to `vertico-previous' when the
layout is single-column."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-row (- (or n 1)))
    (vertico-previous (or n 1))))

(defun fzfa-vertico--move-band (dband)
  "Move DBAND bands vertically, keeping the same column-in-band.

Row index within the destination source is preserved when
possible, clamped to the destination's length.  Crossing the
top/bottom edge is a no-op."
  (when-let* ((gf (fzfa-vertico--group-function))
              (parts (fzfa-vertico--partition gf))
              ((> (length parts) 1))
              (pos (fzfa-vertico--locate parts vertico--index)))
    (let* ((src (car pos))
           (row (cdr pos))
           (max-cols (max 1 fzfa-vertico-columns-max))
           (target (+ src (* dband max-cols)))
           (n (length parts)))
      (when (and (>= target 0) (< target n))
        (let* ((rows (length (cdr (nth target parts))))
               (new-row (min (1- rows) (max 0 row))))
          (when-let* ((idx (fzfa-vertico--flat-index parts target new-row)))
            (vertico--goto idx)))))))

(defun fzfa-vertico-columns-band-down (&optional n)
  "Jump N bands down to the source in the same column-in-band (default 1).

Falls back to `vertico-next' when the layout is single-column."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-band (or n 1))
    (vertico-next (or n 1))))

(defun fzfa-vertico-columns-band-up (&optional n)
  "Jump N bands up to the source in the same column-in-band (default 1).

Falls back to `vertico-previous' when the layout is single-column."
  (interactive "p")
  (if (fzfa-vertico--multi-columns-p)
      (fzfa-vertico--move-band (- (or n 1)))
    (vertico-previous (or n 1))))

;;; Rendering

(defun fzfa-vertico--path-like-p (s)
  "Heuristic: non-nil when S resembles a file path / grep-style result.

Used by the `auto' value of `fzfa-vertico-columns-truncate' to
pick right-anchored truncation for path-bearing candidates."
  (or (string-match-p "/" s)
      (string-prefix-p "~" s)))

(defconst fzfa-vertico--match-faces
  '(completions-common-part completions-first-difference)
  "Faces vertico applies to matched characters in `vertico--hilit'.

Used to detect when right-truncation would drop a matched span
off the leading edge, so the ellipsis can carry the hint forward.")

(defun fzfa-vertico--has-match-face-p (s)
  "Return non-nil when S has any `fzfa-vertico--match-faces' span.

Walks face text properties with `next-single-property-change'
so the scan stays cheap even on long candidates."
  (let ((i 0) (len (length s)) hit)
    (while (and (< i len) (not hit))
      (let* ((face (get-text-property i 'face s))
             (faces (if (listp face) face (list face))))
        (when (cl-intersection faces fzfa-vertico--match-faces)
          (setq hit t)))
      (setq i (or (next-single-property-change i 'face s) len)))
    hit))

(defun fzfa-vertico--truncate-right (s width)
  "Truncate S to visible WIDTH keeping the trailing characters.

Prepends `fzfa-vertico-columns-ellipsis' when truncation occurs.
Text properties on the surviving suffix are preserved, so
vertico's match highlights and the selection face survive intact
on whatever portion of the candidate remains visible.  When the
dropped prefix contained a match, the ellipsis itself is
propertized with `completions-common-part' so the column still
signals \"there's a match in the part you can't see\"."
  (let* ((ell fzfa-vertico-columns-ellipsis)
         (ell-w (string-width ell))
         (full-w (string-width s)))
    (cond
     ((<= full-w width)
      (concat s (make-string (- width full-w) ?\s)))
     ((>= ell-w width) ell)
     (t
      ;; Advance until we've dropped enough leading visible width to
      ;; let the ellipsis + suffix fit, then keep the rest.  Wide
      ;; chars may overshoot by 1 column; pad to WIDTH if so.
      (let* ((target (- width ell-w))
             (skip (- full-w target))
             (acc 0) (i 0) (len (length s)))
        (while (and (< i len) (< acc skip))
          (setq acc (+ acc (char-width (aref s i))))
          (cl-incf i))
        (let* ((dropped (substring s 0 i))
               (ell-display
                (if (fzfa-vertico--has-match-face-p dropped)
                    (propertize ell 'face 'completions-common-part)
                  ell))
               (out (concat ell-display (substring s i)))
               (ow (string-width out)))
          (if (>= ow width)
              out
            (concat out (make-string (- width ow) ?\s)))))))))

(defun fzfa-vertico--truncate (s width)
  "Truncate S to visible WIDTH per `fzfa-vertico-columns-truncate'.

Falls back to standard left-anchored `truncate-string-to-width'
for unrecognised values."
  (pcase fzfa-vertico-columns-truncate
    ('right (fzfa-vertico--truncate-right s width))
    ('auto (if (fzfa-vertico--path-like-p (substring-no-properties s))
               (fzfa-vertico--truncate-right s width)
             (truncate-string-to-width s width 0 ?\s
                                       fzfa-vertico-columns-ellipsis)))
    ((pred functionp) (funcall fzfa-vertico-columns-truncate s width))
    (_ (truncate-string-to-width s width 0 ?\s
                                 fzfa-vertico-columns-ellipsis))))

(defun fzfa-vertico--render-cell (cand idx width &optional group-fun)
  "Render CAND at flat index IDX, truncated/padded to WIDTH.

Reuses `vertico--format-candidate' so selection highlighting and
match-fontification stay consistent with vertico's defaults.
When GROUP-FUN is non-nil, the candidate is passed through
\(funcall GROUP-FUN cand \\='transform) before rendering, mirroring
the display swap `vertico--arrange-candidates' does for the
stacked layout.  Highlighting runs first so per-source transforms
that take a substring of CAND (e.g. `fzfa--grep-group' stripping
the FILE: prefix) inherit the match faces.

Width fitting is delegated to `fzfa-vertico--truncate' so
path-bearing candidates can keep their identifying suffix
instead of having their basenames chopped off the right."
  (let* ((hi (vertico--hilit (copy-sequence cand)))
         (display (or (and group-fun (funcall group-fun hi 'transform))
                      hi))
         (formatted (vertico--format-candidate display "" "" idx 0))
         (trimmed (string-trim-right formatted "[\r\n]+")))
    (fzfa-vertico--truncate trimmed width)))

(defun fzfa-vertico--header-face-spec ()
  "Return a face spec for header text with `window-divider'-colored rules.

Wraps `fzfa-vertico-columns-header-face' in an overline plus an
underline, both drawn in `window-divider's foreground so the
two rules visually frame each header into a tabular row that
matches the column-separator hairline.  Falls back to plain
foreground-colored rules when `window-divider' has no specified
foreground."
  (let* ((raw (face-attribute 'window-divider :foreground nil t))
         (color (and (stringp raw) raw)))
    `(:inherit ,fzfa-vertico-columns-header-face
               :overline ,(or color t)
               :underline ,(if color `(:color ,color :style line) t))))

(defun fzfa-vertico--scroll-offset (data-cap cur-row n-items)
  "Return per-source scroll offset to keep CUR-ROW visible.

DATA-CAP is the visible row count for the band; N-ITEMS is the
total length of the source's candidate list.  When CUR-ROW is
nil (the source does not contain the selection) returns 0 — only
the source containing the selection scrolls.  Otherwise places
CUR-ROW at the bottom of the visible window once it would
otherwise scroll off-screen, clamped so the source never scrolls
past its last item."
  (if (null cur-row) 0
    (max 0 (min (max 0 (- n-items data-cap))
                (- cur-row (1- data-cap))))))

;; The cl-defmethod can only be defined once `vertico--arrange-candidates'
;; exists as a generic, so defer registration until vertico loads.
(with-eval-after-load 'vertico
(cl-defmethod vertico--arrange-candidates
  (&context (fzfa-vertico-columns-mode (eql t)))
  "Arrange candidates in per-source columns when columns-mode is active."
  (let* ((gf (fzfa-vertico--group-function))
         (parts (and gf (fzfa-vertico--partition gf))))
    (if (or (null parts) (<= (length parts) 1))
        (cl-call-next-method)
      ;; One-shot: place the initial selection at column 0 row 0 of
      ;; the partition.  Goes through `vertico--goto' so the lock-
      ;; candidate flag is set — vertico then preserves our snapped
      ;; candidate through subsequent rescores rather than resetting
      ;; to index 0 on every input change.
      (unless fzfa-vertico--initial-snap-done
        (setq fzfa-vertico--initial-snap-done t)
        (when-let* ((first-cand (cadr (car parts)))
                    (target (cl-position first-cand vertico--candidates
                                         :test #'eq))
                    ((/= target vertico--index)))
          (vertico--goto target)))
      (let* ((nparts (length parts))
             (max-cols (max 1 fzfa-vertico-columns-max))
             (ncols (min max-cols nparts))
             (nbands-total (max 1 (ceiling (/ (float nparts) ncols))))
             ;; Pagination: cap visible bands to fit `page-size' sources.
             ;; `page-size' nil / 0 → show all bands (no pagination).
             (page-size fzfa-vertico-columns-page-size)
             (nbands (if (and page-size (> page-size 0))
                         (max 1 (min nbands-total
                                     (ceiling (/ (float page-size) ncols))))
                       nbands-total))
             ;; Selection's (source-idx . row-in-source).  Only the
             ;; containing source scrolls; others stay parked at top.
             (sel-pos (fzfa-vertico--locate parts vertico--index))
             (sel-src (car sel-pos))
             (sel-row (cdr sel-pos))
             ;; Slide the band window so the selection's band stays
             ;; visible.  Past the right edge → advance offset; past
             ;; the left edge → retreat; otherwise keep the prior
             ;; offset (so a stable selection doesn't trigger scroll
             ;; jitter as rescores reorder).
             (sel-band (and sel-src (/ sel-src ncols)))
             (prev-offset fzfa-vertico--band-offset)
             (raw-offset
              (cond
               ((null sel-band) prev-offset)
               ((< sel-band prev-offset) sel-band)
               ((>= sel-band (+ prev-offset nbands))
                (1+ (- sel-band nbands)))
               (t prev-offset)))
             (band-offset (max 0 (min raw-offset (- nbands-total nbands))))
             (sep fzfa-vertico-columns-separator)
             (sepw (string-width sep))
             (win-w (vertico--window-width))
             (avail (max ncols (- win-w (* (max 0 (1- ncols)) sepw))))
             (col-w (max fzfa-vertico-columns-min-width
                         (min fzfa-vertico-columns-max-width
                              (/ avail ncols))))
             (header? fzfa-vertico-columns-headers)
             ;; Distribute `vertico-count' rows across VISIBLE bands.
             ;; Pagination's headline benefit: fewer bands on-screen →
             ;; more rows per band.
             (rows-per-band (max (if header? 2 1)
                                 (/ vertico-count nbands)))
             (data-cap (max 1 (- rows-per-band (if header? 1 0))))
             (lines nil))
        (setq fzfa-vertico--band-offset band-offset)
        (dotimes (i nbands)
          (let* ((band-idx (+ band-offset i))
                 (start (* band-idx ncols))
                 (end (min nparts (+ start ncols)))
                 (band-entries
                  (cl-loop for s from start below end
                           collect
                           (let* ((part (nth s parts))
                                  (items (cdr part))
                                  (n (length items))
                                  (cur (and sel-src (= s sel-src) sel-row))
                                  (off (fzfa-vertico--scroll-offset
                                        data-cap cur n)))
                             (list :part part :offset off
                                   :visible (max 0 (- n off))))))
                 (band-visible
                  (apply #'max 0
                         (mapcar (lambda (e) (plist-get e :visible))
                                 band-entries)))
                 (data-rows (min data-cap band-visible)))
            (when header?
              (let ((face (fzfa-vertico--header-face-spec)))
                (push
                 (concat
                  (mapconcat
                   (lambda (e)
                     (propertize
                      (truncate-string-to-width
                       (car (plist-get e :part)) col-w 0 ?\s)
                      'face face))
                   band-entries sep)
                  "\n")
                 lines)))
            (dotimes (r data-rows)
              (push
               (concat
                (mapconcat
                 (lambda (e)
                   (let* ((items (cdr (plist-get e :part)))
                          (off (plist-get e :offset))
                          (cand (nth (+ off r) items)))
                     (if-let* ((cand)
                               (idx (cl-position cand vertico--candidates
                                                 :test #'eq)))
                         (fzfa-vertico--render-cell cand idx col-w gf)
                       (make-string col-w ?\s))))
                 band-entries sep)
                "\n")
               lines))))
        (nreverse lines))))))

;;; Setup

;;;###autoload
(defun fzfa-vertico-setup ()
  "Wire fzfa's vertico integration into the current session.

Invoked by `fzfa-setup' when `vertico' is listed in
`fzfa-extensions'.  No-op when `vertico' is not installed —
keeping the default `fzfa-extensions' list portable across users
who use other completion UIs.  Otherwise, when
`fzfa-vertico-columns-auto' is non-nil:

  1. Each symbol in `fzfa-vertico-multiform-categories' is added to
     `vertico-multiform-categories' as
     (CATEGORY fzfa-vertico-columns-mode), so the columns layout
     auto-activates inside those categories' `completing-read' sessions.
  2. `vertico-multiform-mode' is turned on if not already, so the
     categories list is honored."
  (when (and fzfa-vertico-columns-auto
             (locate-library "vertico"))
    (require 'vertico nil t)
    (require 'vertico-multiform nil t)
    (dolist (cat fzfa-vertico-multiform-categories)
      (let ((entry (list cat 'fzfa-vertico-columns-mode)))
        (cl-pushnew entry vertico-multiform-categories :test #'equal)))
    (unless (bound-and-true-p vertico-multiform-mode)
      (vertico-multiform-mode 1))))

(provide 'fzfa-vertico)
;;; fzfa-vertico.el ends here
