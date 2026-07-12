;;; fzfa-posframe.el --- Posframe layout for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Route fzfa's preview into a floating posframe, replacing the buffer-swap
;; behaviour of `fzfa-preview-show'.  Optionally lifts candidates out of the
;; minibuffer into a matching left posframe (telescope.nvim style) when the
;; layout is `side-by-side' and the active frontend supports it.
;;
;; `fzfa-posframe-layout' selects the layout:
;;
;;   preview-centered  Preview posframe sits above the minibuffer, horizontally
;;                     centered.  Candidates stay in the minibuffer.  Default.
;;   side-by-side      Preview posframe pinned to the right edge with margin.
;;                     The candidate pane goes to the left, sourced from
;;                     whichever frontend integration is active:
;;                       vertico   → `vertico-buffer-mode' via vertico-multiform.
;;                       helm      → routed via `helm-display-function' (fzfa
;;                                   supplies its own; only `helm' itself is
;;                                   required).
;;                       ivy       → `ivy-posframe' (MELPA) if installed.
;;                       icomplete → falls back to `preview-centered' — inline
;;                                   icomplete cannot be moved into a buffer.
;;   top-to-bottom     Candidate posframe stacked above the preview posframe,
;;                     both horizontally centered; the combined block is
;;                     vertically centered.  Candidate height tracks the
;;                     frontend's declared candidate count.  Vertico only.
;;
;; Enable with `fzfa-posframe-mode'.  Requires the `posframe' package and a
;; graphical display; enabling on TTY or without `posframe' installed refuses
;; with a `user-error'.

;;; Code:

(require 'fzfa)

;; `posframe-show' is a `cl-defun' with a long `&key ... &allow-other-keys'
;; arglist that `check-declare' cannot match against our forward declaration.
;; Pass `t' as the arglist to skip argument-shape validation while still
;; letting `check-declare' verify the function lives in `posframe.el'.
(declare-function posframe-show "posframe" t t)
(declare-function posframe-hide "posframe" (buffer-or-name))
(declare-function posframe-delete-frame "posframe" (buffer-or-name))
(declare-function posframe-poshandler-frame-center "posframe" (info))
(declare-function image-transform-fit-to-window "image-mode" ())
(declare-function vertico-multiform-mode "vertico-multiform" (&optional arg))
(declare-function vertico--resize-window "vertico" (height))
(declare-function ivy-posframe-mode "ivy-posframe" (&optional arg))
(declare-function ivy-posframe--display "ivy-posframe" (str &optional poshandler))
(defvar fzfa-vertico-columns-max)
(defvar vertico-count)
(defvar vertico--total)

(defvar fzfa-posframe-mode)
(defvar vertico-buffer-display-action)
(defvar vertico-multiform-categories)
(defvar vertico-multiform-mode)
(defvar helm-display-function)
(defvar helm--buffer-in-new-frame-p)
(defvar fzfa-helm-want-follow)
(defvar ivy-posframe-mode)
(defvar ivy-posframe-display-functions-alist)
(defvar ivy-posframe-size-function)
(defvar ivy-posframe-hide-minibuffer)
(defvar ivy-height)

(defcustom fzfa-posframe-layout 'preview-centered
  "Layout for fzfa's posframe(s).

  side-by-side      Preview posframe pinned to the right half of the
                    parent frame; candidates routed to a matching left
                    posframe by the active frontend:
                      vertico   → `vertico-buffer-mode'.
                      helm      → `helm-display-function' (fzfa supplies
                                  its own; only `helm' itself is required).
                      ivy       → `ivy-posframe' if installed.
                      icomplete → downgrades to `preview-centered'.

  preview-centered  Preview posframe centered above the minibuffer;
                    candidates stay in the minibuffer.  Default.

  top-to-bottom     Candidate posframe on top and preview posframe
                    directly below it, both horizontally centered on
                    the parent frame; the combined block is vertically
                    centered.  Candidate height tracks the frontend's
                    declared candidate count so the frame is only as
                    tall as it needs to be.

Change via `setopt' or `customize' to auto-rewire the frontend routing
while the mode is active — plain `setq' does not trigger the setter,
in which case cycle `fzfa-posframe-mode' manually."
  :type '(choice
          (const :tag "Side-by-side (left candidates, right preview)"
                 side-by-side)
          (const :tag "Preview posframe centered above minibuffer"
                 preview-centered)
          (const :tag "Candidates on top, preview below, block centered"
                 top-to-bottom))
  :set (lambda (sym val)
         (set-default sym val)
         (when (bound-and-true-p fzfa-posframe-mode)
           (fzfa-posframe--uninstall-frontend-routing)
           (fzfa-posframe--install-frontend-routing)))
  :group 'fzfa)

(defcustom fzfa-posframe-margin 5
  "Pixel gap between posframe edges and the surrounding frame edges.

In `side-by-side' layout the gap is applied on the outside of each pane
and between the two panes.  In `preview-centered' layout it is applied
between the posframe bottom and the minibuffer top.  In `top-to-bottom'
layout it is applied between the candidate and preview panes."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-posframe-height-ratio 0.65
  "Fraction of the parent frame height used for a posframe pane."
  :type 'number
  :group 'fzfa)

(defcustom fzfa-posframe-centered-width-ratio 0.80
  "Fraction of the parent frame width used by the centered-style layouts.

Consulted when `fzfa-posframe-layout' is `preview-centered' or
`top-to-bottom' — both center their panes horizontally at this fraction
of the parent frame width.  `side-by-side' auto-sizes each pane to half
the frame width minus `fzfa-posframe-margin'."
  :type 'number
  :group 'fzfa)

(defcustom fzfa-posframe-internal-border-width 1
  "Internal border width in pixels for fzfa posframes."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-posframe-internal-border-color nil
  "Internal border color for fzfa posframes.

Nil (default) inherits the foreground of the `window-divider' face so
the posframe borders match on-screen window separators.  Set a color
string to override."
  :type '(choice (const :tag "Follow `window-divider'" nil)
                 (color :tag "Explicit color"))
  :group 'fzfa)

(defcustom fzfa-posframe-respect-mode-line nil
  "Non-nil to render each posframe buffer's mode-line inside the posframe.

Nil (default) suppresses the mode-line so the box shows content only,
matching the telescope.nvim aesthetic and giving a uniform border on
all sides."
  :type 'boolean
  :group 'fzfa)

(defcustom fzfa-posframe-respect-header-line nil
  "Non-nil to render each posframe buffer's header-line inside the posframe.

Nil (default) suppresses the header-line."
  :type 'boolean
  :group 'fzfa)

(defcustom fzfa-posframe-vertico-categories
  '(fzfa-multi "\\`fzfa-")
  "Completion categories that auto-enable `vertico-buffer-mode'.

Consulted in `side-by-side' layout when the active frontend is vertico.
Each entry is merged into `vertico-multiform-categories' so vertico
renders inside a buffer during matching completion sessions — that
buffer is what fzfa routes into the left posframe.

Entries are matched by `vertico-multiform--lookup': a SYMBOL matches
the category exactly; a STRING is treated as a regex tested against
the category's symbol name.  Order matters — earlier entries win — so
specific symbol overrides should come before general regex catch-alls.

Default puts `fzfa-multi' first (it needs an extra column-max override
to fit the narrower posframe) followed by the regex catch-all
\"\\\\`fzfa-\", which covers every other `fzfa-*' category so any new
fzfa command works without touching this list."
  :type '(repeat (choice symbol string))
  :group 'fzfa)

(defcustom fzfa-posframe-embark-categories
  '(embark-keybinding embark-become)
  "Completion categories for embark's nested completing-read prompters.

Consulted in `side-by-side' layout only.  Each symbol is merged into
`vertico-multiform-categories' with a category-scoped override of
`vertico-buffer-display-action' that routes the nested vertico buffer
into the existing left (candidates) posframe — the nested prompter is
just another completing-read, so it belongs in the same visual slot
as the outer one it stacks on top of, not in a third floating frame.

Defaults cover the two prompter shapes that arise from stock embark
plus `embark-prefix-help-command':

  embark-keybinding — `embark-prefix-help-command' / help-shaped prompts.
  embark-become     — `embark-become' target picker."
  :type '(repeat symbol)
  :group 'fzfa)

(defun fzfa-posframe--border-color ()
  "Resolve the posframe border color.

Returns `fzfa-posframe-internal-border-color' when set, otherwise the
foreground of `window-divider' — falling back to a neutral gray if the
face has no specified foreground."
  (or fzfa-posframe-internal-border-color
      (let ((fg (face-attribute 'window-divider :foreground nil t)))
        (if (stringp fg) fg "#888888"))))

(defvar fzfa-posframe--current-buffer nil
  "Live preview buffer currently rendered inside the preview posframe.")

(defvar fzfa-posframe--preview-frame nil
  "Reusable child frame for the preview panel, if created.

Kept alive across preview swaps so the fast path in
`fzfa-posframe--show' can `set-window-buffer' into an existing frame
instead of paying `make-frame' + `set-frame-size' — both of which are
X-server round-trips that dominate the preview hot path.")

(defvar fzfa-posframe--preview-geometry nil
  "Last (WIDTH . HEIGHT) applied to `fzfa-posframe--preview-frame'.

Compared against a fresh `fzfa-posframe--pane-geometry' each preview so
`set-frame-size' only fires when the parent frame actually changed
size — the plain call otherwise dominates the profile even when it is
a no-op.")

(defvar fzfa-posframe--preview-poshandler nil
  "Last poshandler that positioned `fzfa-posframe--preview-frame'.")

(defvar fzfa-posframe--vertico-frame nil
  "Child frame currently displaying the vertico buffer, if any.

Tracked separately from posframe's per-buffer registry because
`vertico-buffer' swaps the buffer inside the returned window during
its own setup — so `posframe-delete-frame' keyed on the original
buffer name cannot find the frame at teardown time.")

(defvar fzfa-posframe--vertico-geometry nil
  "Last (WIDTH . HEIGHT) applied to `fzfa-posframe--vertico-frame'.

`posframe-show' unconditionally calls `set-frame-size' even when it
finds a cached frame, which measured at ~5% of a session's wallclock.
Comparing against a fresh `fzfa-posframe--pane-geometry' lets the fast
path skip that entirely.")

(defvar fzfa-posframe--vertico-poshandler nil
  "Last poshandler that positioned `fzfa-posframe--vertico-frame'.

Included in the fast-path cache-hit check so switching
`fzfa-posframe-layout' between `side-by-side' and `top-to-bottom'
triggers a fresh frame with the right position, instead of leaving the
previous poshandler's placement in effect.")

(defvar fzfa-posframe--saved-vertico-buffer-action nil
  "Saved value of `vertico-buffer-display-action' before mode enable.")

(defvar fzfa-posframe--installed-vertico-categories nil
  "List of (CATEGORY . ENTRY) pairs we pushed onto `vertico-multiform-categories'.

Consulted on mode disable so we remove only what we installed, leaving
user-configured entries intact.")

(defvar fzfa-posframe--enabled-vertico-multiform nil
  "Non-nil when this mode turned `vertico-multiform-mode' on.

Used to know whether to turn it back off on mode disable.")

(defvar fzfa-posframe--saved-helm-display-function 'unset
  "Snapshot of `helm-display-function' before mode enable.")

(defvar fzfa-posframe--saved-helm-want-follow 'unset
  "Snapshot of `fzfa-helm-want-follow' before mode enable.

Sentinel `unset' distinguishes never-captured from captured-as-nil.")

(defvar fzfa-posframe--helm-buffer nil
  "Helm buffer currently rendered inside the fzfa helm posframe.")

(defvar fzfa-posframe--helm-display-buffer-entry nil
  "The `display-buffer-alist' entry we pushed for helm buffers.

Tracked so mode disable removes only what we added.")

(defcustom fzfa-posframe-helm-buffer-regexp "\\` ?\\*helm"
  "Regexp matched against buffer names to route them via the helm posframe.

Consulted when the mode adds an entry to `display-buffer-alist' so
helm's direct `display-buffer' / `pop-to-buffer' calls route through
`fzfa-posframe--helm-display-buffer'."
  :type 'regexp
  :group 'fzfa)

(defvar fzfa-posframe--enabled-ivy-posframe nil
  "Non-nil when this mode turned `ivy-posframe-mode' on.")

(defvar fzfa-posframe--saved-ivy-height 'unset
  "Snapshot of `ivy-height' before mode enable.")

(defvar fzfa-posframe--saved-ivy-display-functions-alist 'unset
  "Snapshot of `ivy-posframe-display-functions-alist' before mode enable.")

(defvar fzfa-posframe--saved-ivy-size-function 'unset
  "Snapshot of `ivy-posframe-size-function' before mode enable.")

(defconst fzfa-posframe--restored-locals
  '(display-line-numbers
    left-margin-width right-margin-width
    left-fringe-width right-fringe-width
    fringes-outside-margins fringe-indicator-alist
    truncate-lines
    mode-line-format header-line-format
    show-trailing-whitespace
    cursor-type cursor-in-non-selected-windows)
  "Buffer-local variables that `posframe-show' clobbers.

fzfa saves each var's pre-`posframe-show' state and restores it after,
so previewing a buffer that is also displayed elsewhere (typically the
origin window when the previewed file is already open) does not leak
posframe's display tweaks — most notably `display-line-numbers' being
forced to nil, and `mode-line-format' / `header-line-format' being
nulled under `:respect-mode-line' / `:respect-header-line' = nil — into
the user's editing buffer.")

(defun fzfa-posframe--capture-locals (buffer)
  "Return an alist snapshot of `fzfa-posframe--restored-locals' in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mapcar (lambda (var)
                (list var
                      (local-variable-p var)
                      (and (boundp var) (symbol-value var))))
              fzfa-posframe--restored-locals))))

(defun fzfa-posframe--restore-locals (buffer snapshot)
  "Restore `fzfa-posframe--restored-locals' in BUFFER from SNAPSHOT."
  (when (and snapshot (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (entry snapshot)
        (let ((var (nth 0 entry))
              (was-local (nth 1 entry))
              (val (nth 2 entry)))
          (if was-local
              (set (make-local-variable var) val)
            (kill-local-variable var)))))))

(defun fzfa-posframe--side-by-side-outer (info)
  "Return the outer X-offset for the side-by-side pair on the parent frame.

Centers the two-pane layout inside the parent frame so any slack from
`:width'-in-chars rounding is split symmetrically between the left and
right outer margins, keeping the inner (inter-pane) gap equal to
`fzfa-posframe-margin' exactly."
  (let ((fw (plist-get info :parent-frame-width))
        (pw (plist-get info :posframe-width)))
    (max fzfa-posframe-margin
         (/ (- fw (* 2 pw) fzfa-posframe-margin) 2))))

(defun fzfa-posframe-poshandler-side-by-side-left (info)
  "Anchor a posframe to the left pane slot of the side-by-side layout.

INFO is the plist supplied by `posframe-show'.  Vertically centered.
X derives from `fzfa-posframe--side-by-side-outer' so the whole pair is
centered horizontally in the parent frame."
  (let ((fh (plist-get info :parent-frame-height))
        (ph (plist-get info :posframe-height)))
    (cons (fzfa-posframe--side-by-side-outer info)
          (max 0 (/ (- fh ph) 2)))))

(defun fzfa-posframe-poshandler-side-by-side-right (info)
  "Anchor a posframe to the right pane slot of the side-by-side layout.

INFO is the plist supplied by `posframe-show'.  Vertically centered.
X = outer + `posframe-width' + `fzfa-posframe-margin'; both panes are
sized identically so the right pane's width matches the left's."
  (let ((fh (plist-get info :parent-frame-height))
        (pw (plist-get info :posframe-width))
        (ph (plist-get info :posframe-height)))
    (cons (+ (fzfa-posframe--side-by-side-outer info) pw fzfa-posframe-margin)
          (max 0 (/ (- fh ph) 2)))))

(defun fzfa-posframe-poshandler-above-minibuffer (info)
  "Center a posframe horizontally and anchor above the minibuffer.

INFO is the plist supplied by `posframe-show'.  Leaves
`fzfa-posframe-margin' pixels between posframe bottom and the
minibuffer top."
  (let ((fw  (plist-get info :parent-frame-width))
        (fh  (plist-get info :parent-frame-height))
        (pw  (plist-get info :posframe-width))
        (ph  (plist-get info :posframe-height))
        (mbh (plist-get info :minibuffer-height))
        (mlh (plist-get info :mode-line-height)))
    (cons (max 0 (/ (- fw pw) 2))
          (max 0 (- fh ph mbh mlh fzfa-posframe-margin)))))

(defun fzfa-posframe-poshandler-top-center (info)
  "Center a posframe horizontally and anchor to the top of the parent frame.

INFO is the plist supplied by `posframe-show'.  Used by the
`preview-centered' layout when helm is the active frontend — helm's
candidate window
occupies the lower half of the frame, so anchoring above the minibuffer
would drop the preview posframe on top of helm.  Top-anchoring keeps
the two panes visually separated."
  (let ((fw (plist-get info :parent-frame-width))
        (pw (plist-get info :posframe-width)))
    (cons (max 0 (/ (- fw pw) 2))
          fzfa-posframe-margin)))

(defun fzfa-posframe--centered-stack-block-top (parent this-h)
  "Return the Y of the top of the centered candidate+preview block.

PARENT is the parent frame.  THIS-H is the current frame's pixel
height (from the poshandler's INFO plist).  We recompute the OTHER
frame's expected pixel height via `fzfa-posframe--pane-geometry' so
the block stays symmetric even when the two frames have very
different sizes."
  (let* ((char-h (with-selected-frame parent (default-line-height)))
         (cand-h (* (cdr (fzfa-posframe--pane-geometry parent 'candidate))
                    char-h))
         (prev-h (* (cdr (fzfa-posframe--pane-geometry parent 'preview))
                    char-h))
         (block-h (+ cand-h fzfa-posframe-margin prev-h))
         (fh (frame-pixel-height parent)))
    ;; Sanity: if this frame is neither cand-h nor prev-h (e.g. sizing
    ;; drifted between measure and paint), fall back to a plain vertical
    ;; center for the current frame.
    (unless (or (= this-h cand-h) (= this-h prev-h))
      (setq block-h (* 2 this-h)))
    (max 0 (/ (- fh block-h) 2))))

(defun fzfa-posframe-poshandler-centered-stack-top (info)
  "Anchor a posframe as the TOP frame of a vertically-stacked centered pair.

INFO is the plist supplied by `posframe-show'.  The block (this frame
+ margin + the sibling frame from `fzfa-posframe--pane-geometry') is
vertically centered on the parent frame; horizontally centered too.
Sibling can be shorter or taller than this frame — geometry is read
per-purpose so the sizes need not match."
  (let* ((fw (plist-get info :parent-frame-width))
         (pw (plist-get info :posframe-width))
         (ph (plist-get info :posframe-height))
         (parent (plist-get info :parent-frame)))
    (cons (max 0 (/ (- fw pw) 2))
          (fzfa-posframe--centered-stack-block-top parent ph))))

(defun fzfa-posframe-poshandler-centered-stack-bottom (info)
  "Anchor a posframe as the BOTTOM frame of a vertically-stacked centered pair.

Companion to `fzfa-posframe-poshandler-centered-stack-top' — the two
frames need not share a size; the block is centered as a whole using
per-purpose geometry."
  (let* ((fw (plist-get info :parent-frame-width))
         (pw (plist-get info :posframe-width))
         (ph (plist-get info :posframe-height))
         (parent (plist-get info :parent-frame))
         (char-h (with-selected-frame parent (default-line-height)))
         (cand-h (* (cdr (fzfa-posframe--pane-geometry parent 'candidate))
                    char-h)))
    (cons (max 0 (/ (- fw pw) 2))
          (+ (fzfa-posframe--centered-stack-block-top parent ph)
             cand-h
             fzfa-posframe-margin))))

(defun fzfa-posframe--top-frame ()
  "Return the top-level frame that should host our posframes.

Walks up `frame-parent' pointers from the frame of the active minibuffer
window (or the selected window as fallback) until it reaches a
non-child frame."
  (let ((frame (window-frame (or (active-minibuffer-window)
                                 (selected-window)))))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defmacro fzfa-posframe--with-top-frame (frame &rest body)
  "Run BODY with `selected-window' pinned to a window on FRAME.

`posframe-show' internally reads `(selected-window)' to derive
`parent-frame' — the `:parent-frame' keyword is not honored (see
`posframe.el' near line 407).  If the preview timer fires while
`selected-window' is inside one of our own posframes, that posframe
becomes the derived parent — either producing a circular specification
error or (more subtly) sizing/positioning the new posframe against the
wrong parent.

`with-selected-frame' alone doesn't guarantee this — under some helm
paths `frame-selected-window' on the main frame still points at a
child-frame window we entered earlier — so we explicitly select the
main frame's root window before running BODY."
  (declare (indent 1) (debug t))
  (let ((f (gensym))
        (w (gensym)))
    `(let* ((,f ,frame)
            (,w (and (frame-live-p ,f) (frame-root-window ,f))))
       (if (window-live-p ,w)
           (with-selected-frame ,f
             (save-selected-window
               (select-window ,w 'norecord)
               ,@body))
         (progn ,@body)))))

(defun fzfa-posframe--reparent (frame top)
  "Force FRAME's `parent-frame' to TOP.

Backstop for cases where posframe's internal `(selected-window)'
reading (posframe.el:407) resolves to a child frame despite our
`with-selected-frame' wrapper — the resulting posframe would inherit
that child as its parent, sizing and positioning against it rather
than the intended top-level frame.  Re-asserting the parent parameter
after the fact keeps the posframe geometry anchored to TOP."
  (when (and (frame-live-p frame) (frame-live-p top))
    (unless (eq (frame-parent frame) top)
      (set-frame-parameter frame 'parent-frame top))))

(defun fzfa-posframe--active-frontend ()
  "Return a symbol identifying the currently active completion frontend.

Recognized: `vertico', `helm', `ivy', `icomplete'.  Returns nil when
none of the known minor modes are on."
  (cond
   ((bound-and-true-p vertico-mode)   'vertico)
   ((bound-and-true-p helm-mode)      'helm)
   ((bound-and-true-p ivy-mode)       'ivy)
   ((bound-and-true-p icomplete-mode) 'icomplete)
   ((bound-and-true-p fido-mode)      'icomplete)))

(defun fzfa-posframe--effective-layout ()
  "Return the layout to actually use given the active frontend.

`side-by-side' and `top-to-bottom' both need the frontend to render
its candidates into a buffer we can route into a posframe — icomplete
does not, so both downgrade to `preview-centered' under icomplete."
  (if (and (memq fzfa-posframe-layout '(side-by-side top-to-bottom))
           (eq (fzfa-posframe--active-frontend) 'icomplete))
      'preview-centered
    fzfa-posframe-layout))

(defcustom fzfa-posframe-centered-candidate-count-multiplier 1.0
  "Height multiplier applied to the frontend's candidate count in `top-to-bottom'.

The `top-to-bottom' layout sizes the candidate posframe to
(COUNT × this) character rows, so it is only as tall as it needs to be
for a reasonable number of candidates — instead of the fixed
`fzfa-posframe-height-ratio' the preview pane uses.  Default 1.0 sizes
the frame at exactly the frontend's declared visible count; bump
above 1.0 if you want headroom, drop below 1.0 to trim tighter."
  :type 'number
  :group 'fzfa)

(defun fzfa-posframe--frontend-candidate-count ()
  "Best-guess visible-candidate count for the active frontend.

Reads `vertico-count' / `ivy-height' / a sensible default for helm —
whichever frontend is on decides how tall the candidate posframe
should be in `top-to-bottom' layout."
  (pcase (fzfa-posframe--active-frontend)
    ('vertico (or (bound-and-true-p vertico-count) 10))
    ('ivy     (or (bound-and-true-p ivy-height) 10))
    ('helm    10)
    (_        10)))

(defun fzfa-posframe--pane-geometry (parent &optional purpose)
  "Return (WIDTH-CHARS . HEIGHT-CHARS) for a posframe pane inside PARENT frame.

PURPOSE is `preview' or `candidate' (defaults to `preview').  Only the
`top-to-bottom' layout uses PURPOSE to size the candidate pane against
the frontend's own candidate count — every other layout gives both
purposes the same geometry."
  (let* ((char-w (with-selected-frame parent (frame-char-width)))
         (char-h (with-selected-frame parent (default-line-height)))
         (fw     (frame-pixel-width parent))
         (fh     (frame-pixel-height parent))
         (margin fzfa-posframe-margin)
         (layout (fzfa-posframe--effective-layout))
         (pw     (pcase layout
                   ('side-by-side
                    (/ (max 0 (- fw (* 3 margin))) 2))
                   (_
                    (round (* fw fzfa-posframe-centered-width-ratio)))))
         (ph     (if (and (eq layout 'top-to-bottom) (eq purpose 'candidate))
                     (round (* (fzfa-posframe--frontend-candidate-count)
                               fzfa-posframe-centered-candidate-count-multiplier
                               char-h))
                   (round (* fh fzfa-posframe-height-ratio)))))
    (cons (max 20 (/ pw char-w))
          (max 3  (/ ph char-h)))))

(defun fzfa-posframe--dismiss (&optional buffer)
  "Delete the preview posframe rendering BUFFER (or the tracked one).

Also drops the cached-frame state (`fzfa-posframe--preview-frame' and
its geometry / poshandler siblings) so the next `fzfa-posframe--show'
takes the slow path and rebuilds from scratch."
  (let ((buf (or buffer fzfa-posframe--current-buffer)))
    (when (and buf (or (bufferp buf) (get-buffer buf)))
      (ignore-errors (posframe-delete-frame buf))))
  (when (frame-live-p fzfa-posframe--preview-frame)
    (ignore-errors (delete-frame fzfa-posframe--preview-frame)))
  (setq fzfa-posframe--current-buffer   nil
        fzfa-posframe--preview-frame    nil
        fzfa-posframe--preview-geometry nil
        fzfa-posframe--preview-poshandler nil))

(defun fzfa-posframe--dismiss-vertico ()
  "Delete the vertico posframe if any.

Also drops the geometry cache so the next
`fzfa-posframe--display-vertico-buffer' takes the slow path and
rebuilds from scratch."
  (when (frame-live-p fzfa-posframe--vertico-frame)
    (ignore-errors (delete-frame fzfa-posframe--vertico-frame)))
  (setq fzfa-posframe--vertico-frame      nil
        fzfa-posframe--vertico-geometry   nil
        fzfa-posframe--vertico-poshandler nil))

(defun fzfa-posframe--hide-vertico ()
  "Make the vertico posframe invisible but keep it in the cache.

Called on minibuffer exit so the next `completing-read' session hits
the fast path in `fzfa-posframe--display-vertico-buffer' — the frame
survives, only the vertico temp buffer is swapped in via
`set-window-buffer'.  Full deletion still happens on mode disable via
`fzfa-posframe--dismiss-vertico'."
  (when (frame-live-p fzfa-posframe--vertico-frame)
    (make-frame-invisible fzfa-posframe--vertico-frame)))

(defun fzfa-posframe--restore-vertico-minibuffer ()
  "Restore Vertico's minibuffer height after a posframe preview update.

`posframe-show' and child-frame redisplay can cause Emacs to re-run
minibuffer window fitting outside Vertico's own `vertico--exhibit'
dynamic context.  In inline-Vertico layouts this can collapse the
candidate display to one line even though `vertico--total' and
`vertico--candidates' still contain the full result set.  Re-applying
Vertico's resize helper keeps the active fzfa minibuffer at the height
Vertico computed for the current candidate set."
  (when-let* ((win (active-minibuffer-window))
              ((window-live-p win)))
    (with-selected-window win
      (when (and (bound-and-true-p vertico-mode)
                 (fboundp 'vertico--resize-window))
        (vertico--resize-window
         (min (or (bound-and-true-p vertico-count) 10)
              (max 0 (or (bound-and-true-p vertico--total) 0))))))))

(defun fzfa-posframe--restore-vertico-minibuffer-soon ()
  "Restore Vertico minibuffer sizing now and after pending redisplay."
  (fzfa-posframe--restore-vertico-minibuffer)
  (run-at-time 0 nil #'fzfa-posframe--restore-vertico-minibuffer))

(defun fzfa-posframe--show (buffer)
  "Render BUFFER inside the preview posframe.

Poshandler and dimensions are chosen from
`fzfa-posframe--effective-layout'.  Buffer-local vars posframe would
otherwise clobber (see `fzfa-posframe--restored-locals') are snapshotted
and restored so the origin buffer's display state stays intact when
BUFFER doubles as the origin.

Fast path: when a cached preview frame is already alive with the same
parent, geometry, and poshandler, only `set-window-buffer' fires — the
X-server round-trips of `make-frame' and `set-frame-size' are skipped.
Falls through to `posframe-show' when the frame is missing or the
layout / geometry has changed."
  (let* ((parent (fzfa-posframe--top-frame))
         (geom (fzfa-posframe--pane-geometry parent 'preview))
         (poshandler
          (pcase (fzfa-posframe--effective-layout)
            ('side-by-side
             #'fzfa-posframe-poshandler-side-by-side-right)
            ('preview-centered
             (if (eq (fzfa-posframe--active-frontend) 'helm)
                 #'fzfa-posframe-poshandler-top-center
               #'fzfa-posframe-poshandler-above-minibuffer))
            ('top-to-bottom
             ;; Preview sits below the candidate frame as the bottom of
             ;; the stacked pair — treats both frames as one unit and
             ;; centers that unit on the parent frame.
             #'fzfa-posframe-poshandler-centered-stack-bottom)
            (_
             #'fzfa-posframe-poshandler-above-minibuffer))))
    (cond
     ;; Fast path: reuse the cached child frame.
     ((and (frame-live-p fzfa-posframe--preview-frame)
           (eq (frame-parent fzfa-posframe--preview-frame) parent)
           (equal fzfa-posframe--preview-geometry geom)
           (eq fzfa-posframe--preview-poshandler poshandler)
           (buffer-live-p buffer))
      (let* ((frame fzfa-posframe--preview-frame)
             (win (frame-root-window frame)))
        (unless (eq (window-buffer win) buffer)
          ;; `posframe-show' dedicates the child-frame's root window to
          ;; its initial buffer; a plain `set-window-buffer' with a new
          ;; buffer errors on that dedication.  Drop it for the swap,
          ;; then reassert on the new buffer so any consumer that
          ;; inspects the dedication flag still sees a dedicated window.
          (let ((snapshot (fzfa-posframe--capture-locals buffer)))
            (set-window-dedicated-p win nil)
            (set-window-buffer win buffer)
            (set-window-dedicated-p win buffer)
            (fzfa-posframe--restore-locals buffer snapshot)))
        (unless (frame-visible-p frame)
          (make-frame-visible frame))))
     ;; Slow path: (re)build the frame via posframe-show.
     (t
      (when (and fzfa-posframe--current-buffer
                 (not (eq fzfa-posframe--current-buffer buffer)))
        (fzfa-posframe--dismiss fzfa-posframe--current-buffer))
      ;; A stale cached frame here means fast-path preconditions changed
      ;; (parent, geometry, or poshandler differ).  Drop it explicitly —
      ;; `posframe-show' would otherwise create a fresh frame and leave
      ;; the old one behind, since it keys its cache on BUFFER identity
      ;; and our tracking pointer would silently be overwritten.
      (when (frame-live-p fzfa-posframe--preview-frame)
        (ignore-errors (delete-frame fzfa-posframe--preview-frame)))
      (let ((snapshot (fzfa-posframe--capture-locals buffer))
            (frame (fzfa-posframe--with-top-frame parent
                     (posframe-show
                      buffer
                      :parent-frame          parent
                      :poshandler            poshandler
                      :width                 (car geom)
                      :height                (cdr geom)
                      :accept-focus          nil
                      :respect-mode-line     fzfa-posframe-respect-mode-line
                      :respect-header-line   fzfa-posframe-respect-header-line
                      :internal-border-width fzfa-posframe-internal-border-width
                      :internal-border-color (fzfa-posframe--border-color)))))
        (fzfa-posframe--reparent frame parent)
        (fzfa-posframe--restore-locals buffer snapshot)
        (setq fzfa-posframe--preview-frame     frame
              fzfa-posframe--preview-geometry  geom
              fzfa-posframe--preview-poshandler poshandler))))
    (fzfa-posframe--fit-image buffer)
    (setq fzfa-posframe--current-buffer buffer)))

(defun fzfa-posframe--fit-image (buffer)
  "Scale BUFFER's image to the posframe window when in `image-mode'.

`image-transform-fit-to-window' reads dimensions off the selected
window, so run it inside the posframe's window."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer (derived-mode-p 'image-mode)))
    (when-let* ((win (get-buffer-window buffer t)))
      (with-selected-window win
        (ignore-errors (image-transform-fit-to-window))))))

(defun fzfa-posframe--display-vertico-buffer (buffer _alist)
  "Display action routing BUFFER (vertico's buffer) into a candidate posframe.

Used as `vertico-buffer-display-action' when `fzfa-posframe-mode' is on,
layout is `side-by-side' or `top-to-bottom', and the active frontend is
vertico.  The poshandler is picked from
`fzfa-posframe--effective-layout' — `side-by-side' anchors to the left
pane; `top-to-bottom' anchors as the top of a vertically-stacked pair
whose combined block is centered on the parent frame.

Posframe marks its window dedicated to the buffer it was shown with;
`vertico-buffer' immediately swaps that window's buffer to the real
completion buffer, which errors on a dedicated window.  Undedicate
before returning so the swap succeeds; `vertico-buffer' re-dedicates
after the swap.

Fast path: when a cached candidate frame is alive with matching
parent, geometry, and poshandler, only `set-window-buffer' fires.
Falls through to a full teardown + `posframe-show' when any of those
differ (e.g. after the user toggled the layout)."
  (let* ((parent (fzfa-posframe--top-frame))
         (geom (fzfa-posframe--pane-geometry parent 'candidate))
         (preview-enabled (and (boundp 'fzfa-preview-delay)
                               fzfa-preview-delay))
         (poshandler
          (pcase (fzfa-posframe--effective-layout)
            ('top-to-bottom
             (if preview-enabled
                 ;; Preview also fires, so treat candidate+preview as a
                 ;; vertically stacked pair centered together.
                 #'fzfa-posframe-poshandler-centered-stack-top
               ;; No preview will fire — full frame-center is fine.
               #'posframe-poshandler-frame-center))
            (_ #'fzfa-posframe-poshandler-side-by-side-left))))
    (cond
     ;; Fast path: reuse the cached child frame.
     ((and (frame-live-p fzfa-posframe--vertico-frame)
           (eq (frame-parent fzfa-posframe--vertico-frame) parent)
           (equal fzfa-posframe--vertico-geometry geom)
           (eq fzfa-posframe--vertico-poshandler poshandler))
      (let ((win (frame-root-window fzfa-posframe--vertico-frame)))
        (set-window-dedicated-p win nil)
        (set-window-buffer win buffer)
        (unless (frame-visible-p fzfa-posframe--vertico-frame)
          (make-frame-visible fzfa-posframe--vertico-frame))
        win))
     ;; Slow path: rebuild.
     (t
      (fzfa-posframe--dismiss-vertico)
      (let* ((snapshot (fzfa-posframe--capture-locals buffer))
             (frame (fzfa-posframe--with-top-frame parent
                      (posframe-show
                       buffer
                       :parent-frame          parent
                       :poshandler            poshandler
                       :width                 (car geom)
                       :height                (cdr geom)
                       :accept-focus          nil
                       :respect-mode-line     fzfa-posframe-respect-mode-line
                       :respect-header-line   fzfa-posframe-respect-header-line
                       :internal-border-width fzfa-posframe-internal-border-width
                       :internal-border-color (fzfa-posframe--border-color))))
             (win (frame-root-window frame)))
        (fzfa-posframe--reparent frame parent)
        (fzfa-posframe--restore-locals buffer snapshot)
        (set-window-dedicated-p win nil)
        (setq fzfa-posframe--vertico-frame      frame
              fzfa-posframe--vertico-geometry   geom
              fzfa-posframe--vertico-poshandler poshandler)
        win)))))

(defun fzfa-posframe--preview-show-advice (orig-fn buffer &optional pos)
  "Around advice for `fzfa-preview-show'; render via posframe when active.

Falls through to ORIG-FN with BUFFER and POS when the mode is off, we
are on a TTY, or BUFFER is not live."
  (cond
   ((or (not fzfa-posframe-mode)
        (not (display-graphic-p))
        (not (buffer-live-p buffer)))
    (funcall orig-fn buffer pos))
   (t
    (when pos
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (goto-char (if (markerp pos) (marker-position pos) pos)))))
    (fzfa-posframe--show buffer)
    (fzfa-posframe--restore-vertico-minibuffer-soon)
    (let ((win (get-buffer-window buffer t)))
      (when (and win (window-live-p win))
        (with-selected-window win
          (when pos (goto-char (window-point win)))
          (run-hooks 'fzfa-after-preview-hook)))
      win))))

(defun fzfa-posframe--minibuffer-exit ()
  "Tear down our owned posframes when the current minibuffer session ends.

Excludes the helm posframe on purpose: `helm-cleanup' (helm-core.el:4322)
already handles frame deletion for a helm session — deleting the frame
here first strands `helm-cleanup' with `(helm--frame) = nil', which
then reaches `(delete-frame nil)' at line 4351 and errors with
\"Attempt to delete the sole visible or iconified frame\".  Orphan
sweep for the helm posframe lives in the mode-disable path.

Hides — rather than deletes — the vertico posframe so its cache
survives across sessions.  The full teardown lives on mode disable."
  (fzfa-posframe--dismiss)
  (fzfa-posframe--hide-vertico))

;;; Frontend-specific routing

(defun fzfa-posframe--multiform-merge (cat settings)
  "Merge SETTINGS into CAT's entry on `vertico-multiform-categories'.

Returns the ORIGINAL entry that existed for CAT (or nil if none did),
so the caller can restore or delete it on uninstall.

`vertico-multiform-categories' lookup is `seq-find'-based — first
match wins — so a fresh entry for CAT that lands in front of an
existing entry (e.g. the `fzfa-vertico-columns-mode' entry
`fzfa-vertico-setup' `cl-pushnew's for `fzfa-multi') would mask it.
This function collapses all entries for CAT into a single entry with
the union of settings, then re-pushes that combined entry at the head
of the list."
  (let* ((matching (cl-remove-if-not (lambda (e) (equal (car e) cat))
                                     vertico-multiform-categories))
         (existing (car matching))
         (existing-settings (mapcan (lambda (e) (copy-sequence (cdr e)))
                                    matching))
         (combined (cl-remove-duplicates
                    (append existing-settings settings)
                    :test #'equal :from-end t)))
    (setq vertico-multiform-categories
          (cons (cons cat combined)
                (cl-remove-if (lambda (e) (equal (car e) cat))
                              vertico-multiform-categories)))
    existing))

(defun fzfa-posframe--multiform-restore (cat original)
  "Undo `fzfa-posframe--multiform-merge' for CAT with ORIGINAL entry.

ORIGINAL is the entry that existed before the merge (or nil if we
created the entry from scratch)."
  (when (boundp 'vertico-multiform-categories)
    (setq vertico-multiform-categories
          (cl-remove-if (lambda (e) (equal (car e) cat))
                        vertico-multiform-categories))
    (when original
      (push original vertico-multiform-categories))))

(defun fzfa-posframe--install-vertico-routing ()
  "Wire vertico-buffer into the left posframe for fzfa completion categories.

Requires `vertico-buffer' and `vertico-multiform'.  Merges
`vertico-buffer-mode' into each `fzfa-posframe-vertico-categories'
entry on `vertico-multiform-categories' (creating fresh entries when
none exist), sets `vertico-buffer-display-action' to the fzfa-supplied
action, and enables `vertico-multiform-mode' if it wasn't already."
  (cond
   ((not (require 'vertico-buffer nil t))
    (message "fzfa-posframe: `vertico-buffer' unavailable; \
side-by-side falls back to inline vertico"))
   ((not (require 'vertico-multiform nil t))
    (message "fzfa-posframe: `vertico-multiform' unavailable; \
side-by-side falls back to inline vertico"))
   (t
    (unless fzfa-posframe--saved-vertico-buffer-action
      (setq fzfa-posframe--saved-vertico-buffer-action
            (bound-and-true-p vertico-buffer-display-action)))
    (setq vertico-buffer-display-action
          '(fzfa-posframe--display-vertico-buffer))
    (fzfa--ensure-setup)
    ;; Iterate reverse so the FIRST entry in `fzfa-posframe-vertico-categories'
    ;; lands at the HEAD of `vertico-multiform-categories' after all pushes.
    ;; `vertico-multiform--lookup' returns the first match, so specific
    ;; symbol overrides must precede the regex catch-all in the alist.
    (dolist (cat (reverse fzfa-posframe-vertico-categories))
      (unless (assoc cat fzfa-posframe--installed-vertico-categories)
        (let* ((base '(vertico-buffer-mode))
               ;; `fzfa-multi' additionally gets a column-max override:
               ;; the columns default was tuned for the full-width
               ;; minibuffer, so the narrower posframe produces cramped
               ;; or cut-off bands.  Dropping the user's setting by one
               ;; keeps the layout balanced across the smaller pane
               ;; width — but only when the user has 3+ columns, so
               ;; small values (2 or 1) pass through unchanged.
               (settings
                (if (and (eq cat 'fzfa-multi)
                         (boundp 'fzfa-vertico-columns-max)
                         (> fzfa-vertico-columns-max 2))
                    (append base
                            (list (cons 'fzfa-vertico-columns-max
                                        (1- fzfa-vertico-columns-max))))
                  base)))
          (push (cons cat (fzfa-posframe--multiform-merge cat settings))
                fzfa-posframe--installed-vertico-categories))))
    ;; Embark's nested completing-read (action prompter, `embark-become')
    ;; is another completing-read, so it belongs in the same left/candidates
    ;; slot as the outer fzfa completion it stacks on top of.
    ;; `vertico-buffer's own restore hook swaps the outer buffer back into
    ;; the pane when the nested completion exits.
    (dolist (cat fzfa-posframe-embark-categories)
      (unless (assoc cat fzfa-posframe--installed-vertico-categories)
        (push (cons cat (fzfa-posframe--multiform-merge
                         cat '(vertico-buffer-mode)))
              fzfa-posframe--installed-vertico-categories)))
    (unless (bound-and-true-p vertico-multiform-mode)
      (vertico-multiform-mode 1)
      (setq fzfa-posframe--enabled-vertico-multiform t)))))

(defun fzfa-posframe--uninstall-vertico-routing ()
  "Undo `fzfa-posframe--install-vertico-routing'."
  (when (boundp 'vertico-buffer-display-action)
    (setq vertico-buffer-display-action
          (or fzfa-posframe--saved-vertico-buffer-action
              '(display-buffer-use-least-recent-window))))
  (setq fzfa-posframe--saved-vertico-buffer-action nil)
  (dolist (installed fzfa-posframe--installed-vertico-categories)
    (fzfa-posframe--multiform-restore (car installed) (cdr installed)))
  (setq fzfa-posframe--installed-vertico-categories nil)
  (when (and fzfa-posframe--enabled-vertico-multiform
             (fboundp 'vertico-multiform-mode)
             (bound-and-true-p vertico-multiform-mode))
    (vertico-multiform-mode -1))
  (setq fzfa-posframe--enabled-vertico-multiform nil))

(defun fzfa-posframe--helm-show (buffer)
  "Show BUFFER in the fzfa left posframe; return the posframe window.

Shared render path for both `helm-display-function' and the
`display-buffer-alist' action — the former hands us a buffer name and
does not care about the return value, the latter is a display action
function that must return a window."
  (let* ((parent (fzfa-posframe--top-frame))
         (geom (fzfa-posframe--pane-geometry parent))
         (buf (if (bufferp buffer) buffer (get-buffer-create buffer)))
         (snapshot (fzfa-posframe--capture-locals buf))
         (frame (fzfa-posframe--with-top-frame parent
                  (posframe-show
                   buf
                   :parent-frame          parent
                   :poshandler            #'fzfa-posframe-poshandler-side-by-side-left
                   :width                 (car geom)
                   :height                (cdr geom)
                   :accept-focus          nil
                   :respect-mode-line     fzfa-posframe-respect-mode-line
                   :respect-header-line   fzfa-posframe-respect-header-line
                   :internal-border-width fzfa-posframe-internal-border-width
                   :internal-border-color (fzfa-posframe--border-color))))
         (win (frame-root-window frame)))
    (fzfa-posframe--reparent frame parent)
    (fzfa-posframe--restore-locals buf snapshot)
    (set-window-dedicated-p win nil)
    ;; Retitle so helm's `helm--frame' (helm-core.el:3796) discovery
    ;; predicate — which searches `frame-list' for
    ;; `(frame-parameter f 'title) = \"Helm\"' — actually finds our
    ;; posframe.  Without this, `helm--frame' returns nil during
    ;; `helm-cleanup', and the cleanup code at helm-core.el:4351 calls
    ;; `(delete-frame nil)', which targets the selected frame — the
    ;; user's main frame — and errors with \"Attempt to delete the sole
    ;; visible or iconified frame\".
    (when (frame-live-p frame)
      (set-frame-parameter frame 'title "Helm"))
    (setq fzfa-posframe--helm-buffer buf)
    ;; Tell helm the helm-buffer lives in a child frame.  With this flag
    ;; set, `helm-persistent-action-display-window' (helm-core.el:7703)
    ;; picks a window off `helm-initial-frame' (the main frame) instead
    ;; of falling through to the buggy `(get-buffer-window helm-buffer)'
    ;; call at line 7718 that only searches the selected frame — which
    ;; returns nil for a child-frame buffer and errors
    ;; `(wrong-type-argument window-live-p nil)' under
    ;; `select-window'.
    (setq helm--buffer-in-new-frame-p t)
    win))

(defun fzfa-posframe--helm-display (buffer &optional _resume)
  "Registered as `helm-display-function' — routes BUFFER via posframe."
  (fzfa-posframe--helm-show buffer))

(defun fzfa-posframe--helm-display-buffer (buffer _alist)
  "`display-buffer' action routing helm BUFFER via the posframe.

Registered in `display-buffer-alist' so helm's direct
`display-buffer' / `pop-to-buffer' calls (which bypass
`helm-display-function') also land inside the left posframe instead of
opening a second visible copy in a regular window."
  (fzfa-posframe--helm-show buffer))

(defun fzfa-posframe--dismiss-helm ()
  "Delete the helm posframe if any."
  (when (and fzfa-posframe--helm-buffer
             (buffer-live-p fzfa-posframe--helm-buffer))
    (ignore-errors (posframe-delete-frame fzfa-posframe--helm-buffer)))
  (setq fzfa-posframe--helm-buffer nil))

(defun fzfa-posframe--install-helm-routing ()
  "Route helm's display through the fzfa left posframe.

Overrides `helm-display-function' AND prepends a matching entry to
`display-buffer-alist' — helm has multiple display paths (some helm
sources bypass `helm-display-function' entirely with direct
`display-buffer' / `switch-to-buffer' calls), so belt-and-suspenders
catches both.  Only requires `helm' itself; no separate posframe
integration package."
  (cond
   ((not (require 'helm nil t))
    (message "fzfa-posframe: `helm' not installed; \
helm layout skipped"))
   (t
    (when (eq fzfa-posframe--saved-helm-display-function 'unset)
      (setq fzfa-posframe--saved-helm-display-function
            (bound-and-true-p helm-display-function)))
    (setq helm-display-function #'fzfa-posframe--helm-display)
    (let ((entry `(,fzfa-posframe-helm-buffer-regexp
                   (fzfa-posframe--helm-display-buffer))))
      (unless (member entry display-buffer-alist)
        (push entry display-buffer-alist)
        (setq fzfa-posframe--helm-display-buffer-entry entry))))))

(defun fzfa-posframe--uninstall-helm-routing ()
  "Undo `fzfa-posframe--install-helm-routing'."
  (unless (eq fzfa-posframe--saved-helm-display-function 'unset)
    (when (boundp 'helm-display-function)
      (setq helm-display-function
            fzfa-posframe--saved-helm-display-function))
    (setq fzfa-posframe--saved-helm-display-function 'unset))
  (when fzfa-posframe--helm-display-buffer-entry
    (setq display-buffer-alist
          (delete fzfa-posframe--helm-display-buffer-entry
                  display-buffer-alist))
    (setq fzfa-posframe--helm-display-buffer-entry nil))
  (fzfa-posframe--dismiss-helm))

(declare-function helm-window "helm-lib" ())

(defun fzfa-posframe--helm-cancel-stranded-follow-timer ()
  "Cancel helm's global follow-mode persistent-action timer if scheduled.

`helm-core' schedules `helm-execute-persistent-action' via
`helm--execute-persistent-action-timer' when `:follow 1' is set (which
we force via `fzfa-helm-want-follow' = t while our mode is active).
`helm-cleanup' does not cancel that timer; if it fires after helm has
exited, `helm-window' is nil and the persistent-action handler errors
with `(wrong-type-argument window-live-p nil)'.  Attaching this
canceller to `helm-cleanup-hook' drains the timer before helm's state
tears down."
  (when (and (boundp 'helm--execute-persistent-action-timer)
             (timerp helm--execute-persistent-action-timer))
    (cancel-timer helm--execute-persistent-action-timer)
    (setq helm--execute-persistent-action-timer nil)))

(defun fzfa-posframe--helm-persistent-action-guard (orig-fn &rest args)
  "Skip `helm-execute-persistent-action' when `helm-window' has been torn down.

Between `replace-buffer-in-windows' and `helm-alive-p = nil' at the tail
of `helm-cleanup', `helm-alive-p' is still `t' but `(helm-window)'
already returns nil.  A stranded follow-mode idle timer firing in that
window trips `with-helm-window' inside `helm-execute-persistent-action'
\(helm-core.el:7660) — which is `(with-selected-window nil ...)' and
errors with `(wrong-type-argument window-live-p nil)'.  The cleanup-
hook canceller (`fzfa-posframe--helm-cancel-stranded-follow-timer')
covers most cases; this advice is the belt-and-suspenders check for
the race window."
  (when (and (fboundp 'helm-window) (helm-window))
    (apply orig-fn args)))

(defun fzfa-posframe--install-helm-preview-follow ()
  "Force `fzfa-helm-want-follow' to t so preview fires under helm.

Independent of `fzfa-posframe-layout' — helm's preview handler runs
through helm's persistent-action machinery, which only auto-fires on
selection movement when `fzfa-helm-want-follow' is non-nil.  Without
this override, `preview-centered' layout + helm shows no preview at
all.

Also attaches `fzfa-posframe--helm-cancel-stranded-follow-timer' to
`helm-cleanup-hook' — forcing `:follow 1' makes the well-known
helm-follow / helm-cleanup timer race far more likely to bite."
  (when (eq (fzfa-posframe--active-frontend) 'helm)
    (when (eq fzfa-posframe--saved-helm-want-follow 'unset)
      (setq fzfa-posframe--saved-helm-want-follow
            (bound-and-true-p fzfa-helm-want-follow)))
    (setq fzfa-helm-want-follow t)
    (when (boundp 'helm-cleanup-hook)
      (add-hook 'helm-cleanup-hook
                #'fzfa-posframe--helm-cancel-stranded-follow-timer))
    (advice-add 'helm-execute-persistent-action :around
                #'fzfa-posframe--helm-persistent-action-guard)))

(defun fzfa-posframe--uninstall-helm-preview-follow ()
  "Restore state captured by `fzfa-posframe--install-helm-preview-follow'."
  (unless (eq fzfa-posframe--saved-helm-want-follow 'unset)
    (setq fzfa-helm-want-follow fzfa-posframe--saved-helm-want-follow
          fzfa-posframe--saved-helm-want-follow 'unset))
  (when (boundp 'helm-cleanup-hook)
    (remove-hook 'helm-cleanup-hook
                 #'fzfa-posframe--helm-cancel-stranded-follow-timer))
  (advice-remove 'helm-execute-persistent-action
                 #'fzfa-posframe--helm-persistent-action-guard))


(defun fzfa-posframe--ivy-display (str)
  "Display STR via `ivy-posframe--display' anchored in the left pane.

Registered as the sole entry in `ivy-posframe-display-functions-alist'
while `fzfa-posframe-mode' is active with `side-by-side' layout."
  (ivy-posframe--display str #'fzfa-posframe-poshandler-side-by-side-left))

(defun fzfa-posframe--ivy-size ()
  "Return the size plist ivy-posframe should use for its posframe.

Sized to match the fzfa left pane geometry, recomputed on every call so
frame resizes are picked up without cycling the mode."
  (let* ((parent (fzfa-posframe--top-frame))
         (geom (fzfa-posframe--pane-geometry parent)))
    (list :width      (car geom)
          :height     (cdr geom)
          :min-width  (car geom)
          :min-height (cdr geom))))

(defun fzfa-posframe--install-ivy-routing ()
  "Enable `ivy-posframe-mode' and pin its posframe to the left pane.

Also raises `ivy-height' to the pane's row count so ivy renders enough
candidates to fill the posframe — ivy's default of 10 lines otherwise
leaves the bottom half of the pane empty regardless of posframe size.

Saves the pre-mode values of `ivy-posframe-display-functions-alist',
`ivy-posframe-size-function', and `ivy-height' so disable can restore
them."
  (cond
   ((not (require 'ivy-posframe nil t))
    (message "fzfa-posframe: `ivy-posframe' not installed; \
ivy keeps its default display"))
   (t
    (when (eq fzfa-posframe--saved-ivy-display-functions-alist 'unset)
      (setq fzfa-posframe--saved-ivy-display-functions-alist
            (bound-and-true-p ivy-posframe-display-functions-alist)))
    (when (eq fzfa-posframe--saved-ivy-size-function 'unset)
      (setq fzfa-posframe--saved-ivy-size-function
            (bound-and-true-p ivy-posframe-size-function)))
    (when (eq fzfa-posframe--saved-ivy-height 'unset)
      (setq fzfa-posframe--saved-ivy-height
            (bound-and-true-p ivy-height)))
    (setq ivy-posframe-display-functions-alist
          '((t . fzfa-posframe--ivy-display))
          ivy-posframe-size-function
          #'fzfa-posframe--ivy-size
          ivy-height
          (max (or fzfa-posframe--saved-ivy-height 10)
               (cdr (fzfa-posframe--pane-geometry
                     (fzfa-posframe--top-frame)))))
    (unless (bound-and-true-p ivy-posframe-mode)
      (ivy-posframe-mode 1)
      (setq fzfa-posframe--enabled-ivy-posframe t)))))

(defun fzfa-posframe--uninstall-ivy-routing ()
  "Undo `fzfa-posframe--install-ivy-routing'."
  (unless (eq fzfa-posframe--saved-ivy-display-functions-alist 'unset)
    (when (boundp 'ivy-posframe-display-functions-alist)
      (setq ivy-posframe-display-functions-alist
            fzfa-posframe--saved-ivy-display-functions-alist))
    (setq fzfa-posframe--saved-ivy-display-functions-alist 'unset))
  (unless (eq fzfa-posframe--saved-ivy-size-function 'unset)
    (when (boundp 'ivy-posframe-size-function)
      (setq ivy-posframe-size-function
            fzfa-posframe--saved-ivy-size-function))
    (setq fzfa-posframe--saved-ivy-size-function 'unset))
  (unless (eq fzfa-posframe--saved-ivy-height 'unset)
    (when (boundp 'ivy-height)
      (setq ivy-height fzfa-posframe--saved-ivy-height))
    (setq fzfa-posframe--saved-ivy-height 'unset))
  (when (and fzfa-posframe--enabled-ivy-posframe
             (fboundp 'ivy-posframe-mode)
             (bound-and-true-p ivy-posframe-mode))
    (ivy-posframe-mode -1))
  (setq fzfa-posframe--enabled-ivy-posframe nil))

(defun fzfa-posframe--install-frontend-routing ()
  "Dispatch candidate-pane routing to the active frontend.

Layouts that route candidates to a posframe:

  side-by-side   Vertico / helm / ivy — candidates go to the left pane.
  top-to-bottom  Vertico only — candidates stacked above the preview
                 with the combined block centered on the parent frame
                 (helm / ivy are left as-is for now; wiring their
                 display-functions to a stack-top poshandler would need
                 per-frontend work not yet done here).

`preview-centered' does not route candidates (they stay inline in the
minibuffer), so it is a no-op.  Icomplete has no buffer-mode
equivalent — `--effective-layout' downgrades to `preview-centered' at
render time."
  (pcase fzfa-posframe-layout
    ('side-by-side
     (pcase (fzfa-posframe--active-frontend)
       ('vertico   (fzfa-posframe--install-vertico-routing))
       ('helm      (fzfa-posframe--install-helm-routing))
       ('ivy       (fzfa-posframe--install-ivy-routing))
       ('icomplete
        (message "fzfa-posframe: icomplete cannot render candidates \
in a posframe; using preview-centered layout"))))
    ('top-to-bottom
     (pcase (fzfa-posframe--active-frontend)
       ('vertico   (fzfa-posframe--install-vertico-routing))
       ('icomplete
        (message "fzfa-posframe: icomplete cannot render candidates \
in a posframe; using preview-centered layout"))))))

(defun fzfa-posframe--uninstall-frontend-routing ()
  "Undo any candidate-pane routing installed by mode-on."
  (fzfa-posframe--uninstall-vertico-routing)
  (fzfa-posframe--uninstall-helm-routing)
  (fzfa-posframe--uninstall-ivy-routing))

;;; Embark keymap-prompter filter (switch-frame from trackpad scroll)

(declare-function embark-keymap-prompter "embark" (keymap update))

(defvar fzfa-posframe--embark-swallow-commands
  '(handle-switch-frame handle-focus-in handle-focus-out)
  "Commands `embark-keymap-prompter' should treat as non-picked events.

`embark-keymap-prompter's pcase catch-all returns any unrecognized
command as the picked action.  When the actions posframe is a child
frame, macOS trackpad scroll generates `switch-frame' events that bind
to `handle-switch-frame' — landing in the catch-all and being picked
as the action, which then runs a stale-state closure that later errors
on the dead frame handle at drain time.

Around advice at `fzfa-posframe--embark-prompter-around' re-invokes
the prompter when the returned command is in this list.")

(defun fzfa-posframe--embark-prompter-around (orig keymap update)
  "Discard frame-focus command returns from embark's prompter, re-read.

Only kicks in when `fzfa-posframe-mode' is active AND we are inside a
fzfa session — so unrelated embark sessions keep normal semantics."
  (if (and fzfa-posframe-mode
           (fzfa-posframe--in-fzfa-session-p))
      (let ((result (funcall orig keymap update)))
        (while (memq result fzfa-posframe--embark-swallow-commands)
          (setq result (funcall orig keymap update)))
        result)
    (funcall orig keymap update)))

;;; Embark-actions routing (case 1: the verbose indicator buffer)

(defvar fzfa-posframe--embark-actions-buffer " *Embark Actions*"
  "Name of embark's verbose-indicator buffer we route into the preview frame.")

(defvar fzfa-posframe--saved-display-buffer-entry nil
  "Pointer back to the alist entry we pushed on `display-buffer-alist'.

Kept so `--uninstall-embark-actions-routing' can `delq' precisely what
we added rather than pattern-matching entries and risking a false hit
on a user-installed entry that happens to reuse our functions.")

(defun fzfa-posframe--in-fzfa-session-p ()
  "Return non-nil when any live minibuffer belongs to an fzfa session.

Walks the minibuffer stack (`\" *Minibuf-N*\"' buffers) rather than
just checking the currently active one — embark's action prompter
opens a nested minibuffer whose buffer does NOT have the fzfa marker,
but the outer fzfa minibuffer is still on the stack underneath and
should still be treated as \"we're in fzfa\"."
  (cl-loop for depth from 1 to (minibuffer-depth)
           for buf = (get-buffer (format " *Minibuf-%d*" depth))
           thereis (and buf
                        (buffer-local-value 'fzfa--minibuffer-marker buf))))

(defun fzfa-posframe--embark-actions-condition (buf _action)
  "`display-buffer-alist' condition for `\" *Embark Actions*\"' routing.

Fires only when BUF names the embark verbose-indicator buffer AND we
are inside an fzfa session — so non-fzfa embark sessions retain the
user's own `embark-verbose-indicator-display-action' behavior."
  (and (or (stringp buf) (bufferp buf))
       (string= (if (bufferp buf) (buffer-name buf) buf)
                fzfa-posframe--embark-actions-buffer)
       (fzfa-posframe--in-fzfa-session-p)))

(defun fzfa-posframe--display-embark-actions (buffer _alist)
  "Display action routing BUFFER (embark's verbose actions) into the preview frame.

The action buffer is a passive indicator, so it clobbers whatever the
preview frame currently shows.  When embark closes and the next
candidate hover fires, `fzfa--preview-call' repaints the preview
naturally through the existing pipeline — no explicit restore needed."
  (fzfa-posframe--show buffer)
  (when-let* ((frame fzfa-posframe--preview-frame)
              ((frame-live-p frame)))
    (frame-root-window frame)))

(defun fzfa-posframe--install-embark-actions-routing ()
  "Prepend a scoped entry on `display-buffer-alist' for embark's actions buffer.

Idempotent."
  (let ((entry `(fzfa-posframe--embark-actions-condition
                 (fzfa-posframe--display-embark-actions))))
    (unless (member entry display-buffer-alist)
      (setq fzfa-posframe--saved-display-buffer-entry entry)
      (push entry display-buffer-alist)))
  (with-eval-after-load 'embark
    (advice-add 'embark-keymap-prompter :around
                #'fzfa-posframe--embark-prompter-around)))

(defun fzfa-posframe--uninstall-embark-actions-routing ()
  "Remove the `display-buffer-alist' entry installed by mode-on."
  (when fzfa-posframe--saved-display-buffer-entry
    (setq display-buffer-alist
          (delq fzfa-posframe--saved-display-buffer-entry
                display-buffer-alist))
    (setq fzfa-posframe--saved-display-buffer-entry nil))
  (when (fboundp 'embark-keymap-prompter)
    (advice-remove 'embark-keymap-prompter
                   #'fzfa-posframe--embark-prompter-around)))

;;; Mode

;;;###autoload
(define-minor-mode fzfa-posframe-mode
  "Global minor mode routing fzfa's preview (and optionally its
candidates) into floating posframes.

When enabled:
- `fzfa-preview-show' renders into a posframe positioned per
  `fzfa-posframe-layout'.
- When layout is `side-by-side' and a supported frontend is active,
  candidates route into a matching left posframe:
    vertico   → `vertico-buffer-mode' via vertico-multiform.
    helm      → fzfa's own `helm-display-function' (only `helm' needed).
    ivy       → `ivy-posframe' (MELPA) if installed.
    icomplete → downgrades to `preview-centered' layout.
- When layout is `top-to-bottom' and vertico is active, candidates
  route into a centered posframe on top; the preview posframe sits
  directly below, and the combined block is vertically centered.

All installed state is restored on disable."
  :global t
  :group 'fzfa
  (cond
   ((not fzfa-posframe-mode)
    (advice-remove 'fzfa-preview-show
                   #'fzfa-posframe--preview-show-advice)
    (remove-hook 'minibuffer-exit-hook
                 #'fzfa-posframe--minibuffer-exit)
    (fzfa-posframe--uninstall-frontend-routing)
    (fzfa-posframe--uninstall-embark-actions-routing)
    (fzfa-posframe--uninstall-helm-preview-follow)
    (fzfa-posframe--dismiss)
    (fzfa-posframe--dismiss-vertico)
    (fzfa-posframe--dismiss-helm))
   ((not (display-graphic-p))
    (setq fzfa-posframe-mode nil)
    (user-error "`fzfa-posframe-mode' requires a graphical display"))
   ((not (require 'posframe nil t))
    (setq fzfa-posframe-mode nil)
    (user-error "`fzfa-posframe-mode' requires the `posframe' package"))
   (t
    (advice-add 'fzfa-preview-show :around
                #'fzfa-posframe--preview-show-advice)
    (add-hook 'minibuffer-exit-hook
              #'fzfa-posframe--minibuffer-exit)
    (fzfa-posframe--install-helm-preview-follow)
    (fzfa-posframe--install-frontend-routing)
    (fzfa-posframe--install-embark-actions-routing))))

(provide 'fzfa-posframe)
;;; fzfa-posframe.el ends here
