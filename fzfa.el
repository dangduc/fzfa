;;; fzfa.el --- Async fuzzy completion via `fzf-native' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (fzf-native "1.4"))
;; Keywords: matching, completion, fzf, fuzzy, fussy
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides async shell command completion using fzf-native for scoring.
;; The native layer handles process I/O on a background thread, ANSI
;; stripping, and parallel fzf scoring.  The Elisp layer provides
;; while-no-input responsiveness, a candidate cap, and a live stats overlay.
;;
;; Quick start:
;;   (fzfa-setup)   ; register completion style + category override
;;   (fzfa-find-files)

(require 'cl-lib)
(require 'fzf-native)

;;; Code:

(defgroup fzfa nil
  "Async fuzzy completion via fzf-native."
  :group 'completion
  :link '(url-link :tag "GitHub" "https://github.com/jojojames/fzfa"))

(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)
(defvar embark-general-map)
(defvar fzf-native-case-mode)
(defvar fzf-native-fuzzy)
(defvar fzf-native-async-highlight)
(defvar fzf-native-max-line-length)
(defvar fzf-native-async-cache-size)
(defvar marginalia-annotate-file)
(defvar marginalia-annotator-registry)
(defvar marginalia-command-categories)
(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function icomplete-exhibit "icomplete")
(defvar icomplete-overlay)
(defvar ivy-text)
(defvar ivy--index)
(defvar ivy--all-candidates)
(defvar ivy-count-format)
(defvar ivy-completing-read-dynamic-collection)
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")
(declare-function ivy--insert-prompt "ivy")
(declare-function ivy-dispatching-call "ivy")
(declare-function ivy-state-dynamic-collection "ivy")
(defvar ivy-last)
(defvar ivy--actions-list)
(defvar ivy-pre-prompt-function)
(declare-function projectile-project-root "projectile")
(declare-function project-root "project")
(declare-function vertico--exhibit "vertico")
(defvar vertico--index)
(defvar vertico--input)
(defvar recentf-list)
(defvar marginalia-annotators)
(declare-function fzf-native-score "fzf-native")
(declare-function fzf-native-score-all "fzf-native")
(declare-function fzf-native-async-start "fzf-native")
(declare-function fzf-native-async-stop "fzf-native")
(declare-function fzf-native-async-generation "fzf-native")
(declare-function fzf-native-async-candidates "fzf-native")
(declare-function fzf-native-async-stats "fzf-native")
(declare-function fzf-native-async-result-fresh-p "fzf-native")

;;; Debug logging

(defvar fzfa-debug nil
  "When non-nil at load time, compile debug logging into fzfa.
Set before loading or re-evaluating fzfa.el; toggling at runtime
has no effect (the check is macro-expanded at load time, like #ifdef).")

(defmacro fzfa--log (fmt &rest args)
  "Emit a debug message FMT with ARGS if `fzfa-debug' is non-nil at load.
Expands to nothing when disabled — zero runtime cost.  Logs to *Messages*
only; variable `inhibit-message' is bound around the `message' call so
the echo-area write is suppressed, otherwise it would stomp the
active minibuffer/mini-window display."
  (when (bound-and-true-p fzfa-debug)
    `(let ((inhibit-message t)) (message ,fmt ,@args))))

;;; Customization

(defcustom fzfa-max-candidates 10000
  "Max candidates returned to Elisp from `fzfa-async-completing-read'.
The full filtered/total counts are still tracked and shown in the prompt.
Set to nil or 0 to disable the cap (may be slow for very large result sets)."
  :type '(choice (const  :tag "No cap" nil)
                 (integer :tag "Max candidates"))
  :group 'fzfa)

(defcustom fzfa-refresh-delay 0.05
  "Seconds between polls for new C-side candidate generations.
The background reader thread increments a generation counter as lines
arrive; this timer checks that counter and schedules a display refresh
when new data is available.  Lower values feel more responsive but burn
more CPU on the polling loop.  Analogous to `consult-async-refresh-delay'."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-input-debounce 0.1
  "Seconds of idle time to wait before retrying after interrupted scoring.
When the user types fast, `while-no-input' aborts the scoring call.  This
idle timer fires once typing pauses and re-triggers the display so results
self-heal."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-input-throttle 0.2
  "Minimum seconds between display refreshes driven by new incoming data.
Even when new candidate generations arrive continuously (e.g. a fast `find'
streaming thousands of files), the completion UI is only re-exhibited once
per this interval.  The debounce retry path is unaffected — after the user
pauses typing the display always self-heals regardless of this value."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-preview-delay 0.8
  "Seconds of idle time before live preview fires after a candidate change.
Used by commands that preview the highlighted candidate as the selection
moves (e.g. `fzfa-theme' loading the theme under point).  Implemented with
`run-with-idle-timer', so fast typing or arrow-key bursts naturally suppress
intermediate previews — the timer only fires once input settles.
A value of 0 previews immediately on every selection change, which can make
typing feel sluggish for expensive preview actions.  Set to nil to disable
live preview entirely (global escape hatch)."
  :type '(choice (const  :tag "Disabled" nil)
                 (number :tag "Idle seconds"))
  :group 'fzfa)

(defcustom fzfa-highlight 200
  "Controls C-side match highlighting of completion candidates.
nil or a negative integer — no highlighting.
t                        — highlight every returned candidate.
a positive integer N     — highlight only the top N candidates.
The C layer applies `completions-common-part' face to each contiguous
run of matched bytes via fzf_get_positions."
  :type '(choice (const   :tag "Disabled" nil)
                 (const   :tag "All candidates" t)
                 (integer :tag "Top N candidates"))
  :group 'fzfa)

(defcustom fzfa-max-line-length 256
  "Maximum character length of a candidate line.
nil           — no limit.
positive N    — exclude lines longer than N characters.
negative -N   — include but truncate lines to N characters.

Applies at read time: lines from the subprocess are filtered or
truncated before entering the candidate pool, so scoring never
sees the excess characters.

For the rg / ag / ugrep grep-style commands the cap is also pushed
upstream into the search tool itself (via `--max-columns' / `--width')."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzfa)

(defcustom fzfa-case-mode 'smart
  "Case-sensitivity mode propagated to `fzf-native-case-mode'.

Mirrors fzf-native's enum:
smart    Case-insensitive when the query is all lowercase; case-sensitive
         once it contains any uppercase character (fzf's default).
ignore   Always case-insensitive.
respect  Always case-sensitive."
  :type '(choice (const :tag "Smart case (default)" smart)
                 (const :tag "Ignore case"          ignore)
                 (const :tag "Respect case"         respect))
  :group 'fzfa)

(defcustom fzfa-fuzzy t
  "Whether to fuzzy match with `fzf-native'.

If t, use fuzzy matching, if nil, use exact/substring matching.

If t, prefixing a term with ' switches that term to exact matching.

If nil, prefixing a term with ' switches that term to fuzzy matching.

Read at the start of every scoring call.

Propagated to `fzf-native-fuzzy' via `:around' advice on the
`fzf-native' async entry points."
  :type 'boolean
  :group 'fzfa)

(defcustom fzfa-cache-size 40
  "Maximum number of scored snapshots cached per async session.
Each entry stores the top-K results and the full matched-candidate
index for one query, enabling exact-fresh hits (skip scoring) and
prefix-refinement hits (rescore only previously-matched candidates
plus deltas) without re-scanning the full pool.

A larger value keeps a longer typing trail in cache, which improves
backspace coverage — backspacing past N keystrokes will still hit
the LRU as long as those intermediate queries weren't evicted by
unrelated lookups.

Read at session start; changing it does not affect running sessions."
  :type 'integer
  :group 'fzfa)

(defcustom fzfa-extensions
  '(ag chrome company emacs embark evil fd find flymake git grep helm hg
       hungry imenu info ivy locate mail make music notmuch org pass project rg
       shell spotlight ugrep vertico vc)
  "List of fzfa extensions to load from `fzfa-setup'.
Each SYMBOL causes `fzfa-setup' to `require' the feature
`fzfa-SYMBOL' and, if defined, call `fzfa-SYMBOL-setup'."
  :type '(set (const :tag "ag (the_silver_searcher)" ag)
              (const :tag "Chrome bookmarks + passwords" chrome)
              (const :tag "company-mode completions" company)
              (const :tag "Emacs built-in sources" emacs)
              (const :tag "Embark actions" embark)
              (const :tag "Evil-mode marks + registers" evil)
              (const :tag "fd (find alternative)" fd)
              (const :tag "POSIX find" find)
              (const :tag "Flymake diagnostics" flymake)
              (const :tag "Git" git)
              (const :tag "POSIX grep" grep)
              (const :tag "Helm frontend" helm)
              (const :tag "Mercurial (hg)" hg)
              (const :tag "Hungry (buffer-derived dirs)" hungry)
              (const :tag "Imenu (buffer index)" imenu)
              (const :tag "Info manuals" info)
              (const :tag "Ivy frontend" ivy)
              (const :tag "locate" locate)
              (const :tag "macOS Mail.app" mail)
              (const :tag "make / ninja targets" make)
              (const :tag "macOS Music.app" music)
              (const :tag "notmuch mail search" notmuch)
              (const :tag "Org-mode headings" org)
              (const :tag "password-store (pass)" pass)
              (const :tag "project.el" project)
              (const :tag "ripgrep (rg)" rg)
              (const :tag "Shell command + history" shell)
              (const :tag "macOS Spotlight (mdfind)" spotlight)
              (const :tag "ugrep" ugrep)
              (const :tag "vc.el" vc)
              (const :tag "Vertico" vertico))
  :group 'fzfa)

(defcustom fzfa-project-backend 'project
  "How to resolve the root directory for fzfa commands.
project    Use `project.el' to find the project root (default, matches consult).
projectile Use `projectile-project-root'.
nil        Use `default-directory' (no project detection).
function   Call the function with no arguments; Returns a directory string."
  :type '(choice (const :tag "project.el" project)
                 (const :tag "Projectile" projectile)
                 (const :tag "None (default-directory)" nil)
                 (function :tag "Custom function"))
  :group 'fzfa)

;;; Variables

(defvar fzfa--setup-done nil
  "Non-nil once `fzfa--ensure-setup' has installed registrations.
Reset to nil to force a re-setup on the next entry-point call.")

(defvar fzfa--multi-mode nil
  "Dispatch flag for `fzfa-async-completing-read' / `fzfa-sync-completing-read'.
- `:extract'         — throw `fzfa-extracted' with the call's keyword args.
- (`:inject' . CAND) — return CAND directly without prompting.
Bound by `fzfa-multi-read' to derive multi-source sources from
existing single-source commands without modifying their definitions.")

(defvar fzfa-directory nil
  "Per-call directory override for fzfa commands.
When non-nil, supersedes `fzfa-project-backend' and `default-directory'.
Intended for `let'-binding when extending built-in commands:

Priority: `fzfa-directory' > project backend > `default-directory'.")

;;; `completion-styles'

(defun fzfa-try-completion (string _table _pred _point)
  "Try-completion for the fzfa completion style.
Always accepts STRING as-is; scoring is done in C."
  (cons string (length string)))

(defun fzfa-all-completions (string table pred _point)
  "All-completions for the fzfa completion style.
Passes STRING through to the collection TABLE filtered by PRED.
Highlighting is applied by the C layer (see `fzfa-highlight')."
  (funcall table string pred t))

;;; Frontend abstraction

(defvar fzfa-async-helm-handler nil
  "Function called by `fzfa-async-completing-read' under `helm-mode'.
Receives the same keyword args (:prompt :command :directory
:skip-executable-check) and returns the selected candidate string,
or nil on cancel.  Populated by `fzfa-helm.el' on load.  When nil,
the helm dispatch is skipped and fzfa runs through `completing-read'.")

(defvar fzfa-sync-helm-handler nil
  "Function called by `fzfa-sync-completing-read' under `helm-mode'.
Receives the same keyword args (:candidates :prompt :category
:annotate :affix :group :history :require-match :default :preview)
and returns the selected candidate string, or nil on cancel.
Populated by `fzfa-helm.el' on load.  When nil, the sync path runs
through `completing-read' (where helm-mode's advice picks it up but
ignores the `display-sort-function' metadata, so per-history ordering
is lost).")

(defvar fzfa-multi-helm-handler nil
  "Function called by `fzfa--multi-read' under `helm-mode'.
Receives the SOURCES list as the first argument and :prompt as a
keyword.  Returns the action's return value, or nil.  When nil,
fzfa signals a `user-error' under `helm-mode'.")

(defun fzfa--frontend-index ()
  "Return the active completion UI's selection index (0-based), or nil.
Returns nil for frontends without a selection index (e.g. icomplete)."
  (cond
   ((bound-and-true-p vertico-mode) (max 0 vertico--index))
   ((bound-and-true-p ivy-mode) (and (boundp 'ivy--index) (max 0 ivy--index)))
   (t nil)))

(defun fzfa--frontend-candidate ()
  "Return the currently highlighted candidate string in the active UI, or nil.

Used for live preview (e.g. `fzfa-theme') and persistent-action
\(`fzfa-apply-current')."
  ;; Ensure we're in the minibuffer because the user could've pressed away
  ;; by the time this function is called.
  ;; e.g. M-x `fzfa-find-any'
  ;; C-h k <key>
  ;; Focus switches to help window -> timer fires -> this function is called.
  (when-let* ((win (active-minibuffer-window)))
    (with-current-buffer (window-buffer win)
      (cond
       ((bound-and-true-p vertico-mode)
        (when (and (boundp 'vertico--candidates) vertico--candidates)
          (nth (max 0 vertico--index) vertico--candidates)))
       ((bound-and-true-p ivy-mode)
        (when (and (boundp 'ivy--all-candidates) ivy--all-candidates)
          (nth (max 0 ivy--index) ivy--all-candidates)))
       ((bound-and-true-p icomplete-mode)
        (car (completion-all-sorted-completions)))))))

(defun fzfa--icomplete-fit-mini-window ()
  "Grow the mini-window to fit `icomplete-overlay's `after-string'.

Mini-window auto-resize during redisplay treats the empty-input case
\(zero-length overlay) as needing 1 line, ignoring the multi-line
`after-string', so the initial display under icomplete-vertical never
grows for our completions.  Cap at `max-mini-window-height'.  Pair
with `resize-mini-windows' set buffer-locally to `grow-only' (in
`fzfa--minibuffer-format-reset') so subsequent redisplays can't shrink
the pane back."
  (when-let* ((win (and (bound-and-true-p icomplete-mode)
                        (active-minibuffer-window)))
              ((eq win (selected-window)))
              (after (and (bound-and-true-p icomplete-overlay)
                          (overlay-get icomplete-overlay 'after-string)))
              ((stringp after))
              (lines (1+ (cl-count ?\n after)))
              (max-lines
               (cond ((floatp max-mini-window-height)
                      (max 1 (floor (* max-mini-window-height
                                       (frame-height)))))
                     ((integerp max-mini-window-height) max-mini-window-height)
                     (t 25)))
              (target (min lines max-lines))
              ((> target (window-text-height win))))
    (ignore-errors
      (set-window-text-height win target))))

(defun fzfa--icomplete-exhibit ()
  "Refresh icomplete's candidate display after an async generation bump.

Flushes the sorted-completions cache (icomplete reads it via the
buffer-local variable `completion-all-sorted-completions'), runs the
standard `icomplete-exhibit' to repopulate the overlay's
`after-string', and fits the mini-window."
  (completion--flush-all-sorted-completions)
  ;; Belt-and-suspenders flush: the function above can short-circuit
  ;; based on region args, leaving the cache populated.
  (setq completion-all-sorted-completions nil)
  (icomplete-exhibit)
  (fzfa--icomplete-fit-mini-window))

(defun fzfa--frontend-exhibit ()
  "Trigger a display refresh in the active completion UI.
Handles vertico and icomplete.  `ivy' is handled separately."
  (when-let* ((win (active-minibuffer-window)))
    (with-selected-window win
      (cond
       ((bound-and-true-p vertico-mode)
        (setq vertico--input t)
        (vertico--exhibit))
       ((bound-and-true-p icomplete-mode)
        (fzfa--icomplete-exhibit))))))

(defun fzfa--frontend-push (ivy-push-fn)
  "Refresh the active completion display.
Under `ivy-mode' (push model) `funcall' IVY-PUSH-FN; otherwise hand
off to `fzfa--frontend-exhibit' for the pull-model frontends.
Designed to be passed straight to `run-with-idle-timer' with
IVY-PUSH-FN as the trailing argument, or called directly inline."
  (if (bound-and-true-p ivy-mode)
      (funcall ivy-push-fn)
    (fzfa--frontend-exhibit)))

(defun fzfa--insert-prompt-if-ivy ()
  "Force ivy to redraw its prompt if `ivy-mode' is active.
Required because `ivy--exhibit' skips the prompt redraw when the
candidate body didn't change; our dynamic-state pre-prompt content
\(stats line, narrow indicator) would otherwise stay stale.  No-op
under non-ivy frontends."
  (when (and (bound-and-true-p ivy-mode)
             (active-minibuffer-window))
    (with-selected-window (active-minibuffer-window)
      (ivy--insert-prompt))))

;;; Visit / preview hooks

(defcustom fzfa-after-visit-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after a fzfa command visits its selection.
Each command's action lambda wraps its body in `fzfa-with-visit', which
fires this hook once the visit completes (point is at the destination)."
  :type 'hook :group 'fzfa)

(defcustom fzfa-after-apply-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after `fzfa-apply-current' displays a candidate."
  :type 'hook :group 'fzfa)

(defcustom fzfa-after-preview-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after `fzfa-preview-show' displays a candidate.
Fires on every preview tick (point is at the previewed location in the
origin window)."
  :type 'hook :group 'fzfa)

(defmacro fzfa-with-visit (&rest body)
  "Run BODY as a visit action; fire `fzfa-after-visit-hook' on completion."
  (declare (indent 0) (debug t))
  `(prog1 (progn ,@body)
     (run-hooks 'fzfa-after-visit-hook)))

;;; Apply (persistent-action)

(defcustom fzfa-apply-key "C-M-m"
  "Key string bound to `fzfa-apply-current' in `fzfa' minibuffer sessions.

This only applies to non-`ivy', non-`helm' sessions.

`ivy' uses `ivy-call' and `helm' uses `helm-execute-persistent-action'.

Matches `ivy''s default `ivy-call' binding."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'fzfa)

(defcustom fzfa-apply-functions
  '((fzfa-file     :apply find-file)
    (fzfa-buffer   :apply switch-to-buffer)
    (fzfa-bookmark :apply bookmark-jump)
    (fzfa-grep     :apply fzfa--grep-jump)
    (fzfa-location :apply fzfa--location-jump))
  "Per-category apply handlers.

Alist of (CATEGORY . PLIST), where PLIST recognizes the `:apply' slot
\(a function taking a single CANDIDATE string).  Used by
`fzfa--resolve-apply' when a session/source doesn't declare an explicit
`:apply' lambda.  Drop a category's entry (or its `:apply' slot) to
disable the fallback for that category."
  :type '(alist :key-type symbol
                :value-type (plist :options ((:apply function))))
  :group 'fzfa)

(defvar fzfa--session-apply nil
  "Apply function for the active single-source session.

Lambda taking a single CANDIDATE; invoked by `fzfa-apply-current'
without exiting the session.  `let'-bound by
`fzfa-async-completing-read' / `fzfa-sync-completing-read' from the
constructor's `:apply' keyword \(falling back to `fzfa-apply-functions'
by category).  Multi sessions look up `:apply' per-source via
`fzfa--resolve-apply' instead.")

(defvar fzfa--session-resolve-paths nil
  "Whether to expand path candidates for the active single-source session.

Mirrors the constructor's `:resolve-paths' argument; `let'-bound by
`fzfa-async-completing-read'.  Passed
to `fzfa--maybe-expand' in `fzfa-apply-current'; the directory comes
from the minibuffer's `default-directory' which the setup hook
already binds to the constructor's `:directory'.  Multi sessions read
`:resolve-paths' (and `:directory') per-source instead.")

(defun fzfa--resolve-apply (cand)
  "Resolve the apply function for CAND in the active fzfa session.
Returns the function or nil when no apply is available.

Multi: route via `fzfa--multi-source-of', prefer the source's `:apply'
slot, fall back to `:action' (helm semantics) then to the
category default from `fzfa-apply-functions'.

Single: return `fzfa--session-apply' (already pre-resolved against
`fzfa-apply-functions' at constructor time)."
  (cond
   ((bound-and-true-p fzfa--multi-active-sources)
    (when-let* ((src (fzfa--multi-source-of
                      cand fzfa--multi-active-sources nil)))
      (or (plist-get src :apply)
          (plist-get src :action)
          (plist-get
           (alist-get (plist-get src :category) fzfa-apply-functions)
           :apply))))
   (t fzfa--session-apply)))

(defun fzfa--resolve-candidate (cand)
  "Return CAND with path resolution applied per session/source rules.

Mirrors the commit-path `fzfa--maybe-expand' call so the apply lambda
sees the same string the post-`completing-read' dispatch would.

Multi: source plist's `:directory' + `:resolve-paths'.
Single:  the minibuffer's `default-directory' (set by the constructor's
setup hook) + `fzfa--session-resolve-paths'."
  (let* ((clean (fzfa--tofu-hide cand))
         (src (and (bound-and-true-p fzfa--multi-active-sources)
                   (fzfa--multi-source-of
                    cand fzfa--multi-active-sources nil)))
         (dir (if src
                  (plist-get src :directory)
                default-directory))
         (resolve (if src
                      (plist-get src :resolve-paths)
                    fzfa--session-resolve-paths)))
    (fzfa--maybe-expand clean dir resolve)))

(defvar fzfa--preview-session)

(defun fzfa--promote-from-preview (cand buffer)
  "Drop BUFFER from CAND's source preview opener kill-list, if any.

Preview opens files via `fzfa--temporary-files', which tracks the
buffers it created and kills them on `:return'.  When an apply visit
reuses one of those buffers (via `find-buffer-visiting'), this call
removes it from the kill-list so it survives session cleanup.

Multi sessions store the per-source cells on the router handler
itself via `:multi-cells'; route through the cell matching CAND's
source idx.  Single sessions read the opener directly from the outer
session.  No-op for sessions without a preview opener (multi sources
without preview, buffer category, etc.)."
  (let* ((handler (car-safe fzfa--preview-session))
         (cell (when-let* ((cells (and handler
                                       (plist-get handler :multi-cells)))
                           (table (fzfa-preview-get :multi-cand->src))
                           (idx (fzfa--multi-source-idx cand table)))
                 (aref cells idx))))
    (if cell
        (let ((fzfa--preview-session cell))
          (when-let* ((opener (fzfa-preview-get :opener)))
            (funcall opener buffer)))
      (when-let* ((opener (fzfa-preview-get :opener)))
        (funcall opener buffer)))))

(defun fzfa--pin-window-buffer (window buffer)
  "Ensure WINDOW stays on BUFFER once the active minibuffer session exits.

Preview uses `display-buffer-same-window' to render candidates in
WINDOW; the minibuffer unwind path consults `quit-restore' and related
`display-buffer' bookkeeping to revert the window after exit.  Apply
\(`fzfa-apply-current') calls this helper so the user's deliberate
\\[fzfa-apply-current] visit survives that reversion.

Implementation: `minibuffer-exit-hook' fires BEFORE the unwind's
window restoration, so a synchronous `set-window-buffer' inside the
hook gets clobbered afterward.  Defer the re-assert via `run-at-time
0 nil' from within the exit hook so the set fires at the next event
loop tick, after the unwind has fully drained.

On commit, the calling command's post-`completing-read' body runs
before our deferred re-assert, but it places the candidate's buffer
in WINDOW anyway, so the re-assert is a harmless no-op when the two
agree.  On abort the body doesn't run and the re-assert sticks."
  (when-let* ((mb (active-minibuffer-window))
              ((window-live-p window))
              ((buffer-live-p buffer)))
    (with-current-buffer (window-buffer mb)
      (add-hook 'minibuffer-exit-hook
                (lambda ()
                  (run-at-time
                   0 nil
                   (lambda ()
                     (when (and (window-live-p window)
                                (buffer-live-p buffer))
                       (set-window-buffer window buffer)))))
                t t))))

(defun fzfa-apply-current ()
  "Invoke the current candidate's `:apply' function without exiting.

Looks up the apply via `fzfa--resolve-apply', resolves the candidate
to an absolute path (when the session was created with
`:resolve-paths' non-nil) via `fzfa--maybe-expand', then runs the
apply lambda inside the origin window so file/buffer visits land there
and the picker keeps focus.

Two post-apply concerns, each delegated to its own helper:
- `fzfa--promote-from-preview' saves a reused preview buffer from the
  session's ephemeral cleanup.
- `fzfa--pin-window-buffer' makes the visit survive the minibuffer
  unwind path's implicit window-state restoration.

Silently no-ops when no `:apply' is defined for the source/session."
  (interactive)
  (when-let* ((cand (fzfa--frontend-candidate))
              (apply (fzfa--resolve-apply cand))
              (resolved (fzfa--resolve-candidate cand))
              (origin (or (minibuffer-selected-window) (selected-window))))
    (condition-case err
        (with-selected-window origin
          (funcall apply resolved)
          (fzfa--promote-from-preview cand (current-buffer))
          (fzfa--pin-window-buffer origin (current-buffer))
          (run-hooks 'fzfa-after-apply-hook))
      (error (message "fzfa-apply: %s" (error-message-string err))))))

(defun fzfa--minibuffer-install-apply-key ()
  "Bind `fzfa-apply-key' to `fzfa-apply-current' in the active minibuffer.
Installed via a per-instance child of `current-local-map' so we don't
mutate the frontend's shared keymap.  No-op under `ivy-mode' (ivy uses
`fzfa-ivy.el's `:around' advice on `ivy-call' instead) or when
`fzfa-apply-key' is nil."
  (when (and fzfa-apply-key
             (not (bound-and-true-p ivy-mode)))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd fzfa-apply-key) #'fzfa-apply-current)
      (use-local-map map))))

;;; Live preview
;;
;; Categories declare per-action handlers in `fzfa-preview-functions'.
;; The lifecycle is:
;;
;;   :setup    ()             once at minibuffer entry
;;   :preview  (CAND)         each debounced candidate change; nil = reset
;;   :exit     ()             just before minibuffer closes (still live)
;;   :return   (CAND-OR-NIL)  after minibuffer closes; nil = aborted
;;
;; Handlers share state via `fzfa-preview-get' / `fzfa-preview-put',
;; backed by a dynamically-bound session cons whose cdr is a plist.
;; The session let-binding spans both `completing-read' and the
;; :return dispatch in `unwind-protect', so :return sees the same
;; state that :setup stashed even though the minibuffer is gone.
;; Only :preview is required.

(defcustom fzfa-preview-file-size-limit (* 1 1024 1024)
  "Maximum file size in bytes that `fzfa--file-preview' will open.
Files larger than this are skipped (no preview) to keep selection
movement snappy even when the cursor lands on multi-megabyte binaries.
Set to nil to remove the cap (not recommended for large repositories).
Set to 0 to disable file preview entirely without dropping the
`fzfa-file' handler from `fzfa-preview-functions'."
  :type '(choice (const :tag "No cap" nil)
                 (integer :tag "Bytes"))
  :group 'fzfa)

(defcustom fzfa-preview-functions
  '((fzfa-buffer   :preview fzfa--buffer-preview)
    (fzfa-file     :setup   fzfa--file-preview-setup
                   :preview fzfa--file-preview
                   :return  fzfa--file-preview-return)
    (fzfa-grep     :preview fzfa--grep-preview)
    (fzfa-location :preview fzfa--location-preview))
  "Per-category preview handlers.
Alist of (CATEGORY . PLIST), where PLIST recognizes :setup, :preview,
:exit, :return slots (see commentary in fzfa.el).  Only :preview is
required.  Categories without an entry get no preview.
Disable preview globally by setting `fzfa-preview-delay' to nil.

Built-in handlers are listed in the default; redefine the entire
alist to opt out of a category, or extend it (e.g. via `customize'
or `setq') to add categories of your own."
  :type '(alist :key-type symbol
                :value-type (plist :options ((:setup function)
                                             (:preview function)
                                             (:exit function)
                                             (:return function))))
  :group 'fzfa)

(defvar fzfa--preview-session nil
  "Active preview session: (HANDLER . STATE-PLIST).
`let'-bound by `fzfa-sync/async-completing-read' so the session is
visible from :setup, :preview, :exit, and :return.  Use
`fzfa-preview-get' / `fzfa-preview-put' to access the state plist.")

(defvar-local fzfa--preview-timer nil
  "Buffer-local debounce timer; lives in the minibuffer only.")
(defvar-local fzfa--preview-last 'unset
  "Last previewed candidate in this minibuffer (for change detection).")

(defun fzfa-preview-get (key &optional default)
  "Return KEY from the active preview session's state plist, or DEFAULT."
  (let ((cell (plist-member (cdr fzfa--preview-session) key)))
    (if cell (cadr cell) default)))

(defun fzfa-preview-put (key value)
  "Set KEY to VALUE in the active preview session's state plist."
  (setcdr fzfa--preview-session
          (plist-put (cdr fzfa--preview-session) key value)))

(defun fzfa--preview-handler (preview category)
  "Resolve the handler plist for this call, or nil if preview is disabled.
PREVIEW is the explicit `:preview' keyword value (nil means \"fall back
to the registry\"):
  nil        — look up CATEGORY in `fzfa-preview-functions'.
  a function — treat as a `:preview'-only plist (shorthand for the
               common ad-hoc case with no lifecycle).
  a plist    — use as-is.
Returns nil unconditionally when `fzfa-preview-delay' is nil (the global
escape hatch — users disable preview by setting the delay rather than by
wiring nil into individual calls)."
  (when fzfa-preview-delay
    (cond
     ((functionp preview) (list :preview preview))
     ((and (listp preview) preview) preview)
     (t (alist-get category fzfa-preview-functions)))))

(defun fzfa--preview-call (action &rest args)
  "Dispatch ACTION to the active session's handler with ARGS.
Selects the captured origin window, makes its buffer current, rebinds
`default-directory' to the value captured at install time, and traps
errors so a bad handler can't break completion.  No-op when ACTION has
no slot in the handler or when there is no active session."
  (when-let* ((handler (car fzfa--preview-session))
              (fn (plist-get handler action)))
    (let ((win (fzfa-preview-get :origin-window))
          (buf (fzfa-preview-get :origin-buffer))
          (dir (fzfa-preview-get :default-directory)))
      (condition-case err
          (if (and (window-live-p win) (buffer-live-p buf))
              (with-selected-window win
                (with-current-buffer buf
                  (let ((default-directory (or dir default-directory)))
                    (apply fn args))))
            (let ((default-directory (or dir default-directory)))
              (apply fn args)))
        (error
         (message "fzfa preview %s error: %s"
                  action (error-message-string err)))))))

(defun fzfa--preview-install (&optional delay)
  "Install live preview in the current minibuffer for the active session.
Call from inside a `minibuffer-with-setup-hook' lambda.  Reads the
handler from `fzfa--preview-session', captures origin window/buffer
and `default-directory' into the session state, dispatches :setup, and
registers the debounced `post-command-hook' + `minibuffer-exit-hook'.

DELAY defaults to `fzfa-preview-delay'.  When positive, scheduling
uses `run-with-idle-timer' so fast typing or arrow-key bursts suppress
intermediate previews until input settles.  A pending timer is reused
rather than reset; the callback re-reads the current candidate at fire
time so reuse never previews a stale selection.  DELAY of 0 previews
immediately on every selection change."
  (let* ((delay (or delay fzfa-preview-delay 0))
         (mb (current-buffer))
         (run (lambda ()
                (when-let* ((cand (fzfa--frontend-candidate)))
                  (unless (equal cand fzfa--preview-last)
                    (setq fzfa--preview-last cand)
                    (fzfa--preview-call :preview cand))))))
    (fzfa-preview-put :origin-window    (minibuffer-selected-window))
    (fzfa-preview-put :origin-buffer    (window-buffer
                                         (minibuffer-selected-window)))
    (fzfa-preview-put :default-directory default-directory)
    (setq fzfa--preview-last 'unset)
    (fzfa--preview-call :setup)
    (add-hook
     'post-command-hook
     (if (<= delay 0)
         run
       ;; The idle-timer callback fires with whatever buffer is current
       ;; at fire time, not necessarily this minibuffer.  All state we
       ;; touch (`fzfa--preview-timer', `fzfa--preview-last') is
       ;; buffer-local here, so route the callback through MB or we
       ;; silently corrupt the wrong buffer's locals and leave a stale
       ;; timer object behind that blocks every subsequent preview.
       (lambda ()
         (unless (timerp fzfa--preview-timer)
           (setq fzfa--preview-timer
                 (run-with-idle-timer
                  delay nil
                  (lambda ()
                    (when (buffer-live-p mb)
                      (with-current-buffer mb
                        (when (timerp fzfa--preview-timer)
                          (cancel-timer fzfa--preview-timer))
                        (setq fzfa--preview-timer nil)
                        (funcall run)))))))))
     nil t)
    (add-hook
     'minibuffer-exit-hook
     (lambda ()
       (when (timerp fzfa--preview-timer)
         (cancel-timer fzfa--preview-timer)
         (setq fzfa--preview-timer nil))
       (fzfa--preview-call :preview nil)
       (fzfa--preview-call :exit))
     nil t)))

(defun fzfa--preview-return (cand)
  "Dispatch :return on the active session with CAND (nil = aborted).
Called from the constructors after `completing-read' unwinds.  The
session `let'-binding still encloses this call, so handlers see their
stashed state and the captured `default-directory'."
  (fzfa--preview-call :return cand))

;;; Built-in preview handlers

(defmacro fzfa-with-quiet-find-file (&rest body)
  "Run BODY with file-loading prompts suppressed.

`find-file-noselect' can trigger minibuffer prompts via file-local
variables, `find-file-hook', or warnings — inside an active completion
those signal \"Command attempted to use minibuffer while in minibuffer\".
Custom `:preview' handlers that load files should wrap the call in this
macro."
  (declare (indent 0) (debug t))
  `(let ((enable-local-variables :safe)
         (enable-local-eval nil)
         (enable-dir-local-variables nil)
         (non-essential t)
         (inhibit-message t))
     ,@body))

(defun fzfa-preview-show (buffer &optional pos)
  "Show BUFFER (optionally moved to POS) in the originating window.
Does not steal the minibuffer's input focus.  POS may be a buffer
position number or a marker; when nil, point is left where it was.
Public helper for `:preview' handlers to call.

The originating window — the one selected just before the minibuffer
opened — is the same window the post-selection action (e.g. `find-file')
will land in once the minibuffer exits, so preview and selection share
one slot by construction.  `fzfa--preview-call' already selects it
before invoking handlers; this function just uses `display-buffer-same-window'
so `display-buffer' honors that selection instead of routing to an LRU
non-selected window.

To customize placement (side window, popup frame, fresh split), configure
`display-buffer-alist' at the Emacs level — that applies uniformly to both
the preview here and the eventual selection action, avoiding the half-broken
\"preview lands here, selection lands there\" split."
  (when (buffer-live-p buffer)
    (when pos
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (goto-char (if (markerp pos) (marker-position pos) pos)))))
    (let ((win (display-buffer buffer '(display-buffer-same-window))))
      (when (window-live-p win)
        (with-selected-window win
          (run-hooks 'fzfa-after-preview-hook)))
      win)))

(defun fzfa--grep-preview (cand)
  "Open the FILE from a FILE:LINE:CONTENT grep CAND at LINE for preview.
Resolves FILE against the captured `default-directory' (the search root
when invoked from `fzfa-async-completing-read')."
  (when (and cand
             (string-match "\\`\\(.+?\\):\\([0-9]+\\):" cand))
    (let* ((file (match-string 1 cand))
           (line (string-to-number (match-string 2 cand)))
           (path (expand-file-name file)))
      (when (file-readable-p path)
        (let ((buf (fzfa-with-quiet-find-file
                     (find-file-noselect path 'nowarn))))
          (with-current-buffer buf
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-line (1- line))))
          (fzfa-preview-show buf))))))

(defun fzfa--location-preview (cand)
  "Preview SOURCE at LINE for an `fzfa-location' CAND.
Reads `(SOURCE . LINE)' off CAND's `fzfa-location' text property.
SOURCE is resolved as a file path when `file-readable-p', otherwise as
a live buffer name.  Computes the line's start position in the source
buffer and hands off to `fzfa-preview-show'.  No-op when the property
is missing or the target cannot be resolved."
  (when-let* ((loc (and (stringp cand) (> (length cand) 0)
                        (get-text-property 0 'fzfa-location cand)))
              (source (car loc))
              (line   (cdr loc))
              (buf    (cond
                       ((and (stringp source) (file-readable-p source))
                        (fzfa-with-quiet-find-file
                          (find-file-noselect source 'nowarn)))
                       ((get-buffer source)))))
    (let ((pos (with-current-buffer buf
                 (save-restriction
                   (widen)
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (point))))))
      (fzfa-preview-show buf pos))))

(defun fzfa--buffer-preview (cand)
  "Show CAND (a buffer name) in a side window for preview."
  (when-let* ((buf (and cand (get-buffer cand))))
    (fzfa-preview-show buf)))

(defun fzfa--temporary-files ()
  "Return an opener closure that owns ephemeral preview buffers.

The closure has three call forms:

  (FN PATH)  → return a buffer for PATH.  Uses an existing
               file-visiting buffer when one is already loaded;
               otherwise creates one and remembers it as ephemeral.
  (FN BUF)   → promote BUF: it is no longer considered ephemeral
               and will not be killed on cleanup.
  (FN)       → kill every still-ephemeral buffer.  Idempotent.

Intended pattern: stash the opener via `fzfa-preview-put :opener'
during `:setup', call it with paths during `:preview', and on
`:return' promote the chosen file's buffer (if any) then call with
no args to reap the rest."
  (let (ephemerals)
    (lambda (&optional arg)
      (cond
       ((null arg)
        (dolist (b ephemerals)
          (when (buffer-live-p b) (kill-buffer b)))
        (setq ephemerals nil))
       ((bufferp arg)
        (setq ephemerals (delq arg ephemerals)))
       ((stringp arg)
        ;; `find-buffer-visiting' matches by truename, so it handles
        ;; Windows DOS short paths and Unix symlinks uniformly —
        ;; unlike `get-file-buffer', which is a literal string match.
        (let ((path (expand-file-name arg)))
          (or (find-buffer-visiting path)
              (let ((buf (fzfa-with-quiet-find-file
                           (find-file-noselect path 'nowarn))))
                (push buf ephemerals)
                buf))))))))

(defun fzfa--file-preview-setup ()
  "Initialize a fresh `fzfa--temporary-files' opener for this session."
  (fzfa-preview-put :opener (fzfa--temporary-files)))

(defun fzfa--file-preview (cand)
  "Open CAND (a file path) for preview, gated by size + readability."
  (when (and cand fzfa-preview-file-size-limit
             (> fzfa-preview-file-size-limit 0))
    (let ((path (expand-file-name cand)))
      (when-let* (((file-readable-p path))
                  ((not (file-directory-p path)))
                  (attrs (file-attributes path))
                  (size (file-attribute-size attrs))
                  ((< size fzfa-preview-file-size-limit))
                  (opener (fzfa-preview-get :opener))
                  (buf (funcall opener path)))
        (fzfa-preview-show buf)))))

(defun fzfa--file-preview-return (cand)
  "Promote CAND's buffer (if accepted) and kill the remaining ephemerals.
The promoted buffer survives so the caller's subsequent `find-file'
reuses it instead of re-loading from disk."
  (when-let* ((opener (fzfa-preview-get :opener)))
    (when cand
      (when-let* ((buf (find-buffer-visiting (expand-file-name cand))))
        (funcall opener buf)))
    (funcall opener)))

;;; Completing-read helpers

(defun fzfa--minibuffer-format-reset ()
  "Disable frontend count formats in the active minibuffer.

Called from a `minibuffer-with-setup-hook' lambda so that `vertico''s
`vertico-count-format' and icomplete's `icomplete-matches-format' don't
overwrite fzfa's own stats overlay / pre-prompt text.  Ivy is handled
separately via `ivy-count-format' bound at the call site.  No-ops when
the target package isn't loaded.

Under icomplete, also pin `resize-mini-windows' to `grow-only'.  The
auto-resize logic that fires on every redisplay treats the empty-input
state as needing only 1 line — ignoring `after-string' on the
zero-length `icomplete-overlay' — so a timer-driven refresh would grow
the mini-window only for it to collapse on the next redisplay tick."
  (when (boundp 'vertico-count-format)
    (setq-local vertico-count-format nil))
  (when (boundp 'icomplete-matches-format)
    (setq-local icomplete-matches-format nil))
  (fzfa--minibuffer-install-apply-key)
  (when (bound-and-true-p icomplete-mode)
    ;; Empty-input state hits the zero-length-overlay resize blind spot:
    ;; the overlay's multi-line `after-string' isn't counted, so any
    ;; redisplay collapses the pane to 1 line.  Toggle the resize policy
    ;; based on input state — `grow-only' (plus an explicit fit) when
    ;; empty so the pane stays visible, user's original value otherwise
    ;; so narrowing can shrink naturally.  Covers initial entry and
    ;; backspace-to-empty alike.
    (let ((orig resize-mini-windows))
      (setq-local resize-mini-windows 'grow-only)
      (add-hook 'after-change-functions
                (lambda (&rest _)
                  (if (= (minibuffer-prompt-end) (point-max))
                      (progn
                        (setq-local resize-mini-windows 'grow-only)
                        ;; `after-change-functions' fires during the
                        ;; edit, BEFORE icomplete's post-command-hook
                        ;; rebuilds the overlay.  Fitting now would
                        ;; read stale content.  Defer so the fit sees
                        ;; the freshly-rendered candidate list.
                        (run-at-time
                         0 nil #'fzfa--icomplete-fit-mini-window))
                    (setq-local resize-mini-windows orig)))
                nil t))
    ;; One-shot fit after icomplete's initial `post-command-hook'-driven
    ;; exhibit populates the overlay.
    (run-at-time 0 nil #'fzfa--icomplete-fit-mini-window)))

(defun fzfa--commas (n)
  "Format integer N with comma thousand-separators.

e.g., 1234567 → 1,234,567."
  (let ((s (number-to-string n))
        (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s   (substring s 0 -3)))
    (concat s out)))

(defun fzfa--format-stats (prefix idx filtered total)
  "Format the fzfa stats text \"PREFIX[N/][FILTERED](TOTAL) \".
PREFIX is the leading text — e.g. \"PROMPT DIR \", \"DIR \", or just
\"PROMPT\" — and is emitted verbatim.  IDX is the 0-based selection
index; when non-nil it's rendered as \"N/\", when nil that segment is
omitted (frontends like icomplete don't expose a selection index).
FILTERED and TOTAL are integer candidate counts, comma-formatted."
  (format "%s%s[%s](%s) "
          prefix
          (if idx (format "%d/" (1+ idx)) "")
          (fzfa--commas filtered)
          (fzfa--commas total)))

(defun fzfa--current-query (str)
  "Return the live query for a `completing-read' collection lambda.

STR is the string the collection function was called with.  Under
`ivy-mode' STR is reliably `ivy-text' (the user's input), so trust
it verbatim — ivy renders candidates as inserted minibuffer text
below the prompt, and falling back to `minibuffer-contents' would
pull in the rendered candidate display as a giant garbage query.

Other frontends (vertico, icomplete) sometimes pass an empty STR
even when the minibuffer holds a real query; they render candidates
via display overlays so `minibuffer-contents' stays clean.  Fall
back to that there.

Returns the empty string otherwise."
  (or (if (or (not (string-empty-p str))
              (bound-and-true-p ivy-mode))
          str
        (when-let* ((win (active-minibuffer-window)))
          (with-current-buffer (window-buffer win)
            (minibuffer-contents-no-properties))))
      ""))

(defun fzfa--candidate-limit ()
  "Return `fzfa-max-candidates' when it is a positive integer, else nil.
Nil disables the cap on the C side."
  (and fzfa-max-candidates
       (> fzfa-max-candidates 0)
       fzfa-max-candidates))

(defun fzfa--async-final-p (r handle query)
  "Return non-nil when R is the final answer for QUERY on HANDLE.
R is the value returned by `fzf-native-async-candidates' for QUERY.
The result is final in either of two cases:
  - R is non-nil (candidates to display); or
  - R is nil AND `fzf-native-async-result-fresh-p' reports the cache
    fresh for QUERY — scoring has completed at the current pool size,
    so zero matches is the authoritative answer.
A nil return means scoring for QUERY is still in flight: an empty R
should be treated as \"no information yet\", and callers should
preserve the previous display rather than blanking it.

Use this guard around any `setq' that updates a stored result, and
around any side-effect that commits R to the display.

When the loaded fzf-native predates `fzf-native-async-result-fresh-p',
fall back to treating any nil R as in-flight.  That loses the
authoritative-zero distinction (consult-style display will keep showing
stale candidates when scoring legitimately matched nothing), but it
keeps fzfa functional on older fzf-native builds."
  (or r (and (fboundp 'fzf-native-async-result-fresh-p)
             (fzf-native-async-result-fresh-p handle query))))

(defun fzfa--defer-async-stop (handles)
  "Schedule `fzf-native-async-stop' on HANDLES off the synchronous unwind path.

HANDLES may be a single async handle, a list, or a vector; nil values
\(including nil HANDLES) are ignored.  Stops are chained one-per-idle-tick
so redisplay can run between them; the closure retains the live-handle
list so it survives the caller's unwind.

The C-side destroy does pthread_join on the scoring thread
\(uninterruptible snapshot/score work for huge pools) and frees the
candidate arena — easily hundreds of ms for a `find ~'-scale session.
None of it is needed before minibuffer dismissal, so deferring this
lets ESC return instantly.  An idle timer (rather than `run-at-time' 0)
ensures the join is wedged between user keystrokes only when the user
has actually paused — keeping the pthread_join out of the user's typing
rhythm in trade for holding the arena slightly longer.

For multi sessions with N async sources, joining all N in one idle tick
freezes Emacs for the sum of their join times.  Popping one handle per
tick yields the main thread between joins so the user sees a responsive
UI even when several large pools are tearing down."
  (let ((live (cond
               ((null handles) nil)
               ((vectorp handles)
                (cl-loop for h across handles when h collect h))
               ((listp handles) (delq nil (copy-sequence handles)))
               (t (list handles)))))
    (when live
      (letrec ((step (lambda ()
                       (when live
                         (fzf-native-async-stop (pop live))
                         (when live
                           (run-with-idle-timer 0 nil step))))))
        (run-with-idle-timer 0 nil step)))))

;;; History

(defvar fzfa--hist-hash nil
  "Cached string→position hash for the active minibuffer history.
Lower positions are more recent.  Built lazily by `fzfa--history-hash'.")

(defvar fzfa--hist-hash-last-val nil
  "List `fzfa--hist-hash' was built from; `eq'-compared per call.
Any update to the history list (entries cons onto the head) breaks
identity and triggers a rebuild.")

(defun fzfa--history-hash ()
  "Return a cached string→position hash for the active minibuffer history.

Reads `minibuffer-history-variable', which `completing-read' dynamically
binds to the HIST argument supplied by the caller.  Returns nil when no
real history variable is in effect.  Lower positions are more recent.

Reuses `fzfa--hist-hash' as long as the underlying list is `eq' to the
value the hash was built from; new entries cons onto the head, so any
update invalidates the cache."
  (let* ((sym (and (not (eq minibuffer-history-variable t))
                   minibuffer-history-variable))
         (hist (and sym (symbol-value sym))))
    (cond
     ((eq hist fzfa--hist-hash-last-val) fzfa--hist-hash)
     (t
      (setq fzfa--hist-hash-last-val hist)
      (setq fzfa--hist-hash
            (when hist
              (let ((table (make-hash-table :test 'equal :size (length hist))))
                (cl-loop for index from 0
                         for item in hist
                         unless (gethash item table)
                         do (puthash item index table))
                table)))))))

(defun fzfa--sort-by-history (completions)
  "Order COMPLETIONS by recency in the active minibuffer history.

Only reorders when the fzf query is empty: with no query the candidate
list comes back in its source order (typically alphabetical), so we
promote recently used entries to the top.  When the query is non-empty
COMPLETIONS arrive in fzf-native's scored order and are returned
unchanged.  `sort' is stable, so entries absent from history keep their
incoming relative order."
  (let ((query (fzfa--current-query "")))
    (if (not (string-empty-p query))
        completions
      (if-let* ((hist (fzfa--history-hash)))
          (mapcar
           #'car
           (sort
            (mapcar
             (lambda (c)
               (cons c (or (gethash c hist) most-positive-fixnum)))
             completions)
            (lambda (a b) (< (cdr a) (cdr b)))))
        completions))))

(defun fzfa--history-rank (candidates hist-sym)
  "Return CANDIDATES reordered by recency in HIST-SYM, tofu-stripped.

HIST-SYM is a history variable symbol (or nil).  Each candidate is
looked up via `fzfa--tofu-hide' so multi-source entries carrying an
invisible PUA suffix still match the bare strings stored in history.
Candidates absent from HIST-SYM keep their incoming relative order — a
stable `sort' preserves source order for ties at `most-positive-fixnum'.

Returns CANDIDATES unchanged when HIST-SYM is nil, unbound, or empty.
This helper is the multi-source analogue of `fzfa--sort-by-history',
where the active `minibuffer-history-variable' isn't meaningful because
multi's outer `completing-read' is intentionally called with HIST nil."
  (let ((hist (and hist-sym (boundp hist-sym) (symbol-value hist-sym))))
    (if (not hist)
        candidates
      (let ((table (make-hash-table :test 'equal :size (length hist))))
        (cl-loop for index from 0
                 for item in hist
                 unless (gethash item table)
                 do (puthash item index table))
        (mapcar
         #'car
         (sort
          (mapcar
           (lambda (c)
             (cons c (or (gethash (fzfa--tofu-hide c) table)
                         most-positive-fixnum)))
           candidates)
          (lambda (a b) (< (cdr a) (cdr b)))))))))

(cl-defun fzfa--completion-metadata (category &key annotate affix group)
  "Return the `metadata' alist for fzfa's `completing-read' collection lambdas.

CATEGORY is the completion category symbol.  Optional ANNOTATE / AFFIX /
GROUP attach `annotation-function', `affixation-function', and
`group-function' when non-nil.  `display-sort-function' and
`cycle-sort-function' route through `fzfa--sort-by-history' so the empty
query surfaces recent picks first, while scored output produced by the C
scorer is preserved verbatim."
  `(metadata
    (category . ,category)
    (display-sort-function . fzfa--sort-by-history)
    (cycle-sort-function . fzfa--sort-by-history)
    ,@(when annotate `((annotation-function . ,annotate)))
    ,@(when affix    `((affixation-function . ,affix)))
    ,@(when group    `((group-function      . ,group)))))

(defun fzfa--maybe-expand (result directory resolve-paths)
  "Return RESULT expanded against DIRECTORY when RESOLVE-PATHS is non-nil.

For RESOLVE-PATHS=t the whole RESULT is passed through `expand-file-name'
— this works for both plain paths and FILE:LINE:CONTENT grep candidates,
since `expand-file-name' prepends DIRECTORY and leaves the suffix
untouched.  Returns RESULT unchanged for non-strings, empty strings, or
when RESOLVE-PATHS is nil."
  (if (and resolve-paths (stringp result) (not (string-empty-p result)))
      (expand-file-name result directory)
    result))

(defun fzfa--default-dir ()
  "Return the working directory for fzfa commands.
Priority: `fzfa-directory' >
          `fzfa-project-backend' >
          `default-directory'."
  (or fzfa-directory
      (pcase fzfa-project-backend
        ((pred functionp)
         (funcall fzfa-project-backend))
        ('project
         (when-let* ((pr (project-current)))
           (project-root pr)))
        ('projectile
         (when (bound-and-true-p projectile-mode)
           (projectile-project-root))))
      default-directory))

;;; Async `completing-read'

(defun fzfa--async-separator-heal-hook (overlay is-after _beg _end
                                                &optional _prelength)
  "Re-insert OVERLAY's separator char if the overlay's range collapsed.

Used as a `modification-hooks' entry on the protective overlays placed
over the `#…#' splitter separators.  Fires for both content changes and
text-property changes; we self-heal only when the overlay's range
collapsed to zero, which is the signature of a deletion of the covered
character.  Face additions (e.g. vertico's `add-face-text-property')
leave the range intact, so they pass through unmodified.

`inhibit-modification-hooks' is bound during the re-insertion so it
doesn't recurse through this hook."
  (when (and is-after
             (overlay-buffer overlay)
             (= (overlay-start overlay) (overlay-end overlay)))
    (let ((inhibit-modification-hooks t)
          (p (overlay-start overlay))
          (sep (overlay-get overlay 'fzfa-async-separator-char)))
      (when (and p sep)
        (save-excursion
          (goto-char p)
          (insert (char-to-string sep)))
        (move-overlay overlay p (1+ p))))))

(defun fzfa--async-protect-separator (pos sep-char)
  "Place a self-healing protective overlay at POS covering SEP-CHAR.

Returns the overlay.  The overlay tracks its single character via
`front-advance' t / `rear-advance' nil so insertions adjacent to the
separator don't extend its range; if the covered char is deleted by
any path, `fzfa--async-separator-heal-hook' restores it."
  (let ((ov (make-overlay pos (1+ pos) nil t nil)))
    (overlay-put ov 'fzfa-async-separator-char sep-char)
    (overlay-put ov 'cursor-intangible t)
    (overlay-put ov 'modification-hooks
                 (list #'fzfa--async-separator-heal-hook))
    ov))

;;;###autoload
(cl-defun fzfa-async-completing-read (&key
                                      prompt
                                      command
                                      (directory (fzfa--default-dir))
                                      (category 'fzfa-file)
                                      group
                                      initial-input
                                      (resolve-paths t)
                                      (display 'hidden)
                                      (split-style nil)
                                      skip-executable-check
                                      preview
                                      apply)
  "Run shell COMMAND with asynchronous `completing-read'.

The minibuffer input is split into a shell-CMD part and an
fzf-FILTER part via `fzfa-async-split-style' (or :SPLIT-STYLE).
With the default `perl' style the buffer text has shape
\"#CMD#FILTER\".  Changing CMD restarts the subprocess; changing
FILTER rescores in place via fzf-native.

:DISPLAY controls how much of the CMD region is visible.  Press
`fzfa-async-display-key' (default \">\") to cycle:
  hidden  — entire `#CMD#' prefix is invisible; only FILTER appears
            editable.  This is the default for legacy async callers.
  compact — flags within CMD collapse behind ` ... '; program name
            and the trailing argument slot remain visible.
  full    — whole `#CMD#FILTER' string is shown verbatim.

:PROMPT                 Minibuffer prompt.  Derived from the first token of
                        COMMAND (e.g. \"find: \" for \"find .\") when omitted.
:COMMAND                Shell command whose stdout lines become candidates.
                        Pre-inserted into the minibuffer as `#COMMAND#'.
:DIRECTORY              Working directory for COMMAND.  Defaults to
                        `fzfa--default-dir' (respects
                        `fzfa-project-backend').
:CATEGORY               Completion category symbol.  Defaults to
                        `fzfa-file' (most async commands return file
                        paths).  Pass `fzfa-grep' for FILE:LINE:CONTENT
                        candidates, `fzfa-misc' for non-file output, etc.
:GROUP                  Optional grouping function for completion metadata.
:INITIAL-INPUT          Optional initial minibuffer text overriding the
                        auto-built `#COMMAND#'.  Either a string or
                        (TEXT . POSITION) cons with 0-based cursor offset.
:RESOLVE-PATHS          When non-nil (the default), the returned
                        candidate is passed through `expand-file-name'
                        against :DIRECTORY before being handed back to the
                        caller.  Lets file and grep commands stay agnostic
                        of the caller's `default-directory'.  Pass nil for
                        commands that return non-path output (e.g. shell
                        output where the raw text matters).
:DISPLAY                Initial display mode (`hidden', `compact', or
                        `full'); see above.
:SPLIT-STYLE            Override `fzfa-async-split-style' for this call.
:SKIP-EXECUTABLE-CHECK  When non-nil, skip the `executable-find' guard on
                        the first token of COMMAND.
:PREVIEW                Per-call live-preview handler that bypasses the
                        `fzfa-preview-functions' registry lookup for
                        CATEGORY.  See `fzfa-sync-completing-read'.
:APPLY                  Lambda (CAND) -> any. Action to run without existing
                        `completing-read' session.
                        Can be invoked from `vertico' / `icomplete' / etc by:
                          `fzfa-apply-key'
                        Or from `ivy' by:
                          `ivy-call'
                        Or from `helm' by:
                          `helm-execute-persistent-action'
                        When omitted, falls back to the category
                          default in `fzfa-apply-functions'.

The prompt overlay shows: DIR IDX/[FILTERED](TOTAL)
  DIR      — abbreviated working directory
  IDX      — current selection index (omitted for frontends without one)
  FILTERED — candidates matching the current query
  TOTAL    — total candidates collected so far"
  (fzfa--ensure-setup)
  (unless skip-executable-check
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (let ((prompt (or prompt
                    (when command
                      (concat (car (split-string command nil t)) ": ")))))
    (cond
     ((eq fzfa--multi-mode :extract)
      (throw 'fzfa-extracted
             (list :prompt prompt :command command
                   :directory directory :category category :group group
                   :initial-input initial-input
                   :resolve-paths resolve-paths
                   :display display :split-style split-style
                   :preview preview)))
     ((eq (car-safe fzfa--multi-mode) :inject)
      ;; One-shot consume: mutate the outer action's `let' cell so the
      ;; rest of the caller's body (and any nested fzfa calls) run with
      ;; multi-mode = nil instead of replaying our inject value.
      (let ((cand (cdr fzfa--multi-mode)))
        (setq fzfa--multi-mode nil)
        (cl-return-from fzfa-async-completing-read
          (fzfa--maybe-expand cand directory resolve-paths)))))
    (when (and (bound-and-true-p helm-mode) fzfa-async-helm-handler)
      (cl-return-from fzfa-async-completing-read
        (fzfa--maybe-expand
         (funcall fzfa-async-helm-handler
                  :prompt prompt :command command :directory directory
                  :skip-executable-check skip-executable-check
                  :category category :preview preview :apply apply)
         directory resolve-paths)))
    (let* ((completion-styles '(fzfa))
           (dir (expand-file-name directory))
           (dir-abbrev (abbreviate-file-name directory))
           (handler (fzfa--preview-handler preview category))
           (fzfa--preview-session (and handler (list handler)))
           (fzfa--session-apply
            (or apply (plist-get (alist-get category fzfa-apply-functions) :apply)))
           (fzfa--session-resolve-paths resolve-paths)
           (style-sym (or split-style fzfa-async-split-style 'perl))
           (style (or (alist-get style-sym fzfa-async-split-styles-alist)
                      (user-error
                       "Unknown fzfa-async split style: %s" style-sym)))
           (initial-char (plist-get style :initial))
           ;; Build initial input.  Hidden mode keeps `#CMD#' OUT of the
           ;; minibuffer entirely — CMD lives in the `command' closure
           ;; variable (mutated in place by the display-cycle when the
           ;; user edits inside `#CMD#'), and the editable region is
           ;; just FILTER.  Compact / full pre-seed `#CMD#' as before
           ;; so the user can edit it.
           (init-text
            (cond
             ((consp initial-input) (car initial-input))
             ((stringp initial-input) initial-input)
             ((and command initial-char (memq display '(compact full)))
              (concat (char-to-string initial-char) command
                      (char-to-string initial-char)))
             (t nil)))
           ;; Place point at the end of the seeded text (start of the
           ;; FILTER region in compact/full; same physical position as
           ;; start of an empty editable region in hidden).
           (init-point
            (cond
             ((consp initial-input) (cdr initial-input))
             (init-text (length init-text))
             (t nil)))
           (splitter (plist-get style :function))
           (limit (fzfa--candidate-limit))
           (selection nil)
           (handle nil)
           (current-cmd nil)
           (last-gen -1)
           (last-result nil)
           (last-filtered 0)
           (last-total 0)
           (last-exhibit-scheduled 0.0)
           (last-restart-time 0.0)
           (stats-overlay nil)
           (separator-overlays nil)
           (display-overlays nil)
           (display-state display)
           (display-clear
            (lambda ()
              (mapc #'delete-overlay display-overlays)
              (setq display-overlays nil)))
           (display-apply
            (lambda ()
              ;; Only compact mode installs an overlay (flags collapse).
              ;; Hidden and full are pure buffer states — hidden has no
              ;; `#CMD#' to hide; full shows it verbatim.
              (funcall display-clear)
              (when (eq display-state 'compact)
                (when-let* ((bounds (fzfa--async-display-cmd-bounds
                                     initial-char)))
                  (setq display-overlays
                        (fzfa--async-display-make-overlays
                         (car bounds) (cdr bounds)))))))
           (display-cycle
            (lambda ()
              (interactive)
              (let* ((from display-state)
                     (to (fzfa--async-display-next-state from)))
                ;; Transitions across the hidden boundary mutate the
                ;; buffer via the shared materialize / extract helpers
                ;; (see `fzfa--async-display-materialize' and
                ;; `fzfa--async-display-extract').  Both helpers are
                ;; frontend-agnostic — they operate on `current-buffer'
                ;; using `minibuffer-prompt-end' as the start anchor, so
                ;; the same logic powers the helm `>' key when that
                ;; lands.  See `fzfa--async-display-next-state' for the
                ;; cycle order.
                (cond
                 ((and (eq from 'hidden) (eq to 'compact))
                  (setq separator-overlays
                        (fzfa--async-display-materialize
                         command initial-char)))
                 ((eq to 'hidden)
                  (setq command (fzfa--async-display-extract
                                 splitter style separator-overlays))
                  (setq separator-overlays nil)))
                (setq display-state to)
                (funcall display-apply))))
           restart-timer retry-timer poll-timer ivy-push
           (refresh-overlay
            (lambda ()
              (when (and stats-overlay (active-minibuffer-window))
                (with-selected-window (active-minibuffer-window)
                  (overlay-put
                   stats-overlay 'display
                   (fzfa--format-stats (concat prompt dir-abbrev " ")
                                       (fzfa--frontend-index)
                                       last-filtered last-total))))
              (fzfa--insert-prompt-if-ivy)))
           (do-restart
            (lambda (cmd)
              (when handle
                (fzfa--defer-async-stop handle)
                (setq handle nil))
              ;; Keep `last-result' across restarts so the display stays
              ;; populated with stale candidates until new ones stream in.
              (setq current-cmd cmd
                    last-gen -1
                    last-filtered 0
                    last-total 0
                    last-restart-time (float-time))
              (when (and cmd (not (string-empty-p cmd)))
                (setq handle (fzf-native-async-start cmd dir)))
              (fzfa--frontend-push ivy-push)))
           (table
            (lambda (str _pred action)
              (pcase action
                ('metadata (fzfa--completion-metadata category :group group))
                (`(boundaries . ,_) (cons 0 0))
                ('t
                 (let* ((input (fzfa--current-query str))
                        (split (fzfa--async-split
                                input display-state command splitter style))
                        (cmd (car split))
                        (query (cdr split)))
                   (cond
                    ((not (equal cmd current-cmd))
                     ;; Cancel any pending restart and reschedule.  Each
                     ;; keystroke debounces; the throttle term is the
                     ;; floor on the gap between actual spawns when the
                     ;; user keeps typing past one debounce window.
                     (when restart-timer
                       (cancel-timer restart-timer)
                       (setq restart-timer nil))
                     (let* ((elapsed (- (float-time) last-restart-time))
                            (delay (max fzfa-shell-command-debounce
                                        (- fzfa-shell-command-throttle elapsed))))
                       (setq restart-timer
                             (run-with-timer
                              (max 0.01 delay) nil
                              (lambda ()
                                (setq restart-timer nil)
                                (funcall do-restart cmd)))))
                     ;; Fetch from the *current* handle (previous cmd's
                     ;; process) so the display reflects something while the
                     ;; user is mid-typing in the cmd portion.
                     (let ((r (and handle
                                   (fzf-native-async-candidates
                                    handle query limit))))
                       (when (and handle (fzfa--async-final-p r handle query))
                         (setq last-result r))
                       last-result))
                    ((null handle) last-result)
                    (t
                     (let ((r (while-no-input
                                (fzf-native-async-candidates
                                 handle query limit))))
                       (cond
                        ((eq r t)
                         (when retry-timer (cancel-timer retry-timer))
                         (setq retry-timer
                               (run-with-idle-timer
                                fzfa-input-debounce nil
                                (lambda ()
                                  (setq retry-timer nil)
                                  (fzfa--frontend-push ivy-push))))
                         last-result)
                        (t
                         (when-let* ((stats (fzf-native-async-stats handle)))
                           (setq last-filtered (car stats)
                                 last-total    (cdr stats)))
                         (when-let* ((win (active-minibuffer-window)))
                           (with-selected-window win
                             (unless stats-overlay
                               (setq stats-overlay
                                     (make-overlay (point-min)
                                                   (minibuffer-prompt-end))))
                             (funcall refresh-overlay)))
                         (when (fzfa--async-final-p r handle query)
                           (setq last-result r))
                         last-result)))))))
                (_ t)))))
      ;; Install `ivy-push' into the placeholder declared in the let*
      ;; above.  Deferred so its lambda can close over `do-restart' (which
      ;; references back into `ivy-push'), and so `setq' mutates the
      ;; placeholder's cell rather than shadowing it.
      (setq ivy-push
            (lambda ()
              (when-let* ((win   (active-minibuffer-window))
                          ((or (not (bound-and-true-p ivy-last))
                               (ivy-state-dynamic-collection ivy-last)))
                          (query (and (boundp 'ivy-text) ivy-text)))
                (with-selected-window win
                  (let* ((split (fzfa--async-split
                                 query display-state command splitter style))
                         (cmd    (car split))
                         (filter (cdr split)))
                    (cond
                     ((not (equal cmd current-cmd))
                      (when restart-timer
                        (cancel-timer restart-timer)
                        (setq restart-timer nil))
                      (let* ((elapsed (- (float-time) last-restart-time))
                             (delay (max fzfa-shell-command-debounce
                                         (- fzfa-shell-command-throttle
                                            elapsed))))
                        (setq restart-timer
                              (run-with-timer
                               (max 0.01 delay) nil
                               (lambda ()
                                 (setq restart-timer nil)
                                 (funcall do-restart cmd)))))
                      (let ((r (and handle
                                    (fzf-native-async-candidates
                                     handle filter limit))))
                        (when (and handle
                                   (fzfa--async-final-p r handle filter))
                          (setq last-result r))))
                     ((null handle) nil)
                     (t
                      (let ((r (while-no-input
                                 (fzf-native-async-candidates
                                  handle filter limit))))
                        (cond
                         ((eq r t) nil)
                         (t
                          (when-let* ((stats (fzf-native-async-stats handle)))
                            (setq last-filtered (car stats)
                                  last-total    (cdr stats)))
                          (when (fzfa--async-final-p r handle filter)
                            (setq last-result r)))))))
                    (when last-result
                      (ivy--set-candidates last-result)
                      (ivy--exhibit)
                      (ivy--insert-prompt)))))))
      ;; Pre-arm the subprocess so candidates start streaming before the
      ;; minibuffer is even shown.  Prefer :COMMAND; fall back to parsing
      ;; init-text (covers callers that pass :INITIAL-INPUT without :COMMAND).
      (cond
       ((and command (not (string-empty-p command)))
        (funcall do-restart command))
       (init-text
        (let ((cmd (car (funcall splitter init-text style))))
          (when (and cmd (not (string-empty-p cmd)))
            (funcall do-restart cmd)))))
      (setq poll-timer
            (run-with-timer
             0 fzfa-refresh-delay
             (lambda ()
               (when handle
                 (let ((gen (fzf-native-async-generation handle)))
                   (when (and gen (not (= gen last-gen)) (not (input-pending-p))
                              (>= (- (float-time) last-exhibit-scheduled)
                                  fzfa-input-throttle))
                     (setq last-gen gen
                           last-exhibit-scheduled (float-time))
                     (run-with-idle-timer
                      0 nil #'fzfa--frontend-push ivy-push)))))))
      (add-hook 'post-command-hook refresh-overlay)
      (sit-for fzfa-refresh-delay)
      (fzfa--maybe-expand
       (unwind-protect
           (minibuffer-with-setup-hook
               (lambda ()
                 ;; Bind the minibuffer's default-directory so that callers
                 ;; running outside fzfa (notably embark, which captures
                 ;; default-directory and rebinds it around the action) resolve
                 ;; relative candidates against the working directory the
                 ;; command actually ran in.
                 (setq-local default-directory directory)
                 ;; Legacy auto-insert of a single opening separator when
                 ;; the caller passed no init-text and no command but the
                 ;; session started in compact/full so they intend to
                 ;; type `#CMD#FILTER' freestyle.  Skip in hidden mode
                 ;; (no `#…#' belongs in the editable region).
                 (when (and initial-char (null init-text)
                            (not (eq display-state 'hidden)))
                   (save-excursion
                     (goto-char (minibuffer-prompt-end))
                     (unless (equal initial-char (char-after))
                       (insert (char-to-string initial-char)))))
                 (fzfa--minibuffer-format-reset)
                 (when handler (fzfa--preview-install))
                 (cursor-intangible-mode 1)
                 (when fzfa-async-display-key
                   (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map (current-local-map))
                     (define-key map (kbd fzfa-async-display-key)
                                 display-cycle)
                     (use-local-map map)))
                 (funcall display-apply)
                 ;; Install protective separator overlays only when the
                 ;; session actually has `#…#' in the buffer (compact /
                 ;; full).  Hidden mode keeps CMD in the closure and the
                 ;; editable region is plain FILTER, so there's nothing
                 ;; to protect.
                 (when (and command initial-char (null initial-input)
                            (memq display-state '(compact full)))
                   (let* ((mbe (minibuffer-prompt-end))
                          (close-pos (+ mbe 1 (length command))))
                     (setq separator-overlays
                           (list (fzfa--async-protect-separator
                                  mbe initial-char)
                                 (fzfa--async-protect-separator
                                  close-pos initial-char)))))
                 ;; Place point synchronously at the seeded text's end.
                 (when init-point
                   (goto-char (+ (minibuffer-prompt-end) init-point))))
             (let ((ivy-completing-read-dynamic-collection t)
                   (ivy-count-format
                    (when (bound-and-true-p ivy-mode) ""))
                   (ivy-pre-prompt-function
                    (when (bound-and-true-p ivy-mode)
                      (lambda ()
                        (fzfa--format-stats (concat prompt dir-abbrev " ")
                                            (fzfa--frontend-index)
                                            last-filtered last-total)))))
               (setq selection
                     (completing-read prompt table nil nil init-text nil))))
         (when poll-timer (cancel-timer poll-timer))
         (when retry-timer (cancel-timer retry-timer))
         (when restart-timer (cancel-timer restart-timer))
         (remove-hook 'post-command-hook refresh-overlay)
         (when stats-overlay (delete-overlay stats-overlay))
         (mapc #'delete-overlay separator-overlays)
         (funcall display-clear)
         (fzfa--defer-async-stop handle)
         (when handler (fzfa--preview-return selection)))
       directory resolve-paths))))

;;; Split-style + display infrastructure

(defcustom fzfa-async-split-style 'perl
  "Splitting style for `fzfa-async-completing-read'.
See `fzfa-async-split-styles-alist' for available styles."
  :type '(choice (const :tag "Perl-style (#cmd#filter)" perl))
  :group 'fzfa)

(defcustom fzfa-shell-command-debounce 0.2
  "Seconds of typing silence before a shell-command restart fires.
Each keystroke that changes the command portion of the minibuffer
reschedules a fresh restart timer; the producer process is not
spawned until the user pauses for this long, so a burst of
keystrokes ends with exactly one restart on the final cmd value."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-shell-command-throttle 0.5
  "Minimum seconds between shell-command restarts."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-async-split-styles-alist
  `((perl :initial ?# :function ,#'fzfa--async-split-perl))
  "Splitting styles for `fzfa-async-completing-read'.
Each entry is (SYMBOL . PLIST).  Recognized PLIST keys:
  :function  (STR PLIST) -> (CMD . FILTER).  Required.
             CMD is the `shell-command' portion (re-runs on change);
             FILTER is passed to fzf-native for scoring.
  :initial   Optional character inserted at minibuffer setup so the
             user can start typing inside the delimited region.
  :separator Optional character; consumed by separator-based splitters."
  :type '(alist :key-type symbol :value-type plist)
  :group 'fzfa)

(defun fzfa--async-split-perl (str &optional _plist)
  "Split STR into (CMD . FILTER) using a perl-style separator.
If the first character of STR is punctuation it is the separator: text
between the first and second occurrence is CMD; text after the second
is FILTER.  With one separator only, the trailing text is CMD and
FILTER is empty.  Without a leading separator, the whole STR is CMD."
  (if (string-match-p "^[[:punct:]]" str)
      (save-match-data
        (let ((q (regexp-quote (substring str 0 1))))
          (cond
           ((string-match (concat "^" q "\\([^" q "]*\\)" q "\\(.*\\)") str)
            (cons (match-string 1 str) (match-string 2 str)))
           (t
            (cons (substring str 1) "")))))
    (cons str "")))

(defun fzfa--async-split (input display-state command splitter style)
  "Return (CMD . FILTER) for INPUT given the current session state.

In `hidden' DISPLAY-STATE, CMD lives outside the buffer (in COMMAND,
the closure variable in `fzfa-async-completing-read''s body or its
helm/multi analogue), so the split is trivial: CMD = COMMAND, FILTER
= INPUT.  In any other display state, the configured SPLITTER parses
INPUT against STYLE (a plist from `fzfa-async-split-styles-alist').

Frontend-agnostic — the table-lambda in completing-read sessions
calls it with `(fzfa--current-query str)' as INPUT; helm's
`:candidates' callback (once `>' support lands) calls it with
`helm-pattern'.  Same body for both."
  (if (eq display-state 'hidden)
      (cons (or command "") input)
    (funcall splitter input style)))

(defcustom fzfa-async-display-key ">"
  "Key string that toggles compact view of the CMD portion.
When compact, only the program name and the quoted-argument slot
\(if any) are visible; flags are hidden behind a `...' display.
Press the key again to expand and edit the full command.  The
session starts compact.  Set to nil to disable the feature entirely
\(no binding, no initial compaction)."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'fzfa)

(defun fzfa--async-display-cmd-bounds (sep)
  "Return (CMD-BEG . CMD-END) for the CMD region in the current minibuffer.
SEP is the separator character used by the active split style.  Returns
nil when the minibuffer does not begin with SEP or no closing SEP is
present yet."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (when (eq (char-after) sep)
      (let ((beg (1+ (point))))
        (goto-char beg)
        (when (search-forward (char-to-string sep) nil t)
          (cons beg (1- (point))))))))

(defun fzfa--async-display-make-overlays (cmd-beg cmd-end)
  "Return overlays compacting flag regions between CMD-BEG and CMD-END.
Uses the *last* balanced `\\='…\\=' / \"…\" pair as anchor (so an
earlier quoted flag value like `-flag=\\='val\\='' is ignored in favor
of the trailing input slot): text from the end of the program name up
to the opening quote renders as \" ... \", and any tail after the
closing quote renders as \"\".  Without a quoted argument, everything
after the program name is hidden with an empty display.  Whitespace-
only spans are left alone."
  (let* ((cmd-text (buffer-substring-no-properties cmd-beg cmd-end))
         (prog-end (if (string-match "\\`[^ \t]+" cmd-text)
                       (+ cmd-beg (match-end 0))
                     cmd-end))
         (last-pair
          (save-match-data
            (let ((pos 0) last)
              (while (string-match "\\('[^']*'\\|\"[^\"]*\"\\)"
                                   cmd-text pos)
                (setq last (cons (match-beginning 0) (match-end 0))
                      pos  (match-end 0)))
              last)))
         overlays)
    (cl-flet ((mk (beg end display)
                (when (and (< beg end)
                           (string-match-p
                            "[^ \t]"
                            (buffer-substring-no-properties beg end)))
                  (let ((ov (make-overlay beg end nil t nil)))
                    (overlay-put ov 'display display)
                    (overlay-put ov 'fzfa-async-display t)
                    (push ov overlays)))))
      (cond
       (last-pair
        (let ((qb (+ cmd-beg (car last-pair)))
              (qe (+ cmd-beg (cdr last-pair))))
          (mk prog-end qb " ... ")
          (mk qe cmd-end "")))
       (t
        (mk prog-end cmd-end ""))))
    overlays))

(defun fzfa--async-display-next-state (state)
  "Return the display state cycled one step forward.

The cycle is `hidden' → `compact' → `full' → `hidden'.  Any unknown
input falls back to `hidden' (defensive default for an unknown
session state)."
  (cl-case state
    ((hidden) 'compact)
    ((compact) 'full)
    ((full) 'hidden)
    (otherwise 'hidden)))

(defun fzfa--async-display-materialize (cmd initial-char)
  "Materialize `#CMD#' at the start of the editable region.

Inserts INITIAL-CHAR + CMD + INITIAL-CHAR at `minibuffer-prompt-end'
in the current buffer.  Preserves point's offset within FILTER —
i.e. the distance from `minibuffer-prompt-end' before the call
equals the distance from the start of the new FILTER region after.

Installs two self-healing protective overlays on the separators and
returns them as a two-element list `(OPEN-OVERLAY CLOSE-OVERLAY)'.

Operates on `current-buffer'.  Inside an actual minibuffer
`minibuffer-prompt-end' returns the position past the prompt's
`field' boundary; in temp buffers it returns `(point-min)' so this
helper is testable in isolation."
  (let* ((mbe (minibuffer-prompt-end))
         (filter-offset (max 0 (- (point) mbe)))
         (cmd-str (or cmd ""))
         (cmd-text (concat (char-to-string initial-char)
                           cmd-str
                           (char-to-string initial-char))))
    (goto-char mbe)
    (insert cmd-text)
    (goto-char (+ mbe (length cmd-text) filter-offset))
    (let ((close-pos (+ mbe 1 (length cmd-str))))
      (list (fzfa--async-protect-separator mbe initial-char)
            (fzfa--async-protect-separator close-pos initial-char)))))

(defun fzfa--async-display-extract (splitter style separator-overlays)
  "Parse `#CMD#FILTER' at start of editable region, delete `#CMD#'.

Reads the buffer contents from `minibuffer-prompt-end' to
`point-max', runs SPLITTER on it with STYLE, and uses the resulting
CMD to delete the leading `#CMD#' prefix.

Removes SEPARATOR-OVERLAYS first so their `modification-hooks'
don't self-heal the very deletion we're about to perform.

Returns the extracted CMD string.  The caller is responsible for
storing it into the session's closure variable so the hidden-mode
trivial-split picks it up on the next tick.

Operates on `current-buffer'."
  (let* ((mbe (minibuffer-prompt-end))
         (input (buffer-substring-no-properties mbe (point-max)))
         (split (funcall splitter input style))
         (cmd (car split)))
    (mapc #'delete-overlay separator-overlays)
    (delete-region mbe (min (point-max) (+ mbe 2 (length cmd))))
    cmd))

(defun fzfa--async-extract-args (cmd)
  "Run CMD in `:extract' mode and return its keyword args plist.
Returns nil if CMD does not flow through a fzfa `completing-read'."
  (catch 'fzfa-extracted
    (let ((fzfa--multi-mode :extract))
      (funcall cmd))
    nil))


;;; Smart dispatch

(defun fzfa--smart-resolve (clauses)
  "Return the first CMD in CLAUSES whose conditions are satisfied, else nil.
Each CLAUSE is (CMD &key executable predicate).  A clause matches when
CMD is `fboundp', its `:executable' (if any) is on PATH, and its
`:predicate' (if any) returns non-nil.  Clauses without either keyword
are unconditional fallbacks."
  (catch 'fzfa-smart-found
    (dolist (clause clauses)
      (let* ((cmd  (car clause))
             (rest (cdr clause))
             (exe  (plist-get rest :executable))
             (pred (plist-get rest :predicate)))
        (when (and (fboundp cmd)
                   (or (null exe)  (executable-find exe))
                   (or (null pred) (funcall pred)))
          (throw 'fzfa-smart-found cmd))))
    nil))

(defun fzfa-smart-define (name clauses)
  "Define `fzfa-smart-NAME' that dispatches to the first available CLAUSE.

NAME is a symbol naming the command group (e.g. `find', `grep'); the
generated command is interned as `fzfa-smart-NAME'.

CLAUSES is a list of (CMD &key executable predicate) entries.  Each
clause is tried in order; the first whose `:executable' (if supplied)
is on PATH and whose `:predicate' (if supplied) returns non-nil wins,
provided CMD is `fboundp'.  A clause with neither key is an
unconditional fallback.

The generated command is interactive and calls the chosen CMD via
`funcall', so the active `fzfa--multi-mode' (if any) propagates and
the smart command transparently participates in multi-source dispatch
\(`fzfa-multi-read') without needing any special-casing.

Resolution runs on every invocation — no caching — so PATH changes,
TRAMP buffers, and containerized shells are handled naturally.

Returns the new symbol."
  (let ((fname (intern (format "fzfa-smart-%s" name))))
    (defalias fname
      (lambda ()
        (interactive)
        (let ((cmd (fzfa--smart-resolve clauses)))
          (unless cmd
            (user-error "`%s': no available backend among %s"
                        fname
                        (mapconcat (lambda (c) (symbol-name (car c)))
                                   clauses ", ")))
          (funcall cmd)))
      (format "Smart `%s' dispatcher.
Tries backends in order and invokes the first whose executable is on
PATH and whose command symbol is bound: %s."
              name
              (mapconcat (lambda (c) (format "`%s'" (car c))) clauses ", ")))
    fname))

(fzfa-smart-define
 'find
 '((fzfa-fd       :executable "fd")
   (fzfa-rg-files :executable "rg")
   (fzfa-ag-files :executable "ag")
   (fzfa-find     :executable "find"))) ;; -> `fzfa-smart-find'

(fzfa-smart-define
 'grep
 '((fzfa-ugrep :executable "ugrep")
   (fzfa-rg    :executable "rg")
   (fzfa-ag    :executable "ag")
   (fzfa-grep  :executable "grep"))) ;; -> `fzfa-smart-grep'

;;; Sync `completing-read'

(cl-defun fzfa-sync-completing-read (&key
                                     candidates
                                     (prompt "fzf > ")
                                     (category 'fzfa-misc)
                                     annotate
                                     affix
                                     group
                                     history
                                     (require-match t)
                                     default
                                     preview
                                     apply)
  "Run `completing-read' over CANDIDATES using fzf-native for scoring.

:CANDIDATES List of strings to score with `fzf-native-score-all'.
:PROMPT     Minibuffer prompt string.  Defaults to \"fzf > \".
:CATEGORY   Completion category symbol.  Defaults to `fzfa-misc'.
            Use `fzfa-file' for file-path candidates so marginalia
            can annotate them with file metadata.
:ANNOTATE   Optional function (CANDIDATE) -> string appended after each
            candidate.  Exposed as `annotation-function' in completion
            metadata.  Annotations start immediately after the candidate
            string, so column alignment depends on candidate widths.
:AFFIX      Optional function (CANDIDATES) -> list of (CANDIDATE PREFIX
            SUFFIX).  Exposed as `affixation-function'.  Vertico
            right-pads each candidate to a consistent width before
            appending SUFFIX, giving true column alignment.  Prefer this
            over :ANNOTATE when alignment matters.
:GROUP      Optional function (CANDIDATE TRANSFORM) -> string.  When
            TRANSFORM is nil return the group name; when non-nil return
            the display string for CANDIDATE within its group.  Frontends
            like vertico render group headers between sections.
:HISTORY    Optional history variable symbol passed to `completing-read'.
            Selected entries are pushed onto this list and recallable
            with \\[previous-history-element].
:REQUIRE-MATCH Forwarded to `completing-read'.  Defaults to t.  Set nil
            to accept free-form input (treat CANDIDATES as suggestions).
:DEFAULT    Forwarded to `completing-read'.  Returned when the user
            submits empty input; also seeded into history.
:PREVIEW    Per-call live-preview handler that bypasses the
            `fzfa-preview-functions' registry lookup for CATEGORY.
            Pass a function that takes a CAND for a simple preview-only
            handler, or a full handler plist with `:setup',
            `:preview', `:exit', `:return' slots.  Nil (the default)
            falls back to the registry.  Set `fzfa-preview-delay' to nil
            to disable previews.
:APPLY      Lambda (CAND) -> any.
            See `fzfa-async-completing-read' for semantics."
  (fzfa--ensure-setup)
  (cond
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted
           ;; Translate :candidates → :items so multi consumes one key.
           (list :items candidates :prompt prompt :category category
                 :annotate annotate :affix affix :group group
                 :history history :preview preview)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (let ((cand (cdr fzfa--multi-mode)))
      (setq fzfa--multi-mode nil)
      (cl-return-from fzfa-sync-completing-read cand))))
  (when (and (bound-and-true-p helm-mode) fzfa-sync-helm-handler)
    (cl-return-from fzfa-sync-completing-read
      (funcall fzfa-sync-helm-handler
               :candidates candidates :prompt prompt :category category
               :annotate annotate :affix affix :group group
               :history history :require-match require-match
               :default default :preview preview :apply apply)))
  (let* ((completion-styles '(fzfa))
         (ivy-completing-read-dynamic-collection t) ;; Don't let `ivy' filter.
         (handler (fzfa--preview-handler preview category))
         (fzfa--preview-session (and handler (list handler)))
         (fzfa--session-apply
          (or apply (plist-get (alist-get category fzfa-apply-functions) :apply)))
         ;; Ivy ignores `display-sort-function' in completion metadata,
         ;; so apply the history sort ourselves on the empty-query
         ;; branch.  Vertico/icomplete reach this via the metadata, so
         ;; gating on `ivy-mode' avoids a double sort there.
         (ivy-history-sort-p (and history (bound-and-true-p ivy-mode)))
         (selection nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (fzfa--minibuffer-format-reset)
              (when handler (fzfa--preview-install)))
          (setq selection
                (completing-read
                 prompt
                 (lambda (str _pred action)
                   (pcase action
                     ('metadata (fzfa--completion-metadata category
                                                           :annotate annotate
                                                           :affix affix
                                                           :group group))
                     (`(boundaries . ,_) (cons 0 0))
                     ('lambda t)
                     ('t (let ((query (fzfa--current-query str)))
                           (cond
                            ((not (string-empty-p query))
                             (fzfa--bridge-defcustoms
                              #'fzf-native-score-all candidates query))
                            (ivy-history-sort-p
                             (fzfa--history-rank candidates history))
                            (t candidates))))))
                 nil require-match nil history default)))
      (when handler (fzfa--preview-return selection)))
    selection))

;;; Multi-source `completing-read'

(defcustom fzfa-multi-narrow-key "<"
  "Key string that activates source narrowing in `fzfa-multi-read'.
Press this key in a multi-source minibuffer, then a source's narrow
character, to restrict candidates to that source.  Re-pressing the
same combination widens back to all sources.  Set to nil to disable
narrowing entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Key string (passed to `kbd')"))
  :group 'fzfa)

(defconst fzfa--tofu-base #x100000
  "Base Unicode Private Use Area codepoint for source-disambiguation suffixes.
Each multi source's candidates carry a single trailing codepoint at
`fzfa--tofu-base' + source-idx, propertized `display \"\"' so it renders
invisibly while making cross-source duplicates `string='-unique.
See consult's `consult--tofu-encode' for the same trick.")

(defvar fzfa--tofu-cache (make-hash-table :test 'eql)
  "Cache of propertized tofu suffix strings, keyed by source index.")

(defvar fzfa--multi-active-sources nil
  "Source vector bound across the active `fzfa--multi-read' session.
Read by frontends that need per-source rendering — currently the
ivy display transformer in `fzfa-ivy.el', which decodes each
candidate's tofu suffix into its source name.")

(defun fzfa--tofu-suffix (idx)
  "Return the cached invisible tofu suffix string for source IDX."
  (or (gethash idx fzfa--tofu-cache)
      (puthash idx
               (propertize (string (+ fzfa--tofu-base idx))
                           'invisible t 'display "")
               fzfa--tofu-cache)))

(defun fzfa--tofu-hide (s)
  "Return S without its trailing tofu codepoint, if any.
Returns S unchanged when there is no tofu suffix."
  (if (and (stringp s)
           (let ((n (length s)))
             (and (> n 0)
                  (>= (aref s (1- n)) fzfa--tofu-base))))
      (substring s 0 (1- (length s)))
    s))

(defun fzfa--multi-tag (cand idx hash)
  "Return CAND tagged for source IDX.
Appends an invisible tofu suffix so cross-source duplicates remain
`string='-distinct, sets a `fzfa-src-idx' text property at position 0
for in-band dispatch, and records the tagged string in HASH as a
fallback lookup when text properties are stripped."
  (let ((tagged (concat cand (fzfa--tofu-suffix idx))))
    (when (> (length tagged) 0)
      (put-text-property 0 1 'fzfa-src-idx idx tagged))
    (puthash tagged idx hash)
    tagged))

(defun fzfa--multi-source-of (cand sources-v hash)
  "Return the source plist responsible for CAND, or nil.
SOURCES-V is the vector of source plists; HASH maps CAND to source index."
  (and (stringp cand) (> (length cand) 0)
       (let ((idx (or (get-text-property 0 'fzfa-src-idx cand)
                      (gethash cand hash))))
         (and idx (aref sources-v idx)))))

(defun fzfa--multi-source-idx (cand hash)
  "Return the source index for CAND, or nil.
HASH maps CAND to source index."
  (and (stringp cand) (> (length cand) 0)
       (or (get-text-property 0 'fzfa-src-idx cand)
           (gethash cand hash))))

(defun fzfa--multi-build-router (sources-v cand->src)
  "Build a synthetic preview handler that dispatches per source.

SOURCES-V is the vector of source plists; CAND->SRC is the
candidate→source-idx hash table.  Returns nil when no source has a
registered handler (preview wiring then no-ops); otherwise returns a
plist with `:setup' / `:preview' / `:exit' / `:return' slots plus a
`:multi-cells' slot exposing the per-source session cell vector —
callers stash this in the outer preview session so per-candidate
dispatch outside the router (e.g. `fzfa--promote-from-preview' on a
\\[fzfa-apply-current] apply) can reach the right source's `:opener'.

For each source, a fresh handler plist is resolved via
`fzfa--preview-handler' using the source's own `:preview' override
and `:category'.  Per-source state is stored in its own session
cell, so an `:opener' stashed by one source's `:setup' never
collides with another's.

Lifecycle:
  :setup    Broadcast to every source; propagates origin
            window/buffer/`default-directory' from the parent
            session into each per-source cell first.
  :preview  Routes to the source of CAND only.
  :exit     Broadcast to every source.
  :return   Broadcast: the source containing CAND receives CAND,
            every other source receives nil (\"aborted from its
            perspective\")."
  (let* ((n (length sources-v))
         (cells (make-vector n nil))
         (any nil))
    (dotimes (i n)
      (let* ((src (aref sources-v i))
             (handler (fzfa--preview-handler
                       (plist-get src :preview)
                       (plist-get src :category))))
        (when handler
          (aset cells i (cons handler nil))
          (setq any t))))
    (when any
      (cl-flet ((broadcast (action &optional cand cand-i)
                  (dotimes (i n)
                    (when-let* ((cell (aref cells i)))
                      (let ((fzfa--preview-session cell))
                        (fzfa--preview-call
                         action
                         (when (and cand (eql i cand-i))
                           (fzfa--tofu-hide cand))))))))
        (list
         :setup
         (lambda ()
           (let ((win (fzfa-preview-get :origin-window))
                 (buf (fzfa-preview-get :origin-buffer))
                 (dir (fzfa-preview-get :default-directory)))
             (dotimes (i n)
               (when-let* ((cell (aref cells i)))
                 (let ((fzfa--preview-session cell))
                   (fzfa-preview-put :origin-window    win)
                   (fzfa-preview-put :origin-buffer    buf)
                   (fzfa-preview-put :default-directory dir)
                   (fzfa--preview-call :setup))))))
         :preview
         (lambda (cand)
           (if-let* ((i (and cand
                             (fzfa--multi-source-idx cand cand->src)))
                     (cell (aref cells i)))
               (let ((fzfa--preview-session cell))
                 (fzfa--preview-call :preview (fzfa--tofu-hide cand)))
             ;; cand=nil (reset) — broadcast.
             (unless cand (broadcast :preview nil nil))))
         :exit  (lambda () (broadcast :exit))
         :return
         (lambda (cand)
           (let ((i (and cand (fzfa--multi-source-idx cand cand->src))))
             (broadcast :return cand i)))
         ;; Expose cells so callers can route per-source from outside.
         :multi-cells cells)))))

(defun fzfa--multi-rank (results query async-p)
  "Top fzf score for RESULTS under QUERY.
For async sources (ASYNC-P non-nil) the C async path does not attach
`completion-score' text properties, so we re-score the top candidate
once via `fzf-native-score'.  For sync sources the score is read off
the property set by `fzf-native-score-all'.  Returns 0 on empty input."
  (cond
   ((or (null results) (string-empty-p query)) 0)
   (async-p
    (or (car (fzfa--bridge-defcustoms
              #'fzf-native-score (car results) query))
        0))
   (t (or (get-text-property 0 'completion-score (car results)) 0))))

(defun fzfa--multi-narrow->string (k)
  "Coerce narrow key value K (symbol, character, or string) to length-1 string."
  (let ((s (cond
            ((stringp k) k)
            ((characterp k) (string k))
            ((symbolp k) (symbol-name k))
            (t (error "Bad fzfa narrow key %S" k)))))
    (cl-assert (= (length s) 1) nil
               "fzfa narrow key must be a single character, got %S" k)
    s))

(defun fzfa--multi-derive-narrow-key (name used)
  "Return a free single-character narrow key for source NAME.
USED is a hash table of already-allocated length-1 strings.  Tries
each word's first character (case preserved) from NAME split on
\"-\"; falls back to a-z / A-Z / 0-9.  Errors when the pool is
exhausted."
  (or
   (cl-loop for word in (split-string name "-" t)
            for s = (substring word 0 1)
            unless (gethash s used)
            return s)
   (cl-loop for c across (concat "abcdefghijklmnopqrstuvwxyz"
                                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                 "0123456789")
            for s = (string c)
            unless (gethash s used)
            return s)
   (error "Fzfa narrow key pool exhausted")))

(defun fzfa--format-narrow-hint (sources-v narrow-idx
                                           &optional width prefix-key)
  "Format the narrow-menu hint.
SOURCES-V is the source vector; NARROW-IDX is the active narrow
index or nil.  Shows every source as `KEY:NAME', separated by two
spaces, wrapping between entries when a line would exceed WIDTH
\(defaults to the active minibuffer body width, or 200 in batch).
When NARROW-IDX is non-nil, that source is highlighted with the
`minibuffer-prompt' face.  When PREFIX-KEY is non-nil, a trailing
`PREFIX-KEY:widen' marker (faced `shadow') is appended to
indicate the prefix widens.  Multi-line output relies on
`resize-mini-windows' (the Emacs default) to grow the minibuffer."
  (let* ((entries
          (append
           (mapcar
            (lambda (i)
              (let* ((src (aref sources-v i))
                     (k (or (plist-get src :narrow) "?"))
                     (n (or (plist-get src :name) ""))
                     (s (format "%s:%s" k n)))
                (if (eql i narrow-idx)
                    (propertize s 'face 'minibuffer-prompt)
                  s)))
            (number-sequence 0 (1- (length sources-v))))
           (when prefix-key
             (list (propertize (format "%s:widen" prefix-key)
                               'face 'shadow)))))
         (width (or width
                    (max 20 (1- (or (when-let*
                                        ((w (active-minibuffer-window)))
                                      (window-body-width w))
                                    200)))))
         (sep "  ")
         (sep-w 2)
         lines cur (cur-w 0))
    (dolist (e entries)
      (let ((ew (string-width e)))
        (cond
         ((null cur)
          (setq cur (list e) cur-w ew))
         ((<= (+ cur-w sep-w ew) width)
          (push e cur)
          (setq cur-w (+ cur-w sep-w ew)))
         (t
          (push (mapconcat #'identity (nreverse cur) sep) lines)
          (setq cur (list e) cur-w ew)))))
    (when cur
      (push (mapconcat #'identity (nreverse cur) sep) lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun fzfa--multi-allocate-narrow-keys (sources)
  "Return SOURCES with each plist augmented with a length-1 :narrow key.
Two passes: explicit :narrow values are coerced and reserved first
\(duplicates signal an error); remaining sources derive keys from
their :name via `fzfa--multi-derive-narrow-key'."
  (let ((used (make-hash-table :test 'equal))
        (with-explicit (make-vector (length sources) nil))
        (sources-v (vconcat sources))
        result)
    (dotimes (i (length sources-v))
      (let* ((src (aref sources-v i))
             (k (plist-get src :narrow)))
        (when k
          (let ((s (fzfa--multi-narrow->string k)))
            (when (gethash s used)
              (error "Duplicate fzfa narrow key %S for source %S"
                     s (plist-get src :name)))
            (puthash s t used)
            (aset with-explicit i s)))))
    (dotimes (i (length sources-v))
      (let* ((src (aref sources-v i))
             (s (or (aref with-explicit i)
                    (let ((d (fzfa--multi-derive-narrow-key
                              (or (plist-get src :name) "") used)))
                      (puthash d t used)
                      d))))
        (push (plist-put (copy-sequence src) :narrow s) result)))
    (nreverse result)))

(cl-defun fzfa--multi-read (sources &key (prompt "fzf-multi: "))
  "Run `completing-read' across multiple SOURCES, fzfa style.
PROMPT is shown in the minibuffer.

Internal — users should call `fzfa-multi-read' which derives sources
from existing single-source commands.  This function takes pre-built
source plists directly.

SOURCES is a list of plists.  Each source contributes a labeled group of
candidates; group order is recomputed on every keystroke from each
group's top fzf score, so the strongest-matching group floats to the
top.  Within a group, candidates stay in fzf order.  An empty query
falls back to declared source order.

Per-source plist keys:
  :name      Group header (required).
  :items     Sync items: list of strings, or zero-arg function returning one.
             Mutually exclusive with :command.
  :command   Async source: shell command string.
  :directory Working directory for :command (default `default-directory').
  :annotate  Optional (cand) -> string annotation function.
  :action    Optional (cand) -> any.  Called with the selection.  When
             omitted, the raw selection string is returned.
  :history   Optional history variable symbol.  When set, the cleaned
             selection (tofu suffix stripped) is pushed via
             `add-to-history' on exit — mirroring the HIST push that
             would have happened if the source's own `completing-read'
             had run.  On empty input, the source's candidate slot is
             additionally reordered by this history so recent picks
             surface first.  Sync sources extracted from existing
             `fzfa-*' commands inherit this from each source's
             `fzfa-sync-completing-read' :history argument."
  (cond
   ;; Composability: when this multi is being extracted by an outer
   ;; `fzfa-multi-read', throw our merged SOURCES so the
   ;; outer can flatten them into its own source list.  Each source
   ;; already carries its own :action closure, so dispatch from the
   ;; outer multi routes back to the correct underlying command.
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted (list :multi-sources sources)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (let ((cand (cdr fzfa--multi-mode)))
      (setq fzfa--multi-mode nil)
      (cl-return-from fzfa--multi-read cand))))
  (when (bound-and-true-p helm-mode)
    (if fzfa-multi-helm-handler
        (cl-return-from fzfa--multi-read
          (funcall fzfa-multi-helm-handler sources :prompt prompt))
      (user-error "Fzfa--multi-read does not yet support helm-mode")))
  (let* ((n            (length sources))
         (sources-v    (vconcat sources))
         (handles      (make-vector n nil))
         (sync-items   (make-vector n nil))
         (last-results (make-vector n nil))
         (rank         (make-vector n 0))
         (totals       (make-vector n 0))
         (filtered     (make-vector n 0))
         (last-gen     (make-vector n -1))
         (limit        (fzfa--candidate-limit))
         (cand->src    (make-hash-table :test 'equal :size 1024))
         (last-exhibit 0.0)
         (stats-overlay nil)
         ;; Captured by `minibuffer-exit-hook' from the propertized text
         ;; in the minibuffer before `completing-read' returns and strips
         ;; properties.  Reliable per-instance source dispatch even when
         ;; the same string appears in multiple sources.
         (selected-idx nil)
         ;; Index into sources-v of the currently-narrowed source, or nil
         ;; for "all sources".  Mutated by `narrow-handler' on a narrow
         ;; key press and read by the candidate function in `'t' below.
         (narrow-idx nil)
         ;; When the narrow menu is on screen (during the
         ;; `narrow-handler''s `read-char') we must NOT overwrite the
         ;; overlay with the stats line on every tick — otherwise async
         ;; sources streaming new generations (a 50ms cadence) erase
         ;; the menu before the user has had a chance to read it.
         (menu-active nil)
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay
                       (not menu-active)
                       (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (overlay-put
                 stats-overlay 'display
                 (fzfa--format-stats
                  (if narrow-idx
                      (concat prompt
                              (propertize
                               (format "{%s} "
                                       (or (plist-get
                                            (aref sources-v narrow-idx)
                                            :name)
                                           "?"))
                               'face 'minibuffer-prompt))
                    prompt)
                  (fzfa--frontend-index)
                  (cl-loop for x across filtered sum x)
                  (cl-loop for x across totals sum x)))))
            (fzfa--insert-prompt-if-ivy)))
         ;; Ivy push closure: ivy doesn't re-call the collection on
         ;; timer ticks (push model), so async sources would stay
         ;; stuck on the initial pattern.  Mirrors the per-source
         ;; iterate/score/rank/concat from the `'t' collection
         ;; action but reads `ivy-text' and pushes via
         ;; `ivy--set-candidates' + `ivy--exhibit'.  Mutates the
         ;; same per-source state vectors, so state stays consistent
         ;; whether updated via the collection (vertico/icomplete)
         ;; or this closure (ivy).
         (ivy-push-multi
          (lambda ()
            (when-let* ((win   (active-minibuffer-window))
                        ((or (not (bound-and-true-p ivy-last))
                             (ivy-state-dynamic-collection ivy-last)))
                        (query (and (boundp 'ivy-text) ivy-text)))
              ;; Run the ivy ops with the minibuffer buffer current —
              ;; the closure can fire from `run-with-idle-timer'
              ;; whose buffer context is whatever was current at idle
              ;; time, and `ivy--insert-prompt' / `ivy--exhibit'
              ;; silently write to the wrong buffer otherwise.
              (with-selected-window win
                (let ((interrupted nil))
                  (dotimes (i n)
                    (if (and narrow-idx (/= narrow-idx i))
                        (progn
                          (aset last-results i nil)
                          (aset filtered i 0)
                          (aset rank i 0))
                      (let* ((h     (aref handles i))
                             (items (aref sync-items i))
                             (out
                              (cond
                               (h (while-no-input
                                    (fzf-native-async-candidates
                                     h query limit)))
                               (items
                                (if (string-empty-p query)
                                    items
                                  (while-no-input
                                    (fzfa--bridge-defcustoms
                                     #'fzf-native-score-all
                                     items query)))))))
                        (cond
                         ((eq out t) (setq interrupted t))
                         ((and h (not (fzfa--async-final-p out h query)))
                          (when-let* ((s (fzf-native-async-stats h)))
                            (aset totals i (cdr s))))
                         (t
                          (when h
                            (setq out
                                  (mapcar
                                   (lambda (c)
                                     (fzfa--multi-tag c i cand->src))
                                   out)))
                          (aset last-results i out)
                          (aset rank i (fzfa--multi-rank out query h))
                          (cond
                           (h (when-let* ((s (fzf-native-async-stats h)))
                                (aset filtered i (car s))
                                (aset totals   i (cdr s))))
                           (t (aset filtered i (length out)))))))))
                  (unless interrupted
                    (let* ((order (number-sequence 0 (1- n)))
                           (empty-q (string-empty-p query))
                           (sorted
                            (if empty-q
                                order
                              (sort order
                                    (lambda (a b)
                                      (> (aref rank a) (aref rank b))))))
                           (cands
                            (apply #'append
                                   (mapcar
                                    (lambda (i)
                                      (let* ((slot (aref last-results i))
                                             (hist (and empty-q
                                                        (plist-get
                                                         (aref sources-v i)
                                                         :history))))
                                        (if hist
                                            (fzfa--history-rank slot hist)
                                          slot)))
                                    sorted))))
                      (ivy--set-candidates cands)
                      (ivy--exhibit)
                      ;; `ivy--exhibit' skips the prompt redraw when the
                      ;; candidate body didn't change.  Force it so our
                      ;; `ivy-pre-prompt-function' lambda runs again with
                      ;; the freshest stats.
                      (ivy--insert-prompt))))))))
         ;; Ivy action list for narrow dispatch.  One entry per
         ;; source's :narrow key (mutates `narrow-idx' and refreshes
         ;; via `ivy-push-multi'), plus a widen entry on
         ;; `fzfa-multi-narrow-key' so pressing the prefix key twice
         ;; widens — matching the existing `<<' muscle memory.  Bound
         ;; into `ivy--actions-list' across `completing-read' below;
         ;; `ivy-dispatching-call' triggers the action menu via
         ;; `fzfa-multi-narrow-key' in the keymap install.
         (ivy-multi-actions
          (when (bound-and-true-p ivy-mode)
            (let (acts)
              (dotimes (i n)
                (when-let* ((src    (aref sources-v i))
                            (narrow (plist-get src :narrow)))
                  (let ((idx i)
                        (name (or (plist-get src :name) "?")))
                    (push (list narrow
                                (lambda (_cand)
                                  (setq narrow-idx idx)
                                  (funcall ivy-push-multi))
                                (format "narrow → %s" name))
                          acts))))
              (when fzfa-multi-narrow-key
                (push (list fzfa-multi-narrow-key
                            (lambda (_cand)
                              (setq narrow-idx nil)
                              (funcall ivy-push-multi))
                            "widen")
                      acts))
              (nreverse acts))))
         (narrow-handler
          (lambda ()
            (interactive)
            ;; Three states:
            ;;   1. widened (narrow-idx nil) — no menu, query freely
            ;;   2. narrow menu — this handler is running
            ;;   3. narrowed (narrow-idx set) — no menu, query freely
            ;;
            ;; From the menu: source letter narrows/switches; the
            ;; prefix key widens (-> 1); any other key cancels the
            ;; menu and returns to the prior state (1 or 3).
            (let* ((seq (and fzfa-multi-narrow-key
                             (listify-key-sequence
                              (kbd fzfa-multi-narrow-key))))
                   (prefix-event (car (last seq)))
                   (before narrow-idx))
              ;; Suspend `refresh-overlay' (which otherwise restores the
              ;; stats line on every async tick) so the menu stays put
              ;; until `read-char' returns.  `unwind-protect' guarantees
              ;; the flag clears on `C-g' / error too.
              (setq menu-active t)
              (unwind-protect
                  (progn
                    (when (and stats-overlay (active-minibuffer-window))
                      (with-selected-window (active-minibuffer-window)
                        (overlay-put stats-overlay 'display
                                     (concat (fzfa--format-narrow-hint
                                              sources-v narrow-idx nil
                                              fzfa-multi-narrow-key)
                                             " "))
                        (redisplay)))
                    (let* ((c (read-char))
                           (target
                            (cl-position-if
                             (lambda (src)
                               (when-let* ((k (plist-get src :narrow)))
                                 (and (stringp k)
                                      (= (length k) 1)
                                      (= (string-to-char k) c))))
                             sources-v)))
                      (cond
                       ((and prefix-event (eql c prefix-event))
                        (setq narrow-idx nil))
                       (target (setq narrow-idx target))
                       (t nil))
                      (setq menu-active nil)
                      (unless (eql before narrow-idx)
                        (fzfa--frontend-push ivy-push-multi))
                      ;; Restore the normal overlay now that the menu
                      ;; is dismissed (the 't action's own refresh path
                      ;; only fires on candidate computations).
                      (funcall refresh-overlay)
                      ;; Under ivy, force a prompt redraw so the
                      ;; `ivy-pre-prompt-function' lambda runs again
                      ;; with `menu-active' = nil and swaps the menu
                      ;; hint back to the stats line.  Cheap if
                      ;; `ivy-push-multi' already redrew.
                      (when (bound-and-true-p ivy-mode)
                        (ivy--exhibit))))
                (setq menu-active nil)))))
         (router      (fzfa--multi-build-router sources-v cand->src))
         (fzfa--preview-session
          (and router
               ;; Stash the candidate→source-idx table so
               ;; `fzfa--promote-from-preview' can route to the right
               ;; source's `:opener' on C-z apply.  Cells are exposed
               ;; via `:multi-cells' on ROUTER itself.
               (list router :multi-cand->src cand->src)))
         retry-timer timer result)
    (dotimes (i n)
      (let* ((src   (aref sources-v i))
             (cmd   (plist-get src :command))
             (items (plist-get src :items)))
        (cond
         (cmd
          (aset handles i
                (fzf-native-async-start
                 cmd
                 (expand-file-name
                  (or (plist-get src :directory) default-directory)))))
         (items
          (let ((tagged
                 (mapcar (lambda (s)
                           (fzfa--multi-tag (copy-sequence s) i cand->src))
                         (if (functionp items) (funcall items) items))))
            (aset sync-items i tagged)
            (aset totals i (length tagged))
            (aset filtered i (length tagged)))))))
    (unwind-protect
        (progn
          (setq timer
                (run-with-timer
                 0 fzfa-refresh-delay
                 (lambda ()
                   (when (active-minibuffer-window)
                     (let (bumped)
                       (dotimes (i n)
                         (when-let* ((h (aref handles i))
                                     (g (fzf-native-async-generation h)))
                           (when (/= g (aref last-gen i))
                             (aset last-gen i g)
                             (setq bumped t))))
                       (when (and bumped (not (input-pending-p))
                                  (>= (- (float-time) last-exhibit)
                                      fzfa-input-throttle))
                         (setq last-exhibit (float-time))
                         (run-with-idle-timer
                          0 nil #'fzfa--frontend-push ivy-push-multi)))))))
          (add-hook 'post-command-hook refresh-overlay)
          (sit-for fzfa-refresh-delay)
          (setq result
                (minibuffer-with-setup-hook
                    (lambda ()
                      (fzfa--minibuffer-format-reset)
                      (when router (fzfa--preview-install))
                      ;; Capture source idx from the propertized minibuffer
                      ;; text before completing-read returns and strips text
                      ;; properties from its return value.  Reliable
                      ;; per-instance dispatch even for cross-source
                      ;; duplicate strings.
                      (add-hook 'minibuffer-exit-hook
                                (lambda ()
                                  (let ((s (buffer-substring
                                            (minibuffer-prompt-end)
                                            (point-max))))
                                    (when (> (length s) 0)
                                      (setq selected-idx
                                            (get-text-property
                                             0 'fzfa-src-idx s)))))
                                nil 'local)
                      ;; Install narrow-key binding as a per-instance
                      ;; child of the active completion keymap so we
                      ;; don't mutate vertico/icomplete/ivy's shared
                      ;; map.  Under ivy, hand off to its native
                      ;; action dispatch (`ivy-dispatching-call' +
                      ;; per-source entries in `ivy--actions-list');
                      ;; under other frontends, run the in-house
                      ;; `narrow-handler' that does its own read-char
                      ;; menu.
                      (when fzfa-multi-narrow-key
                        (let ((map (make-sparse-keymap)))
                          (set-keymap-parent map (current-local-map))
                          (define-key map (kbd fzfa-multi-narrow-key)
                                      (if (bound-and-true-p ivy-mode)
                                          #'ivy-dispatching-call
                                        narrow-handler))
                          (use-local-map map))))
                  (let ((fzfa--multi-active-sources sources-v)
                        (ivy-completing-read-dynamic-collection t)
                        (ivy-count-format
                         (when (bound-and-true-p ivy-mode) ""))
                        (ivy--actions-list
                         (if (bound-and-true-p ivy-mode)
                             (plist-put (cl-copy-list
                                         (or ivy--actions-list '()))
                                        t ivy-multi-actions)
                           ivy--actions-list))
                        (ivy-pre-prompt-function
                         (when (bound-and-true-p ivy-mode)
                           (lambda ()
                             (fzfa--format-stats
                              (if narrow-idx
                                  (concat prompt
                                          (propertize
                                           (format "{%s} "
                                                   (or (plist-get
                                                        (aref sources-v
                                                              narrow-idx)
                                                        :name)
                                                       "?"))
                                           'face 'minibuffer-prompt))
                                prompt)
                              (fzfa--frontend-index)
                              (cl-loop for x across filtered sum x)
                              (cl-loop for x across totals sum x))))))
                    (completing-read
                     prompt
                     (lambda (str _pred action)
                       (pcase action
                         ('metadata
                          (fzfa--completion-metadata
                           'fzfa-multi
                           :group
                           (lambda (cand transform)
                             (let* ((src (fzfa--multi-source-of
                                          cand sources-v cand->src))
                                    (g   (plist-get src :group)))
                               (if transform
                                   ;; Per-source :group transform — lets a
                                   ;; source strip an internal "IDX:" prefix
                                   ;; or otherwise reshape its display string
                                   ;; while keeping the raw value as the
                                   ;; lookup/match key.  Falls back to the raw
                                   ;; candidate when a source has no :group
                                   ;; function (or its transform returns nil).
                                   ;; The tofu suffix is hidden via its
                                   ;; `display ""' text property, so the raw
                                   ;; CAND fallback renders cleanly without an
                                   ;; explicit strip.
                                   (or (and g (funcall
                                               g (fzfa--tofu-hide cand) t))
                                       cand)
                                 ;; Section header.  When narrowed to a single
                                 ;; source, delegate to the per-source :group's
                                 ;; nil branch so any internal sub-grouping
                                 ;; (e.g. per-file headers for grep-style
                                 ;; sources) takes over — matching the
                                 ;; standalone command's layout.  Across
                                 ;; sources, the source name is the only
                                 ;; header that meaningfully separates them.
                                 (or (and narrow-idx g
                                          (funcall g (fzfa--tofu-hide cand) nil))
                                     (plist-get src :name) ""))))
                           :affix
                           ;; Pin annotations to window-relative column
                           ;; maxw+1 via a `(space :align-to ...)' display
                           ;; spec.  Vertico just concatenates suffixes
                           ;; verbatim (no padding of its own) so it needs
                           ;; the spec; icomplete's own slice-relative
                           ;; padding stacks badly with literal spaces, so
                           ;; the spec wins there too.
                           (lambda (cands)
                             (let* ((displays
                                     (mapcar
                                      (lambda (c)
                                        (let* ((src (fzfa--multi-source-of
                                                     c sources-v cand->src))
                                               (g (and src
                                                       (plist-get src :group))))
                                          (or (and g (funcall
                                                      g (fzfa--tofu-hide c) t))
                                              c)))
                                      cands))
                                    (maxw (apply #'max 0
                                                 (mapcar #'string-width
                                                         displays))))
                               (cl-mapcar
                                (lambda (cand _display)
                                  (let* ((src (fzfa--multi-source-of
                                               cand sources-v cand->src))
                                         (ann (and src
                                                   (plist-get src :annotate)))
                                         (s   (and ann (funcall
                                                        ann
                                                        (fzfa--tofu-hide cand)))))
                                    (list cand ""
                                          (if s
                                              (concat
                                               (propertize
                                                " " 'display
                                                `(space :align-to
                                                        (+ left ,(1+ maxw))))
                                               s)
                                            ""))))
                                cands displays)))))
                         (`(boundaries . ,_) (cons 0 0))
                         ('lambda t)
                         ('t
                          (let ((query (fzfa--current-query str))
                                (interrupted nil))
                            (dotimes (i n)
                              (if (and narrow-idx (/= narrow-idx i))
                                  ;; Source filtered out by narrow — drop
                                  ;; its prior results and zero its filtered
                                  ;; count so the overlay reflects the
                                  ;; narrowed pool.  `totals' is preserved
                                  ;; so re-widening shows the full size.
                                  (progn
                                    (aset last-results i nil)
                                    (aset filtered i 0)
                                    (aset rank i 0))
                                (let* ((h     (aref handles i))
                                       (items (aref sync-items i))
                                       (out
                                        (cond
                                         (h (while-no-input
                                              (fzf-native-async-candidates
                                               h query limit)))
                                         (items
                                          (if (string-empty-p query)
                                              items
                                            (while-no-input
                                              (fzfa--bridge-defcustoms
                                               #'fzf-native-score-all
                                               items query)))))))
                                  (cond
                                   ((eq out t) (setq interrupted t))
                                   ;; Async source whose result is not yet
                                   ;; final — keep the prior per-source slot;
                                   ;; refresh `totals' so the overlay still
                                   ;; reflects the live pool.
                                   ((and h (not (fzfa--async-final-p
                                                 out h query)))
                                    (when-let* ((s (fzf-native-async-stats h)))
                                      (aset totals i (cdr s))))
                                   (t
                                    ;; Async returns fresh strings each call;
                                    ;; re-tag them so group/action lookup works.
                                    ;; out may be nil (zero matches) — still ok.
                                    (when h
                                      (setq out
                                            (mapcar
                                             (lambda (c)
                                               (fzfa--multi-tag c i cand->src))
                                             out)))
                                    (aset last-results i out)
                                    (aset rank i
                                          (fzfa--multi-rank out query h))
                                    (cond
                                     (h (when-let* ((s (fzf-native-async-stats h)))
                                          (aset filtered i (car s))
                                          (aset totals   i (cdr s))))
                                     (t (aset filtered i (length out)))))))))
                            (when interrupted
                              (when retry-timer (cancel-timer retry-timer))
                              (setq retry-timer
                                    (run-with-idle-timer
                                     fzfa-input-debounce nil
                                     (lambda ()
                                       (setq retry-timer nil)
                                       (fzfa--frontend-push ivy-push-multi)))))
                            (when-let* ((win (active-minibuffer-window)))
                              (with-selected-window win
                                (unless stats-overlay
                                  (setq stats-overlay
                                        (make-overlay (point-min)
                                                      (minibuffer-prompt-end))))
                                (funcall refresh-overlay)))
                            (let* ((order (number-sequence 0 (1- n)))
                                   (empty-q (string-empty-p query))
                                   ;; `sort' is stable since Emacs 25, so equal
                                   ;; ranks preserve declared source order.
                                   (sorted
                                    (if empty-q
                                        order
                                      (sort order
                                            (lambda (a b)
                                              (> (aref rank a)
                                                 (aref rank b)))))))
                              (apply #'append
                                     (mapcar
                                      (lambda (i)
                                        (let* ((slot (aref last-results i))
                                               ;; Per-source recency only on
                                               ;; empty input — when scoring
                                               ;; ran, fzf order wins.
                                               (hist (and empty-q
                                                          (plist-get
                                                           (aref sources-v i)
                                                           :history))))
                                          (if hist
                                              (fzfa--history-rank slot hist)
                                            slot)))
                                      sorted)))))
                         (_ t)))
                     nil t)))))
      (when timer (cancel-timer timer))
      (when retry-timer (cancel-timer retry-timer))
      (remove-hook 'post-command-hook refresh-overlay)
      (when stats-overlay (delete-overlay stats-overlay))
      (fzfa--defer-async-stop handles)
      (when router (fzfa--preview-return result)))
    (when result
      (let* ((src    (or (and selected-idx (aref sources-v selected-idx))
                         (fzfa--multi-source-of
                          result sources-v cand->src)))
             (action (and src (plist-get src :action)))
             (clean  (fzfa--tofu-hide result))
             (hist   (and src (plist-get src :history))))
        ;; Multi bypasses each source's inner `completing-read', so the
        ;; source's natural HIST push never fires.  Mirror it here so
        ;; recency-aware sources (e.g. `extended-command-history') stay
        ;; consistent whether picked directly or via a multi.
        (when (and hist (symbolp hist) (not (eq hist t)))
          (add-to-history hist clean))
        (if action (funcall action clean) clean)))))

;;;###autoload
(defun fzfa-multi-read (commands &rest options)
  "Run a multi-source completing-read over COMMANDS.
Each entry in COMMANDS is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key for
that source (KEY is a single character — symbol, ?char, or string).

Each command is funcalled twice per multi session — once in
`:extract' mode (capture keyword args, abort), once in `:inject' mode after
the user picks (so the command's post-action runs).  OPTIONS is forwarded
to `fzfa--multi-read'.  Commands whose body does not reach
`fzfa-async-completing-read' or `fzfa-sync-completing-read' are skipped.
Commands must be arg-less (no interactive `read-*' prompts in their body).

Composes: if a command in COMMANDS itself calls `fzfa--multi-read'
\(e.g. `fzfa-find-any'), its inner sources are flattened in alongside
the other commands' sources, with each inner source keeping its own
:action.  Explicit :narrow on a nested-multi entry is ignored —
inner sources receive auto-derived keys from their own :name."
  (fzfa--ensure-setup)
  (let* ((completion-styles '(fzfa))
         (normalized
          (mapcar (lambda (entry)
                    (cond
                     ((symbolp entry) (cons entry nil))
                     ((and (consp entry) (symbolp (car entry)))
                      (cons (car entry) (cdr entry)))
                     (t (error "Bad fzfa multi entry: %S" entry))))
                  commands))
         (source-lists
          (mapcar
           (lambda (pair)
             (let* ((cmd (car pair))
                    (spec (cdr pair))
                    (args (condition-case nil
                              (catch 'fzfa-extracted
                                (let ((fzfa--multi-mode :extract))
                                  (funcall cmd))
                                nil)
                            (error nil))))
               (when args
                 (if-let* ((nested (plist-get args :multi-sources)))
                     ;; Flatten: nested multi command's sources are
                     ;; already fully built with :action closures.
                     ;; A user-provided :narrow here can't sensibly
                     ;; pick one inner source over another, so skip it.
                     nested
                   (let* ((cat (plist-get args :category))
                          (default-annotate
                           (cond
                            ((memq cat '(fzfa-buffer buffer))
                             (lambda (c)
                               (when (fboundp 'marginalia-annotate-buffer)
                                 (marginalia-annotate-buffer c))))
                            ((memq cat '(fzfa-file file))
                             (lambda (c)
                               (when (fboundp 'marginalia-annotate-file)
                                 (marginalia-annotate-file c)))))))
                     ;; Wrap a single source in a list so `append'
                     ;; below treats single and nested cases uniformly.
                     (list
                      (append
                       (list :name (replace-regexp-in-string
                                    "^fzfa-" "" (symbol-name cmd))
                             :annotate (or (plist-get args :annotate)
                                           default-annotate)
                             :action (lambda (cand)
                                       (let ((fzfa--multi-mode
                                              (cons :inject cand)))
                                         (funcall cmd)))
                             :narrow (plist-get spec :narrow))
                       args)))))))
           normalized))
         (sources (apply #'append (delq nil source-lists))))
    ;; Allocation must happen at the OUTERMOST multi level — when we're
    ;; being extracted by an outer `fzfa-multi-read', our inner sources
    ;; will be flattened into its source list and allocated there.  If
    ;; we allocated here first, those keys arrive at the outer as fixed
    ;; explicit reservations that can collide with the outer's own
    ;; explicit `:narrow' annotations.
    (unless (eq fzfa--multi-mode :extract)
      (setq sources (fzfa--multi-allocate-narrow-keys sources)))
    (apply #'fzfa--multi-read sources options)))

(defcustom fzfa-find-any-commands
  '(fzfa-imenu
    (fzfa-vc-modified-locally :narrow l)
    fzfa-vc-added-files
    (fzfa-vc-staged-for-commit :narrow c)
    fzfa-buffer
    fzfa-recent-file
    (fzfa-hungry-find :narrow f)
    (fzfa-imenu-all-but-current :narrow I)
    (fzfa-M-x :narrow x)
    (fzfa-swiper-all :narrow s)
    (fzfa-hungry-swiper :narrow S)
    fzfa-locate)
  "Commands shown by `fzfa-find-any'.
Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

(defcustom fzfa-find-some-commands
  '(fzfa-imenu
    (fzfa-vc-modified-locally :narrow l)
    fzfa-vc-added-files
    (fzfa-vc-staged-for-commit :narrow c)
    fzfa-buffer
    fzfa-recent-file
    (fzfa-smart-find :narrow f)
    (fzfa-M-x-for-buffer :narrow x)
    (fzfa-swiper :narrow s)
    (fzfa-smart-grep :narrow g))
  "Commands shown by `fzfa-find-some'.
Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

;;;###autoload
(defun fzfa-find-any ()
  "Multi-source fuzzy completion over `fzfa-find-any-commands'."
  (interactive)
  (fzfa-multi-read fzfa-find-any-commands :prompt "any?: "))

;;;###autoload
(defun fzfa-find-some ()
  "Multi-source fuzzy completion over `fzfa-find-some-commands'."
  (interactive)
  (fzfa-multi-read fzfa-find-some-commands :prompt "some?: "))

;;;###autoload
(defun fzfa-passwords ()
  "Multi-source fuzzy completion over `pass' and Chrome's password manager.
Selecting an entry copies its password to the kill ring via the
originating source's command (`fzfa-pass-copy' or
`fzfa-chrome-pass-copy')."
  (interactive)
  (fzfa-multi-read
   '(fzfa-pass-copy fzfa-chrome-pass-copy)
   :prompt "passwords: "))

;;; Grep candidate machinery

(defun fzfa--max-columns-flag (tool)
  "Return a max-line-length CLI flag string for grep-style TOOL.

Note: rg's `--max-columns' DROPS the line; ag's and ugrep's
`--width' TRUNCATE display.  Practical effect for our use case
is similar (bounded line length into our pipe), but the
underlying semantics differ slightly.  Either way the
reader-side cap still runs as a backstop."
  (let ((mll fzfa-max-line-length))
    (if (not (and (integerp mll) (> mll 0)))
        ""                                ; nil / 0 / negative → no flag
      (pcase tool
        ('rg    (format "--max-columns=%d" mll))
        ('ugrep (format "--width=%d" mll))
        ('ag    (format "--width=%d" mll))
        (_      "")))))

(defconst fzfa--grep-line-regexp "\\`\\(.*?\\):\\([0-9]+\\):"
  "Lazy parser for `fzfa-grep' category candidates.
Group 1 is SOURCE (file path or buffer name).  Group 2 is LINE.
Lazy match anchors on the first `:DIGITS:' boundary so a colon-bearing
buffer name does not corrupt the split.")

(defun fzfa--goto-source (source line)
  "Open SOURCE and jump to LINE.
SOURCE is a file path when `file-exists-p'; otherwise it is treated as
a buffer name."
  (cond
   ((file-exists-p source) (find-file source))
   ((get-buffer source)    (switch-to-buffer source))
   (t (user-error "Source not found: %s" source)))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun fzfa--grep-jump (cand)
  "Open the SOURCE and jump to the LINE referenced by CAND."
  (when (string-match fzfa--grep-line-regexp cand)
    (fzfa--goto-source
     (match-string 1 cand)
     (string-to-number (match-string 2 cand)))))

(defvar-keymap fzfa-grep-map
  :doc "Embark keymap for `fzfa-grep' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'.")

(defun fzfa--grep-group (cand transform)
  "Group function for FILE:LINE:CONTENT grep candidate CAND.
TRANSFORM nil  → return the filename as the section header.
TRANSFORM non-nil → strip the filename prefix; display LINE:CONTENT only."
  (if (string-match fzfa--grep-line-regexp cand)
      (if transform
          (substring cand (match-beginning 2))
        (match-string 1 cand))
    cand))

;;; Location candidates (in-Emacs line search)

(defun fzfa--location-candidate (cand source line)
  "Tag CAND with SOURCE and LINE as an `fzfa-location' text property.
Modifies CAND in place and returns it.  The property is attached at index
0 only so the interval tree stays one node per candidate.  SOURCE is a
file path or buffer name; LINE is the 1-based line number.

Use this when building candidates for the `fzfa-location' category — line
search commands like `fzfa-swiper' where the source should be carried
in-band for jump but never enter fzf's scoring."
  (when (> (length cand) 0)
    (add-text-properties 0 1 `(fzfa-location (,source . ,line)) cand))
  cand)

(defun fzfa--location-jump (cand)
  "Open SOURCE and jump to LINE recorded on CAND's `fzfa-location' property."
  (when-let* ((loc (and (stringp cand) (> (length cand) 0)
                        (get-text-property 0 'fzfa-location cand))))
    (fzfa--goto-source (car loc) (cdr loc))))

(defvar-keymap fzfa-location-map
  :doc "Embark keymap for `fzfa-location' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'.")

(defun fzfa--location-group (cand transform)
  "Group function for `fzfa-location' candidate CAND.
TRANSFORM nil  → return the source (file or buffer) as the section header.
TRANSFORM non-nil → return CAND unchanged.
Reads the source off CAND's `fzfa-location' text property; returns CAND
when the property is missing so candidates without locations still
render."
  (if transform
      cand
    (or (when-let* ((loc (and (> (length cand) 0)
                              (get-text-property 0 'fzfa-location cand))))
          (car loc))
        cand)))

;;; Setup

(defun fzfa--bridge-defcustoms (orig-fn &rest args)
  "Wrap fzf-native call ORIG-FN with ARGS; bridge fzfa-* into C scorer."
  (let ((fzf-native-async-highlight  fzfa-highlight)
        (fzf-native-max-line-length  fzfa-max-line-length)
        (fzf-native-async-cache-size fzfa-cache-size)
        (fzf-native-case-mode        fzfa-case-mode)
        (fzf-native-fuzzy            fzfa-fuzzy))
    (apply orig-fn args)))

(defun fzfa--ensure-setup ()
  "Install fzfa's registrations exactly once.
Idempotent: subsequent calls are a flag check.  Called from every
public entry point.

 e.g.
 `fzfa-async-completing-read',
 `fzfa-sync-completing-read',
 `fzfa-multi-read'."
  (unless fzfa--setup-done
    (setq fzfa--setup-done t)
    (when (fboundp 'fzf-native-ensure-loaded)
      (fzf-native-ensure-loaded))
    (add-to-list 'completion-styles-alist
                 '(fzfa
                   fzfa-try-completion fzfa-all-completions
                   "Passthrough style for pre-scored async fzf completions."))

    (advice-add 'fzf-native-async-start      :around #'fzfa--bridge-defcustoms)
    (advice-add 'fzf-native-async-candidates :around #'fzfa--bridge-defcustoms)

    (with-eval-after-load 'embark
      (dolist (entry '((fzfa-file     . embark-file-map)
                       (fzfa-buffer   . embark-buffer-map)
                       (fzfa-bookmark . embark-bookmark-map)
                       (fzfa-grep     fzfa-grep-map     embark-general-map)
                       (fzfa-location fzfa-location-map embark-general-map)))
        (add-to-list 'embark-keymap-alist entry))
      (setf (alist-get 'fzfa-grep     embark-default-action-overrides)
            (lambda (cand) (fzfa-with-visit (fzfa--grep-jump cand))))
      (setf (alist-get 'fzfa-location embark-default-action-overrides)
            (lambda (cand) (fzfa-with-visit (fzfa--location-jump cand)))))

    (with-eval-after-load 'marginalia
      (dolist (entry '((fzfa-file     marginalia-annotate-file     none)
                       (fzfa-buffer   marginalia-annotate-buffer   none)
                       (fzfa-bookmark marginalia-annotate-bookmark none)
                       (fzfa-theme    marginalia-annotate-theme    none)
                       (fzfa-imenu    marginalia-annotate-imenu    none)))
        (add-to-list 'marginalia-annotators entry)))

    (when fzfa-extensions
      (dolist (ext fzfa-extensions)
        (require (intern (format "fzfa-%s" ext)))
        (let ((setup-fn (intern (format "fzfa-%s-setup" ext))))
          (when (fboundp setup-fn) (funcall setup-fn)))))))

(provide 'fzfa)
;;; fzfa.el ends here
