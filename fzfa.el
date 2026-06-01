;;; fzfa.el --- Async fuzzy completion via `fzf-native' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (fzf-native "1.1"))
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
(defvar fzf-native-async-highlight)
(defvar fzf-native-max-line-length)
(defvar fzf-native-async-cache-size)
(defvar marginalia-annotate-file)
(defvar marginalia-annotator-registry)
(defvar marginalia-command-categories)
(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function icomplete-exhibit "icomplete")
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(defvar ivy-text)
(defvar ivy--index)
(defvar ivy--all-candidates)
(defvar ivy-count-format)
(defvar ivy-completing-read-dynamic-collection)
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")
(defvar ivy-pre-prompt-function)
(defvar helm-alive-p)
(defvar helm-pattern)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")
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
Expands to nothing when disabled — zero runtime cost."
  (when (bound-and-true-p fzfa-debug) `(message ,fmt ,@args)))

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
  '(ag chrome company emacs evil fd find flymake git grep hg hungry info
       locate mail make music notmuch pass project rg shell spotlight ugrep)
  "List of fzfa extensions to load from `fzfa-setup'.
Each SYMBOL causes `fzfa-setup' to `require' the feature
`fzfa-SYMBOL' and, if defined, call `fzfa-SYMBOL-setup'."
  :type '(set (const :tag "ag (the_silver_searcher)" ag)
              (const :tag "Chrome bookmarks + passwords" chrome)
              (const :tag "company-mode completions" company)
              (const :tag "Emacs built-in sources" emacs)
              (const :tag "Evil-mode marks + registers" evil)
              (const :tag "fd (find alternative)" fd)
              (const :tag "POSIX find" find)
              (const :tag "Flymake diagnostics" flymake)
              (const :tag "Git" git)
              (const :tag "POSIX grep" grep)
              (const :tag "Mercurial (hg)" hg)
              (const :tag "Hungry (buffer-derived dirs)" hungry)
              (const :tag "Info manuals" info)
              (const :tag "locate" locate)
              (const :tag "macOS Mail.app" mail)
              (const :tag "make / ninja targets" make)
              (const :tag "macOS Music.app" music)
              (const :tag "notmuch mail search" notmuch)
              (const :tag "password-store (pass)" pass)
              (const :tag "project.el" project)
              (const :tag "ripgrep (rg)" rg)
              (const :tag "Shell command + history" shell)
              (const :tag "macOS Spotlight (mdfind)" spotlight)
              (const :tag "ugrep" ugrep))
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

(defun fzfa--frontend-index ()
  "Return the active completion UI's selection index (0-based), or nil.
Returns nil for frontends that do not expose a selection index (e.g. icomplete)."
  (cond
   ((bound-and-true-p vertico-mode) (max 0 vertico--index))
   ((bound-and-true-p ivy-mode) (and (boundp 'ivy--index) (max 0 ivy--index)))
   (t nil)))

(defun fzfa--frontend-candidate ()
  "Return the currently highlighted candidate string in the active UI, or nil.
Used to implement live preview (e.g. `fzfa-theme')."
  (cond
   ((bound-and-true-p vertico-mode)
    (when (and (boundp 'vertico--candidates) vertico--candidates)
      (nth (max 0 vertico--index) vertico--candidates)))
   ((bound-and-true-p ivy-mode)
    (when (and (boundp 'ivy--all-candidates) ivy--all-candidates)
      (nth (max 0 ivy--index) ivy--all-candidates)))
   ((bound-and-true-p icomplete-mode)
    (car (completion-all-sorted-completions)))))

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
        (icomplete-exhibit))))))

(defun fzfa--minibuffer-format-reset ()
  "Disable frontend count formats in the active minibuffer.
Called from a `minibuffer-with-setup-hook' lambda so that vertico's
`vertico-count-format' and icomplete's `icomplete-matches-format' don't
overwrite fzfa's own stats overlay / pre-prompt text.  Ivy is handled
separately via `ivy-count-format' bound at the call site.  No-ops when
the target package isn't loaded."
  (when (boundp 'vertico-count-format)
    (setq-local vertico-count-format nil))
  (when (boundp 'icomplete-matches-format)
    (setq-local icomplete-matches-format nil)))

;;; Completing-read helpers

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

STR is the string the collection function was called with; some
frontends pass an empty STR even when the minibuffer holds a real
query, so fall back to the active minibuffer's contents.

Returns the empty string otherwise."
  (or (if (not (string-empty-p str))
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
\(including nil HANDLES) are ignored.  Stops are batched into a single
idle timer; the closure retains the live-handle list so it survives the
caller's unwind.

The C-side destroy does pthread_join on the scoring thread
\(uninterruptible snapshot/score work for huge pools) and frees the
candidate arena — easily hundreds of ms for a `find ~'-scale session.
None of it is needed before minibuffer dismissal, so deferring this
lets ESC return instantly.  An idle timer (rather than `run-at-time' 0)
ensures the join is wedged between user keystrokes only when the user
has actually paused — keeping the pthread_join out of the user's typing
rhythm in trade for holding the arena slightly longer."
  (let ((live (cond
               ((null handles) nil)
               ((vectorp handles)
                (cl-loop for h across handles when h collect h))
               ((listp handles) (delq nil (copy-sequence handles)))
               (t (list handles)))))
    (when live
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (dolist (h live) (fzf-native-async-stop h)))))))

(cl-defun fzfa--completion-metadata (category &key annotate affix group)
  "Return the `metadata' alist for fzfa's `completing-read' collection lambdas.

CATEGORY is the completion category symbol.  Optional ANNOTATE / AFFIX /
GROUP attach `annotation-function', `affixation-function', and
`group-function' when non-nil.  `display-sort-function' and
`cycle-sort-function' are pinned to `identity' so frontends preserve the
order produced by the C scorer."
  `(metadata
    (category . ,category)
    (display-sort-function . identity)
    (cycle-sort-function . identity)
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

(defun fzfa--deduplicate-dirs (dirs)
  "Remove duplicates and subdirectory entries from DIRS.
If directory A is a prefix of directory B, B is dropped — A's recursive
search already covers it.  Exception: B is kept when it is itself a git
root (contains a .git entry), since rg honors per-repo gitignores and a
descend from A may exclude files the user expects to search."
  (let ((unique (cl-delete-duplicates dirs :test #'string=)))
    (cl-loop for dir in unique
             unless (and (not (file-exists-p (expand-file-name ".git" dir)))
                         (cl-some (lambda (other)
                                    (and (not (string= dir other))
                                         (string-prefix-p other dir)))
                                  unique))
             collect dir)))

;;; Async `completing-read'

(cl-defun fzfa--helm-completing-read (&key prompt command directory
                                           skip-executable-check)
  "Helm path for `fzfa-async-completing-read'.
PROMPT is shown in the minibuffer.  COMMAND is the producer shell command run
in DIRECTORY.  SKIP-EXECUTABLE-CHECK bypasses the `executable-find' guard.
Starts an fzf-native async session and opens a helm buffer driven by a
`helm-source-sync' with `:match-dynamic t' so helm never re-filters the
already-scored candidates.  A timer polls the C-side generation counter and
calls `helm-force-update' when new results arrive.
Returns the selected candidate string, or nil on cancel."
  (unless skip-executable-check
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (require 'helm)
  (require 'helm-source)
  (let* ((prompt  (or prompt
                      (when command
                        (concat (car (split-string command nil t)) ": "))))
         (dir     (expand-file-name (or directory default-directory)))
         (handle  (fzf-native-async-start command dir))
         (limit   (fzfa--candidate-limit))
         (last-gen -1)
         (stopped  nil)
         (result   nil)
         (cleanup  (lambda ()
                     (unless stopped
                       (setq stopped t)
                       (fzf-native-async-stop handle))))
         timer)
    (setq timer
          (run-with-timer
           0 fzfa-refresh-delay
           (lambda ()
             (when helm-alive-p
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update)))))))
    (unwind-protect
        (let ((default-directory dir))
          (helm
           :sources
           (helm-make-source
            (or prompt "fzfa") 'helm-source-sync
            :header-name
            (lambda (name)
              (format "%s [%s]" name (abbreviate-file-name dir)))
            :candidates
            (lambda ()
              ;; case-mode and other defcustoms are bridged onto the
              ;; canonical fzf-native names by :around advice on
              ;; `fzf-native-async-candidates' (see EOF).
              (fzf-native-async-candidates handle helm-pattern limit))
            :match-dynamic t
            :nohighlight t
            :candidate-number-limit (or limit 10000)
            :cleanup cleanup
            :action (lambda (cand) (setq result cand)))
           :buffer "*helm fzfa*"))
      (cancel-timer timer)
      (funcall cleanup))
    result))

;;;###autoload
(cl-defun fzfa-async-completing-read (&key
                                      prompt
                                      command
                                      (directory (fzfa--default-dir))
                                      (category 'fzfa-file)
                                      group
                                      (resolve-paths t)
                                      skip-executable-check)
  "Run shell COMMAND with asynchronous `completing-read'.

:PROMPT                 Minibuffer prompt.  Derived from the first token of
                        COMMAND (e.g. \"find: \" for \"find .\") when omitted.
:COMMAND                Shell command whose stdout lines become candidates.
:DIRECTORY              Working directory for COMMAND.  Defaults to
                        `fzfa--default-dir' (respects
                        `fzfa-project-backend').
:CATEGORY               Completion category symbol.  Defaults to
                        `fzfa-file' (most async commands return file
                        paths).  Pass `fzfa-grep' for FILE:LINE:CONTENT
                        candidates, `fzfa-misc' for non-file output, etc.
:RESOLVE-PATHS          When non-nil (the default), the returned
                        candidate is passed through `expand-file-name'
                        against :DIRECTORY before being handed back to the
                        caller.  Lets file and grep commands stay agnostic
                        of the caller's `default-directory'.  Pass nil for
                        commands that return non-path output (e.g. shell
                        output where the raw text matters).
:SKIP-EXECUTABLE-CHECK  When non-nil, skip the `executable-find' guard on
                        the first token of COMMAND.

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
                   :resolve-paths resolve-paths)))
     ((eq (car-safe fzfa--multi-mode) :inject)
      ;; One-shot consume: mutate the outer action's `let' cell so the
      ;; rest of the caller's body (and any nested fzfa calls) run with
      ;; multi-mode = nil instead of replaying our inject value.
      (let ((cand (cdr fzfa--multi-mode)))
        (setq fzfa--multi-mode nil)
        (cl-return-from fzfa-async-completing-read
          (fzfa--maybe-expand cand directory resolve-paths)))))
    (when (bound-and-true-p helm-mode)
      (cl-return-from fzfa-async-completing-read
        (fzfa--maybe-expand
         (fzfa--helm-completing-read
          :prompt prompt :command command :directory directory
          :skip-executable-check skip-executable-check)
         directory resolve-paths)))
    (let* ((handle (fzf-native-async-start command (expand-file-name directory)))
           (dir (abbreviate-file-name directory))
           (last-gen -1)
           (last-result nil)
           (last-query nil)
           (stats-overlay nil)
           (last-filtered 0)
           (last-total 0)
           (limit (fzfa--candidate-limit))
           (last-exhibit-scheduled 0.0)
           (refresh-overlay
            (lambda ()
              (when (and stats-overlay (active-minibuffer-window))
                (with-selected-window (active-minibuffer-window)
                  (let* ((idx (fzfa--frontend-index))
                         (text (fzfa--format-stats
                                (concat prompt dir " ")
                                idx last-filtered last-total)))
                    (fzfa--log "DEBUG: %s" text)
                    (overlay-put stats-overlay 'display text))))))
           ;; Ivy push path: score the current query and push into
           ;; `ivy--all-candidates' directly. Used instead of
           ;; `fzfa--frontend-exhibit' for ivy because
           ;; ivy does not re-call the collection lambda on timer ticks.
           (ivy-push
            (lambda ()
              (when (and handle (active-minibuffer-window))
                (when-let* ((query (and (boundp 'ivy-text) ivy-text)))
                  (let ((cands (while-no-input
                                 (fzf-native-async-candidates
                                  handle query limit))))
                    (unless (eq cands t)
                      (when-let* ((stats (fzf-native-async-stats handle)))
                        (setq last-filtered (car stats)
                              last-total    (cdr stats)))
                      (when (fzfa--async-final-p cands handle query)
                        (setq last-query  query
                              last-result cands)
                        (ivy--set-candidates cands)
                        (ivy--exhibit))))))))
           retry-timer
           timer)
      (setq timer
            (run-with-timer
             0 fzfa-refresh-delay
             (lambda ()
               (when handle
                 (let ((gen (fzf-native-async-generation handle)))
                   (when (and gen (not (= gen last-gen)) (not (input-pending-p)))
                     (when (>= (- (float-time) last-exhibit-scheduled)
                               fzfa-input-throttle)
                       (setq last-gen gen)
                       (setq last-exhibit-scheduled (float-time))
                       (run-with-idle-timer
                        0 nil
                        (if (bound-and-true-p ivy-mode)
                            ivy-push
                          #'fzfa--frontend-exhibit)))))))))
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
                 (fzfa--minibuffer-format-reset))
             (let ((ivy-completing-read-dynamic-collection t)
                   (ivy-count-format
                    (when (bound-and-true-p ivy-mode) ""))
                   (ivy-pre-prompt-function
                    (when (bound-and-true-p ivy-mode)
                      (lambda ()
                        (fzfa--format-stats (concat dir " ")
                                            (fzfa--frontend-index)
                                            last-filtered last-total)))))
               (completing-read
                prompt
                (lambda (str _pred action)
                  (pcase action
                    ('metadata (fzfa--completion-metadata category :group group))
                    ;; Treat the whole input as one field; prevents space-splitting.
                    (`(boundaries . ,_) (cons 0 0))
                    ('t (let* ((query (fzfa--current-query str))
                               (r (while-no-input
                                    (fzf-native-async-candidates handle query limit))))
                          (if (eq r t)
                              ;; Scoring was interrupted by pending input.
                              ;; Debounce a retry so the display self-heals once
                              ;; the user pauses typing.
                              (progn
                                (when retry-timer (cancel-timer retry-timer))
                                (setq retry-timer
                                      (run-with-idle-timer
                                       fzfa-input-debounce nil
                                       (lambda ()
                                         (setq retry-timer nil)
                                         (if (bound-and-true-p ivy-mode)
                                             (funcall ivy-push)
                                           (fzfa--frontend-exhibit))))))
                            (when-let* ((stats (fzf-native-async-stats handle)))
                              (setq last-filtered (car stats)
                                    last-total    (cdr stats)))
                            (unless (bound-and-true-p ivy-mode)
                              (when-let* ((win (active-minibuffer-window)))
                                (with-selected-window win
                                  (unless stats-overlay
                                    (setq stats-overlay
                                          (make-overlay (point-min) (minibuffer-prompt-end))))
                                  (funcall refresh-overlay))))
                            (when (fzfa--async-final-p r handle query)
                              (setq last-query query
                                    last-result r)))
                          (when (equal query last-query) last-result)))
                    (_ t))))))
         (cancel-timer timer)
         (when retry-timer (cancel-timer retry-timer))
         (remove-hook 'post-command-hook refresh-overlay)
         (when stats-overlay (delete-overlay stats-overlay))
         (fzfa--defer-async-stop handle))
       directory resolve-paths))))

;;; Two-pass (consult-style) `completing-read'

(defcustom fzfa-2pass-split-style 'perl
  "Splitting style for `fzfa-2pass-completing-read'.
See `fzfa-2pass-split-styles-alist' for available styles."
  :type '(choice (const :tag "Perl-style (#cmd#filter)" perl))
  :group 'fzfa)

(defcustom fzfa-2pass-split-styles-alist
  `((perl :initial ?# :function ,#'fzfa--2pass-split-perl))
  "Splitting styles for `fzfa-2pass-completing-read'.
Each entry is (SYMBOL . PLIST).  Recognized PLIST keys:
  :function  (STR PLIST) -> (CMD . FILTER).  Required.
             CMD is the `shell-command' portion (re-runs on change);
             FILTER is passed to fzf-native for scoring.
  :initial   Optional character inserted at minibuffer setup so the
             user can start typing inside the delimited region.
  :separator Optional character; consumed by separator-based splitters."
  :type '(alist :key-type symbol :value-type plist)
  :group 'fzfa)

(defun fzfa--2pass-split-perl (str &optional _plist)
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

;;;###autoload
(cl-defun fzfa-2pass-completing-read
    (&key prompt
          (directory (fzfa--default-dir))
          (category 'fzfa-misc)
          group
          initial-input
          (resolve-paths nil)
          (split-style nil))
  "Two-pass (consult-style) async `completing-read'.

Input is split into a shell-CMD part and an fzf-FILTER part via
`fzfa-2pass-split-style' (or :SPLIT-STYLE override).  With the
default `perl' style the prompt has shape \"#CMD#FILTER\" (the
leading `#' is inserted automatically when no :INITIAL-INPUT is
supplied).  Changing CMD restarts the underlying process;
changing FILTER rescores in place via fzf-native.

:PROMPT         Minibuffer prompt.  Defaults to \"fzfa-2pass: \".
:DIRECTORY      Working directory for CMD.
:CATEGORY       Completion category (e.g. `fzfa-file', `fzfa-grep').
:GROUP          Optional grouping function.
:INITIAL-INPUT  Optional initial minibuffer contents.  Either a string
                or a cons (TEXT . POSITION); POSITION is a 0-based
                offset within TEXT at which point is placed.
:RESOLVE-PATHS  When non-nil, the returned candidate is passed through
                `expand-file-name' against :DIRECTORY.  Off by default
                since the shell command's output is often free-form.
:SPLIT-STYLE    Override `fzfa-2pass-split-style' for this call."
  (fzfa--ensure-setup)
  (cond
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted
           (list :prompt prompt :directory directory
                 :category category :group group
                 :initial-input initial-input
                 :resolve-paths resolve-paths
                 :split-style split-style
                 :2pass t)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (let ((cand (cdr fzfa--multi-mode)))
      (setq fzfa--multi-mode nil)
      (cl-return-from fzfa-2pass-completing-read
        (fzfa--maybe-expand cand directory resolve-paths)))))
  (when (bound-and-true-p helm-mode)
    (user-error "fzfa-2pass-completing-read does not yet support helm-mode"))
  (let* ((prompt (or prompt "fzfa-2pass: "))
         (dir (expand-file-name directory))
         (dir-abbrev (abbreviate-file-name directory))
         (style-sym (or split-style fzfa-2pass-split-style 'perl))
         (style (or (alist-get style-sym fzfa-2pass-split-styles-alist)
                    (user-error "Unknown fzfa-2pass split style: %s" style-sym)))
         (initial-char (plist-get style :initial))
         (init-text (if (consp initial-input) (car initial-input) initial-input))
         (init-point (and (consp initial-input) (cdr initial-input)))
         (splitter (plist-get style :function))
         (limit (fzfa--candidate-limit))
         (handle nil)
         (current-cmd nil)
         (last-gen -1)
         (last-result nil)
         (last-filtered 0)
         (last-total 0)
         (last-exhibit-scheduled 0.0)
         (last-restart-time 0.0)
         (pending-cmd nil)
         (stats-overlay nil)
         restart-timer retry-timer poll-timer
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (overlay-put
                 stats-overlay 'display
                 (fzfa--format-stats (concat prompt dir-abbrev " ")
                                     (fzfa--frontend-index)
                                     last-filtered last-total))))))
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
            (fzfa--frontend-exhibit)))
         (table
          (lambda (str _pred action)
            (pcase action
              ('metadata (fzfa--completion-metadata category :group group))
              (`(boundaries . ,_) (cons 0 0))
              ('t
               (let* ((input (fzfa--current-query str))
                      (split (funcall splitter input style))
                      (cmd (car split))
                      (query (cdr split)))
                 (cond
                  ((not (equal cmd current-cmd))
                   ;; Rate-limited restart: leading edge fires immediately
                   ;; when the throttle window has elapsed, trailing edge
                   ;; fires the latest pending cmd after the window.
                   (setq pending-cmd cmd)
                   (let* ((now     (float-time))
                          (elapsed (- now last-restart-time)))
                     (cond
                      ((>= elapsed fzfa-input-throttle)
                       (when restart-timer
                         (cancel-timer restart-timer)
                         (setq restart-timer nil))
                       (funcall do-restart cmd))
                      ((null restart-timer)
                       (setq restart-timer
                             (run-with-timer
                              (max 0.01
                                   (- fzfa-input-throttle elapsed))
                              nil
                              (lambda ()
                                (setq restart-timer nil)
                                (funcall do-restart pending-cmd)))))))
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
                                (fzfa--frontend-exhibit))))
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
    (when init-text
      (let ((cmd (car (funcall splitter init-text style))))
        (when (and cmd (not (string-empty-p cmd)))
          (funcall do-restart cmd))))
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
                    0 nil #'fzfa--frontend-exhibit)))))))
    (add-hook 'post-command-hook refresh-overlay)
    (sit-for fzfa-refresh-delay)
    (fzfa--maybe-expand
     (unwind-protect
         (minibuffer-with-setup-hook
             (lambda ()
               (setq-local default-directory directory)
               ;; Auto-insert the separator only when no init-text was
               ;; supplied; otherwise the caller is responsible for the
               ;; prefix.
               (when (and initial-char (null init-text))
                 (save-excursion
                   (goto-char (minibuffer-prompt-end))
                   (unless (equal initial-char (char-after))
                     (insert (char-to-string initial-char)))))
               (when init-point
                 (let ((p init-point))
                   (run-at-time
                    0 nil
                    (lambda ()
                      (when-let* ((win (active-minibuffer-window)))
                        (with-selected-window win
                          (goto-char (+ (minibuffer-prompt-end) p))))))))
               (fzfa--minibuffer-format-reset))
           (completing-read prompt table nil nil init-text nil))
       (when poll-timer (cancel-timer poll-timer))
       (when retry-timer (cancel-timer retry-timer))
       (when restart-timer (cancel-timer restart-timer))
       (remove-hook 'post-command-hook refresh-overlay)
       (when stats-overlay (delete-overlay stats-overlay))
       (fzfa--defer-async-stop handle))
     directory resolve-paths)))

(defcustom fzfa-2p-functions
  '(fzfa-ag-2p
    fzfa-ag-files-2p
    fzfa-fd-2p
    fzfa-find-2p
    fzfa-git-grep-2p
    fzfa-grep-2p
    fzfa-locate-2p
    fzfa-rg-2p
    fzfa-ugrep-2p)
  "Two-pass (consult-style) command variants to enable.
Each entry is a symbol of the form `fzfa-COMMAND-2p'.  When the
matching extension file is loaded, its top-level guard checks this
list and calls `fzfa-2p-define' for opted-in entries only.

Defining the variant is the user's opt-in; making it callable from
a cold `M-x' before the extension file has been loaded additionally
requires an `autoload' form in the user's init (see the README)."
  :type '(repeat symbol)
  :group 'fzfa)

(defun fzfa--2pass-extract-args (cmd)
  "Run CMD in `:extract' mode and return its keyword args plist.
Returns nil if CMD does not flow through a fzfa completing-read."
  (catch 'fzfa-extracted
    (let ((fzfa--multi-mode :extract))
      (funcall cmd))
    nil))

(defun fzfa--2pass-initial-input (shell-cmd separator)
  "Build (TEXT . CURSOR-POS) initial input from SHELL-CMD using SEPARATOR.
SEPARATOR is the leading char (e.g. ?#).  When SHELL-CMD ends in an
empty-quote pair (\"''\" or `\"\"'), point lands between the quotes;
otherwise it lands after the trailing separator so the user can start
typing a filter immediately."
  (let* ((sep (char-to-string separator))
         (text (concat sep shell-cmd sep)))
    (cons text
          (cond
           ((or (string-suffix-p "''" shell-cmd)
                (string-suffix-p "\"\"" shell-cmd))
            (- (length text) 2))
           (t (length text))))))

(defun fzfa--2pass-dispatch (cmd)
  "Run CMD in two-pass mode.
Extracts CMD's async args, pre-fills the minibuffer with its shell
command, and routes the selection back to CMD via `:inject' so its
post-action (e.g. `find-file', grep-jump) still runs."
  (let ((args (fzfa--2pass-extract-args cmd)))
    (unless args
      (user-error "`%s' is not a fzfa command" cmd))
    (when (plist-get args :2pass)
      (user-error "`%s' is already a two-pass command" cmd))
    (let ((shell-cmd (plist-get args :command)))
      (unless shell-cmd
        (user-error "`%s' is not an async (`:command') fzfa command — \
two-pass only wraps async producers" cmd))
      (let* ((style-sym (or fzfa-2pass-split-style 'perl))
             (style (alist-get style-sym fzfa-2pass-split-styles-alist))
             (sep (or (plist-get style :initial) ?#))
             (initial (fzfa--2pass-initial-input shell-cmd sep))
             (result (fzfa-2pass-completing-read
                      :prompt (plist-get args :prompt)
                      :directory (plist-get args :directory)
                      :category (plist-get args :category)
                      :group (plist-get args :group)
                      :initial-input initial
                      :resolve-paths nil)))
        (when result
          (let ((fzfa--multi-mode (cons :inject result)))
            (funcall cmd)))))))

(defun fzfa-2p-define (cmd)
  "Define CMD-2p as the two-pass variant of CMD.
Intended for use inside an extension file, gated on membership of the
generated symbol in `fzfa-2p-functions'.  Returns the new symbol."
  (let ((name (intern (concat (symbol-name cmd) "-2p"))))
    (defalias name
      (lambda ()
        (interactive)
        (fzfa--2pass-dispatch cmd))
      (format "Two-pass (consult-style) variant of `%s'.
Edit the command portion of the minibuffer (between the leading and
trailing separators) to re-run the underlying producer; type after
the trailing separator to fuzzy-filter via fzf-native.  Selection is
routed to `%s' so its post-action runs."
              cmd cmd))
    name))

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
                                     default)
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
            submits empty input; also seeded into history."
  (fzfa--ensure-setup)
  (cond
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted
           ;; Translate :candidates → :items so multi consumes one key.
           (list :items candidates :prompt prompt :category category
                 :annotate annotate :affix affix :group group
                 :history history)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (let ((cand (cdr fzfa--multi-mode)))
      (setq fzfa--multi-mode nil)
      (cl-return-from fzfa-sync-completing-read cand))))
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
             (if (string-empty-p query)
                 candidates
               (fzfa--bridge-defcustoms
                #'fzf-native-score-all candidates query))))))
   nil require-match nil history default))

;;; Multi-source `completing-read'

(defconst fzfa--tofu-base #x100000
  "Base Unicode Private Use Area codepoint for source-disambiguation suffixes.
Each multi source's candidates carry a single trailing codepoint at
`fzfa--tofu-base' + source-idx, propertized `display \"\"' so it renders
invisibly while making cross-source duplicates `string='-unique.
See consult's `consult--tofu-encode' for the same trick.")

(defvar fzfa--tofu-cache (make-hash-table :test 'eql)
  "Cache of propertized tofu suffix strings, keyed by source index.")

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
             omitted, the raw selection string is returned."
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
    (user-error "Fzfa--multi-read does not yet support helm-mode"))
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
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (overlay-put stats-overlay 'display
                             (fzfa--format-stats
                              prompt
                              (fzfa--frontend-index)
                              (cl-loop for x across filtered sum x)
                              (cl-loop for x across totals sum x)))))))
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
                          0 nil #'fzfa--frontend-exhibit)))))))
          (add-hook 'post-command-hook refresh-overlay)
          (sit-for fzfa-refresh-delay)
          (setq result
                (minibuffer-with-setup-hook
                    (lambda ()
                      (fzfa--minibuffer-format-reset)
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
                                nil 'local))
                  (completing-read
                   prompt
                   (lambda (str _pred action)
                     (pcase action
                       ('metadata
                        (fzfa--completion-metadata
                         'fzfa-multi
                         :group
                         (lambda (cand transform)
                           (let ((src (fzfa--multi-source-of
                                       cand sources-v cand->src)))
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
                                 (or (when-let* ((g (plist-get src :group)))
                                       (funcall g (fzfa--tofu-hide cand) t))
                                     cand)
                               (or (plist-get src :name) ""))))
                         :affix
                         (lambda (cands)
                           (let* ((displays
                                   (mapcar
                                    (lambda (c)
                                      (let* ((src (fzfa--multi-source-of
                                                   c sources-v cand->src))
                                             (g (and src (plist-get src :group))))
                                        (or (and g (funcall g (fzfa--tofu-hide c) t))
                                            c)))
                                    cands))
                                  (maxw (apply #'max 0
                                               (mapcar #'string-width
                                                       displays))))
                             (cl-mapcar
                              (lambda (cand display)
                                (let* ((src (fzfa--multi-source-of
                                             cand sources-v cand->src))
                                       (ann (and src (plist-get src :annotate)))
                                       (s   (and ann (funcall ann (fzfa--tofu-hide cand))))
                                       (pad (- (1+ maxw)
                                               (string-width display))))
                                  (list cand ""
                                        (if s
                                            (concat
                                             (make-string (max 1 pad) ?\s)
                                             s)
                                          ""))))
                              cands displays)))))
                       (`(boundaries . ,_) (cons 0 0))
                       ('lambda t)
                       ('t
                        (let ((query (fzfa--current-query str))
                              (interrupted nil))
                          (dotimes (i n)
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
                                ;; out may be nil (zero matches) — still valid.
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
                                 (t (aset filtered i (length out))))))))
                          (when interrupted
                            (when retry-timer (cancel-timer retry-timer))
                            (setq retry-timer
                                  (run-with-idle-timer
                                   fzfa-input-debounce nil
                                   (lambda ()
                                     (setq retry-timer nil)
                                     (fzfa--frontend-exhibit)))))
                          (when-let* ((win (active-minibuffer-window)))
                            (with-selected-window win
                              (unless stats-overlay
                                (setq stats-overlay
                                      (make-overlay (point-min)
                                                    (minibuffer-prompt-end))))
                              (funcall refresh-overlay)))
                          (let* ((order (number-sequence 0 (1- n)))
                                 ;; `sort' is stable since Emacs 25, so equal
                                 ;; ranks preserve declared source order.
                                 (sorted
                                  (if (string-empty-p query)
                                      order
                                    (sort order
                                          (lambda (a b)
                                            (> (aref rank a)
                                               (aref rank b)))))))
                            (apply #'append
                                   (mapcar (lambda (i) (aref last-results i))
                                           sorted)))))
                       (_ t)))
                   nil t))))
      (when timer (cancel-timer timer))
      (when retry-timer (cancel-timer retry-timer))
      (remove-hook 'post-command-hook refresh-overlay)
      (when stats-overlay (delete-overlay stats-overlay))
      (fzfa--defer-async-stop handles))
    (when result
      (let* ((src    (or (and selected-idx (aref sources-v selected-idx))
                         (fzfa--multi-source-of
                          result sources-v cand->src)))
             (action (and src (plist-get src :action)))
             (clean  (fzfa--tofu-hide result)))
        (if action (funcall action clean) clean)))))

;;;###autoload
(defun fzfa-multi-read (commands &rest options)
  "Run a multi-source completing-read over COMMANDS.
Each command in COMMANDS is funcalled twice per multi session — once in
`:extract' mode (capture keyword args, abort), once in `:inject' mode after
the user picks (so the command's post-action runs).  OPTIONS is forwarded
to `fzfa--multi-read'.  Commands whose body does not reach
`fzfa-async-completing-read' or `fzfa-sync-completing-read' are skipped.
Commands must be arg-less (no interactive `read-*' prompts in their body).

Composes: if a command in COMMANDS itself calls `fzfa--multi-read'
\(e.g. `fzfa-find-any'), its inner sources are flattened in alongside
the other commands' sources, with each inner source keeping its own
:action."
  (fzfa--ensure-setup)
  (let* ((source-lists
          (mapcar
           (lambda (cmd)
             (let ((args (condition-case nil
                             (catch 'fzfa-extracted
                               (let ((fzfa--multi-mode :extract))
                                 (funcall cmd))
                               nil)
                           (error nil))))
               (when args
                 (if-let* ((nested (plist-get args :multi-sources)))
                     ;; Flatten: nested multi command's sources are
                     ;; already fully built with :action closures.
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
                                         (funcall cmd))))
                       args)))))))
           commands))
         (sources (apply #'append (delq nil source-lists))))
    (apply #'fzfa--multi-read sources options)))

(defcustom fzfa-find-any-commands
  '(fzfa-imenu
    fzfa-buffer
    fzfa-recent-file
    fzfa-hungry-find
    fzfa-imenu-all-but-current
    fzfa-M-x
    fzfa-hungry-swiper
    fzfa-locate)
  "Commands shown by `fzfa-find-any'."
  :type '(repeat function)
  :group 'fzfa)

(defcustom fzfa-find-some-commands
  '(fzfa-imenu
    fzfa-buffer
    fzfa-recent-file
    fzfa-find
    fzfa-M-x-for-buffer
    fzfa-rg)
  "Commands shown by `fzfa-find-some'."
  :type '(repeat function)
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
  "Open the SOURCE and jump to the LINE referenced by CAND.
Used both by the grep commands' selected-candidate handling and by the
embark default action for the `fzfa-grep' category."
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

;;; Setup

(defun fzfa--bridge-defcustoms (orig-fn &rest args)
  "Wrap fzf-native call ORIG-FN with ARGS; bridge fzfa-* into C scorer."
  (let ((fzf-native-async-highlight  fzfa-highlight)
        (fzf-native-max-line-length  fzfa-max-line-length)
        (fzf-native-async-cache-size fzfa-cache-size)
        (fzf-native-case-mode        fzfa-case-mode))
    (apply orig-fn args)))

(defun fzfa--pin-completion-styles (orig-fn &rest args)
  "Run ORIG-FN with ARGS, pinning `completion-styles' to `fzfa'.

`fzfa''s pre-scored async candidates must not be re-filtered by another
completion style.  `completion-category-overrides' alone is not enough:
`completion--styles' silently appends the global `completion-styles' to
any override, so a fzfa-style call that legitimately returns nil
\(warmup, in-flight scoring) falls through to whatever the user has
configured globally (e.g. `basic', `fussy', `flex', `orderless'), which
re-enters fzfa's table with substring inputs and corrupts the cmd
parse / restart logic."
  (let ((completion-styles '(fzfa)))
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

    (dolist (sym '(fzfa-async-completing-read
                   fzfa-sync-completing-read
                   fzfa-2pass-completing-read
                   fzfa-multi-read))
      (advice-add sym :around #'fzfa--pin-completion-styles))

    (with-eval-after-load 'embark
      (dolist (entry '((fzfa-file     . embark-file-map)
                       (fzfa-buffer   . embark-buffer-map)
                       (fzfa-bookmark . embark-bookmark-map)
                       (fzfa-grep     fzfa-grep-map embark-general-map)))
        (add-to-list 'embark-keymap-alist entry))
      (setf (alist-get 'fzfa-grep embark-default-action-overrides)
            #'fzfa--grep-jump))

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
