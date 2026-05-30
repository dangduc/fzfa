;;; fzfa.el --- Async fuzzy completion via `fzf-native' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (fzf-native "0.3"))
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
  '(ag chrome company emacs fd find git grep hg hungry
       locate mail music notmuch pass rg shell spotlight ugrep)
  "List of fzfa extensions to load from `fzfa-setup'.
Each SYMBOL causes `fzfa-setup' to `require' the feature
`fzfa-SYMBOL' and, if defined, call `fzfa-SYMBOL-setup'."
  :type '(set (const :tag "ag (the_silver_searcher)" ag)
              (const :tag "Chrome bookmarks + passwords" chrome)
              (const :tag "company-mode completions" company)
              (const :tag "Emacs built-in sources" emacs)
              (const :tag "fd (find alternative)" fd)
              (const :tag "POSIX find" find)
              (const :tag "Git" git)
              (const :tag "POSIX grep" grep)
              (const :tag "Mercurial (hg)" hg)
              (const :tag "Hungry (buffer-derived dirs)" hungry)
              (const :tag "locate" locate)
              (const :tag "macOS Mail.app" mail)
              (const :tag "macOS Music.app" music)
              (const :tag "notmuch mail search" notmuch)
              (const :tag "password-store (pass)" pass)
              (const :tag "ripgrep (rg)" rg)
              (const :tag "Shell command + history" shell)
              (const :tag "macOS Spotlight (mdfind)" spotlight)
              (const :tag "ugrep" ugrep))
  :group 'fzfa)

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

;;; Completion style

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

(defun fzfa--commas (n)
  "Format integer N with comma thousand-separators.

e.g., 1234567 → 1,234,567."
  (let ((s (number-to-string n))
        (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s   (substring s 0 -3)))
    (concat s out)))

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
         (limit   (and fzfa-max-candidates
                       (> fzfa-max-candidates 0)
                       fzfa-max-candidates))
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
      (cl-return-from fzfa-async-completing-read
        (fzfa--maybe-expand (cdr fzfa--multi-mode)
                                 directory resolve-paths))))
    (fzfa--check-completion-setup)
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
           (limit (and fzfa-max-candidates
                       (> fzfa-max-candidates 0)
                       fzfa-max-candidates))
           (last-exhibit-scheduled 0.0)
           (refresh-overlay
            (lambda ()
              (when (and stats-overlay (active-minibuffer-window))
                (with-selected-window (active-minibuffer-window)
                  (let ((idx (fzfa--frontend-index)))
                    (fzfa--log "DEBUG: %s%s %s[%d](%d) "
                                    prompt dir
                                    (if idx (format "%d/" (1+ idx)) "")
                                    last-filtered last-total)
                    (overlay-put stats-overlay 'display
                                 (if idx
                                     (format "%s%s %d/[%s](%s) "
                                             prompt dir (1+ idx)
                                             (fzfa--commas last-filtered)
                                             (fzfa--commas last-total))
                                   (format "%s%s [%s](%s) "
                                           prompt dir
                                           (fzfa--commas last-filtered)
                                           (fzfa--commas last-total)))))))))
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
                    (when (and cands (not (eq cands t)))
                      (when-let* ((stats (fzf-native-async-stats handle)))
                        (setq last-filtered (car stats)
                              last-total    (cdr stats)))
                      (setq last-query query
                            last-result cands)
                      (ivy--set-candidates cands)
                      (ivy--exhibit)))))))
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
                 ;; case-mode and other defcustoms are bridged onto the
                 ;; canonical fzf-native names by :around advice on
                 ;; `fzf-native-async-candidates' (see EOF), so no
                 ;; setq-local needed here for the async path.
                 (when (boundp 'vertico-count-format)
                   (setq-local vertico-count-format nil))
                 (when (boundp 'icomplete-matches-format)
                   (setq-local icomplete-matches-format nil)))
             (let ((ivy-completing-read-dynamic-collection t)
                   (ivy-count-format
                    (when (bound-and-true-p ivy-mode) ""))
                   (ivy-pre-prompt-function
                    (when (bound-and-true-p ivy-mode)
                      (lambda ()
                        (let ((idx (fzfa--frontend-index)))
                          (if idx
                              (format "%s %d/[%s](%s) "
                                      dir (1+ idx)
                                      (fzfa--commas last-filtered)
                                      (fzfa--commas last-total))
                            (format "%s [%s](%s) "
                                    dir
                                    (fzfa--commas last-filtered)
                                    (fzfa--commas last-total))))))))
               (completing-read
                prompt
                (lambda (str _pred action)
                  (pcase action
                    ('metadata `(metadata (category . ,category)
                                          (display-sort-function . identity)
                                          (cycle-sort-function . identity)
                                          ,@(when group `((group-function . ,group)))))
                    ;; Treat the whole input as one field; prevents space-splitting.
                    (`(boundaries . ,_) (cons 0 0))
                    ('t (let* (;; Str is sometimes empty when there's a valid query.
                               ;; Prefer str when non-empty to avoid calculations
                               ;; in the minibuffer but fall back if str is empty.
                               (query (if (not (string-empty-p str))
                                          str
                                        (when-let* ((win (active-minibuffer-window)))
                                          (with-current-buffer (window-buffer win)
                                            (minibuffer-contents-no-properties))))))
                          (if (null query)
                              last-result
                            (let ((r (while-no-input
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
                                (setq last-query query
                                      last-result r))
                              (when (equal query last-query) last-result)))))
                    (_ t))))))
         (cancel-timer timer)
         (when retry-timer (cancel-timer retry-timer))
         (remove-hook 'post-command-hook refresh-overlay)
         (when stats-overlay (delete-overlay stats-overlay))
         ;; Defer `fzf-native-async-stop' off the synchronous unwind path.
         ;; The C-side destroy does pthread_join on the scoring thread
         ;; (uninterruptible snapshot/score work for huge pools) and frees
         ;; the candidate arena — easily hundreds of ms for a `find ~'-scale
         ;; session.  None of it is needed before minibuffer dismissal, so
         ;; we let the user see ESC return instantly and clean up on the
         ;; next idle tick.
         (when handle
           (let ((h handle))
             (run-at-time 0 nil (lambda () (fzf-native-async-stop h))))))
       directory resolve-paths))))

(cl-defun fzfa-sync-completing-read (&key
                                    candidates
                                    (prompt "fzf > ")
                                    (category 'fzfa-misc)
                                    annotate
                                    affix
                                    group
                                    history)
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
            with \\[previous-history-element]."
  (fzfa--ensure-setup)
  (cond
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted
           ;; Translate :candidates → :items so multi consumes one key.
           (list :items candidates :prompt prompt :category category
                 :annotate annotate :affix affix :group group
                 :history history)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (cl-return-from fzfa-sync-completing-read
      (cdr fzfa--multi-mode))))
  (fzfa--check-completion-setup)
  (completing-read
   prompt
   (lambda (str _pred action)
     (pcase action
       ('metadata
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          ,@(when annotate `((annotation-function  . ,annotate)))
          ,@(when affix    `((affixation-function  . ,affix)))
          ,@(when group    `((group-function       . ,group)))))
       (`(boundaries . ,_) (cons 0 0))
       ('lambda t)
       ('t (let ((query (if (not (string-empty-p str))
                            str
                          (when-let* ((win (active-minibuffer-window)))
                            (with-current-buffer (window-buffer win)
                              (minibuffer-contents-no-properties))))))
             (if (or (null query) (string-empty-p query))
                 candidates
               (fzfa--bridge-defcustoms
                #'fzf-native-score-all candidates query))))))
   nil t nil history nil))

;;; Multi-source `completing-read'

(defun fzfa--multi-tag (cand idx hash)
  "Tag CAND with source IDX (text-prop + HASH lookup table); return CAND."
  (when (> (length cand) 0)
    (put-text-property 0 1 'fzfa-src-idx idx cand))
  (puthash cand idx hash)
  cand)

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
    (cl-return-from fzfa--multi-read (cdr fzfa--multi-mode))))
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
         (limit        (and fzfa-max-candidates
                            (> fzfa-max-candidates 0)
                            fzfa-max-candidates))
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
                (let* ((idx (fzfa--frontend-index))
                       (f   (fzfa--commas
                             (cl-loop for x across filtered sum x)))
                       (tot (fzfa--commas
                             (cl-loop for x across totals sum x)))
                       (text (if idx
                                 (format "%s%d/[%s](%s) "
                                         prompt (1+ idx) f tot)
                               (format "%s[%s](%s) "
                                       prompt f tot))))
                  (overlay-put stats-overlay 'display text))))))
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
                      (when (boundp 'vertico-count-format)
                        (setq-local vertico-count-format nil))
                      (when (boundp 'icomplete-matches-format)
                        (setq-local icomplete-matches-format nil))
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
                        `(metadata
                          (category . fzfa-multi)
                          (display-sort-function . identity)
                          (cycle-sort-function . identity)
                          (group-function
                           . ,(lambda (cand transform)
                                (let ((src (fzfa--multi-source-of
                                            cand sources-v cand->src)))
                                  (if transform
                                      ;; Per-source :group transform —
                                      ;; lets a source strip an internal
                                      ;; "IDX:" prefix or otherwise
                                      ;; reshape its display string while
                                      ;; keeping the raw value as the
                                      ;; lookup/match key.  Falls back to
                                      ;; the raw candidate when a source
                                      ;; has no :group function (or its
                                      ;; transform returns nil).
                                      (or (when-let* ((g (plist-get src :group)))
                                            (funcall g cand t))
                                          cand)
                                    (or (plist-get src :name) "")))))
                          (affixation-function
                           . ,(lambda (cands)
                                (let* ((displays
                                        (mapcar
                                         (lambda (c)
                                           (let* ((src (fzfa--multi-source-of
                                                        c sources-v cand->src))
                                                  (g (and src (plist-get src :group))))
                                             (or (and g (funcall g c t)) c)))
                                         cands))
                                       (maxw (apply #'max 0
                                                    (mapcar #'string-width
                                                            displays))))
                                  (cl-mapcar
                                   (lambda (cand display)
                                     (let* ((src (fzfa--multi-source-of
                                                  cand sources-v cand->src))
                                            (ann (and src (plist-get src :annotate)))
                                            (s   (and ann (funcall ann cand)))
                                            (pad (- (1+ maxw)
                                                    (string-width display))))
                                       (list cand ""
                                             (if s
                                                 (concat
                                                  (make-string (max 1 pad) ?\s)
                                                  s)
                                               ""))))
                                   cands displays))))))
                       (`(boundaries . ,_) (cons 0 0))
                       ('lambda t)
                       ('t
                        (let* ((query
                                (if (not (string-empty-p str))
                                    str
                                  (or (when-let* ((win (active-minibuffer-window)))
                                        (with-current-buffer (window-buffer win)
                                          (minibuffer-contents-no-properties)))
                                      "")))
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
                               (t
                                ;; Async returns fresh strings each call;
                                ;; re-tag them so group/action lookup works.
                                ;; out may be nil (zero matches) — still valid.
                                (when h
                                  (dolist (c out)
                                    (fzfa--multi-tag c i cand->src)))
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
      ;; Defer the async-stops so ESC returns instantly — see the same
      ;; comment in `fzfa-async-completing-read'.  Stops are scheduled
      ;; together so the runtime can decide its own scheduling, and the
      ;; closure owns the handle vector to keep it alive across the gap.
      (let ((live nil))
        (dotimes (i n)
          (when-let* ((h (aref handles i)))
            (push h live)))
        (when live
          (run-at-time 0 nil
                       (lambda ()
                         (dolist (h live) (fzf-native-async-stop h)))))))
    (when result
      (let* ((src    (or (and selected-idx (aref sources-v selected-idx))
                         (fzfa--multi-source-of
                          result sources-v cand->src)))
             (action (and src (plist-get src :action))))
        (if action (funcall action result) result)))))

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
    fzfa-hungry-swiper)
  "Commands shown by `fzfa-find-any'."
  :type '(repeat function)
  :group 'fzfa)

(defcustom fzfa-find-some-commands
  '(fzfa-imenu
    fzfa-buffer
    fzfa-recent-file
    fzfa-find
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

;;; Commands

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

;;; Helpers

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

;;; Setup

(defconst fzfa--categories
  '(fzfa-misc
    fzfa-file
    fzfa-buffer
    fzfa-grep
    fzfa-bookmark
    fzfa-theme
    fzfa-imenu
    fzfa-multi)
  "Completion categories owned by fzfa.
Each is registered in `completion-category-overrides' so the
pre-scored passthrough style runs instead of style re-filtering.")

(defun fzfa--check-completion-setup ()
  "Signal a user-error if `fzfa' has been added to global `completion-styles'.
That applies the passthrough style to every `completing-read', including
ones that pass a plain list/hash-table — which the style errors on.
fzfa wires itself in via `completion-category-overrides' only;
`fzfa--ensure-setup' guarantees that, so the per-category check
that used to live here is no longer needed."
  (when (memq 'fzfa completion-styles)
    (user-error
     "Fzfa must not be in `completion-styles' globally (it is).  \
Remove it; fzfa wires itself in via `completion-category-overrides'")))

(defun fzfa--bridge-defcustoms (orig-fn &rest args)
  "Wrap fzf-native call ORIG-FN with ARGS; bridge fzfa-* into C scorer."
  (let ((fzf-native-async-highlight  fzfa-highlight)
        (fzf-native-max-line-length  fzfa-max-line-length)
        (fzf-native-async-cache-size fzfa-cache-size)
        (fzf-native-case-mode        fzfa-case-mode))
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
    (add-to-list 'completion-styles-alist
                 '(fzfa
                   fzfa-try-completion fzfa-all-completions
                   "Passthrough style for pre-scored async fzf completions."))

    (advice-add 'fzf-native-async-start      :around #'fzfa--bridge-defcustoms)
    (advice-add 'fzf-native-async-candidates :around #'fzfa--bridge-defcustoms)

    ;; Register each fzfa category with the passthrough style so other
    ;; styles (e.g. fussy on `file', `basic' on multi) don't re-filter our
    ;; pre-scored candidates or cache them client-side past the first call.
    (dolist (cat fzfa--categories)
      (add-to-list 'completion-category-overrides `(,cat (styles fzfa))))

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
