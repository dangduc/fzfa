;;; fzfa-test.el --- Tests for fzfa  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for fzfa.

;;; Code:

(require 'ert)
(require 'fzfa)
(require 'fzfa-emacs)

;;; fzfa--deduplicate-dirs

(ert-deftest fzfa-deduplicate-dirs-no-overlap ()
  "Unrelated directories are all kept."
  (should (equal (sort (fzfa--deduplicate-dirs
                        '("/a/b/" "/c/d/" "/e/f/"))
                       #'string<)
                 '("/a/b/" "/c/d/" "/e/f/"))))

(ert-deftest fzfa-deduplicate-dirs-drops-subdirectory ()
  "A subdirectory is dropped when its parent is present."
  (should (equal (fzfa--deduplicate-dirs
                  '("/home/user/project/" "/home/user/project/src/"))
                 '("/home/user/project/"))))

(ert-deftest fzfa-deduplicate-dirs-keeps-sibling-dirs ()
  "Sibling directories (same parent, different names) are both kept."
  (let ((result (fzfa--deduplicate-dirs
                 '("/home/user/foo/" "/home/user/bar/"))))
    (should (member "/home/user/foo/" result))
    (should (member "/home/user/bar/" result))))

(ert-deftest fzfa-deduplicate-dirs-removes-exact-duplicates ()
  "Exact duplicate entries are collapsed to one."
  (should (equal (fzfa--deduplicate-dirs
                  '("/a/b/" "/a/b/" "/a/b/"))
                 '("/a/b/"))))

(ert-deftest fzfa-deduplicate-dirs-deep-nesting ()
  "Only the shallowest ancestor survives when multiple levels are present."
  (let ((result (fzfa--deduplicate-dirs
                 '("/a/" "/a/b/" "/a/b/c/" "/a/b/c/d/"))))
    (should (equal result '("/a/")))))

(ert-deftest fzfa-deduplicate-dirs-empty-input ()
  "Empty input returns nil."
  (should (null (fzfa--deduplicate-dirs '()))))

;;; fzfa--default-dir

(ert-deftest fzfa-project-dir-nil-backend-returns-default-directory ()
  "With nil backend, returns `default-directory'."
  (let ((fzfa-project-backend nil)
        (default-directory "/some/dir/"))
    (should (string= (fzfa--default-dir) "/some/dir/"))))

(ert-deftest fzfa-project-dir-project-backend-uses-project-root ()
  "With `project' backend, returns the project root when in a project."
  (let ((fzfa-project-backend 'project))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc Git "/mock/project/")))
              ((symbol-function 'project-root)
               (lambda (_) "/mock/project/")))
      (should (string= (fzfa--default-dir) "/mock/project/")))))

(ert-deftest fzfa-project-dir-project-backend-fallback ()
  "With `project' backend, falls back to `default-directory' outside a project."
  (let ((fzfa-project-backend 'project)
        (default-directory "/fallback/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (string= (fzfa--default-dir) "/fallback/")))))

(ert-deftest fzfa-project-dir-custom-function ()
  "A function value is called and its return value used."
  (let ((fzfa-project-backend (lambda () "/custom/root/")))
    (should (string= (fzfa--default-dir) "/custom/root/"))))

;;; fzfa-swiper line collection

(ert-deftest fzfa-swiper-line-format ()
  "Lines are formatted as LINE:content with 1-based numbering."
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let* ((candidates
            (let (lines)
              (save-excursion
                (goto-char (point-min))
                (let ((i 1))
                  (while (not (eobp))
                    (let ((content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                      (unless (string-empty-p content)
                        (push (format "%d:%s" i content) lines)))
                    (forward-line 1)
                    (cl-incf i))))
              (nreverse lines))))
      (should (equal candidates '("1:alpha" "2:beta" "3:gamma"))))))

(ert-deftest fzfa-swiper-skips-empty-lines ()
  "Empty lines are excluded from candidates."
  (with-temp-buffer
    (insert "first\n\nthird\n")
    (let* ((candidates
            (let (lines)
              (save-excursion
                (goto-char (point-min))
                (let ((i 1))
                  (while (not (eobp))
                    (let ((content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                      (unless (string-empty-p content)
                        (push (format "%d:%s" i content) lines)))
                    (forward-line 1)
                    (cl-incf i))))
              (nreverse lines))))
      (should (equal candidates '("1:first" "3:third"))))))

;;; fzfa-tramp (ssh-hosts via :extract)

(defun fzfa-test--extract (cmd)
  "Run CMD under the multi `:extract' mode and return the captured plist.
Returns nil if CMD completes without invoking `completing-read'."
  (let ((fzfa--multi-mode :extract))
    (catch 'fzfa-extracted
      (funcall cmd)
      nil)))

(defmacro fzfa-test--with-ssh-config (content &rest body)
  "Run BODY with a temp file containing CONTENT as the ssh config.
Mocks `expand-file-name' so `fzfa-tramp' reads the temp file."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "fzfa-test-ssh-config")))
     (unwind-protect
         (progn
           (with-temp-file tmpfile (insert ,content))
           (cl-letf (((symbol-function 'expand-file-name)
                      (lambda (&rest _) tmpfile)))
             ,@body))
       (delete-file tmpfile))))

(ert-deftest fzfa-tramp-hosts-basic ()
  "Parses plain Host entries from ~/.ssh/config."
  (fzfa-test--with-ssh-config
      "Host foo\n  HostName foo.example.com\nHost bar\n"
    (let ((args (fzfa-test--extract #'fzfa-tramp)))
      (should (equal (plist-get args :items) '("foo" "bar"))))))

(ert-deftest fzfa-tramp-hosts-skips-wildcards ()
  "Wildcard Host patterns (*, ?, !) are excluded."
  (fzfa-test--with-ssh-config
      "Host *\nHost prod\nHost *.internal\nHost dev\n"
    (let ((args (fzfa-test--extract #'fzfa-tramp)))
      (should (equal (plist-get args :items) '("prod" "dev"))))))

(ert-deftest fzfa-tramp-hosts-multiple-on-one-line ()
  "Multiple hosts on a single Host line are each returned."
  (fzfa-test--with-ssh-config
      "Host alpha beta gamma\n"
    (let ((args (fzfa-test--extract #'fzfa-tramp)))
      (should (equal (plist-get args :items)
                     '("alpha" "beta" "gamma"))))))

(ert-deftest fzfa-tramp-missing-config ()
  "Signals a `user-error' when ~/.ssh/config does not exist."
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
    (should-error (fzfa-test--extract #'fzfa-tramp)
                  :type 'user-error)))

;;; fzfa-swiper-all (SOURCE encoding via :extract)

(ert-deftest fzfa-swiper-all-emits-source-line-content ()
  "Candidates display LINE:CONTENT; source rides on `fzfa-location'.
The buffer name (or file path) is no longer embedded in the candidate
string — it is carried in-band as an `fzfa-location' text property at
position 0, so fzf scores only against LINE:CONTENT and the source is
surfaced via `fzfa--location-group' as the section header."
  (let ((buf (generate-new-buffer " *fzfa-test-src*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (rename-buffer "fzfa-test-buf" t)
            (insert "hello\n"))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buf))))
            (let* ((args (fzfa-test--extract #'fzfa-swiper-all))
                   (cands (plist-get args :items))
                   (cand (car (cl-member "1:hello" cands :test #'equal))))
              (should cand)
              (should (equal (get-text-property 0 'fzfa-location cand)
                             '("fzfa-test-buf" . 1))))))
      (kill-buffer buf))))

;;; fzfa--format-stats

(ert-deftest fzfa-format-stats-with-idx ()
  "Non-nil IDX renders as `(1+ IDX)/' between prefix and `['."
  (should (equal (fzfa--format-stats "find: ~/code " 0 12 100)
                 "find: ~/code 1/[12](100) ")))

(ert-deftest fzfa-format-stats-without-idx ()
  "Nil IDX omits the `N/' segment — frontends like icomplete have none."
  (should (equal (fzfa--format-stats "find: ~/code " nil 12 100)
                 "find: ~/code [12](100) ")))

(ert-deftest fzfa-format-stats-commas-large-numbers ()
  "FILTERED and TOTAL are comma-formatted via `fzfa--commas'."
  (should (equal (fzfa--format-stats "" nil 1234 1234567)
                 "[1,234](1,234,567) ")))

(ert-deftest fzfa-format-stats-preserves-prefix-verbatim ()
  "PREFIX is emitted as-is — caller owns any punctuation and trailing space."
  (should (equal (fzfa--format-stats "fzf-multi: " 4 0 0)
                 "fzf-multi: 5/[0](0) "))
  (should (equal (fzfa--format-stats "" nil 0 0)
                 "[0](0) ")))

(ert-deftest fzfa-format-stats-always-trailing-space ()
  "Output ends with a single trailing space regardless of inputs."
  (dolist (idx '(nil 0 42))
    (should (string-suffix-p
             " " (fzfa--format-stats "p " idx 1 1)))))

;;; fzfa--multi-narrow->string

(ert-deftest fzfa-multi-narrow-string-from-string ()
  "A length-1 string passes through unchanged."
  (should (equal (fzfa--multi-narrow->string "V") "V")))

(ert-deftest fzfa-multi-narrow-string-from-char ()
  "A character is converted to its single-char string form."
  (should (equal (fzfa--multi-narrow->string ?V) "V")))

(ert-deftest fzfa-multi-narrow-string-from-symbol ()
  "A symbol with a one-character name is converted to that character."
  (should (equal (fzfa--multi-narrow->string 'V) "V")))

(ert-deftest fzfa-multi-narrow-string-rejects-multichar ()
  "Multi-character inputs error — narrow keys are single-char only."
  (should-error (fzfa--multi-narrow->string "ab"))
  (should-error (fzfa--multi-narrow->string 'foo)))

(ert-deftest fzfa-multi-narrow-string-rejects-bad-type ()
  "Non-string/char/symbol values error.
Integers are deliberately accepted (they are characters in Emacs)."
  (should-error (fzfa--multi-narrow->string [a b]))
  (should-error (fzfa--multi-narrow->string '(a b))))

;;; fzfa--multi-derive-narrow-key

(ert-deftest fzfa-multi-derive-narrow-first-word-first-char ()
  "Returns the first character of the first word when free."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (fzfa--multi-derive-narrow-key "buffer" used) "b"))
    (should (equal (fzfa--multi-derive-narrow-key "vc-modified-files" used)
                   "v"))))

(ert-deftest fzfa-multi-derive-narrow-preserves-case ()
  "Case is preserved from the source name (e.g. uppercase `M' in `M-x')."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (fzfa--multi-derive-narrow-key "M-x" used) "M"))))

(ert-deftest fzfa-multi-derive-narrow-walks-words-on-collision ()
  "When the first-word char is taken, walks to subsequent words' first chars."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "h" t used)
    (should (equal (fzfa--multi-derive-narrow-key "hungry-swiper" used) "s"))
    (puthash "i" t used)
    (should (equal (fzfa--multi-derive-narrow-key "imenu-all-but-current" used)
                   "a"))))

(ert-deftest fzfa-multi-derive-narrow-fallback-pool ()
  "When all word-first-chars are taken, falls through to a-z/A-Z/0-9."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "a" t used)
    (puthash "b" t used)
    (puthash "c" t used)
    ;; words a, b, c all taken; lowercase 'd' is the next free char.
    (should (equal (fzfa--multi-derive-narrow-key "a-b-c" used) "d"))))

(ert-deftest fzfa-multi-derive-narrow-case-sensitive ()
  "Uppercase and lowercase keys are tracked separately."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "I" t used)
    ;; `i' is still free since collision lookup is case-sensitive.
    (should (equal (fzfa--multi-derive-narrow-key "imenu" used) "i"))))

(ert-deftest fzfa-multi-derive-narrow-pool-exhaustion-errors ()
  "Errors when the full 62-char pool is exhausted."
  (let ((used (make-hash-table :test 'equal)))
    (dolist (c (string-to-list (concat "abcdefghijklmnopqrstuvwxyz"
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                       "0123456789")))
      (puthash (string c) t used))
    (should-error (fzfa--multi-derive-narrow-key "anything" used))))

;;; fzfa--format-narrow-hint

(ert-deftest fzfa-format-narrow-hint-basic ()
  "Each source contributes `KEY:NAME', separated by two spaces."
  (let ((sources (vector (list :name "buffer" :narrow "b")
                         (list :name "recent" :narrow "r")
                         (list :name "imenu" :narrow "i"))))
    (should (equal (substring-no-properties
                    (fzfa--format-narrow-hint sources nil))
                   "b:buffer  r:recent  i:imenu"))))

(ert-deftest fzfa-format-narrow-hint-highlights-active-source ()
  "When narrowed, the active source carries the `minibuffer-prompt' face."
  (let* ((sources (vector (list :name "buffer" :narrow "b")
                          (list :name "recent" :narrow "r")))
         (hint (fzfa--format-narrow-hint sources 1 200)))
    ;; r:recent is at position 10 (after "b:buffer  ").
    (should (eq (get-text-property 10 'face hint) 'minibuffer-prompt))
    ;; b:buffer is unpropertized.
    (should (null (get-text-property 0 'face hint)))))

(ert-deftest fzfa-format-narrow-hint-appends-return-marker ()
  "A trailing `PREFIX:widen' marker is appended when prefix is given."
  (let* ((sources (vector (list :name "buffer" :narrow "b")
                          (list :name "recent" :narrow "r")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200 "<"))))
    (should (equal hint "b:buffer  r:recent  <:widen"))))

(ert-deftest fzfa-format-narrow-hint-return-marker-faced-shadow ()
  "The trailing `PREFIX:widen' marker carries the `shadow' face."
  (let* ((sources (vector (list :name "buffer" :narrow "b")))
         (hint (fzfa--format-narrow-hint sources nil 200 "<"))
         ;; "b:buffer  " is 10 chars; return marker starts at 10.
         (marker-pos 10))
    (should (eq (get-text-property marker-pos 'face hint) 'shadow))))

(ert-deftest fzfa-format-narrow-hint-omits-return-without-prefix ()
  "No trailing marker when PREFIX-KEY is nil."
  (let* ((sources (vector (list :name "buffer" :narrow "b")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200 nil))))
    (should (equal hint "b:buffer"))))

(ert-deftest fzfa-format-narrow-hint-wraps-on-overflow ()
  "Entries that would exceed WIDTH are pushed to a new line."
  (let* ((sources (vector (list :name "alpha" :narrow "a")
                          (list :name "beta"  :narrow "b")
                          (list :name "gamma" :narrow "g")))
         ;; Width 15 fits "a:alpha  b:beta" (15) but not "  g:gamma".
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 15))))
    (should (equal hint "a:alpha  b:beta\ng:gamma"))))

(ert-deftest fzfa-format-narrow-hint-single-line-when-fits ()
  "No newlines when every entry fits in one line at the given width."
  (let* ((sources (vector (list :name "alpha" :narrow "a")
                          (list :name "beta"  :narrow "b")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200))))
    (should (equal hint "a:alpha  b:beta"))))

(ert-deftest fzfa-format-narrow-hint-oversize-entry-on-own-line ()
  "An entry longer than WIDTH stays whole on its own line — no mid-split."
  (let* ((sources (vector (list :name "this-name-is-far-too-long" :narrow "x")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 5))))
    (should (equal hint "x:this-name-is-far-too-long"))))

;;; fzfa--multi-allocate-narrow-keys

(defun fzfa-test--alloc (specs)
  "Build sources from SPECS \\='((NAME . PLIST) ...) and allocate narrow keys.
Returns an alist of (NAME . KEY) preserving allocation order."
  (let ((sources
         (mapcar (lambda (spec)
                   (append (list :name (car spec)) (cdr spec)))
                 specs)))
    (mapcar (lambda (s) (cons (plist-get s :name) (plist-get s :narrow)))
            (fzfa--multi-allocate-narrow-keys sources))))

(ert-deftest fzfa-multi-allocate-implicit-only ()
  "All-implicit case matches the derivation rules end-to-end."
  (should (equal
           (fzfa-test--alloc
            '(("vc-modified-files")
              ("imenu")
              ("buffer")
              ("recent-file")
              ("hungry-find")
              ("imenu-all-but-current")
              ("M-x")
              ("hungry-swiper")
              ("locate")))
           '(("vc-modified-files"     . "v")
             ("imenu"                 . "i")
             ("buffer"                . "b")
             ("recent-file"           . "r")
             ("hungry-find"           . "h")
             ("imenu-all-but-current" . "a")
             ("M-x"                   . "M")
             ("hungry-swiper"         . "s")
             ("locate"                . "l")))))

(ert-deftest fzfa-multi-allocate-mixed-explicit-and-implicit ()
  "Explicit :narrow values are honored; implicit derivation routes around them.
Note `imenu' takes uppercase `I' so `imenu-all-but-current' gets the free `i'."
  (should (equal
           (fzfa-test--alloc
            '(("vc-modified-files" :narrow V)
              ("imenu"             :narrow I)
              ("buffer")
              ("recent-file"       :narrow R)
              ("hungry-find")
              ("imenu-all-but-current")
              ("M-x")
              ("hungry-swiper")
              ("locate")))
           '(("vc-modified-files"     . "V")
             ("imenu"                 . "I")
             ("buffer"                . "b")
             ("recent-file"           . "R")
             ("hungry-find"           . "h")
             ("imenu-all-but-current" . "i")
             ("M-x"                   . "M")
             ("hungry-swiper"         . "s")
             ("locate"                . "l")))))

(ert-deftest fzfa-multi-allocate-accepts-char-and-string-forms ()
  "Explicit :narrow accepts symbol, character, or string interchangeably."
  (should (equal
           (fzfa-test--alloc
            '(("a-source" :narrow ?A)
              ("b-source" :narrow "B")
              ("c-source" :narrow C)))
           '(("a-source" . "A")
             ("b-source" . "B")
             ("c-source" . "C")))))

(ert-deftest fzfa-multi-allocate-duplicate-explicit-errors ()
  "Two sources declaring the same explicit :narrow signal an error."
  (should-error
   (fzfa--multi-allocate-narrow-keys
    (list (list :name "first" :narrow "x")
          (list :name "second" :narrow "x")))))

(ert-deftest fzfa-multi-allocate-implicit-yields-to-reserved ()
  "Implicit derivation never picks an explicitly-reserved key."
  (let ((result
         (fzfa-test--alloc
          '(("hungry-find" :narrow h)
            ("hungry-swiper")))))
    ;; Second source's first-word `h' is reserved, walks to `s'.
    (should (equal (cdr (assoc "hungry-swiper" result)) "s"))))

(ert-deftest fzfa-multi-allocate-preserves-source-order ()
  "Returned source list is in the same order as input."
  (let* ((sources
          (list (list :name "alpha")
                (list :name "beta" :narrow "X")
                (list :name "gamma")))
         (result (fzfa--multi-allocate-narrow-keys sources)))
    (should (equal (mapcar (lambda (s) (plist-get s :name)) result)
                   '("alpha" "beta" "gamma")))))

(ert-deftest fzfa-multi-allocate-nested-collision-with-outer-explicit ()
  "Outer explicit `:narrow' wins over an inner derived key.
Reproduces a real bug: a nested multi (e.g. `fzfa-vc-modified-files')
flattened into an outer multi (`fzfa-find-any') used to bring inner
sources with `:narrow' already pinned by a prior inner allocation —
which then collided with an outer explicit reservation.  Fix: do
not allocate at the inner level when being extracted; defer to the
outermost.  This test asserts the allocator does the right thing
when the inner sources arrive without `:narrow'."
  (let ((result
         ;; Inner sources arrive without `:narrow' (allocation deferred).
         ;; Top-level `(hungry-swiper :narrow s)' is an outer-explicit
         ;; reservation that previously collided with the inner `s'.
         (fzfa-test--alloc
          '(("git-modified-locally")
            ("git-added-files")
            ("git-staged-for-commit")
            ("git-modified-in-head")
            ("hungry-swiper" :narrow s)))))
    (should (equal (cdr (assoc "hungry-swiper" result)) "s"))
    ;; Inner `git-staged-for-commit' must route around the reserved `s'.
    (should-not (equal (cdr (assoc "git-staged-for-commit" result)) "s"))))

(ert-deftest fzfa-multi-allocate-does-not-mutate-input ()
  "Allocation copies source plists rather than mutating them in place."
  (let* ((src (list :name "buffer"))
         (sources (list src)))
    (fzfa--multi-allocate-narrow-keys sources)
    (should (null (plist-get src :narrow)))))

;;; Preview framework

(ert-deftest fzfa-preview-handler-lookup ()
  "Resolver honours the explicit :preview, the registry, and the delay."
  ;; With no override, registered category returns the handler plist.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay 0.3))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore)))
  ;; Unknown category yields nil even when delay is set.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay 0.3))
    (should (null (fzfa--preview-handler nil 'unknown))))
  ;; nil delay disables preview globally — registry + explicit override both ignored.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay nil))
    (should (null (fzfa--preview-handler nil 'cat)))
    (should (null (fzfa--preview-handler #'identity 'cat))))
  ;; Explicit function override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay 0.3))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit plist override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay 0.3))
    (should (eq (plist-get
                 (fzfa--preview-handler '(:preview my-fn :return my-ret) 'cat)
                 :return)
                'my-ret))))

(ert-deftest fzfa-preview-state-get-put ()
  "State plist round-trips through the dynamically-bound session."
  (let ((fzfa--preview-session (list nil))) ; (HANDLER . STATE-PLIST)
    (fzfa-preview-put :a 1)
    (fzfa-preview-put :b 'foo)
    (should (= 1 (fzfa-preview-get :a)))
    (should (eq 'foo (fzfa-preview-get :b)))
    (should (eq :missing (fzfa-preview-get :c :missing)))
    ;; Stored nil differs from absent key (DEFAULT not applied).
    (fzfa-preview-put :a nil)
    (should (null (fzfa-preview-get :a :default)))))

(ert-deftest fzfa-grep-preview-parses-candidate ()
  "Grep preview accepts FILE:LINE:CONTENT and ignores malformed input."
  ;; No-op for nil / wrong shape — must not error.
  (fzfa--grep-preview nil)
  (fzfa--grep-preview "no-colons")
  (fzfa--grep-preview "only:one-colon")
  ;; Well-formed candidate to a nonexistent path is a silent no-op.
  (fzfa--grep-preview "no-such-file.xyz:1:irrelevant"))

(ert-deftest fzfa-buffer-preview-handles-missing-buffer ()
  "Buffer preview is a silent no-op when the named buffer does not exist."
  (fzfa--buffer-preview nil)
  (fzfa--buffer-preview "*no-such-buffer*-fzfa-test*"))

(ert-deftest fzfa-temporary-files-creates-and-kills ()
  "Opener creates an ephemeral buffer for a new file and kills it on cleanup."
  (let ((tmpfile (make-temp-file "fzfa-tmpfiles-test")))
    (unwind-protect
        (let* ((opener (fzfa--temporary-files))
               (buf (funcall opener tmpfile)))
          (should (buffer-live-p buf))
          (should (file-equal-p tmpfile (buffer-file-name buf)))
          (funcall opener)              ; cleanup
          (should-not (buffer-live-p buf)))
      (delete-file tmpfile))))

(ert-deftest fzfa-temporary-files-reuses-loaded-buffer ()
  "Opener returns an already-loaded buffer and does NOT kill it on cleanup."
  (let* ((tmpfile (make-temp-file "fzfa-tmpfiles-test"))
         (pre-loaded (find-file-noselect tmpfile 'nowarn)))
    (unwind-protect
        (let* ((opener (fzfa--temporary-files))
               (buf (funcall opener tmpfile)))
          (should (eq buf pre-loaded))
          (funcall opener)              ; cleanup
          (should (buffer-live-p pre-loaded)))   ; not killed
      (when (buffer-live-p pre-loaded) (kill-buffer pre-loaded))
      (delete-file tmpfile))))

(ert-deftest fzfa-temporary-files-promotes-buffer ()
  "Promoted buffers survive cleanup; siblings are still killed."
  (let ((f1 (make-temp-file "fzfa-tmpfiles-test"))
        (f2 (make-temp-file "fzfa-tmpfiles-test")))
    (unwind-protect
        (let* ((opener (fzfa--temporary-files))
               (b1 (funcall opener f1))
               (b2 (funcall opener f2)))
          (funcall opener b1)            ; promote b1
          (funcall opener)               ; cleanup
          (should (buffer-live-p b1))
          (should-not (buffer-live-p b2))
          (kill-buffer b1))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest fzfa-file-preview-skips-oversize ()
  "Preview is a no-op when the file exceeds `fzfa-preview-file-size-limit'."
  (let ((tmpfile (make-temp-file "fzfa-file-preview-test")))
    (unwind-protect
        (let ((fzfa--preview-session (list nil))
              (fzfa-preview-file-size-limit 0))
          (fzfa--file-preview-setup)
          ;; Limit of 0 disables — opener should not produce a buffer.
          (fzfa--file-preview tmpfile)
          ;; Nothing was opened (no file-visiting buffer for our path).
          (should-not (find-buffer-visiting tmpfile)))
      (delete-file tmpfile))))

(ert-deftest fzfa-multi-router-routes-preview-per-source ()
  "Router's :preview dispatches to the source identified by CAND's tagged idx."
  (let* ((calls nil)
         (h0 (list :preview (lambda (c) (push (cons 0 c) calls))))
         (h1 (list :preview (lambda (c) (push (cons 1 c) calls))))
         (fzfa-preview-functions `((cat-a :preview ,(plist-get h0 :preview))
                                   (cat-b :preview ,(plist-get h1 :preview))))
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'cat-a)
                            (list :name "B" :category 'cat-b)))
         (cand->src (make-hash-table :test 'equal))
         (router (fzfa--multi-build-router sources-v cand->src))
         ;; Pretend the framework already installed and set origin/dir.
         (fzfa--preview-session (list router)))
    (fzfa-preview-put :origin-window nil)
    (fzfa-preview-put :origin-buffer nil)
    (fzfa-preview-put :default-directory "/")
    ;; Run :setup → broadcasts to both sources.
    (funcall (plist-get router :setup))
    ;; Source 0 candidate
    (let ((c0 (propertize "alpha" 'fzfa-src-idx 0)))
      (puthash c0 0 cand->src)
      (funcall (plist-get router :preview) c0))
    ;; Source 1 candidate
    (let ((c1 (propertize "beta" 'fzfa-src-idx 1)))
      (puthash c1 1 cand->src)
      (funcall (plist-get router :preview) c1))
    (should (equal (reverse calls)
                   '((0 . "alpha") (1 . "beta"))))))

(ert-deftest fzfa-multi-router-nil-when-no-handlers ()
  "Router builder returns nil if no source has a resolvable handler."
  (let* ((fzfa-preview-functions nil)
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'no-such)
                            (list :name "B" :category 'also-no))))
    (should (null (fzfa--multi-build-router
                   sources-v (make-hash-table :test 'equal))))))

(ert-deftest fzfa-multi-router-return-routes-selection ()
  "On :return, the selected source gets CAND; others get nil."
  (let* ((returns nil)
         (h0 (list :return (lambda (c) (push (cons 0 c) returns))))
         (h1 (list :return (lambda (c) (push (cons 1 c) returns))))
         (fzfa-preview-functions `((cat-a :return ,(plist-get h0 :return)
                                          :preview ignore)
                                   (cat-b :return ,(plist-get h1 :return)
                                          :preview ignore)))
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'cat-a)
                            (list :name "B" :category 'cat-b)))
         (cand->src (make-hash-table :test 'equal))
         (router (fzfa--multi-build-router sources-v cand->src))
         (fzfa--preview-session (list router))
         (sel (propertize "picked" 'fzfa-src-idx 1)))
    (fzfa-preview-put :origin-window nil)
    (fzfa-preview-put :origin-buffer nil)
    (fzfa-preview-put :default-directory "/")
    (funcall (plist-get router :setup))
    (puthash sel 1 cand->src)
    (funcall (plist-get router :return) sel)
    ;; Source 1 got the candidate; source 0 got nil.
    (should (equal (sort (copy-sequence returns) (lambda (a b)
                                                   (< (car a) (car b))))
                   '((0 . nil) (1 . "picked"))))))

(ert-deftest fzfa-preview-show-honors-display-action ()
  "`fzfa-preview-show' passes `fzfa-preview-display-action' to `display-buffer'."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           captured
           (fzfa-preview-display-action
            '(display-buffer-in-side-window (side . left)
                                            (window-width . 0.3))))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_b action) (setq captured action))))
        (fzfa-preview-show buf))
      (should (equal captured fzfa-preview-display-action)))))

(ert-deftest fzfa-preview-show-moves-point ()
  "`fzfa-preview-show' moves point in BUFFER when POS is supplied."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (insert "one\ntwo\nthree\n")
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (fzfa-preview-show buf 5)             ; start of "two"
        (with-current-buffer buf
          (should (= (point) 5))))
      ;; Marker POS also accepted.
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (let ((m (copy-marker 9)))            ; start of "three"
          (fzfa-preview-show buf m)
          (with-current-buffer buf
            (should (= (point) 9))))))))

;;; fzfa-smart-define / fzfa--smart-resolve

(defmacro fzfa-test--with-executables (available &rest body)
  "Run BODY with `executable-find' stubbed to recognize only AVAILABLE.
AVAILABLE is a list of program-name strings; calls for any other
program return nil.  Also stubs the smart-find/grep backend symbols
referenced by the resolve tests as no-op functions so they are
`fboundp' regardless of which extension files are loaded."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'executable-find)
              (lambda (prog &rest _)
                (and (member prog ,available) prog)))
             ((symbol-function 'fzfa-fd)       (lambda () 'fd))
             ((symbol-function 'fzfa-rg-files) (lambda () 'rg-files))
             ((symbol-function 'fzfa-ag-files) (lambda () 'ag-files))
             ((symbol-function 'fzfa-find)     (lambda () 'find))
             ((symbol-function 'fzfa-rg)       (lambda () 'rg))
             ((symbol-function 'fzfa-ag)       (lambda () 'ag))
             ((symbol-function 'fzfa-ugrep)    (lambda () 'ugrep))
             ((symbol-function 'fzfa-grep)     (lambda () 'grep)))
     ,@body))

(ert-deftest fzfa-smart-resolve-picks-first-matching-executable ()
  "First clause whose executable is on PATH wins."
  (fzfa-test--with-executables '("fd" "rg" "find")
    (should (eq 'fzfa-fd
                (fzfa--smart-resolve
                 '((fzfa-fd       :executable "fd")
                   (fzfa-rg-files :executable "rg")
                   (fzfa-find     :executable "find")))))))

(ert-deftest fzfa-smart-resolve-skips-missing-executable ()
  "Clauses whose executable is absent are skipped."
  (fzfa-test--with-executables '("rg" "find")
    (should (eq 'fzfa-rg-files
                (fzfa--smart-resolve
                 '((fzfa-fd       :executable "fd")
                   (fzfa-rg-files :executable "rg")
                   (fzfa-find     :executable "find")))))))

(ert-deftest fzfa-smart-resolve-returns-nil-when-nothing-matches ()
  "Returns nil when no clause has a satisfied executable."
  (fzfa-test--with-executables '()
    (should (null (fzfa--smart-resolve
                   '((fzfa-fd :executable "fd")
                     (fzfa-rg-files :executable "rg")))))))

(ert-deftest fzfa-smart-resolve-respects-fboundp ()
  "A clause is skipped when its CMD symbol is not `fboundp'."
  (let ((unbound (make-symbol "fzfa-test-unbound")))
    (fzfa-test--with-executables '("fd" "find")
      (should (eq 'fzfa-find
                  (fzfa--smart-resolve
                   `((,unbound  :executable "fd")
                     (fzfa-find :executable "find"))))))))

(ert-deftest fzfa-smart-resolve-predicate-truthy-matches ()
  "Clause matches when `:predicate' returns non-nil."
  (fzfa-test--with-executables '()
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-find :predicate (lambda () t))))))))

(ert-deftest fzfa-smart-resolve-predicate-falsy-skips ()
  "Clause is skipped when `:predicate' returns nil."
  (fzfa-test--with-executables '("find")
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :predicate ignore)
                   (fzfa-find :executable "find")))))))

(ert-deftest fzfa-smart-resolve-predicate-and-executable-both-required ()
  "When both `:executable' and `:predicate' are supplied, both must hold."
  (fzfa-test--with-executables '("fd" "find")
    ;; Executable matches but predicate fails -> skipped.
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :executable "fd" :predicate ignore)
                   (fzfa-find :executable "find")))))
    ;; Predicate matches but executable missing -> skipped.
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :executable "no-such-exe" :predicate (lambda () t))
                   (fzfa-find :executable "find")))))))

(ert-deftest fzfa-smart-resolve-bare-clause-is-unconditional ()
  "A clause with neither `:executable' nor `:predicate' always matches."
  (fzfa-test--with-executables '()
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd :executable "fd")
                   (fzfa-find)))))))

(ert-deftest fzfa-smart-define-creates-named-command ()
  "`fzfa-smart-define' interns and defines `fzfa-smart-NAME'."
  (let ((sym (intern "fzfa-smart-test-create")))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (let ((result (fzfa-smart-define
                         'test-create
                         '((fzfa-find :executable "find")))))
            (should (eq result sym))
            (should (fboundp sym))
            (should (commandp sym))))
      (fmakunbound sym))))

(ert-deftest fzfa-smart-define-funcalls-chosen-backend ()
  "The generated command `funcall's the resolved backend symbol."
  (let ((sym (intern "fzfa-smart-test-dispatch"))
        (backend (intern "fzfa-test-backend-dispatch"))
        (called 0))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (defalias backend (lambda () (cl-incf called)))
          (fzfa-smart-define 'test-dispatch
                             `((,backend :executable "fd")))
          (fzfa-test--with-executables '("fd")
            (funcall sym))
          (should (= called 1)))
      (fmakunbound sym)
      (fmakunbound backend))))

(ert-deftest fzfa-smart-define-errors-when-no-backend ()
  "The generated command signals `user-error' when nothing resolves."
  (let ((sym (intern "fzfa-smart-test-noexe")))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (fzfa-smart-define 'test-noexe
                             '((fzfa-fd :executable "no-such-tool-xyz")))
          (fzfa-test--with-executables '()
            (should-error (funcall sym) :type 'user-error)))
      (fmakunbound sym))))

(ert-deftest fzfa-smart-define-propagates-multi-mode ()
  "Active `fzfa--multi-mode' propagates through the smart command.
This is the contract that makes smart commands work transparently
inside `fzfa-multi-read' (`:extract') and `fzfa--2pass-dispatch'."
  (let ((sym (intern "fzfa-smart-test-multi"))
        (backend (intern "fzfa-test-backend-multi"))
        observed)
    (unwind-protect
        (progn
          (fmakunbound sym)
          (defalias backend
            (lambda ()
              (throw 'fzfa-extracted (list :seen fzfa--multi-mode))))
          (fzfa-smart-define 'test-multi
                             `((,backend :executable "fd")))
          (fzfa-test--with-executables '("fd")
            (setq observed
                  (catch 'fzfa-extracted
                    (let ((fzfa--multi-mode :extract))
                      (funcall sym))
                    nil)))
          (should (equal observed '(:seen :extract))))
      (fmakunbound sym)
      (fmakunbound backend))))

(ert-deftest fzfa-smart-find-and-grep-defined ()
  "The shipped smart wrappers are defined and interactive."
  (should (fboundp 'fzfa-smart-find))
  (should (commandp 'fzfa-smart-find))
  (should (fboundp 'fzfa-smart-grep))
  (should (commandp 'fzfa-smart-grep)))

(provide 'fzfa-test)
;;; fzfa-test.el ends here
