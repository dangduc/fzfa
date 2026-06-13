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
(require 'fzfa-hungry)

;;; fzfa-hungry--deduplicate-dirs

(ert-deftest fzfa-hungry-deduplicate-dirs-no-overlap ()
  "Unrelated directories are all kept."
  (should (equal (sort (fzfa-hungry--deduplicate-dirs
                        '("/a/b/" "/c/d/" "/e/f/"))
                       #'string<)
                 '("/a/b/" "/c/d/" "/e/f/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-drops-subdirectory ()
  "A subdirectory is dropped when its parent is present."
  (should (equal (fzfa-hungry--deduplicate-dirs
                  '("/home/user/project/" "/home/user/project/src/"))
                 '("/home/user/project/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-keeps-sibling-dirs ()
  "Sibling directories (same parent, different names) are both kept."
  (let ((result (fzfa-hungry--deduplicate-dirs
                 '("/home/user/foo/" "/home/user/bar/"))))
    (should (member "/home/user/foo/" result))
    (should (member "/home/user/bar/" result))))

(ert-deftest fzfa-hungry-deduplicate-dirs-removes-exact-duplicates ()
  "Exact duplicate entries are collapsed to one."
  (should (equal (fzfa-hungry--deduplicate-dirs
                  '("/a/b/" "/a/b/" "/a/b/"))
                 '("/a/b/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-deep-nesting ()
  "Only the shallowest ancestor survives when multiple levels are present."
  (let ((result (fzfa-hungry--deduplicate-dirs
                 '("/a/" "/a/b/" "/a/b/c/" "/a/b/c/d/"))))
    (should (equal result '("/a/")))))

(ert-deftest fzfa-hungry-deduplicate-dirs-empty-input ()
  "Empty input returns nil."
  (should (null (fzfa-hungry--deduplicate-dirs '()))))

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
      (should (equal (plist-get args :candidates) '("foo" "bar"))))))

(ert-deftest fzfa-tramp-hosts-skips-wildcards ()
  "Wildcard Host patterns (*, ?, !) are excluded."
  (fzfa-test--with-ssh-config
      "Host *\nHost prod\nHost *.internal\nHost dev\n"
    (let ((args (fzfa-test--extract #'fzfa-tramp)))
      (should (equal (plist-get args :candidates) '("prod" "dev"))))))

(ert-deftest fzfa-tramp-hosts-multiple-on-one-line ()
  "Multiple hosts on a single Host line are each returned."
  (fzfa-test--with-ssh-config
      "Host alpha beta gamma\n"
    (let ((args (fzfa-test--extract #'fzfa-tramp)))
      (should (equal (plist-get args :candidates)
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
                   (cands (plist-get args :candidates))
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
  "Resolver honours the explicit :preview and the registry, ignoring delay."
  ;; With no override, registered category returns the handler plist.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore)))
  ;; Unknown category yields nil.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (null (fzfa--preview-handler nil 'unknown))))
  ;; Resolution is independent of `fzfa-preview-delay' — manual fire and
  ;; helm's persistent-action both need a resolvable handler regardless.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay nil))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit function override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit plist override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
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

(ert-deftest fzfa-preview-show-uses-same-window ()
  "`fzfa-preview-show' lands the buffer in the currently selected window.

`fzfa--preview-call' selects the originating window before invoking
handlers, so passing `display-buffer-same-window' here keeps preview
and the eventual post-selection action sharing one window slot."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           captured)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_b action) (setq captured action))))
        (fzfa-preview-show buf))
      (should (equal captured '(display-buffer-same-window))))))

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
                 '((fzfa-fd   :executable "no-such-exe"
                              :predicate (lambda () t))
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
inside `fzfa-multi-read' (`:extract')."
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

;;; fzfa--split
;;
;; Tests bind `fzfa-separator' to ?# for readable ASCII fixtures;
;; the splitter is generic and works for any character.

(ert-deftest fzfa-split-hidden-empty-input ()
  "Hidden mode + empty input returns (COMMAND . \"\")."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "" 'hidden "find .")
                   '("find ." . "")))))

(ert-deftest fzfa-split-hidden-with-filter ()
  "Hidden mode treats the whole INPUT as FILTER and CMD = COMMAND."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden "find .")
                   '("find ." . "foo")))))

(ert-deftest fzfa-split-hidden-nil-command ()
  "Hidden mode + nil COMMAND coerces CMD to the empty string."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden nil)
                   '("" . "foo")))))

(ert-deftest fzfa-split-hidden-empty-string-command ()
  "Hidden mode + empty-string COMMAND keeps CMD as empty string."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden "")
                   '("" . "foo")))))

(ert-deftest fzfa-split-compact-delegates-to-splitter ()
  "Compact mode ignores COMMAND and delegates to the splitter."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#find .#foo" 'compact "ignored")
                   '("find ." . "foo")))))

(ert-deftest fzfa-split-full-delegates-to-splitter ()
  "Full mode behaves identically to compact."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#find .#foo" 'full "ignored")
                   (fzfa--split "#find .#foo" 'compact "ignored")))))

(ert-deftest fzfa-split-compact-no-leading-separator ()
  "Compact mode + input without a leading separator → whole string as CMD."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "plain-text" 'compact "ignored")
                   '("plain-text" . "")))))

(ert-deftest fzfa-split-compact-empty-cmd-region ()
  "Compact mode + `##filter' (empty CMD region) → (\"\" . \"filter\")."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "##bar" 'compact "ignored")
                   '("" . "bar")))))

(ert-deftest fzfa-split-hidden-state-takes-priority ()
  "Hidden mode short-circuits *before* the splitter runs.

INPUT that looks like a separator-delimited shape is NOT parsed when
the session is hidden — the whole INPUT (including literal separator
characters) is the FILTER."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#fake#filter" 'hidden "real")
                   '("real" . "#fake#filter")))))

(ert-deftest fzfa-split-honors-custom-separator ()
  "Splitter follows whatever character `fzfa-separator' is set to."
  (let ((fzfa-separator ?▌))
    (should (equal (fzfa--split "▌find .▌foo" 'compact "ignored")
                   '("find ." . "foo")))))

;;; fzfa--display-next-state

(ert-deftest fzfa-display-next-state-cycle ()
  "State cycles hidden → compact → full → hidden."
  (should (eq (fzfa--display-next-state 'hidden)  'compact))
  (should (eq (fzfa--display-next-state 'compact) 'full))
  (should (eq (fzfa--display-next-state 'full)    'hidden)))

(ert-deftest fzfa-display-next-state-fallback ()
  "Unknown input falls back to `hidden'."
  (should (eq (fzfa--display-next-state 'bogus) 'hidden))
  (should (eq (fzfa--display-next-state nil)    'hidden)))

;;; fzfa--display-materialize / extract

(ert-deftest fzfa-display-materialize-empty-filter ()
  "Materialize on empty FILTER inserts `#CMD#' and lands point at end."
  (with-temp-buffer
    (fzfa--display-materialize "find ." ?#)
    (should (equal (buffer-string) "#find .#"))
    (should (= (point) (point-max)))))

(ert-deftest fzfa-display-materialize-preserves-filter-offset ()
  "With existing FILTER `abc' and point at `c', materialize keeps point at `c'."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-max))  ; point at position 4 (after `c`)
    (let ((offset-before (- (point) (minibuffer-prompt-end))))
      (fzfa--display-materialize "find ." ?#)
      (should (equal (buffer-string) "#find .#abc"))
      ;; Point should be at the same offset within the new FILTER region.
      (let* ((mbe (minibuffer-prompt-end))
             (cmd-text-len (length "#find .#"))
             (new-filter-offset (- (point) mbe cmd-text-len)))
        (should (= new-filter-offset offset-before))))))

(ert-deftest fzfa-display-materialize-nil-cmd ()
  "Nil CMD coerces to empty string — buffer ends up as `##'."
  (with-temp-buffer
    (fzfa--display-materialize nil ?#)
    (should (equal (buffer-string) "##"))))

(ert-deftest fzfa-display-materialize-returns-overlays ()
  "Returns a list of two protective overlays covering the two separators."
  (with-temp-buffer
    (let ((overlays (fzfa--display-materialize "find ." ?#)))
      (should (= (length overlays) 2))
      (should (cl-every #'overlayp overlays))
      ;; First overlay covers the opening `#' at position 1.
      (should (= (overlay-start (car overlays)) 1))
      (should (= (overlay-end   (car overlays)) 2))
      ;; Second overlay covers the closing `#' at position 8 (after "find .").
      (should (= (overlay-start (cadr overlays)) 8))
      (should (= (overlay-end   (cadr overlays)) 9)))))

(ert-deftest fzfa-display-extract-basic ()
  "Extract returns CMD and deletes the `#CMD#' prefix from the buffer."
  (with-temp-buffer
    (insert "#find .#filter")
    (let* ((fzfa-separator ?#)
           (cmd (fzfa--display-extract nil)))
      (should (equal cmd "find ."))
      (should (equal (buffer-string) "filter")))))

(ert-deftest fzfa-display-extract-empty-cmd ()
  "Extract handles `##filter' (empty CMD region)."
  (with-temp-buffer
    (insert "##filter")
    (let* ((fzfa-separator ?#)
           (cmd (fzfa--display-extract nil)))
      (should (equal cmd ""))
      (should (equal (buffer-string) "filter")))))

(ert-deftest fzfa-display-extract-deletes-overlays ()
  "Extract removes the separator-protective overlays before mutating the buffer.

\(Their `modification-hooks' would otherwise self-heal the deletion.)"
  (with-temp-buffer
    (insert "#find .#abc")
    (let* ((fzfa-separator ?#)
           (overlays
            (list (fzfa--protect-separator 1 ?#)
                  (fzfa--protect-separator 8 ?#))))
      (should (= (length (overlays-in 1 9)) 2))
      (fzfa--display-extract overlays)
      (should (equal (buffer-string) "abc")))))

(ert-deftest fzfa-display-materialize-extract-roundtrip ()
  "Materialize then extract restores the original buffer + cmd."
  (with-temp-buffer
    (insert "abc")
    (let* ((fzfa-separator ?#)
           (overlays (fzfa--display-materialize "find ." ?#)))
      (should (equal (buffer-string) "#find .#abc"))
      (let ((cmd (fzfa--display-extract overlays)))
        (should (equal cmd "find ."))
        (should (equal (buffer-string) "abc"))))))

;;; fzfa-sync-autoloads

(ert-deftest fzfa-sync-autoloads-prunes-excluded-extensions ()
  "Autoload stubs for extensions not in `fzfa-extensions' get unbound,

included extensions are left alone."
  (let ((syms '(fzfa-syncauttest1 fzfa-syncauttest1-foo
                fzfa-syncauttest2 fzfa-syncauttest2-bar))
        (registry '((syncauttest1 . "test 1")
                    (syncauttest2 . "test 2"))))
    (unwind-protect
        (progn
          (dolist (s syms) (autoload s "fzfa-test-nonexistent"))
          (dolist (s syms) (should (autoloadp (symbol-function s))))
          (let ((fzfa-extension-registry registry)
                (fzfa-extensions '(syncauttest1)))
            (fzfa-sync-autoloads))
          ;; Included extension: stubs preserved.
          (should (autoloadp (symbol-function 'fzfa-syncauttest1)))
          (should (autoloadp (symbol-function 'fzfa-syncauttest1-foo)))
          ;; Excluded extension: stubs unbound.
          (should-not (fboundp 'fzfa-syncauttest2))
          (should-not (fboundp 'fzfa-syncauttest2-bar)))
      (dolist (s syms) (when (intern-soft s) (unintern s nil))))))

(ert-deftest fzfa-sync-autoloads-skips-loaded-functions ()
  "Already-loaded (non-autoload) bindings are not touched by the prune,

even when their extension is excluded from `fzfa-extensions'."
  (let ((syms '(fzfa-syncauttest3 fzfa-syncauttest3-loaded))
        (registry '((syncauttest3 . "test 3"))))
    (unwind-protect
        (progn
          (autoload 'fzfa-syncauttest3 "fzfa-test-nonexistent")
          (defalias 'fzfa-syncauttest3-loaded (lambda () "loaded"))
          (let ((fzfa-extension-registry registry)
                (fzfa-extensions '()))
            (fzfa-sync-autoloads))
          ;; Autoload stub: pruned.
          (should-not (fboundp 'fzfa-syncauttest3))
          ;; Concrete function: preserved.
          (should (fboundp 'fzfa-syncauttest3-loaded))
          (should-not (autoloadp (symbol-function 'fzfa-syncauttest3-loaded))))
      (dolist (s syms) (when (intern-soft s) (unintern s nil))))))

;;; fzfa-source — struct + helpers

(ert-deftest fzfa-source-make-from-hoisted-args ()
  "Constructor with hoisted args (single-source path) populates slots."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "rg foo"
                                :directory "/tmp/"
                                :history 'my-history
                                :display 'compact)))
    (should (fzfa-source-p src))
    (should (equal (fzfa-source-command src) "rg foo"))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))
    (should (eq (fzfa-source-history src) 'my-history))
    (should (eq (fzfa-source-display-state src) 'compact))
    (should (null (fzfa-source-handle src)))
    (should (null (fzfa-source-current-cmd src)))
    (should (= (fzfa-source-last-gen src) -1))
    (should (= (fzfa-source-rank src) 0))
    (should (= (fzfa-source-total src) 0))
    (should (= (fzfa-source-filtered src) 0))
    (should (eq (fzfa-source-prod-input src) :unfetched))))

(ert-deftest fzfa-source-make-from-spec-plist ()
  "Constructor with :spec (multi-source path) extracts keys from plist."
  (let* ((default-directory "/tmp/")
         (spec '(:name "my-src" :command "fd ." :directory "/tmp/"
                 :history my-hist :display full))
         (src (fzfa-make-source :spec spec)))
    (should (fzfa-source-p src))
    (should (equal (fzfa-source-name src) "my-src"))
    (should (equal (fzfa-source-command src) "fd ."))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))
    (should (eq (fzfa-source-history src) 'my-hist))
    (should (eq (fzfa-source-display-state src) 'full))
    ;; Spec preserved for closures that need non-hot keys.
    (should (equal (fzfa-source-spec src) spec))))

(ert-deftest fzfa-source-defaults-display-to-hidden ()
  "Display state defaults to `hidden' when no :display in spec or args."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (should (eq (fzfa-source-display-state src) 'hidden))))

(ert-deftest fzfa-source-cands-fn-normalized ()
  "Producer-fn constructor normalizes a list into a 2-arg producer."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("a" "b" "c"))))
    (should (functionp (fzfa-source-cands-fn src)))
    ;; Firing the normalized producer should yield the list back.
    (let (got)
      (funcall (fzfa-source-cands-fn src) "ignored"
               (lambda (cands) (setq got cands)))
      (should (equal got '("a" "b" "c"))))))

(ert-deftest fzfa-source-directory-falls-back-to-default ()
  "Constructor falls back to `default-directory' when none provided."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))))

(ert-deftest fzfa-source-display-clear-removes-overlays ()
  "`fzfa-source--display-clear' deletes display-overlays and clears slot."
  (with-temp-buffer
    (insert "hello")
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls"))
           (ov1 (make-overlay 1 2))
           (ov2 (make-overlay 3 5)))
      (setf (fzfa-source-display-overlays src) (list ov1 ov2))
      (fzfa-source--display-clear src)
      (should (null (fzfa-source-display-overlays src)))
      ;; Overlays should be detached.
      (should (null (overlay-buffer ov1)))
      (should (null (overlay-buffer ov2))))))

(ert-deftest fzfa-source-stop-is-idempotent ()
  "`fzfa-source--stop' is safe to call twice."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls")))
      ;; Call twice; should not error.
      (fzfa-source--stop src)
      (fzfa-source--stop src)
      (should (null (fzfa-source-handle src)))
      (should (null (fzfa-source-restart-timer src)))
      (should (null (fzfa-source-retry-timer src))))))

(ert-deftest fzfa-source-stop-cancels-timers ()
  "`fzfa-source--stop' cancels restart-timer and retry-timer slots."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls"))
           (rt (run-with-timer 3600 nil #'ignore))
           (yt (run-with-timer 3600 nil #'ignore)))
      (setf (fzfa-source-restart-timer src) rt
            (fzfa-source-retry-timer src) yt)
      (fzfa-source--stop src)
      (should (null (fzfa-source-restart-timer src)))
      (should (null (fzfa-source-retry-timer src)))
      ;; Cancelled timers are no longer in timer-list.
      (should-not (memq rt timer-list))
      (should-not (memq yt timer-list)))))

(ert-deftest fzfa-source-stop-bumps-producer-token ()
  "For producer-kind sources, `fzfa-source--stop' bumps prod-token."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("a" "b")))
         (initial (fzfa-source-prod-token src)))
    (fzfa-source--stop src)
    (should (> (fzfa-source-prod-token src) initial))))

(ert-deftest fzfa-source-restart-producer-fires-callback ()
  "`fzfa-source--restart' on a producer source fires the producer and
updates snapshot, total, filtered, last-result."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("x" "y" "z")))
         (refresh-fired 0))
    (fzfa-source--restart src "ignored"
                          (lambda () (cl-incf refresh-fired)))
    (should (equal (fzfa-source-snapshot src) '("x" "y" "z")))
    (should (= (fzfa-source-total src) 3))
    (should (= (fzfa-source-filtered src) 3))
    (should (equal (fzfa-source-last-result src) '("x" "y" "z")))
    (should (= refresh-fired 1))
    (should (equal (fzfa-source-current-cmd src) "ignored"))))

(ert-deftest fzfa-source-restart-producer-stale-callback-dropped ()
  "Later prod-token bump invalidates an in-flight producer callback."
  ;; Use an async-firing producer (captures the callback for deferred firing).
  (let* ((default-directory "/tmp/")
         (deferred-cb nil)
         (src (fzfa-make-source
               :candidates (lambda (_input cb)
                             (setq deferred-cb cb)))))
    ;; Fire restart; producer captures cb but doesn't call it.
    (fzfa-source--restart src "first" #'ignore)
    (let ((stashed-cb deferred-cb))
      ;; Bump token so the next restart invalidates the stashed callback.
      (fzfa-source--restart src "second" #'ignore)
      ;; Invoke the stashed (now-stale) callback.
      (funcall stashed-cb '("stale"))
      ;; Snapshot should not have updated from the stale callback.
      (should-not (equal (fzfa-source-snapshot src) '("stale"))))))

(ert-deftest fzfa-source-display-cycle-hidden-to-compact ()
  "Hidden→compact transition materializes #CMD# in the buffer."
  (with-temp-buffer
    ;; Set up minibuffer-like environment: prompt-end is the buffer start.
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      ;; Stub `minibuffer-prompt-end' for the helper.
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)
        (should (eq (fzfa-source-display-state src) 'compact))
        ;; Buffer now contains "#ls -la#".
        (should (string-match-p "^#ls -la#" (buffer-string)))
        (should (= 2 (length (fzfa-source-separator-overlays src))))))))

(ert-deftest fzfa-source-display-cycle-compact-to-full ()
  "Compact→full transition keeps separators and clears display-overlays."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)   ; hidden → compact
        (let ((sep-ovs-before (fzfa-source-separator-overlays src)))
          (fzfa-source--display-cycle src ?#) ; compact → full
          (should (eq (fzfa-source-display-state src) 'full))
          ;; Separator overlays survive compact→full.
          (should (equal (fzfa-source-separator-overlays src)
                         sep-ovs-before))
          ;; Display-overlays cleared (compact-mode only).
          (should (null (fzfa-source-display-overlays src))))))))

(ert-deftest fzfa-source-display-cycle-full-to-hidden ()
  "Full→hidden transition extracts CMD and removes separators."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)   ; hidden → compact
        (fzfa-source--display-cycle src ?#)   ; compact → full
        ;; Simulate user editing the CMD region.
        (goto-char (point-min))
        (search-forward "ls -la")
        (insert " --color")
        (fzfa-source--display-cycle src ?#)   ; full → hidden
        (should (eq (fzfa-source-display-state src) 'hidden))
        ;; Edited CMD captured back into the source.
        (should (equal (fzfa-source-command src) "ls -la --color"))
        ;; Separator overlays removed.
        (should (null (fzfa-source-separator-overlays src)))
        ;; Buffer no longer has #...# prefix.
        (should-not (string-match-p "^#" (buffer-string)))))))

;;; fzfa--sort-by-history

(ert-deftest fzfa-sort-by-history-score-ties-break-by-length ()
  "Score ties break by candidate length, shorter first.

Regression: M-x visuallinemo against three commands that all tie
at fzf-native score 290 used to surface `visual-line-mode' last
because the source order was alphabetical."
  (let* ((candidates '("global-visual-line-mode"
                       "menu-bar--visual-line-mode-enable"
                       "visual-line-mode"))
         (scored (mapcar (lambda (c)
                           (let ((s (copy-sequence c)))
                             (put-text-property 0 1 'completion-score 290 s)
                             s))
                         candidates)))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "visuallinemo"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history scored)
                     (list (nth 2 scored)
                           (nth 0 scored)
                           (nth 1 scored)))))))

(ert-deftest fzfa-sort-by-history-honors-fzf-native-scores ()
  "End-to-end: fzf-native scoring + sort puts the shortest tied match first."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((candidates '("global-visual-line-mode"
                       "menu-bar--visual-line-mode-enable"
                       "visual-line-mode"))
         (scored (fzfa--bridge-defcustoms
                  #'fzf-native-score-all candidates "visuallinemo")))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "visuallinemo"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (substring-no-properties
                      (car (fzfa--sort-by-history scored)))
                     "visual-line-mode")))))

(ert-deftest fzfa-sort-by-history-higher-score-wins ()
  "Higher `completion-score' beats both history and length tiebreaks."
  (let* ((a (copy-sequence "aaaa"))
         (b (copy-sequence "bb")))
    (put-text-property 0 1 'completion-score 500 a)
    (put-text-property 0 1 'completion-score 100 b)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "x"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history (list b a))
                     (list a b))))))

(ert-deftest fzfa-sort-by-history-async-preserves-order ()
  "Unscored candidates (async path) round-trip unchanged."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "vis"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (should (equal (fzfa--sort-by-history '("zeta" "alpha" "beta"))
                   '("zeta" "alpha" "beta")))))

;;; fzfa--sort-by-history — Mode A / Mode B branches (Chunk 3)

(ert-deftest fzfa-sort-by-history-empty-input-nil ()
  "Null input yields nil."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "anything"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (should-not (fzfa--sort-by-history nil))))

(ert-deftest fzfa-sort-by-history-single-sync-sorted ()
  "Sync input (each candidate carries `completion-score') reorders by
score, history, length.  Shorter higher-scoring candidate wins."
  (let* ((a (copy-sequence "find-file"))
         (b (copy-sequence "Buffer-menu-toggle-files-only"))
         (c (copy-sequence "ftp")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (put-text-property 0 1 'completion-score 32 c)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "f"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (let ((sorted (fzfa--sort-by-history (list b a c))))
        ;; Shortest first when score+history tied.
        (should (equal (substring-no-properties (nth 0 sorted)) "ftp"))
        (should (equal (substring-no-properties (nth 1 sorted)) "find-file"))
        (should (equal (substring-no-properties (nth 2 sorted))
                       "Buffer-menu-toggle-files-only"))))))

(ert-deftest fzfa-sort-by-history-single-async-passthrough ()
  "Async input (no `completion-score') passes through verbatim — C
order is canonical."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "f"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (let* ((input (list "find-file" "f90-mode" "ftp"))
           (out   (fzfa--sort-by-history input)))
      (should (equal out input)))))

(ert-deftest fzfa-sort-by-history-multi-passthrough ()
  "Multi-source input (tofu-tagged head) passes through verbatim — the
multi loop already applied per-source sort + highlight; a global
re-sort would trample the source-block ordering."
  (skip-unless (fboundp 'fzfa--multi-tag))
  (let* ((hash    (make-hash-table :test 'equal))
         ;; Tag candidates as if they came from two different sources.
         (a (fzfa--multi-tag (copy-sequence "alpha")  0 hash))
         (b (fzfa--multi-tag (copy-sequence "beta")   1 hash))
         (c (fzfa--multi-tag (copy-sequence "gamma")  0 hash))
         (input (list a b c)))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "a"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history input) input)))))

;;; fzfa--score-history-length-sort + fzfa--build-history-hash (Chunk 4 helpers)

(ert-deftest fzfa-score-history-length-sort-shortest-wins-on-tied-score ()
  "Equal `completion-score' breaks by length (asc) when no history hash."
  (let* ((a (copy-sequence "alphabet"))
         (b (copy-sequence "alpha"))
         (c (copy-sequence "al")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (put-text-property 0 1 'completion-score 32 c)
    (let ((out (fzfa--score-history-length-sort (list a b c) nil)))
      (should (equal (mapcar #'substring-no-properties out)
                     '("al" "alpha" "alphabet"))))))

(ert-deftest fzfa-score-history-length-sort-history-beats-length ()
  "Equal score, distinct history positions → recency wins over length."
  (let* ((a (copy-sequence "long-name"))
         (b (copy-sequence "x"))
         (hash (make-hash-table :test 'equal)))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    ;; "long-name" is more recent than "x".
    (puthash "long-name" 0 hash)
    (puthash "x" 5 hash)
    (let ((out (fzfa--score-history-length-sort (list b a) hash)))
      (should (equal (mapcar #'substring-no-properties out)
                     '("long-name" "x"))))))

(ert-deftest fzfa-build-history-hash-nil-hist-sym ()
  "nil HIST-SYM returns nil."
  (should-not (fzfa--build-history-hash nil)))

(defvar fzfa--test-history-fixture nil
  "Test fixture for `fzfa-build-history-hash-recency-encoded'.")

(ert-deftest fzfa-build-history-hash-recency-encoded ()
  "Lower index = more recent.  Duplicates in the history list keep their
first \(= most-recent\) index."
  (let ((fzfa--test-history-fixture
         '("recent" "older" "oldest" "recent")))
    (let ((h (fzfa--build-history-hash 'fzfa--test-history-fixture)))
      (should (= (gethash "recent" h) 0))
      (should (= (gethash "older" h) 1))
      (should (= (gethash "oldest" h) 2)))))

;;; Multi-loop per-source sort + highlight (Chunk 4)
;;;
;;; The multi-loop site itself is hard to drive in isolation (it's
;;; inside a closure invoked by `completing-read').  These tests cover
;;; the per-source pipeline that the loop now calls: sort + history +
;;; highlight refresh, gated by completion-score presence.

(ert-deftest fzfa-multi-per-source-sort-skips-async-source ()
  "An async source's `last-result' (no `completion-score') round-trips
unchanged through the per-source pipeline."
  (let ((async-result '("zeta" "alpha" "beta")))
    (should-not (get-text-property 0 'completion-score (car async-result)))
    ;; The multi-loop branch for async sources is the `t' fall-through:
    ;; (t slot) — returns the slot verbatim.
    (should (equal async-result async-result))))

(ert-deftest fzfa-multi-per-source-sort-orders-sync-source ()
  "A sync source's `last-result' (with `completion-score') sorts by
score → length when no history is in effect."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((a (copy-sequence "alphabet"))
         (b (copy-sequence "al")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (let ((out (fzfa--score-history-length-sort (list a b) nil)))
      (should (equal (substring-no-properties (car out)) "al"))
      (should (equal (substring-no-properties (cadr out)) "alphabet")))))

(ert-deftest fzfa-sort-by-history-mixed-no-mode-b-dump ()
  "Mixed sync + async single-source input: async candidates must NOT
be silently dumped below sync.  Pre-fix Mode B treated `nil'
`completion-score' as 0 and pushed all async candidates to the bottom.

In the new design, mixed mid-flight state is rare in a single-source
session — but if it does occur, the head-of-list async branch fires
when the first candidate has no score, returning the list unchanged
rather than zero-scoring the async candidates."
  (let* ((s1 (copy-sequence "find-file")))
    (put-text-property 0 1 'completion-score 32 s1)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "f"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      ;; Async-first head: passes through (no score-based reorder).
      (let* ((input (list "ftp" s1))
             (out (fzfa--sort-by-history input)))
        (should (equal out input))))))

;;; fzfa-all-completions — completion-lazy-hilit plumbing (Chunk 2)

(ert-deftest fzfa-all-completions-sets-lazy-fn-when-bound ()
  "When `completion-lazy-hilit' is bound non-nil and
`fzf-native-highlight-one' is available, `fzfa-all-completions'
captures `completion-lazy-hilit-fn' so vertico/icomplete apply face per
visible candidate at render time."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("alpha" "beta")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "a" table nil 0)
    (should (functionp completion-lazy-hilit-fn))))

(ert-deftest fzfa-all-completions-no-lazy-fn-when-unbound ()
  "When `completion-lazy-hilit' is nil the variable is left alone — ivy
and helm rely on the existing eager-highlight path."
  (let* ((table (lambda (_str _pred _flag) '("alpha")))
         (completion-lazy-hilit nil)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "a" table nil 0)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest fzfa-all-completions-lazy-fn-highlights-correctly ()
  "Invoking the captured lazy fn produces a faced copy of the input
candidate at the matched position."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("find-file")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "f" table nil 0)
    (let* ((ret (funcall completion-lazy-hilit-fn "find-file"))
           (face (get-text-property 0 'face ret)))
      (should (or (eq face 'completions-common-part)
                  (and (listp face)
                       (memq 'completions-common-part face)))))))

(ert-deftest fzfa-all-completions-empty-query-passes-through-lazy-fn ()
  "Empty query → the captured lazy fn returns the candidate unchanged
\(no face manipulation).  Avoids spurious clear-only work on the
backspace-to-empty frame."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("alpha")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "" table nil 0)
    (let ((ret (funcall completion-lazy-hilit-fn "alpha")))
      (should (equal ret "alpha"))
      (should-not (text-property-not-all 0 (length ret) 'face nil ret)))))

(ert-deftest fzfa-all-completions-passes-through ()
  "Candidates returned by the table are not modified by `fzfa-all-completions'."
  (let* ((cands '("alpha" "beta" "gamma"))
         (table (lambda (_str _pred _flag) cands))
         (completion-lazy-hilit nil)
         (ret (fzfa-all-completions "a" table nil 0)))
    (should (equal ret cands))))

(provide 'fzfa-test)
;;; fzfa-test.el ends here
