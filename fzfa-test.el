;;; fzfa-test.el --- Tests for fzfa  -*- lexical-binding: t; -*-

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
  "Non-file buffers use the buffer name verbatim as the SOURCE field."
  (let ((buf (generate-new-buffer " *fzfa-test-src*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (rename-buffer "fzfa-test-buf" t)
            (insert "hello\n"))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buf))))
            (let* ((args (fzfa-test--extract #'fzfa-swiper-all))
                   (cands (plist-get args :items)))
              (should (member "fzfa-test-buf:1:hello" cands)))))
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
  (let ((fzfa-preview-functions '((cat . (:preview ignore))))
        (fzfa-preview-delay 0.3))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore)))
  ;; Unknown category yields nil even when delay is set.
  (let ((fzfa-preview-functions '((cat . (:preview ignore))))
        (fzfa-preview-delay 0.3))
    (should (null (fzfa--preview-handler nil 'unknown))))
  ;; nil delay disables preview globally — registry + explicit override both ignored.
  (let ((fzfa-preview-functions '((cat . (:preview ignore))))
        (fzfa-preview-delay nil))
    (should (null (fzfa--preview-handler nil 'cat)))
    (should (null (fzfa--preview-handler #'identity 'cat))))
  ;; Explicit function override bypasses the registry.
  (let ((fzfa-preview-functions '((cat . (:preview ignore))))
        (fzfa-preview-delay 0.3))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit plist override bypasses the registry.
  (let ((fzfa-preview-functions '((cat . (:preview ignore))))
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

(provide 'fzfa-test)
;;; fzfa-test.el ends here
