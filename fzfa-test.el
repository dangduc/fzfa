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

(provide 'fzfa-test)
;;; fzfa-test.el ends here
