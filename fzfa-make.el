;;; fzfa-make.el --- Make / ninja target picker for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Make / ninja target picker for fzfa, modeled after helm-make.
;;
;; Loaded automatically when `make' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.
;;
;; Commands:
;;   `fzfa-make'             Pick a Makefile/build.ninja target and compile it.
;;                           A numeric prefix arg sets the job count.
;;   `fzfa-make-reset-cache' Clear the cached target lists.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defgroup fzfa-make nil
  "Make / ninja integration for fzfa."
  :group 'fzfa)

(defcustom fzfa-make-do-save nil
  "When non-nil, save buffers under the Makefile's directory before compiling."
  :type 'boolean
  :group 'fzfa-make)

(defcustom fzfa-make-build-dir ""
  "Build directory relative to the project root.
When non-empty, `fzfa-make' also searches this directory (and \"build\")
for a Makefile/build.ninja."
  :type 'string
  :group 'fzfa-make)
(make-variable-buffer-local 'fzfa-make-build-dir)

(defcustom fzfa-make-sort-targets nil
  "When non-nil, sort targets alphabetically before display."
  :type 'boolean
  :group 'fzfa-make)

(defcustom fzfa-make-cache-targets nil
  "When non-nil, cache parsed targets keyed by Makefile path + mtime.
Reset the cache with `fzfa-make-reset-cache'."
  :type 'boolean
  :group 'fzfa-make)

(defcustom fzfa-make-executable "make"
  "Name of the make executable."
  :type 'string
  :group 'fzfa-make)

(defcustom fzfa-make-ninja-executable "ninja"
  "Name of the ninja executable."
  :type 'string
  :group 'fzfa-make)

(defcustom fzfa-make-niceness 0
  "When non-zero, run make/ninja at this niceness level (via `nice -n N')."
  :type 'integer
  :group 'fzfa-make)

(defcustom fzfa-make-arguments "-j%d"
  "Arguments passed to the make/ninja executable.
`%d' is substituted with the resolved job count."
  :type 'string
  :group 'fzfa-make)

(defcustom fzfa-make-named-buffer nil
  "When non-nil, rename the compilation buffer based on the target."
  :type 'boolean
  :group 'fzfa-make)

(defcustom fzfa-make-comint nil
  "When non-nil, run the compilation in Comint mode (interactive)."
  :type 'boolean
  :group 'fzfa-make)

(defcustom fzfa-make-nproc 1
  "Default number of jobs (the `-j' value).
0 auto-detects via `nproc' / `sysctl'.  A numeric prefix to `fzfa-make'
overrides this."
  :type 'integer
  :group 'fzfa-make)

(defcustom fzfa-make-list-target-method 'default
  "How to enumerate Makefile targets.
default — pure-elisp regex scan; fast but misses `include'd Makefiles.
qp      — parse `make -nqp' output; accurate but slower.
Ninja build files always use ninja's `-t targets' regardless."
  :type '(choice (const :tag "Default" default)
                 (const :tag "make -nqp" qp))
  :group 'fzfa-make)

(defcustom fzfa-make-makefile-names '("Makefile" "makefile" "GNUmakefile")
  "Makefile filenames recognized by make."
  :type '(repeat string)
  :group 'fzfa-make)

(defcustom fzfa-make-ninja-filename "build.ninja"
  "Ninja build filename."
  :type 'string
  :group 'fzfa-make)

(defcustom fzfa-make-directory-functions-list
  '(fzfa-make-current-directory
    fzfa-make-project-directory
    fzfa-make-dominating-directory)
  "Functions that return candidate Makefile directories, in priority order.
The first one whose returned directory contains a Makefile/build.ninja wins."
  :type '(repeat (choice
                  (const :tag "Default directory"   fzfa-make-current-directory)
                  (const :tag "Project root"        fzfa-make-project-directory)
                  (const :tag "Dominating ancestor"
                         fzfa-make-dominating-directory)
                  (function :tag "Custom function")))
  :group 'fzfa-make)

;;; State

(defvar fzfa-make--target-history nil
  "History list of selected targets.")

(defvar fzfa-make--build-system nil
  "Detected build system for the current invocation: `make' or `ninja'.")

(defvar fzfa-make--last-target nil
  "Last selected target, used as initial input on the next call.")

(defvar fzfa-make--command nil
  "Format-string template for the in-flight compile (final `%s' = target).")

(defvar fzfa-make--db (make-hash-table :test 'equal)
  "Target cache, keyed by absolute Makefile path.")

(cl-defstruct fzfa-make--dbfile targets modtime sorted)

;;; Directory resolution

(defun fzfa-make-current-directory ()
  "Return `default-directory'."
  default-directory)

(defun fzfa-make-project-directory ()
  "Return the current project root, or nil.
Respects `fzfa-project-backend' via `fzfa--default-dir'."
  (let ((fzfa-directory nil))
    (and (or (project-current nil)
             (eq fzfa-project-backend 'projectile)
             (functionp fzfa-project-backend))
         (fzfa--default-dir))))

(defun fzfa-make-dominating-directory ()
  "Return the nearest ancestor containing a Makefile/build.ninja, or nil."
  (locate-dominating-file
   default-directory
   (lambda (dir) (fzfa-make--makefile-exists dir))))

(defun fzfa-make--get-nproc ()
  "Return the number of available CPUs, or 1 on failure."
  (cond
   ((member system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
    (if (executable-find "nproc")
        (string-to-number
         (string-trim (shell-command-to-string "nproc")))
      1))
   ((eq system-type 'darwin)
    (if (executable-find "sysctl")
        (string-to-number
         (string-trim (shell-command-to-string "sysctl -n hw.ncpu")))
      1))
   (t 1)))

(defun fzfa-make--makefile-exists (base-dir &optional dir-list)
  "Return the absolute path of the first Makefile/build.ninja in BASE-DIR.
DIR-LIST is an optional list of subdirectories (relative to BASE-DIR)
to also search.  Sets `fzfa-make--build-system' as a side effect."
  (let* ((default-directory (file-truename base-dir))
         (dirs (or dir-list '("")))
         (names `(,@fzfa-make-makefile-names ,fzfa-make-ninja-filename))
         (candidates
          (cl-loop for d in dirs
                   append (cl-loop for n in names
                                   collect (expand-file-name n d))))
         (makefile (cl-find-if #'file-exists-p candidates)))
    (when makefile
      (setq fzfa-make--build-system
            (if (string-match-p "build\\.ninja\\'" makefile) 'ninja 'make)))
    makefile))

(defun fzfa-make--locate ()
  "Walk `fzfa-make-directory-functions-list' and return the first match."
  (let* ((extras (when (and (stringp fzfa-make-build-dir)
                            (not (string-empty-p fzfa-make-build-dir)))
                   (list fzfa-make-build-dir "build")))
         (dir-list (cons "" extras))
         result)
    (cl-dolist (fn fzfa-make-directory-functions-list)
      (when-let* ((dir (funcall fn))
                  (hit (fzfa-make--makefile-exists dir dir-list)))
        (setq result hit)
        (cl-return)))
    result))

;;; Target parsing

(defun fzfa-make--target-list-ninja (makefile)
  "Return the ninja targets for MAKEFILE via `ninja -t targets all'."
  (let ((default-directory (file-name-directory (expand-file-name makefile)))
        targets)
    (with-temp-buffer
      (call-process fzfa-make-ninja-executable nil t nil
                    "-f" (file-name-nondirectory makefile)
                    "-t" "targets" "all")
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\): " nil t)
        (push (match-string 1) targets)))
    targets))

(defun fzfa-make--target-list-qp (makefile)
  "Return targets for MAKEFILE by parsing `make -nqp' output."
  (let ((default-directory (file-name-directory (expand-file-name makefile)))
        targets target)
    (with-temp-buffer
      (insert
       (shell-command-to-string
        (format "%s -f %s -nqp __BASH_MAKE_COMPLETION__=1 .DEFAULT 2>/dev/null"
                (shell-quote-argument fzfa-make-executable)
                (shell-quote-argument makefile))))
      (goto-char (point-min))
      (unless (re-search-forward "^# Files" nil t)
        (error "Unexpected `make -nqp' output"))
      (while (re-search-forward "^\\([^%$:#\n\t ]+\\):\\([^=]\\|$\\)" nil t)
        (setq target (match-string 1))
        (unless (or (save-excursion
                      (goto-char (match-beginning 0))
                      (forward-line -1)
                      (looking-at-p "^# Not a target:"))
                    (string-match-p "\\`\\([/a-zA-Z0-9_. -]+/\\)?\\." target))
          (push target targets))))
    targets))

(defun fzfa-make--target-list-default (makefile)
  "Return targets for MAKEFILE by lexing the file itself (no `include')."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^: \n]+\\) *:\\(?: \\|$\\)" nil t)
        (let ((s (match-string 1)))
          (unless (string-match-p "\\`\\." s)
            (push s targets)))))
    (nreverse targets)))

(defun fzfa-make--cached-targets (makefile)
  "Return the (optionally cached, optionally sorted) target list for MAKEFILE."
  (let* ((att (file-attributes makefile 'integer))
         (modtime (and att (nth 5 att)))
         (entry (gethash makefile fzfa-make--db))
         (targets
          (cond
           ((and fzfa-make-cache-targets entry
                 (equal modtime (fzfa-make--dbfile-modtime entry))
                 (fzfa-make--dbfile-targets entry))
            (fzfa-make--dbfile-targets entry))
           (t
            (delete-dups
             (cond
              ((eq fzfa-make--build-system 'ninja)
               (fzfa-make--target-list-ninja makefile))
              ((eq fzfa-make-list-target-method 'qp)
               (fzfa-make--target-list-qp makefile))
              (t
               (fzfa-make--target-list-default makefile))))))))
    (when (and fzfa-make-sort-targets
               (not (and fzfa-make-cache-targets entry
                         (fzfa-make--dbfile-sorted entry))))
      (setq targets (sort targets #'string<)))
    (when fzfa-make-cache-targets
      (puthash makefile
               (make-fzfa-make--dbfile
                :targets targets :modtime modtime
                :sorted fzfa-make-sort-targets)
               fzfa-make--db))
    targets))

;;;###autoload
(defun fzfa-make-reset-cache ()
  "Clear the `fzfa-make' target cache."
  (interactive)
  (clrhash fzfa-make--db))

;;; Compile

(defun fzfa-make--construct-command (arg makefile)
  "Return a compile command template for MAKEFILE.
ARG is the prefix arg passed to `fzfa-make'.  The template ends with
\"%s\" so a target name can be `format'-substituted in at action time."
  (let* ((exe (if (eq fzfa-make--build-system 'ninja)
                  fzfa-make-ninja-executable
                fzfa-make-executable))
         (nice (if (zerop fzfa-make-niceness)
                   ""
                 (format "nice -n %d " fzfa-make-niceness)))
         (dir (replace-regexp-in-string
               "\\`/\\(scp\\|ssh\\).+?:.+?:" ""
               (shell-quote-argument (file-name-directory makefile))))
         (jobs (abs (if arg (prefix-numeric-value arg)
                      (if (zerop fzfa-make-nproc)
                          (fzfa-make--get-nproc)
                        fzfa-make-nproc))))
         (jobs (if (> jobs 0) jobs 1)))
    (format (concat "%s%s -C %s " fzfa-make-arguments " %%s")
            nice exe dir jobs)))

(defun fzfa-make--save-buffers (dir)
  "Save buffers whose file is under DIR."
  (let ((re (concat "\\`" (regexp-quote (expand-file-name dir)))))
    (dolist (b (buffer-list))
      (when-let* ((name (buffer-file-name b)))
        (when (string-match-p re (expand-file-name name))
          (with-current-buffer b
            (when (buffer-modified-p)
              (save-buffer))))))))

(defun fzfa-make--rename-buffer (buffer target)
  "Rename BUFFER to reflect that it's running TARGET in `default-directory'."
  (let ((name (format "*compilation in %s (%s)*"
                      (abbreviate-file-name default-directory)
                      target)))
    (when (get-buffer name) (kill-buffer name))
    (with-current-buffer buffer (rename-buffer name))))

(declare-function compile "compile" (command &optional comint))

(defun fzfa-make--action (target)
  "Run `compile' on TARGET using `fzfa-make--command'."
  (setq fzfa-make--last-target target)
  (let ((buf (compile (format fzfa-make--command target)
                      fzfa-make-comint)))
    (when fzfa-make-named-buffer
      (fzfa-make--rename-buffer buf target))))

;;;###autoload
(defun fzfa-make (&optional arg)
  "Pick a make/ninja target via fzfa and compile it.
A numeric prefix ARG overrides `fzfa-make-nproc' for the `-j' flag."
  (interactive "P")
  (let ((makefile (fzfa-make--locate)))
    (unless makefile
      (error "No Makefile or build.ninja found from %s"
             default-directory))
    (setq fzfa-make--command
          (fzfa-make--construct-command arg makefile))
    (let ((default-directory (file-name-directory makefile))
          (targets (fzfa-make--cached-targets makefile)))
      (when fzfa-make-do-save
        (fzfa-make--save-buffers default-directory))
      (when fzfa-make--target-history
        (setq fzfa-make--target-history
              (delete-dups fzfa-make--target-history)))
      (when-let* ((target (fzfa-sync-completing-read
                           :candidates targets
                           :prompt
                           (format "%s (%s): "
                                   (if (eq fzfa-make--build-system 'ninja)
                                       "ninja" "make")
                                   (file-name-nondirectory makefile))
                           :category 'fzfa-make-target
                           :history 'fzfa-make--target-history)))
        (fzfa-make--action target)))))

;;;###autoload
(defun fzfa-make-setup ()
  "Register the `fzfa-make-target' completion category."
  (add-to-list 'completion-category-overrides
               '(fzfa-make-target (styles fzfa))))

(provide 'fzfa-make)
;;; fzfa-make.el ends here
