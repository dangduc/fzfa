;;; fzfa-tramp.el --- TRAMP-aware spawning for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; TRAMP support for fzfa.
;;
;; Makes fzfa's shell sources spawn transparently against TRAMP `ssh'
;; paths: for a TRAMP `ssh'-method `default-directory' the shell
;; command is wrapped as `ssh HOST 'cd LOCALNAME && CMD'' and
;; fzf-native is handed a safe local chdir target.  The C-layer's
;; fork+pipe+parallel-score pipeline is unchanged; the remote binary
;; streams over ssh into the same pipe reader as a local process.
;;
;; When `tramp-use-connection-share' is non-nil (Emacs 30 default on
;; non-Windows), TRAMP has already opened a ControlMaster socket for
;; the host, so the wrapped ssh call piggybacks on that socket with
;; no handshake.  With sharing off, each fzfa invocation pays one
;; handshake — acceptable for interactive use.
;;
;; Commands:
;;   `fzfa-tramp'   Connect to a host from ~/.ssh/config

;;; Code:

(require 'cl-lib)
(require 'fzfa)

(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
;; `cl-defstruct' slot accessors — check-declare can't see auto-generated
;; bodies, only literal `defun's.  ARGLIST is `t' (unspecified) not `nil'
;; (which would mean "takes 0 args" and trip byte-compile on 1-arg sites).
(declare-function tramp-file-name-method "tramp" t t)
(declare-function tramp-file-name-user "tramp" t t)
(declare-function tramp-file-name-host "tramp" t t)
(declare-function tramp-file-name-port "tramp" t t)
(declare-function tramp-file-name-localname "tramp" t t)

(defcustom fzfa-tramp-safe-local-dir "/"
  "Local directory handed to `fzf-native-async-start' for ssh-wrapped calls.

The C layer's `chdir' runs before the wrapped `ssh HOST cmd', so the
choice is irrelevant to the actual working directory of the remote
command (that's set inside the ssh payload).  It only needs to be a
`chdir'-able local path."
  :type 'directory
  :group 'fzfa)

(defun fzfa-tramp--build-ssh-cmd (vec cmd)
  "Build `ssh USER@HOST -p PORT \\='cd LOCALNAME && CMD\\='' for VEC.

VEC is a `tramp-file-name' struct.  Returns a shell string suitable
for `fzf-native-async-start' (which execs it via /bin/sh -c)."
  (let* ((user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec))
         (localname (tramp-file-name-localname vec))
         (target (if user (format "%s@%s" user host) host))
         (port-args (if port (format " -p %s" port) ""))
         (remote-cmd (format "cd %s && %s"
                             (shell-quote-argument localname)
                             cmd)))
    (format "ssh%s %s %s"
            port-args
            (shell-quote-argument target)
            (shell-quote-argument remote-cmd))))

(defun fzfa-tramp--transform (cmd dir)
  "Spawn transform for `fzfa-source-spawn-transform-function'.

For TRAMP `ssh'-method paths, wraps CMD as
`ssh HOST \\='cd LOCALNAME && CMD\\='' and returns
`fzfa-tramp-safe-local-dir' as the chdir target.  Non-`ssh' TRAMP
methods and local paths pass through unchanged."
  (if (not (file-remote-p dir))
      (cons cmd dir)
    (require 'tramp)
    (let ((vec (tramp-dissect-file-name dir)))
      (if (equal (tramp-file-name-method vec) "ssh")
          (cons (fzfa-tramp--build-ssh-cmd vec cmd)
                fzfa-tramp-safe-local-dir)
        (cons cmd dir)))))

;;;###autoload
(defun fzfa-tramp ()
  "Connect to a remote host via TRAMP, with hosts from ~/.ssh/config."
  (interactive)
  (cl-labels ((ssh-hosts ()
                (let ((config (expand-file-name "~/.ssh/config"))
                      hosts)
                  (when (file-readable-p config)
                    (with-temp-buffer
                      (insert-file-contents config)
                      (while (re-search-forward
                              "^[Hh]ost[[:space:]]+\\(.+\\)" nil t)
                        (dolist (host (split-string (match-string 1)))
                          (unless (string-match-p "[*?!]" host)
                            (push host hosts))))))
                  (nreverse hosts))))
    (when-let* ((hosts (or (ssh-hosts)
                           (user-error "No SSH hosts in ~/.ssh/config")))
                (host (fzfa-completing-read
                       :candidates hosts :prompt "ssh: ")))
      (fzfa-visit-file (concat "/ssh:" host ":")))))

;;;###autoload
(defun fzfa-tramp-setup ()
  "Install fzfa's TRAMP-aware source spawn transform."
  (setq fzfa-source-spawn-transform-function #'fzfa-tramp--transform))

(provide 'fzfa-tramp)
;;; fzfa-tramp.el ends here
