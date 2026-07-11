;;; fzfa-loader.el --- Load-time helpers for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone home for `fzfa-sync-autoloads' so calling it from
;; `use-package' `:init' does not eagerly load `fzfa.el'.
;;
;; The function reads `fzfa-extension-registry' and `fzfa-extensions',
;; both of which are already `;;;###autoload'-cookied in `fzfa.el' and
;; therefore emitted verbatim into `fzfa-autoloads.el' — so the pruning
;; runs without touching `fzfa.el' proper.

;;; Code:

;;;###autoload
(defun fzfa-sync-autoloads ()
  "Unbind autoload stubs for extensions not in `fzfa-extensions'.

Iterates `fzfa-extension-registry'; for every extension EXT not
present in `fzfa-extensions', `fmakunbound's any symbol whose name
matches `fzfa-EXT' or `fzfa-EXT-...' — but only when the symbol's
current function binding is still an autoload stub (`autoloadp').
Already-loaded functions and non-fzfa symbols are left alone.

Idempotent.  Intended for the `:init' clause of a `use-package'
block — runs after `fzfa-autoloads.el' has been loaded by
`package-activate' but before any fzfa command can be invoked, so
excluded extensions never appear in `M-x', `where-is',
`describe-command', even on a cold start:

  (use-package fzfa
    :defer t
    :init
    (setq fzfa-extensions \\='(ag rg vertico embark))
    (fzfa-sync-autoloads))

If `fzfa-extensions' is changed at runtime after this has run,
call this function again to prune newly-excluded extensions.  Note
that this function can only prune; it cannot resurrect stubs for
extensions added back to the list.  Re-evaluate `fzfa-autoloads.el'
or restart Emacs for that case."
  (interactive)
  (pcase-dolist (`(,ext . ,_) fzfa-extension-registry)
    (unless (memq ext fzfa-extensions)
      (let ((prefix (format "fzfa-%s" ext)))
        (mapatoms
         (lambda (sym)
           (let ((name (symbol-name sym)))
             (when (and (fboundp sym)
                        (autoloadp (symbol-function sym))
                        (or (string= name prefix)
                            (string-prefix-p (concat prefix "-") name)))
               (fmakunbound sym)))))))))

(provide 'fzfa-loader)

;; Local Variables:
;; package-lint-main-file: "fzfa.el"
;; End:

;;; fzfa-loader.el ends here
