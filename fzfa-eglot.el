;;; fzfa-eglot.el --- Eglot symbol picker for fzfa  -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/fzfa
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, languages
;; Version: 0.1
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pick a workspace symbol via the running eglot server(s).  Each
;; keystroke fires a fresh `workspace/symbol' request; the closure
;; token discards stale callbacks so in-flight requests resolve into
;; the void if they arrive late.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function eglot-current-server "eglot")
(declare-function jsonrpc-async-request "jsonrpc"
                  (connection method params &rest keys))
(declare-function url-generic-parse-url "url-parse" (url))
(declare-function url-unhex-string "url-util" (str &optional allow-newlines))
(declare-function url-filename "url-parse" (cl-x) t)

(defun fzfa-eglot--require ()
  "Load dependencies."
  (require 'jsonrpc)
  (require 'url-parse)
  (require 'url-util))

(defun fzfa-eglot--servers ()
  "Return the list of eglot servers for the current project, or signal."
  (let ((server (and (fboundp 'eglot-current-server) (eglot-current-server))))
    (unless server (user-error "No eglot server for current buffer"))
    (list server)))

(defun fzfa-eglot--uri-to-path (uri)
  "Convert URI to a local file path."
  (when uri
    (let ((url (url-generic-parse-url uri)))
      (url-unhex-string (url-filename url)))))

(defun fzfa-eglot--format (sym)
  "Build a candidate string for the LSP SymbolInformation SYM."
  (let* ((name (plist-get sym :name))
         (loc  (plist-get sym :location))
         (uri  (plist-get loc :uri))
         (rng  (plist-get loc :range))
         (line (1+ (or (plist-get (plist-get rng :start) :line) 0)))
         (path (or (fzfa-eglot--uri-to-path uri) ""))
         (file (file-name-nondirectory path)))
    (propertize (format "%s :: %s:%d" name file line)
                'fzfa-eglot-symbol sym)))

(defun fzfa-eglot--producer ()
  "Build a producer that queries eglot for workspace symbols.

Closure-token discards stale callbacks; in-flight requests resolve
into the void if they arrive late."
  (let ((servers (fzfa-eglot--servers))
        (token 0))
    (lambda (input callback)
      (let ((this-token (cl-incf token))
            (responses nil)
            (remaining (length servers)))
        (dolist (server servers)
          (jsonrpc-async-request
           server :workspace/symbol `(:query ,(or input ""))
           :success-fn
           (lambda (resp)
             (when (= this-token token)
               (setq responses
                     (append responses
                             (mapcar #'fzfa-eglot--format (append resp nil))))
               (cl-decf remaining)
               (funcall callback responses)))
           :error-fn
           (lambda (err)
             (when (= this-token token)
               (cl-decf remaining)
               (fzfa--print "fzfa-eglot: %S" err)))
           :timeout-fn
           (lambda ()
             (when (= this-token token)
               (cl-decf remaining)
               (fzfa--print "fzfa-eglot: workspace/symbol timed out")))))))))

(defun fzfa-eglot--visit (cand)
  "Jump to the SymbolInformation location encoded in CAND."
  (when-let* ((sym (get-text-property 0 'fzfa-eglot-symbol cand))
              (loc (plist-get sym :location))
              (uri (plist-get loc :uri))
              (path (fzfa-eglot--uri-to-path uri))
              (rng (plist-get loc :range)))
    (find-file path)
    (when-let* ((start (plist-get rng :start))
                (line (plist-get start :line))
                (char (plist-get start :character)))
      (goto-char (point-min))
      (forward-line line)
      (forward-char char))))

;;;###autoload
(defun fzfa-eglot-symbols ()
  "Pick a workspace symbol via the running eglot server(s).

Input splits on `fzfa-separator': the CMD half is sent to
the LSP server as the `workspace/symbol' query, the FILTER half
re-scores the responses via fzf-native."
  (interactive)
  (fzfa-eglot--require)
  (when-let* ((r (fzfa-completing-read
                  :prompt "symbol: "
                  :candidates (fzfa-eglot--producer)
                  :category 'fzfa-eglot-symbol
                  :display 'compact
                  :resolve-paths nil
                  :apply #'fzfa-eglot--visit)))
    (fzfa-eglot--visit r)))

(provide 'fzfa-eglot)
;;; fzfa-eglot.el ends here
