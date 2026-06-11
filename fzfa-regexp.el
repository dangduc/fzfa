;;; fzfa-regexp.el --- Regexp picker for fzfa  -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/fzfa
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching
;; Version: 0.1

;;; Commentary:

;; Pick a line from the current buffer by Elisp regexp.  Empty input
;; emits every line; FZF then re-scores against the same input.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defface fzfa-regexp-match
  '((t :underline t))
  "Face for the regexp match within an `fzfa-regexp' candidate line."
  :group 'fzfa)

(defun fzfa-regexp--format-line (buf line text)
  "Build candidate for line LINE of BUF; no match face.

TEXT is the line text."
  (propertize (format "%d: %s" line text)
              'fzfa-regexp-buffer buf
              'fzfa-regexp-line line))

(defun fzfa-regexp--format-match (buf line beg end)
  "Build candidate for a regexp match in BUF.

LINE is the line number.  BEG and END bound the match in the
buffer.  The matched region within the candidate's line text
carries `fzfa-regexp-match'."
  (with-current-buffer buf
    (let* ((line-beg (save-excursion (goto-char beg) (pos-bol)))
           (line-end (save-excursion (goto-char beg) (pos-eol)))
           (text     (buffer-substring-no-properties line-beg line-end))
           (col      (- beg line-beg))
           (mlen     (- end beg))
           (start    (+ (length (format "%d:%d: " line col)) col)))
      (let ((cand (format "%d:%d: %s" line col text)))
        (add-face-text-property start (+ start mlen)
                                'fzfa-regexp-match nil cand)
        (propertize cand
                    'fzfa-regexp-buffer buf
                    'fzfa-regexp-position beg)))))

(defun fzfa-regexp--producer ()
  "Build a producer scanning the originating buffer for an Elisp regexp.

Empty INPUT emits every line of the buffer."
  (let ((buf (current-buffer)))
    (lambda (input callback)
      (with-current-buffer buf
        (let (matches)
          (save-excursion
            (goto-char (point-min))
            (if (or (null input) (string-empty-p input))
                (let ((line 1))
                  (while (and (not (input-pending-p))
                              (not (eobp)))
                    (push (fzfa-regexp--format-line
                           buf line
                           (buffer-substring-no-properties (pos-bol) (pos-eol)))
                          matches)
                    (forward-line 1)
                    (cl-incf line)))
              (condition-case nil
                  (while (and (not (input-pending-p))
                              (re-search-forward input nil t))
                    (push (fzfa-regexp--format-match
                           buf (line-number-at-pos)
                           (match-beginning 0) (match-end 0))
                          matches))
                ;; Invalid regexp — surface as empty result for this tick.
                (invalid-regexp nil))))
          (funcall callback (nreverse matches)))))))

(defun fzfa-regexp--jump (cand)
  "Visit the buffer/line CAND points back to."
  (when-let* ((buf (get-text-property 0 'fzfa-regexp-buffer cand)))
    (switch-to-buffer buf)
    (cond
     ((get-text-property 0 'fzfa-regexp-position cand)
      (goto-char (get-text-property 0 'fzfa-regexp-position cand)))
     ((get-text-property 0 'fzfa-regexp-line cand)
      (goto-char (point-min))
      (forward-line (1- (get-text-property 0 'fzfa-regexp-line cand)))))))

;;;###autoload
(defun fzfa-regexp ()
  "Pick a line from the current buffer by Elisp regexp.

Input splits on `fzfa-separator': the CMD half is the Elisp
regexp evaluated against the buffer, the FILTER half re-scores the
matches via fzf-native.  Empty CMD lists every line."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :prompt "regexp: "
                  :candidates (fzfa-regexp--producer)
                  :category 'fzfa-regexp
                  :display 'compact
                  :resolve-paths nil
                  :apply #'fzfa-regexp--jump)))
    (fzfa-regexp--jump r)))

(provide 'fzfa-regexp)
;;; fzfa-regexp.el ends here
