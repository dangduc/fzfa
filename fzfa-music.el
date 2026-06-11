;;; fzfa-music.el --- MacOS Music.app integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A `fzfa' extension to interact with OSX's Music app.
;;
;; Loaded automatically when `music' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  Requires macOS — uses
;; `osascript' (JXA) to dump the Music.app library and to play tracks.
;;
;; Strategy: dump the entire library once via JXA, present via
;; `fzfa-completing-read', play the selection by persistent ID.
;;
;; Commands:
;;   `fzfa-music'             Flat list of all tracks
;;   `fzfa-music-by-artist'   Tracks grouped under per-artist headers
;;   `fzfa-music-by-genre'    Tracks grouped under per-genre headers
;;                                 (candidate prefix includes the genre, so
;;                                 typing e.g. \"rock\" narrows by genre)
;;   `fzfa-music-playlist'    Pick a playlist and play it
;;   `fzfa-music-refresh'     Drop the cached library and playlists
;;
;; Tracks and playlists are cached separately for the session.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(defcustom fzfa-music-dump-timeout 30
  "Seconds to wait for the Music.app library dump before giving up."
  :type 'number
  :group 'fzfa)

(defconst fzfa-music--dump-script
  "var m = Application('Music');

   var t = m.tracks;
   var ids = t.persistentID();
   var ar = t.artist();
   var al = t.album();
   var nm = t.name();
   var gn = t.genre();
   var out = [];
   for (var i = 0; i < ids.length; i++) {
     out.push(ids[i] + '\\t' + ar[i] + '\\t' +
              al[i] + '\\t' + nm[i] + '\\t' + gn[i]);
   }
   out.join('\\n');"
  "JXA snippet returning tab-separated id/artist/album/name/genre lines.")

(defvar fzfa-music--cache nil
  "Cached tracks.

Each entry is a plist with `:id', `:artist', `:album', `:name', and
`:genre' keys.")

(defvar fzfa-music--playlists-cache nil
  "Cached playlists, each entry a plist with `:id' and `:name' keys.")

(defconst fzfa-music--playlists-script
  "var m = Application('Music');

   var p = m.playlists;
   var ids = p.persistentID();
   var names = p.name();
   var out = [];
   for (var i = 0; i < ids.length; i++) {
     out.push(ids[i] + '\\t' + names[i]);
   }
   out.join('\\n');"
  "JXA snippet returning tab-separated id/name lines for each playlist.")

(defvar fzfa-music--items nil
  "Dynamic per-call hash table mapping candidate string -> track plist.

Bound by `fzfa-music--read' so the `:group' callback can look up
metadata for the candidate currently being rendered.")

(defun fzfa-music--osascript-lines (script)
  "Run JXA SCRIPT via `osascript', return non-empty stdout lines."
  (unless (eq system-type 'darwin)
    (user-error "Fzfa-music requires macOS"))
  (with-temp-buffer
    (let ((rc (with-timeout (fzfa-music-dump-timeout
                             (user-error "Music.app query timed out after %ss"
                                         fzfa-music-dump-timeout))
                (call-process "osascript" nil t nil
                              "-l" "JavaScript" "-e" script))))
      (unless (zerop rc)
        (user-error "Osascript failed (exit %s): %s" rc (buffer-string)))
      (split-string (buffer-string) "\n" t))))

(defun fzfa-music--dump ()
  "Dump Music.app's track library into a list of plists."
  (cl-loop for line in (fzfa-music--osascript-lines
                        fzfa-music--dump-script)
           for parts = (split-string line "\t")
           when (>= (length parts) 4)
           collect (list :id     (nth 0 parts)
                         :artist (nth 1 parts)
                         :album  (nth 2 parts)
                         :name   (nth 3 parts)
                         :genre  (or (nth 4 parts) ""))))

(defun fzfa-music--dump-playlists ()
  "Dump Music.app's playlists into a list of plists."
  (cl-loop for line in (fzfa-music--osascript-lines
                        fzfa-music--playlists-script)
           for parts = (split-string line "\t")
           when (= (length parts) 2)
           collect (list :id (nth 0 parts) :name (nth 1 parts))))

(defun fzfa-music--tracks ()
  "Return cached track list, dumping Music.app on first use."
  (or fzfa-music--cache
      (setq fzfa-music--cache
            (with-temp-message "Loading Music.app library..."
              (fzfa-music--dump)))))

(defun fzfa-music--playlists ()
  "Return cached playlist list, dumping Music.app on first use."
  (or fzfa-music--playlists-cache
      (setq fzfa-music--playlists-cache
            (with-temp-message "Loading Music.app playlists..."
              (fzfa-music--dump-playlists)))))

(defun fzfa-music--read (tracks group-key prompt)
  "Read TRACKS via `fzfa-completing-read'; return the chosen plist.

PROMPT is shown in the minibuffer.
GROUP-KEY is one of nil, `:artist', or `:genre'.  When non-nil:
- TRACKS are sorted by GROUP-KEY so consecutive same-key entries cluster.
- For `:genre' the group value is also prefixed to the candidate string
  so fuzzy queries can match on it.
- A `:group' function is installed for vertico-style section headers and
  strips the redundant prefix in TRANSFORM=t mode."
  (let* ((sorted (if group-key
                     (cl-sort (copy-sequence tracks) #'string<
                              :key (lambda (p)
                                     (downcase
                                      (or (plist-get p group-key) ""))))
                   tracks))
         (map (make-hash-table :test #'equal))
         (cands
          (mapcar
           (lambda (p)
             (let* ((base (format "%s — %s — %s"
                                  (plist-get p :artist)
                                  (plist-get p :album)
                                  (plist-get p :name)))
                    (cand (if (eq group-key :genre)
                              (format "%s — %s"
                                      (or (plist-get p :genre) "(no genre)")
                                      base)
                            base)))
               (puthash cand p map)
               cand))
           sorted))
         (fzfa-music--items map)
         (group-fn
          (when group-key
            (lambda (cand transform)
              (let ((p (gethash cand fzfa-music--items)))
                (cond
                 ((null p) cand)
                 ((null transform)
                  (let ((g (plist-get p group-key)))
                    (if (or (null g) (string-empty-p g)) "(none)" g)))
                 ;; TRANSFORM=t — strip the redundant prefix from the
                 ;; per-row display so we don't double-show the group key.
                 ((eq group-key :genre)
                  (format "%s — %s — %s"
                          (plist-get p :artist) (plist-get p :album)
                          (plist-get p :name)))
                 ((eq group-key :artist)
                  (format "%s — %s"
                          (plist-get p :album) (plist-get p :name)))
                 (t cand)))))))
    (when-let* ((sel (fzfa-completing-read
                      :candidates cands
                      :prompt prompt
                      :category 'fzfa-music
                      :group group-fn)))
      (gethash sel fzfa-music--items))))

(defun fzfa-music--pick-and-play (group-key prompt)
  "Pick a track grouped by GROUP-KEY (or flat) with PROMPT, then play it."
  (when-let* ((item (fzfa-music--read
                     (fzfa-music--tracks) group-key prompt)))
    (call-process
     "osascript" nil 0 nil "-e"
     (format
      (concat "tell application \"Music\" to play"
              " (some track whose persistent ID is %S)")
      (plist-get item :id)))))

;;;###autoload
(defun fzfa-music-refresh ()
  "Invalidate cached Music.app library and playlists so they re-dump."
  (interactive)
  (setq fzfa-music--cache           nil
        fzfa-music--playlists-cache nil)
  (message "Music.app caches cleared"))

(defun fzfa-music--pick-playlist (prompt)
  "Fuzzy-select a Music.app playlist with PROMPT; return its plist or nil."
  (let* ((playlists (fzfa-music--playlists))
         (map (make-hash-table :test #'equal))
         (cands (mapcar (lambda (p)
                          (let ((n (plist-get p :name)))
                            (puthash n p map) n))
                        playlists)))
    (when-let* ((sel (fzfa-completing-read
                      :candidates cands
                      :prompt prompt
                      :category 'fzfa-music)))
      (gethash sel map))))

(defun fzfa-music--play-playlist (item shuffle)
  "Play playlist ITEM with shuffle on or off per SHUFFLE."
  (call-process
   "osascript" nil 0 nil "-e"
   (format
    (concat "tell application \"Music\"\n"
            "set shuffle enabled to %s\n"
            "play (first playlist whose persistent ID is %S)\n"
            "end tell")
    (if shuffle "true" "false")
    (plist-get item :id))))

;;;###autoload
(defun fzfa-music-playlist ()
  "Fuzzy-select a Music.app playlist and play it sequentially.

Explicitly disables shuffle so this command always plays in order,
even if `fzfa-music-playlist-shuffle' was used previously."
  (interactive)
  (when-let* ((item (fzfa-music--pick-playlist "playlist: ")))
    (fzfa-music--play-playlist item nil)))

;;;###autoload
(defun fzfa-music-playlist-shuffle ()
  "Fuzzy-select a Music.app playlist and play it in shuffle mode."
  (interactive)
  (when-let* ((item (fzfa-music--pick-playlist "playlist (shuffle): ")))
    (fzfa-music--play-playlist item t)))

;;;###autoload
(defun fzfa-music ()
  "Fuzzy-select and play a track from the macOS Music.app library."
  (interactive)
  (fzfa-music--pick-and-play nil "music: "))

;;;###autoload
(defun fzfa-music-by-artist ()
  "Fuzzy-select and play a track, with results grouped by artist."
  (interactive)
  (fzfa-music--pick-and-play :artist "music (by artist): "))

;;;###autoload
(defun fzfa-music-by-genre ()
  "Fuzzy-select and play a track, with results grouped by genre.

Genre is prefixed to each candidate, so typing the genre narrows results."
  (interactive)
  (fzfa-music--pick-and-play :genre "music (by genre): "))

;;;###autoload
(defun fzfa-music-setup ()
  "Register the `fzfa-music' completion category."
  (add-to-list 'completion-category-overrides
               '(fzfa-music (styles fzfa))))

(provide 'fzfa-music)
;;; fzfa-music.el ends here
