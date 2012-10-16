stumpwm-cmus
============

Stumpwm module for interfacing with the cmus music player

INSTALL
============
add:
(load "/path/to/my/stumpwm-cmus.lisp") 
to your .stumpwmrc

USAGE
============
Once you have Loaded the module,  just run cmus and then use the 
cmus module commands to interact with it.

Here are some example keybindings:
;;Cmus playback controls
;; Get info about the currently playing track
(define-key *root-map* (kbd "C-M-i") "cmus-info")
;; playback controls
(define-key *root-map* (kbd "C-M-p") "cmus-send play")
(define-key *root-map* (kbd "C-M-s") "cmus-send stop")
(define-key *root-map* (kbd "C-M->") "cmus-send next")
(define-key *root-map* (kbd "C-M-<") "cmus-send prev")
(define-key *root-map* (kbd "C-M-f") "cmus-send shuffle")
(define-key *root-map* (kbd "C-M-r") "cmus-send repeat")
(define-key *root-map* (kbd "C-M-c") "cmus-send clear")
;; load m3u or pl playlist (default playlist directory: ~/.cmus)
(define-key *root-map* (kbd "C-M-;") "cmus-load-playlist")
;; browser integration functions: wikipedia, youtube and search for lyrics
(define-key *root-map* (kbd "C-M-w") "cmus-artist-wiki")
(define-key *root-map* (kbd "C-M-v") "cmus-video")
(define-key *root-map* (kbd "C-M-l") "cmus-lyrics")
;; Load and play an album or song by name
(define-key *root-map* (kbd "C-M-P") "cmus-play-album")
;; Search the cmus library
(define-key *root-map* (kbd "C-M-?") "cmus-search-library")

