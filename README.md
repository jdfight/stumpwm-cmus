stumpwm-cmus
============

Stumpwm module for interfacing with the cmus music player

Install
============
add:<br>
(load "/path/to/my/stumpwm-cmus.lisp")<br> 
to your .stumpwmrc

Usage
============
Once you have Loaded the module,  just run cmus and then use the 
cmus module commands to interact with it.

Here are some example keybindings:

;;Cmus playback controls: <br>
;; Get info about the currently playing track<br>
(define-key *root-map* (kbd "C-M-i") "cmus-info")<br>

;; playback controls<br>
(define-key *root-map* (kbd "C-M-p") "cmus-send play")<br>
(define-key *root-map* (kbd "C-M-s") "cmus-send stop")<br>
(define-key *root-map* (kbd "C-M->") "cmus-send next")<br>
(define-key *root-map* (kbd "C-M-<") "cmus-send prev")<br>
(define-key *root-map* (kbd "C-M-f") "cmus-send shuffle")<br>
(define-key *root-map* (kbd "C-M-r") "cmus-send repeat")<br>
(define-key *root-map* (kbd "C-M-c") "cmus-send clear")<br>

;; load m3u or pl playlist (default playlist directory: ~/.cmus)<br>
(define-key *root-map* (kbd "C-M-;") "cmus-load-playlist")<br>

;; browser integration functions: wikipedia, youtube and search for lyrics<br>
(define-key *root-map* (kbd "C-M-w") "cmus-artist-wiki")<br>
(define-key *root-map* (kbd "C-M-v") "cmus-video")<br>
(define-key *root-map* (kbd "C-M-l") "cmus-lyrics")<br>

;; Load and play an album or song by name<br>
(define-key *root-map* (kbd "C-M-P") "cmus-play-album")<br>

;; Search the cmus library<br>
(define-key *root-map* (kbd "C-M-?") "cmus-search-library")<br>
