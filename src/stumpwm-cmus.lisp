
;;: Stumpwm CMus

;;; This is a list of handy functions for dealing with CMus music
;;; player in Stumpwm. It can send commands to cmus-remote. It uses the
;;; output from cmus-remote to  find the currently playing artist, title
;;; and album.  It also includes commands to browse wikipedia, youtube
;;; and search for lyrics via google.

;;; Copyright 2012 JD Adams
;;;
;;; Maintainer: JD Adams
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; DEPENDS
;;; surfaw, cmus, cmus-remote

;;; USAGE:
;;; Put it in your stumpwm contrib directory and then...
;;; (load "/path/to/your/stumpwm-cmus.lisp")
;;;
;;; ...in your ~/.stumpwmrc, followed by some keybindings (according
;;; to your preference)
;;; 
;;;  You can send commands to cmus-remote by using the cmus-send command. 
;;;  It looks for playlists in the ~/.cmus/ directory by default.  You can
;;;  change this directory by adding: 
;;;    (setf *cmus-playlist-directory* "/path/to/your/playlists/")
;;;  to your .stumpwmrc.
;;; 
;;;  Here is an example taken from my .stumpwmrc:
;;;  
;;;  (load-module "stumpwm-cmus")
;;;  ;;Cmus playback controls
;;;  (define-key *root-map* (kbd "C-M-i") "cmus-info")
;;;  (define-key *root-map* (kbd "C-M-p") "cmus-send play")
;;;  (define-key *root-map* (kbd "C-M-s") "cmus-send stop")
;;;  (define-key *root-map* (kbd "C-M->") "cmus-send next")
;;;  (define-key *root-map* (kbd "C-M-<") "cmus-send prev")
;;;  (define-key *root-map* (kbd "C-M-f") "cmus-send shuffle")
;;;  (define-key *root-map* (kbd "C-M-r") "cmus-send repeat")
;;;  (define-key *root-map* (kbd "C-M-c") "cmus-send clear")
;;;  (define-key *root-map* (kbd "C-M-;") "cmus-load-playlist")
;;;  (define-key *root-map* (kbd "C-M-w") "cmus-artist-wiki")
;;;  (define-key *root-map* (kbd "C-M-v") "cmus-video")
;;;  (define-key *root-map* (kbd "C-M-l") "cmus-lyrics")

;;; Code:
;;; Thanks to Diogo F. S. Ramos for helping me get this into a proper package.
(defpackage #:cmus
  (:use #:cl)
  (:export :cat
           :cmus-control
           :*cmus-commands*
           :*cmus-playlist-directory*
           :query-cmus))

(in-package #:cmus)

(defparameter *cmus-commands* '( :PLAY "play" :PAUSE "pause" :STOP "stop" 
                       :NEXT "next" :PREV "prev" :FILE "file" :REPEAT "repeat" 
                       :SHUFFLE "shuffle" :VOLUME "volume" :CLEAR "clear" :RAW "raw"))

(defparameter *cmus-playlist-directory* "~/.cmus/")

;; Thanks to sabetts of #stumpwm
(defun cat (&rest strings) 
   "Concatenates strings"
   (apply 'concatenate 'string strings))

(defun query-cmus (tag)
   "Queries active cmus play session for matching tag"
   (let ((cmus-command "cmus-remote -Q | grep 'tag ") cmus-result)
     (setf cmus-result (stumpwm:run-shell-command (cat cmus-command tag " '") t))
     (string-trim '(#\Newline) (string-left-trim (cat "tag " tag) cmus-result))))

(defun cmus-control (command)
   "sends a command to cmus via cmus-remote"
   (stumpwm:run-shell-command (cat "cmus-remote " command))
   ;; COMMENT: this little line just echoes the executed command to stumpwm
   (stumpwm:echo-string (stumpwm:current-screen) command))

(in-package :stumpwm)

;;Auto-completion funcitonality Thanks to Alexander Vynnyk from dswm.
(defun strings-only (list)
  (cond ((null (car list)) nil)
        ((stringp (car list))
         (cons (car list) (strings-only (cdr list))))
        (t (strings-only (cdr list)))))

(define-stumpwm-type :cmus (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (strings-only cmus:*cmus-commands*))))

(defcommand cmus-send (command) ((:cmus "Enter Command: "))
   "Send control commands to cmus."
     (dolist (com cmus:*cmus-commands*)
       (if (equal command com)
           (cmus:cmus-control (cmus:cat "--" com)))))
      
(defcommand cmus-video () ()
   "Find videos on youtube matching currently playing song in cmus"
   ( stumpwm:run-shell-command (cmus:cat "surfraw youtube " 
                            (cmus:query-cmus "artist") 
                            (cmus:query-cmus "title"))))

(defcommand cmus-artist-video () ()
   "Find videos on youtube matching current artist in cmus"
   ( stumpwm:run-shell-command (cmus:cat "surfraw youtube " 
                            (cmus:query-cmus "artist"))))

(defcommand cmus-artist-wiki () ()
   "Search wikipedia for current artist in cmus"
    ( stumpwm:run-shell-command (cmus:cat "surfraw wikipedia " 
                            (cmus:query-cmus "artist") " '(band)'")))

(defcommand cmus-lyrics () ()
   "Search wikipedia for current artist in cmus"
    ( stumpwm:run-shell-command (cmus:cat "surfraw google  " 
                            (cmus:query-cmus "artist") 
                            " " 
                            (cmus:query-cmus "title")
                            " lyrics")))

(defcommand cmus-load-playlist (playlist) ((:string "Enter Filename: "))
   "Loads and plays a  playlist from the *cmus-playlist-directory*"
   (let ((full-path (cmus:cat *cmus-playlist-directory* playlist)))
     (cmus:cmus-control "--clear")
     (cmus:cmus-control (cmus:cat "-p " full-path))
     ;; next ensures that the currently playing cmus track is cleared.
     (cmus:cmus-control "--next")))

(defcommand cmus-search-library (tag) ((:string "Enter Search: "))
  "search library view for tag. "
 (cmus:cmus-control "--raw 'view tree'")
 (cmus:cmus-control (cmus:cat "--raw ' /" tag "'")))

(defcommand cmus-play-album (tag) ((:string "Enter Search: "))
  "Search and play album matching tag"
  (cmus:cmus-control "--clear")
  (cmus:cmus-control "--raw 'view tree'")
  (cmus:cmus-control (cmus:cat "--raw ' /" tag "'"))
  (cmus:cmus-control "--raw win-add-p")
  (cmus:cmus-control "--next")
  (cmus:cmus-control "--raw 'view playlist'")
  (cmus:cmus-control "--play"))

(defcommand cmus-play-song (tag) ((:string "Enter Search: "))
  "Search and play song matching tag"
  (cmus:cmus-control "--clear") 
  (cmus:cmus-control "--raw 'view tree'")
  (cmus:cmus-control (cmus:cat "--raw ' /" tag "'"))
  (cmus:cmus-control "--raw win-next")
  (cmus:cmus-control "--raw win-add-p")
  (cmus:cmus-control "--next")
  (cmus:cmus-control "--raw 'view playlist'")
  (cmus:cmus-control "--play"))

(defcommand cmus-info () ()
   "Print cmus info to screen"
   (let ((title  (cmus:query-cmus "title")) (artist  (cmus:query-cmus "artist")) (album (cmus:query-cmus "album"))) 
        (echo-string (current-screen) (cmus:cat "Now Playing: " '(#\NewLine) artist ": " title ": " album))))

;; For some reason I need to 'initialize' cmus-control with a throw away command. Otherwise, the very first play-album command 
;; doesn't queue the first album properly. This seems to be some sort of cmus-remote quirk - I am still investigating the issue.
(cmus:cmus-control "-Q")
