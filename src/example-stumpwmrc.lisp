
(in-package :stumpwm)

;; I don't really want to be constantly typing "concatenate 'string" 
;; this simplifies things.
;; Thanks to sabetts of #stumpwm
(defun cat (&rest strings) 
   "Concatenates strings"
   (apply 'concatenate 'string strings))

(set-contrib-dir "$HOME/sources/stumpwm/contrib")
(set-prefix-key (kbd "F20"))
(set-focus-color "#002e63")
(setf *timeout-wait* 10)
(setf *message-window-gravity* :bottom-right)
(setf *input-window-gravity* :bottom-right)

;;Set the font for the input box. Terminus.
(set-font "-*-terminus-*-*-*-*-*-*-*-*-*-*-*-*")

;; bit flag - is the rc file loaded?
(setf initrc 0)
(setf *mode-line-position*  :top)
(setf *mouse-focus-policy* :click)
(setf layout_path "~/.stumpwm.d/layouts/")
(setf terminal "urxvtcd -bg black -fg 'light gray' -fn 'xft:terminus:pixelsize=10' +sb")
(setf exec_terminal (cat "exec " terminal))
(setf file_manager "exec pcmanfm")
(setf dmenu "exec dmenu_run -nb '#000000' -nf '#0067FF'")
(setf clear_mod "exec xmodmap -e 'clear Mod4'")
(setf set_mod "exec xmodmap -e 'keycode 133 = F20'")
(setf set_modr "exec xmodmap -e 'keycode 134 = F20'")
(setf weather "weather -f -i ktpa -c tampa -s fl")
(setf browser "x-www-browser")
(setf search-provider "https://duckduckgo.com/?q=")

(defun exit()
	(if (= initrc 1) run-commands "quit"))

(defcommand emacs() ()
  "run emacs client"
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand browser() ()
  "Launch X-www-browser"
  (run-or-raise "x-www-browser" '(:class "X-www-browser")))

(defcommand banshee() ()
  "Launch Banshee music player"
  (run-or-raise "banshee" '(:class "banshee-1")))

(defcommand cmus() ()
  "Launch cmus music player"
  (run-or-raise  (cat terminal " -n 'cmus' -e cmus") '(:title "cmus")))

(defcommand weather() ()
  "check current weather"
  (echo-string (current-screen) (run-shell-command weather t)))

(defcommand top() ()
  "show top processes"
  (echo-string (current-screen) (run-shell-command "ps aux |sort -nrk 3 |head -10" t)))

(defcommand battery() ()
  "show current battery status"
  (echo-string (current-screen) (run-shell-command "cat /proc/pmu/battery_0" t)))

;;;Web browsing commands
;;Get the X selection and order the GUI browser to open it. Presumably it is a HTTP address.
(defcommand open-selection-browser ()()
  (run-shell-command (cat browser (get-x-selection) )))

(defcommand wiki (search-string) ((:string "Enter Wikipedia Search Term:"))
   "browse wikipedia for string"
   (run-shell-command (cat "surfraw wikipedia " search-string)))

(defcommand search-web (search-string) ((:string "Enter DuckDuckGo Search Term:"))
   "search duck duck go"
   (run-shell-command (cat "surfraw duckduckgo " search-string )))

(defcommand search-selection-browser ()()
   "search selection in browser"
   (run-shell-command (cat "surfraw duckduckgo " (get-x-selection))))

(defcommand wiki-selection-browser ()()
   (run-shell-command (cat "surfraw wikipedia " (get-x-selection))))

(defcommand restore-frames (string-input)((:string "Restore Frames: "))
   "Restores frames from layout_path"
   (restore-from-file (cat layout_path string-input)))

(define-key *root-map* (kbd "Return") exec_terminal)
(define-key *root-map* (kbd "z") "browser")
(define-key *root-map* (kbd "x") file_manager)
(define-key *root-map* (kbd "d") dmenu)
(define-key *root-map* (kbd "e") "emacs")
(define-key *root-map* (kbd "C-q") (exit))

;;Browse somewhere with the GUI WWW browser.
(define-key *root-map* (kbd "B") (cat "colon " "exec " browser " http://www."))
(define-key *root-map* (kbd "b") "open-selection-browser")
(define-key *root-map* (kbd "`") "search-selection-browser")
(define-key *root-map* (kbd "~") "wiki-selection-browser")
(define-key *root-map* (kbd "C-w") "weather")
(define-key *root-map* (kbd "C-t") "top")
(define-key *root-map* (kbd "M-f") "dump-screen")
(define-key *root-map* (kbd "M-r") "restore-frames")
(define-key *root-map* (kbd "M") "cmus")


;; My custom module for interacting with cmus... a work in progress.
(load "~/path/to/my/stumpwm-cmus.lisp")

;;Cmus playback controls
(define-key *root-map* (kbd "C-M-i") "cmus-info")
(define-key *root-map* (kbd "C-M-p") "cmus-send play")
(define-key *root-map* (kbd "C-M-s") "cmus-send stop")
(define-key *root-map* (kbd "C-M->") "cmus-send next")
(define-key *root-map* (kbd "C-M-<") "cmus-send prev")
(define-key *root-map* (kbd "C-M-f") "cmus-send shuffle")
(define-key *root-map* (kbd "C-M-r") "cmus-send repeat")
(define-key *root-map* (kbd "C-M-c") "cmus-send clear")
(define-key *root-map* (kbd "C-M-;") "cmus-load-playlist")
(define-key *root-map* (kbd "C-M-w") "cmus-artist-wiki")
(define-key *root-map* (kbd "C-M-v") "cmus-video")
(define-key *root-map* (kbd "C-M-l") "cmus-lyrics")
(define-key *root-map* (kbd "C-M-P") "cmus-play-album")
(define-key *root-map* (kbd "C-M-?") "cmus-search-library")

;; Load my layout
(restore-from-file (cat layout_path "frames"))

;;Window placement rules - I like to keep this as simple as possible.
(clear-window-placement-rules)

(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t t :role "browser")
  (0 t t :class "Emacs")
  (0 t nil :class "xine")
  (0 t t :role "gimp-image-window")
 
  (1 t nil :class "Lxterminal")
  (1 t nil :class "URxvt")
  (1 t nil :class "Pcmanfm")
  (1 t t :role "gimp-toolbox")

  (2 t t :class "URxvt" :title "cmus")
  (2 t t :title "Downloads")
  (2 t t :role "gimp-dock")

  (3 t t :title "xine Panel"))

(run-shell-command clear_mod)
(run-shell-command set_mod)
(run-shell-command set_modr)

;; Rc File is loaded - initrc set to 1
(setf initrc 1)
