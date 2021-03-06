;;;; Emacs Attacks! - base elisp file; this is the one that kicks it all off

;; TODO
;; - re-render after key/function
;; - render sprite-list for PC and NPC
;; - cursor around; select single unit
;; - key actions
;; - populate side panel correctly per unit
;; - end of turn handler
;; - map paint + save mode
;; - map loading
;; - FARM OUT: AI scripts
;; - FARM OUT: art
;; - FARM OUT: mapscripts
;; - large map scrolling
;; - combat resolution
;; - additional units
;; - production queue
;; - AI register function, set state personality
;; - AI function invoke
;; - game profiles vs start emattacks -> pick module -> pick map size -> mapscript choice -> then proceed?
;;   - proper module picker
;;   - proper size picker
;;   - proper mapscript picker
;; - refactor module code to base package code for cross-module mojination
;; - add hooks? modding methods
;; - add config functions (paths to modules, mapscripts, pieces, etc..)
;; - timer to adv-turn + re-render
;; - rectangle select a group of units? or assign units to a name, and select group by name?
;;
;;

(load-file "contrib.el")

(load-file "libgames.el")
(load-file "libtools.el")
(load-file "libpieces.el")
(load-file "libkeys.el")

;; configuration
(defvar *emx/basepath* "emattacks" "path to add to the .emacs.d location to find emacs attacks dir")
(defvar *emx/gamespath* "games" "path to append to emx/basepath to find where game modules are located")
(defvar *emx/bufname* "*EmacsAttacks!*" "Emacs Attacks! - buffer name to utilize")
(defvar *emx/panelname* "*EmacsAttacksPanel*" "Emacs Attacks! - buffer name for side panel")

;; state - surely I'm doing this wrong, having globals like this :)
(defvar *emx/gamemodes* nil "Emacs Attacks! - list of game modes and particulars about them")

;; register available game modes
(setq *emx/gamemodes* (emx/list-game-modes))
;; (print *emx/gamemodes*)
;; (hash-table-count *emx/gamemodes*)
;; (gethash "Attacks!" *emx/gamemodes*)

;; define the major mode for the windows
;; - special-mode is immutable by default
(define-derived-mode emattacks-mode special-mode "emattacks-mode"
  (use-local-map emattacks-mode-map) ; doesn't seem to work .. apply it during render I guess :/
)

;; create initial game state
(defvar *emx/gamestate* nil "Emacs Attacks! - game state for current game")
(setq *emx/gamestate* (emx/init-module *emx/gamemodes* "Attacks!"))
;; (emx/a-state-print *emx/gamestate*)

;; register all the mapscripts we can find for this module (including loading their elisp)
(setq *emx/selected-mapscript* (emx/register-mapscript-from-file "games/empire" "mapscripts/mapscript-test-1.json"))

;; invoke mapscript(s) to populate the board
(emx/invoke_mapscript *emx/selected-mapscript* *emx/gamestate*)
;;(emx/map-script-test-1 (oref *emx/gamestate* board))

;; try and start up/render the actual module
(emx/start-module *emx/gamestate*)

;; (gethash "fogm" (emx/a-state-artcache *emx/gamestate*))
;; (posframe-delete-all)

(defun emx/test-module ()
  (interactive)
  (emx/start-module *emx/gamestate*)
)
