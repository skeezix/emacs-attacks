;;; emattacks.el --- A wargame for Emacs

;; Copyright (C) 2021 Jeff Mitchell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Emacs Attacks! - base elisp file; this is the one that kicks it all off

;;; Code:

;; TODO
;; - large map scrolling
;; - re-render after key/function
;; - map paint + save mode
;; - map loading
;; - mapscript or two?
;; - cursor around; select single unit
;; - render sprite-list for PC and NPC
;; - key actions .. and move game-specific key handling to the game module somewhere
;; - refactor .. code to separation of concern; and why is module defining viewport size? mapscripts hardcoded?..etc
;; - populate side panel correctly per unit
;; - end of turn handler
;; - FARM OUT: AI scripts
;; - FARM OUT: art
;; - FARM OUT: mapscripts
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
(defvar *emx/gamespath* "games" "path to append to emx/basepath to find where game modules are located")
(defvar *emx/bufname* "*EmacsAttacks!*" "Emacs Attacks! - buffer name to utilize")

;; state - surely I'm doing this wrong, having globals like this :)
(defvar *emx/gamemodes* nil "Emacs Attacks! - list of game modes and particulars about them.")

;; register available game modes
(setq *emx/gamemodes* (emx/list-game-modes))
;; (print *emx/gamemodes*)
;; (hash-table-count *emx/gamemodes*)
;; (gethash "Attacks!" *emx/gamemodes*)

;; define the major mode for the windows
;; - special-mode is immutable by default
(define-derived-mode emattacks-mode special-mode "emattacks-mode"
  "Mode for Emacs Attacks! to use in main game window"
  (use-local-map emattacks-mode-map) ; doesn't seem to work .. apply it during render I guess :/
)

;; create initial game state
(defvar *emx/gamestate* nil "Emacs Attacks! - game state for current game")
(setq *emx/gamestate* (emx/init-module *emx/gamemodes* "Attacks!"))
;; (emx/a-state-print *emx/gamestate*)

;; register all the mapscripts we can find for this module (including loading their elisp)
(defvar *emx/selected-mapscript* nil "Emacs Attacks! - mapscript tp use for current session")
(setq *emx/selected-mapscript* (emx/register-mapscript-from-file "games/empire" "mapscripts/mapscript-test-1.json"))

;; invoke mapscript(s) to populate the board
(emx/invoke_mapscript *emx/selected-mapscript* *emx/gamestate*)
;;(emx/map-script-test-1 (oref *emx/gamestate* board))

;; try and start up/render the actual module
(emx/start-module *emx/gamestate*)

;; (gethash "fogm" (emx/a-state-artcache *emx/gamestate*))
;; (posframe-delete-all)
