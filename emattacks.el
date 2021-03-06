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
;; - cursor around; select single unit
;; - proper documentation directory and link from README
;; - populate side panel correctly per unit (cursor hovered unit, selected unit, tile, help keys)
;; - AI object and handlers, basic move scripts
;; - end of turn handler (iterate across other players AI scripts)
;; - unit enter unit options and combat resolver
;; - move archetype definition from el to json for easier modding
;; - re-render after key/function
;; - map paint + save mode
;; - map loading
;; - mapscript or two? or gen noise from py?
;; - buffer-local vars for it all (multiple gaems at once?), or at least multiple viewports could be very handy/neat (unit, focus? spying?)
;; - key actions .. and move game-specific key handling to the game module somewhere
;; - refactor .. code to separation of concern; and why is module defining viewport size? mapscripts hardcoded?..etc
;; - refactor to ditch the concept of multiple games; keep it simple, its all about the wargame, if we want to do pushem, do a separate package
;; - add to makefile an image generator (merge enemy0 and player bg with units to emit the assorted unit files)
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
;; - refactor ... drop other game modes, all emx; but allow dirs for drop in rule overriders etc? or hooks after units, after turns etc?
;; - rectangle select a group of units? or assign units to a name, and select group by name?
;;
;;

(load-file "contrib.el")

(load-file "libgames.el")
(load-file "libtools.el")
(load-file "libpieces.el")
(load-file "libkeys.el")

(defun emx/run-emacs-attacks ()
  (interactive)

  ;; configuration
  (defvar *emx/gamespath* "games" "path to append to emx/basepath to find where game modules are located")
  (defvar *emx/bufname* "*EmacsAttacks!*" "Emacs Attacks! - buffer name to utilize")
  (defvar *emx/blinkallunits* t "Emacs Attacks! - blink units so terrain tiles can be seen; implies rendering viewport automatically on a timer")
  (defvar *emx/blinkallunitsdelay* "0.5 sec" "Emacs Attacks! - delay (in seconds) between unit blinks; default is \"0.5 sec\"")

  ;; logging buffer
  (defvar *emx/logbufname* "*EmacsAttacks! Debug Log*" "Emacs Attacks! - buffer name to utilize for debug log")
  (defvar *emx/logbuf* nil "Emacs Attacks! - the actual debug log buffer")
  ;;(setq *emx/logbuf* (generate-new-buffer *emx/logbufname*))
  (setq *emx/logbuf* (get-buffer-create *emx/logbufname*))
  (with-current-buffer *emx/logbufname*
    (erase-buffer)
  )

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

) ; defun run

(defun emx/confirm-and-kill ()
  (interactive)
  ;; (kill-buffer *emx/logbufname*) ; lets not kill this, leave it around for debuggery
  (cancel-timer (emx/a-state-rendertimer *emx/gamestate*))
  (kill-buffer *emx/bufname*)
  (kill-buffer *emx/panelname*)
)

(defun emx/force-next-turn ()
  (interactive)
  (emx/a-render *emx/gamestate*)
)

(defun emx/toggle-blinking-units ()
  "Enable or disable the blink timer (that alternates rendering of units and terrain)"
  (interactive)

  (if *emx/blinkallunits*
      (setq *emx/blinkallunits* nil)
    (progn
      (setq *emx/blinkallunits* t)
      (emx/a-render *emx/gamestate*)
    )
  )
  
)
