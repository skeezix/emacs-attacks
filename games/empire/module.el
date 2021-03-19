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

;;; Code:

;; "starter": {
;; 	"method": "simplemenu",
;; 	"simplemenu": [
;; 	    [ "Map size" ],
;; 	    [ "Small", "emx/a/start-new", "s1" ],
;; 	    [ "Medium", "emx/a/start-new", "m1" ],
;; 	    [ "Large", "emx/a/start"=new, "l1" ]
;; 	]
;; }

(defun emx/a-init-module (path)
  "Invoke the module living in the supplied path, expecting a state object back"
  (message "Attacks! module entered; path %s" path)

  ;; do we need to inhale and eval all the code? or have we done it already?
  (unless (fboundp 'emx/a-create-unit)
    (load-file (concat path "/" "state.el"))
    (load-file (concat path "/" "map.el"))
    (load-file (concat path "/" "viewport.el"))
    (load-file (concat path "/" "render.el"))
    (load-file (concat path "/" "sidepanel.el"))
    (load-file (concat path "/" "units.el"))
  ) ; unless

  (let* ( (config_w 30)
	  (config_h 30)
	  board
	  piecehash
	  state
	  vp
	  ulist
	  archhash
	)

    ;; build a board
    (setq board (emx/a-map :w config_w :h config_h))

    ;; load up all the pieces
    (setq piecehash (emx/inhale-module-pieces path "pieces"))
    ;;(print piecehash)

    ;; set up a viewport
    (setq vp '(0 0 24 20) )

    ;; set up archetypes (load from json?)
    (setq archhash (emx/a-load-archetypes))

    ;; build a state, assigning in the board and assets
    (setq state (emx/a-state :board board :guip (display-images-p) :artcache piecehash :viewport vp :unitlist nil :archhash archhash))

    (let ( )
      (emx/a-create-unit state "tank" 0 "nil" 1 1)
      (emx/a-create-unit state "tank" 0 "nil" 1 1)
      (emx/a-create-unit state "tank" 0 "nil" 2 2)
      (print "unit list" *emx/logbuf*)
      (print (emx/a-state-unitlist state) *emx/logbuf*)
      (print "Found unit at 0 0?" *emx/logbuf*)
      (print (emx/a-find-units-at state 0 0) *emx/logbuf*)
      (print "Found unit at 1 1?" *emx/logbuf*)
      (print (emx/a-find-units-at state 1 1) *emx/logbuf*)
      (print "Found unit at 2 2?" *emx/logbuf*)
      (print (emx/a-find-units-at state 2 2) *emx/logbuf*)
    )

    ;;(message "Board internal")
    ;;(print board)

    ;; return newly init'd state
    state
  )
  
)

(defun emx/a-start-module (state)
  "Given a module state object, render and get going"

  ;; enable side panel
  (emx/show-side-panel)

  ;; render to the buffer
  (emx/a-render state)

  ;; enable keymap
  (use-local-map emattacks-mode-map)
 
  t

)
