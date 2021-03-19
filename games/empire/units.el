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

(defclass emx/a-archetype ()
  (
   (name      :initarg :name    :type string :documentation "archetypes type name, for reference" :accessor emx/a-archetype-name :initform "")
  )
)

(defun emx/a-load-archetypes ()

  (let ( archhash
	 arch
       )

    ;; make the hash container
    (setq archhash (make-hash-table :test 'equal))
    
    (setq arch (emx/a-archetype :name "tank"))
    (puthash "tank" arch archhash)

    archhash

  ) ; let
  
)

(defclass emx/a-unit ()
  (
   (playerid  :initarg :playerid :type integer :documentation "identify player id that owns unit" :accessor emx/a-unit-playerid :initform -1)
   (x         :initarg :x       :type integer :documentation "x coord on map" :accessor emx/a-unit-x :initform 0)
   (y         :initarg :y       :type integer :documentation "y coord on map" :accessor emx/a-unit-y :initform 0)
   ;;(mapid   :initarg :mapid   :type integer :documentation "which map?" :accessor emx/a-unit-mapid :initform -1) ; keep it simple..
   ;;
   (intent    :initarg :intent  :type string  :documentation "intent code" :accessor emx/a-unit-intent :initform "") ; ex: "move"
   (goal      :initarg :goal    :type string  :documentation "a goal value meaningful to the intent code" :accessor emx/a-unit-goal :initform "")  ; target (x,y) for a move, say
   (piece     :initarg :piececd :type string  :documentation "the piece that to use for artwork" :accessor emx/a-unit-piece :initform "")
   (arch      :initarg :arch    :type emx/a-archetype :documentation "the kind of this unit" :accessor emx/a-unit-arch)
  )
  "Emacs Attacks! - Unit for player or enemies"
)

(defun emx/a-create-unit ( state archcd playerid piececd x y )
  "Create a new unit, add it to the state"

  ;; verify no other unit present (not currently allowing stacks of doom)
  (if (emx/a-find-units-at state x y)
      (print "Attempt to create unit in location already having a unit" *emx/logbuf*)
    (let* ( unit
	    (ulist (emx/a-state-unitlist state))
	    (artcache (emx/a-state-artcache state))
	    (archhash (emx/a-state-archhash state))
	    piece
	    arch
	  )

      ;; look up the piececd to get the reference
      (setq piece (gethash piececd artcache))

      ;; look up the archetype to get the reference
      (setq arch (gethash archcd archhash))

      ;; create the unit
      (setq unit (emx/a-unit :arch arch :piececd piececd :x x :y y :playerid playerid ))

      ;; store in the list
      (if ulist
	  (push unit (emx/a-state-unitlist state))
	(setf (emx/a-state-unitlist state) (list unit))
      )
      
      unit
    ) ; let

  ) ; if

)

(defun emx/a-find-units-at ( state x y ) 
  "Return a list of units at the defined location, or nil"

  (let ( (matches nil)
	 (ulist (emx/a-state-unitlist state))
       )

    (when ulist
      ;;(mapcar (lambda (x) (message (emx/a-archetype-name (emx/a-unit-arch x)))) ulist)

      (mapc (lambda (u)
		(when (and (= (emx/a-unit-x u) x) (= (emx/a-unit-y u) y))
		  (if matches
		      (push u matches)
		    (setq matches (list u))
		  )
		)
	      ) ulist)

    ) ; when

    matches
  ) ; let
  
)

(defun emx/a-describe-unit ( state )
  "Return text describing selected unit"
  (let ( (text "") )
    ;; title
    (setq text (concat text "Super Blahg Tank\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "Location: (10,37) Plains\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "  [D]    Destroy unit\n"))
    (setq text (concat text "  [a]    Attack towards point\n"))
    (setq text (concat text "  [m]    Move towards point\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "  [8]    Scroll viewport up\n"))
    (setq text (concat text "[4] [6]  Scroll viewport left or right\n"))
    (setq text (concat text "  [2]    Scroll viewport down\n"))

    text
  )
)
