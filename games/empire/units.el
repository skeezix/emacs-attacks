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

(defclass emx/a-unit ()
  (
   (playerid  :initarg :playerid :type integer :documentation "identify player id that owns unit" :accessor emx/a-unit-playerid :initform -1)
   (x         :initarg :x       :type integer :documentation "x coord on map" :accessor emx/a-unit-x :initform 0)
   (y         :initarg :y       :type integer :documentation "y coord on map" :accessor emx/a-unit-y :initform 0)
   ;;(mapid   :initarg :mapid   :type integer :documentation "which map?" :accessor emx/a-unit-mapid :initform -1) ; keep it simple..
   ;;
   (intent    :initarg :intent  :type string  :documentation "intent code" :accessor emx/a-unit-intent :initform "") ; ex: "move"
   (goal      :initarg :goal    :type string  :documentation "a goal value meaningful to the intent code" :accessor emx/a-unit-goal :initform "")  ; target (x,y) for a move, say
   (piececd   :initarg :piececd :type string  :documentation "the piece that to use for artwork" :accessor emx/a-unit-piece :initform "")
  )
  "Emacs Attacks! - Unit for player or enemies"
)

(defun emx/a-create-unit ( state playerid piececd x y )
  "Create a new unit, add it to the state"

  (let ( unit
	 ulist
       )

    (setq unit (emx/a-unit :piececd piececd :x x :y y :playerid playerid ))
    (print unit)

    (setq ulist (emx/a-state-unitlist state))
    (message "ulist %s" ulist)
    (message "poop")

    (add-to-list ulist unit)

    unit
  ) ; let

)

(defun emx/a-find-units-at ( state x y ) 
  "Return a list of units at the defined location, or nil"

  (let ( (matches '()) )

    (mapcan (emx/a-state-unitlist state)
             (lambda (x) (message x)))
    
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
