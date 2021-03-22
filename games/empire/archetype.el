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

;; an archetype is the parent class, the behaviour class of a unit
;; an archview is an option that refers back to an archetype
;; a unit is a playable instance of an archview/archtype
;;
;; ex: a player tank is the same as an AI tank (same archtype), but
;; may be a different artwork (different colour etc)
;; So a player unit tank and an AI unit tank.
;;
;; do we make an archview class to provide alternate views of an arch
;; to a unit, or is that just too much efforT? put a piece reference
;; on a unit for now and call it a day, and break out later if needed..

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
