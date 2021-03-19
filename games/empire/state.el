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

(defclass emx/a-state ()
  (
   (board :initarg :board :type emx/a-map :documentation "The game board" :accessor emx/a-state-board)
   (mapscript :initarg :mapscript :documentation "function to initialize the board" :accessor emx/a-state-mapscript)
   (guip :initarg :guip :initform t :documentation "True implies graphical render, otherwise text" :accessor emx/a-state-gui-p)
   (artcache :initarg :artcache :documentation "hash of string id to Piece details" :accessor emx/a-state-artcache)
   (startfunc :initarg :startfunc :documentation "function ref to invoke to start/render the module" :accessor emx/a-state-startfunc)
   (viewport :initarg :viewport :documentation "( x y w h ) of the window to show" :accessor emx/a-state-viewport)
   (unitlist :initarg :unitlist :documentation "A list of emx/a-unit objects" :accessor emx/a-state-unitlist)
   (archhash :initarg :archhash :documentation "A list of emx/a-archetype available for units" :accessor emx/a-state-archhash)
  )
  "Emacs Attacks! - Attacks! module instance state"
)

;;(emx/a-state-gui-p *emx/gamestate*)
;;(setf (emx/a-state-gui-p *emx/gamestate*) nil)

(cl-defmethod emx/a-state-print ( (obj emx/a-state)  )
  "Dump debug info to *Messages*"
  
  (message "State dump:")
  
  ;; asst
  (message "GUI enabled? %s" (emx/a-state-gui-p obj))
  (message "Artwork cache: %s items" (hash-table-size (emx/a-state-artcache obj)))

)

(defun emx/a-describe-status ( state )

  (let* ( (vp (emx/a-state-viewport *emx/gamestate*))
	  (vpx (nth 0 vp))
	  (vpy (nth 1 vp))
	  (vpw (nth 2 vp))
	  (vph (nth 3 vp))
	  (board (emx/a-state-board *emx/gamestate*))
	  (mw (emx/a-map-w board))
	  (mh (emx/a-map-h board))
	  ( text "" )
	)
  
    (setq text (concat text (format "Viewport: %d,%d\n" vpx vpy)))
    (setq text (concat text (format "Map dimensions: %d,%d\n" mw mh)))
    (setq text (concat text "\n"))

    text
    
  ) ; let

)
