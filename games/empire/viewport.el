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
;;; code for controlling the viewport

;;; Code:

;; if viewport width + viewport offset > mapwidth -> don't allow going right
;; if viewport offset x > 0, allow going left
;; similar rule for up and right

(defun emx/a-viewport-left ()
  (interactive)
  (emx/a-viewport-shift -1 0)
)

(defun emx/a-viewport-right ()
  (interactive)
  (emx/a-viewport-shift +1 0)
)

(defun emx/a-viewport-up ()
  (interactive)
  (emx/a-viewport-shift 0 -1)
)

(defun emx/a-viewport-down ()
  (interactive)
  (emx/a-viewport-shift 0 +1)
)

(defun emx/a-viewport-shift ( dx dy )

  (let* ( (vp (emx/a-state-viewport *emx/gamestate*))
	  (vpx (nth 0 vp))
	  (vpy (nth 1 vp))
	  (vpw (nth 2 vp))
	  (vph (nth 3 vp))
	  (board (emx/a-state-board *emx/gamestate*))
	  (mw (emx/a-map-w board))
	  (mh (emx/a-map-h board))
	)

    ;; left
    (when (< dx 0)
      (if (>= vpx (abs dx))
	  (setf (nth 0 vp) (+ vpx dx))
      )
    )
    
    ;; right
    (when (> dx 0)
      (if (> mw (+ (+ vpx dx) vpw))
	  (setf (nth 0 vp) (+ vpx dx))
      )
    )

    ;; up
    (when (< dy 0)
      (if (>= vpy (abs dy))
	  (setf (nth 1 vp) (+ vpy dy))
      )
    )
    
    ;; right
    (when (> dy 0)
      (if (> mh (+ (+ vpy dy) vph))
	  (setf (nth 1 vp) (+ vpy dy))
      )
    )
   
  ) ; let

  (emx/a-render *emx/gamestate* t) ; true -> reactive .. this is a redraw based on key, not timer automation
  
)
