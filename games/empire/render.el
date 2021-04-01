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

(defun emx/a-render-piece-on-board (state board x y artcache char elsechar)
  "given a board and (x,y) render the image found there to the current point; supply -1,-1 for 'nil' tile"

  (print "render on board" *emx/logbuf*)

  (let* ( id 
	  cachedimage
	  thing
	  (iseven (cl-evenp (emx/a-state-rendercount state)))
	)

    (print "is even tick?" *emx/logbuf*)
    (print iseven *emx/logbuf*)
    (print (emx/a-state-rendercount state) *emx/logbuf*)
    
    ;; what are we rendering? a unit, or a board tile?
    (setq thing (nth 0 (emx/a-find-units-at state x y)))

    ;; if would render a unit, but we're doing blinking and we're on the even-tick, we just ignore the unit
    ;; so the tile will be found instead
    (when (and *emx/blinkallunits* iseven)
      (setq thing nil)
    )
    
    (if thing
	(progn
	  (setq id (emx/a-unit-piececd thing))
	)
      (setq id (if (= -1 x) "nil" (emx/a-map-get-tile board x y)))  ; grab the tile art id
    )

    ;; find the artwork
    (setq cachedimage (emx/get-pieces-image id))
    
    ;; an item in the map .. look it up in the hash?
    (if cachedimage
	(insert-image cachedimage char)
      (insert elsechar)
    )
    
  ) ; let
)

(defun emx/a-render-gui (state)
  ;; (insert "render gui\n")

  (print "render - gui - start" *emx/logbuf*)

  (let ( (board (emx/a-state-board state))
	 (artcache (emx/a-state-artcache state))
	 id
	 asset
	 cachedimage
       )

    ;; strategy: which subset to render
    ;;
    ;; given:
    ;; - a buffer of indeterminate window size; we don't care, we'll just render into it
    ;; - a viewpoint - the size we wish to render (can't render it all due to performance of emacs and a zillion small images)
    ;; - a map - the source map to observe through the viewport
    ;; - a fog - the boolean visibility mapping of what is visible in the game map
    ;;
    ;; nomenclature; vp -> viewport, ox -> offset-x, oy -> offset-y
    ;;
    ;; viewport assumption
    ;; - vpx,vpy will never cause vpw/vph to go off the edge of the map; scrolling the viewport
    ;;   by setting vpx,vpy should ensure this
    ;;
    ;; strategy
    ;; - determine offset; if vp width > map width, ox = 1/2 the diff; same for oy
    ;; - ox,oy are 0 when vp dimension(s) < map dimensions
    ;; - render oy as required
    ;; - for each row of min(maph,vp-y)
    ;;   - render ox
    ;;   - render a stripe of map: width if min(mapw,vp-x))
    ;;
    ;; ex small map (5x5), large vp (12x10) -> ox 3 or 4, oy 2 or 3 (say ox3, oy2)
    ;;    render oy rows
    ;;    repeat for 5 rows:
    ;;      render ox cells, render 5 map cells
    ;;
    ;; ex: large map (12x10), small vp (5x5) -> ox = oy = 0
    ;;    repeat for 5 rows
    ;;      render 0 ox, then render 5 map cells
    ;;

    ;; strategy: unit 'sprites', blinking units
    ;;
    ;; TBD
    ;;

    (when t
      (let* ( (vp (emx/a-state-viewport state))
	      (vpx (nth 0 vp))
	      (vpy (nth 1 vp))
	      (vpw (nth 2 vp))
	      (vph (nth 3 vp))
	      (mw (emx/a-map-w board))
	      (mh (emx/a-map-h board))
	      (ox (if (> vpw mw)
		      (/ (- vpw mw) 2)
		    0))
	      (oy (if (> vph mh)
		      (/ (- vph mh) 2)
		    0))
	      (pw (if (> vpw mw)
		      (- (- vpw ox) mw)
		    0)) ;; horiz back port width
	      (ph (if (> vph mh)
		      (- (- vph oy) mh)
		    0)) ;; vert back porch height
	    )
	;;(message "mw %s mh %s vpw %s vph %s ox %s oy %d" mw mh vpw vph ox oy)

	;; render vertical front porch
	(when oy
	  (cl-loop for y from 0 to (1- oy) do
		   (cl-loop for x from 0 to (1- vpw) do
			    ;;(message "vporch x %s y %s" x y)
			    (emx/a-render-piece-on-board state board -1 -1 artcache "*" "_")
		   )
		   (insert "\n")
	  )
	) ; when

	;; render each viewport row
	(cl-loop for y from 0 to (1- (min vph mh)) do

		 ;; horizontal front porch
		 (when ox
		   (cl-loop for x from 0 to (1- ox) do
			    ;;(message "hporch x %s y %s" x y)
			    (emx/a-render-piece-on-board state board -1 -1 artcache "*" "_")
		   )
		 ) ; when

		 ;; render the cells
		 (cl-loop for x from 0 to (1- (min vpw mw)) do
			  ;;(message "tile x %s y %s" x y)
			  (emx/a-render-piece-on-board state board (+ x vpx) (+ y vpy) artcache "*" "_")
		 )

		 ;; horizontal back porch
		 (when pw
		   (cl-loop for x from 0 to (1- pw) do
			    (emx/a-render-piece-on-board state board -1 -1 artcache "*" "_")
		   )
		 ) ; when

		 ;; go to next line..
		 (insert "\n")
	)
	
	;; render (ignore..) vertical back porch
	(when ph
	  (cl-loop for y from 0 to (1- ph) do
		   (cl-loop for x from 0 to (1- vpw) do
			    ;;(message "vporch x %s y %s" x y)
			    (emx/a-render-piece-on-board state board -1 -1 artcache "*" "_")
		   )
		   (insert "\n")
	  )
	) ; when
	
      ) ; let
    ) ; when

    ;; bump rendercount
    ;;(print (format "rc %d %d" (emx/a-state-rendercount state) (1+ (emx/a-state-rendercount state))) *emx/logbuf*)
    (setf (emx/a-state-rendercount state) (1+ (emx/a-state-rendercount state)))
    
    ;; EARLY EXPERIMENT; works yet very naive and blunt
    (when nil ; render the whole map
      (cl-loop for y from 0 to (1- (emx/a-map-h board)) do
	       (cl-loop for x from 0 to (1- (emx/a-map-w board)) do
			(emx/a-render-piece-on-board state board x y artcache "*" "_")
	       )
	       (insert "\n")
      )
    ) ; when whole map

  ) ; let

)

(defun emx/a-render-text (state)
  (insert "render text\n")

  ;; (cl-loop for x from 1 to 100 do (message "%d" x) )

  (let ( (board (emx/a-state-board state))
	 (artcache (emx/a-state-artcache state))
	 id
	 asset
       )

    (cl-loop for y from 0 to (1- (emx/a-map-h board)) do
	     (cl-loop for x from 0 to (1- (emx/a-map-w board)) do
		      (setq id (emx/a-map-get-tile board x y))
		      (setq asset (gethash id artcache))
		      ;; (message "%d %d %s" x y id)

		      ;; an item in the map .. look it up in the hash?
		      (if asset
			  (progn
			    (insert (cdr (assoc-string "text" (assoc-string "rest" (assoc-string "stance" asset)))))
			  )
			(progn
			  (insert "_")
			)
		      )
	     )
	     (insert "\n")
    )

  ) ; let
  
)

(cl-defmethod emx/a-render ( (state emx/a-state) &optional reactive? )
  "Display the board in STATE to current buffer; if not REACTIVE may schedule blink timer."

  (message "entering emx/a-render")

  (let* ( (bufexist? (get-buffer *emx/bufname*))
	  currpos
	)
    
	;; if buffer doesn't exist
	(if bufexist?
	    ( progn (switch-to-buffer *emx/bufname*)
		    (setq currpos (point))
	    )
	  (progn (switch-to-buffer *emx/bufname*)
		 (toggle-truncate-lines)
		 (emattacks-mode)
	  )
	)

	(let ( (inhibit-read-only t) )
	  
	  (erase-buffer)

	  (if (slot-value state 'guip)
	      (emx/a-render-gui state)
	    (emx/a-render-text state)
	  )

	) ; let inhibit

	;;(message "Current position %s" currpos)
 	(if bufexist?
	    (goto-char currpos) ; currpos
	  (goto-char 1)
	)

  )

  (let ( text )

    (setq text "")
    (setq text (concat text (emx/a-describe-status state)))
    (setq text (concat text (emx/side-panel-separator)))
    (setq text (concat text (emx/a-describe-unit state)))
    
    (emx/refresh-side-panel text)

  ) ; let

  ;; 'nil' repeat, since we're calling ourselves and can re-schedule the next call then .. ie:
  ;; this way the player can enable/disable the blinking effect as needed
  (when (and *emx/blinkallunits* (not reactive?))
    (setf (emx/a-state-rendertimer state) (run-at-time *emx/blinkallunitsdelay* nil 'emx/a-render state)) ; run-at-time time repeat function &rest args
  )
  
)
