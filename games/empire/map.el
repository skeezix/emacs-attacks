;; tests
;;(emx/a-map :w 3 :h 4)

(defclass emx/a-map ()
  (
   (w   :initarg :w   :type integer :documentation "int for map width" :accessor emx/a-map-w :initform 37)
   (h   :initarg :h   :type integer :documentation "int for map height" :accessor emx/a-map-h :initform 27)
   (fog :initarg :fog :type vector :documentation "boolean vector, where true implies fog in the square" :accessor emx/a-map-fog )
   (map :initarg :map :type vector :documentation "string vector, value maps to Pieces" :accessor emx/a-map-map)
  )
  "Emacs Attacks! - Attacks! map instance state"
)

(cl-defmethod initialize-instance :after ((obj emx/a-map) &rest _args)

  (message "initialize emx/a-map with size %dx%d" (oref obj w) (oref obj h) )

  ;; build the map
  (oset obj map (make-vector (* (oref obj w) (oref obj h)) nil))

  ;; build the fog
  (oset obj fog (make-vector (* (oref obj w) (oref obj h)) nil))

  )

(cl-defmethod emx/a-map-vecpos ( (obj emx/a-map) x y )
  (let* ( (arr (oref obj map))
	  (width (oref obj w))
	  n
	)
    (setq n (+ (* y width) x))
  )
)

(cl-defmethod emx/a-map-set-all-tiles ( (obj emx/a-map) id )
  "Flood fill the whole map with a tile"
  (let* ( (w (emx/a-map-w obj))
	  (h (emx/a-map-h obj) )
	  x
	  y)
    (cl-loop for y from 0 to (1- h) do
	     (cl-loop for x from 0 to (1- w) do
		      (emx/a-map-set-tile obj x y id)
	     ) ; cl-loop x
    ) ; cl-loop y
  ) ; let
)

(cl-defmethod emx/a-map-set-tiles ( (obj emx/a-map) x y w h id &optional fill? rounded?)
  "Fill a rectangular region with a given tile; fill? defines a filled or frame rectangle"
  (cl-loop for ty from y to (+ y h) do
	   (cl-loop for tx from x to (+ x w) do
		    (if fill?
			(emx/a-map-set-tile obj tx ty id)
		      (cond
		       ((= ty y) (emx/a-map-set-tile obj tx ty id))
		       ((= ty (+ y h)) (emx/a-map-set-tile obj tx ty id))
		       ((= tx x) (emx/a-map-set-tile obj tx ty id))
		       ((= tx (+ x w)) (emx/a-map-set-tile obj tx ty id))
		      )
		    )
	   ) ; cl-loop x
  ) ; cl-loop y
)

(cl-defmethod emx/a-map-set-tile ( (obj emx/a-map) x y id )
  (let* ( (arr (oref obj map))
	  (width (oref obj w))
	  (n (+ (* y width) x))
	)

    (aset arr n id)
    
  )
  
)

(cl-defmethod emx/a-map-get-tile ( (obj emx/a-map) x y )
  (let* ( (arr (oref obj map))
	  (width (oref obj w))
	  (n (+ (* y width) x))
	)

    (aref arr n)
    
  )
  
)
