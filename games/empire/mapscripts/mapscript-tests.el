
(defun emx/map-script-test-1 (state)

  (let* ( (board (emx/a-state-board state))
	)

    (emx/a-map-set-all-tiles board "watf")               ; fill with water
    (emx/a-map-set-tiles board 1 1 8 8 "grsf" t)         ; top left grass blob
    (emx/a-map-set-tiles board 13 10 8 8 "grsf" t)       ; middle bottom plains blob
    (emx/a-map-set-tiles board 16 13 4 4 "plnf" t)       ; middle bottom plains blob
    (emx/a-map-set-tiles board 13 10 8 8 "mowf" nil)     ; middle bottom plains blob
    (emx/a-map-set-tile board 1 1 "tref")
    (emx/a-map-set-tile board 2 2 "tref")
    (emx/a-map-set-tile board 3 3 "tnkb")
    (emx/a-map-set-tile board 4 4 "plnf")
    (emx/a-map-set-tile board 5 5 "mowf")
    (emx/a-map-set-tile board 10 10 "plnf")
    (emx/a-map-set-tile board 15 11 "tnkr")

    t

  ) ; let

)
