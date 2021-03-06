
(defun emx/a-render-gui (state)
  ;; (insert "render gui\n")

  (let ( (board (emx/a-state-board state))
	 (artcache (emx/a-state-artcache state))
	 id
	 asset
	 cachedimage
       )

    (cl-loop for y from 0 to (1- (emx/a-map-h board)) do
	     (cl-loop for x from 0 to (1- (emx/a-map-w board)) do
		      (setq id (emx/a-map-get-tile board x y))
		      (setq asset (gethash id artcache))
		      ;;(message "%d %d %s" x y id)

		      ;; an item in the map .. look it up in the hash?
		      (if asset
			  (progn
			    (setq cachedimage (cdr (assoc-string "_icon" (assoc-string "rest" (assoc-string "stance" asset)))))
			    ;;(put-image cachedimage 9999 "*")
			    ;;(put-image cachedimage (emx/a-map-vecpos board x y) "*")
			    (insert-image cachedimage "*")
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

(cl-defmethod emx/a-render ( (state emx/a-state) )
  "Display the board to current buffer"

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

)
