
(defclass emx/a-state ()
  (
   (board :initarg :board :type emx/a-map :documentation "The game board" :accessor emx/a-state-board)
   (mapscript :initarg :mapscript :documentation "function to initialize the board" :accessor emx/a-state-mapscript)
   (guip :initarg :guip :initform t :documentation "True implies graphical render, otherwise text" :accessor emx/a-state-gui-p)
   (artcache :initarg :artcache :documentation "hash of string id to Piece details" :accessor emx/a-state-artcache)
   (startfunc :initarg :startfunc :documentation "function ref to invoke to start/render the module" :accessor emx/a-state-startfunc)
  )
  "Emacs Attacks! - Attacks! module instance state")

;;(emx/a-state-gui-p *emx/gamestate*)
;;(setf (emx/a-state-gui-p *emx/gamestate*) nil)

(cl-defmethod emx/a-state-print ( (obj emx/a-state)  )
  "Dump debug info to *Messages*"
  
  (message "State dump:")
  
  ;; asst
  (message "GUI enabled? %s" (emx/a-state-gui-p obj))
  (message "Artwork cache: %s items" (hash-table-size (emx/a-state-artcache obj)))

)
