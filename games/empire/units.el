
(defclass emx/a-unit ()
  (
   (x   :initarg :x   :type integer :documentation "x coord on map" :accessor emx/a-unit-x :initform nil)
   (y   :initarg :y   :type integer :documentation "y coord on map" :accessor emx/a-unit-y :initform nil)
   (mapid   :initarg :mapid   :type integer :documentation "which map?" :accessor emx/a-unit-mapid :initform nil)
   ;;
   (intent   :initarg :intent   :type string :documentation "intent code" :accessor emx/a-unit-intent :initform nil)                               ; "move" say
   (goal     :initarg :goal     :type string :documentation "a goal value meaningful to the intent code" :accessor emx/a-unit-goal :initform nil)  ; target (x,y) for a move, say
  )
  "Emacs Attacks! - Attacks! map instance state"
)
