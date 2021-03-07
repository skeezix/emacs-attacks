
;;;; Emacs Attacks! .. "Attacks!" module

;; "starter": {
;; 	"method": "simplemenu",
;; 	"simplemenu": [
;; 	    [ "Map size" ],
;; 	    [ "Small", "emx/a/start-new", "s1" ],
;; 	    [ "Medium", "emx/a/start-new", "m1" ],
;; 	    [ "Large", "emx/a/start"=new, "l1" ]
;; 	]
;; }

(defun emx/a-init-module (path)
  "Invoke the module living in the supplied path, expecting a state object back"
  (message "Attacks! module entered; path %s" path)

  (load-file (concat path "/" "state.el"))
  (load-file (concat path "/" "map.el"))
  (load-file (concat path "/" "render.el"))
  (load-file (concat path "/" "sidepanel.el"))

  (let* ( (config_w 28)
	  (config_h 20)
	  board
	  piecehash
	  state
	)

    ;; build a board
    (setq board (emx/a-map :w config_w :h config_h))

    ;; load up all the pieces
    (setq piecehash (emx/inhale-module-pieces path "pieces"))

    ;; build a state, assigning in the board and assets
    (setq state (emx/a-state :board board :guip (display-images-p) :artcache piecehash ))

    ;;(message "Board internal")
    ;;(print board)

    ;; return newly init'd state
    state
  )
  
)

(defun emx/a-start-module (state)
  "Given a module state object, render and get going"

  ;; render to the buffer
  (emx/a-render state)

  ;; enable keymap
  (use-local-map emattacks-mode-map)

  ;; enable side panel
  (emx/show-side-panel)
  (emx/refresh-side-panel (emx/a-describe-unit))
 
  t

)

(defun emx/a-describe-unit ()
  "Return text describing selected unit"
  (let ( (text "") )
    ;; title
    (setq text (concat text "Super Blahg Tank\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "Location: (10,37) Plains\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "[D]  Destroy unit\n"))
    (setq text (concat text "[a]  Attack towards point\n"))
    (setq text (concat text "[m]  Move towards point\n"))
  )
)
