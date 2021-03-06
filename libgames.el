
;; user-emacs-directory -> ~/.emacs.d say
(defun emx/gamebase ()
  "Return path to where emacks-attacks base elisp lives"
  (interactive)
  (concat user-emacs-directory "/" *emx/basepath* ))

(defun emx/gamespath ()
  "Return path to where emacks-attacks games subdirs are located"
  (interactive)
  (concat (emx/gamebase) "/" *emx/gamespath*))

(defun emx/list-games-paths ()
  "Return a list of available games by name .. from within the games dir's init files"
  (interactive)
  (let ( (gamesfound (f-entries (*emx/gamespath*))) (gamelist ()) js jst apath modpath)
	 (while gamesfound
	   (progn

	     (setq apath (car gamesfound))
	     (setq modpath (f-join apath "module.json"))
	     
	     ;; echo
	     (message "Investigating game module: %s (%s)" (f-filename modpath) modpath)

	     ;; does it look like a game module?
	     (when (f-exists? modpath)

	       ;; try loading game module .. to string
	       ;;(setq jst (get-string-from-file modpath))
	       ;; convert string to json

	       (setq js (json-read-file modpath))
	       ;;(print js)
	       (when (assoc 'name js)
		 ;; if valid game, add to list
		 (add-to-list 'gamelist apath t)
	       ) ; when 'name' key exists within
	       
	     ) ; when module.json exists
	     
	     ;; remove first game from the list, so we can end eventually
	     (setq gamesfound (cdr gamesfound))

	   ) ; progn
	 ) ; while

	 ;; return the identified games
	 gamelist
  ) ; let
)

(defun emx/list-game-modes ()
  "Return a list of available game modes .. summarizing the gamedirs module file"
  (interactive)
  (let* ( (gamesfound (f-entries (emx/gamespath)))
	  (gamelist (make-hash-table :test 'equal))
	  js
	  jst
	  apath
	  modpath)
    (while gamesfound
      (progn

	(setq apath (car gamesfound))
	(setq modpath (f-join apath "module.json"))
	
	;; echo
	(message "Investigating game module: %s (%s)" (f-filename modpath) modpath)

	;; does it look like a game module?
	(when (f-exists? modpath)

	  ;; try loading game module .. to string
	  ;;(setq jst (get-string-from-file modpath))
	  ;; convert string to json

	  (setq js (json-read-file modpath))
	  ;;(print js)

	  (when (assoc 'name js)
	    ;; if valid game, add to list
	    ;;(add-to-list 'gamelist apath t)

	    (message "-> Identifies as game '%s'" (cdr (assoc 'name js)))

	    (let (detail)

	      (setq detail (list (cons "path" apath)
				 (cons "name" (cdr (assoc 'name js)))
				 (cons "elisp" (cdr (assoc 'elisp js)))
				 (cons "init" (cdr (assoc 'init js)))
				 (cons "initargs" (cdr (assoc 'initargs js)))
				 (cons "start" (cdr (assoc 'start js)))
				 (cons "startargs" (cdr (assoc 'startargs js)))
					;(cons "module" js)
				 ))

	      ;; store to the game 'list'
	      ;;(add-to-list 'gamelist detail)
	      (puthash (cdr (assoc "name" detail)) detail gamelist) ; key is the game name, like "Attacks!"
	      
	      ) ; let

	    ) ; when 'name' key exists within
	  
	  ) ; when module.json exists
	
	;; remove first game from the list, so we can end eventually
	(setq gamesfound (cdr gamesfound))

	) ; progn
      ) ; while

    ;; return the identified games
    gamelist
    ) ; let
)

;; (hash-table-count *emx/gamemodes*)
;; (clrhash myHash)

(defun emx/init-module (gamemodes selected-game)
  "invoke the specified game modes init function, if it can be found"
  ;;(print gamemodes) (print selected-game)

  (let* ( (mode (gethash selected-game gamemodes))
	  (file (cdr (assoc "elisp" mode)))
	  (path (cdr (assoc "path" mode)))
	  (fullpath (concat path "/" file))
	  (funcname (cdr (assoc "init" mode)))
	  (args (cdr (assoc "initargs" mode)))
	  (starter (cdr (assoc "start" mode)))
	  state
	)

    ;; can we load the modules elisp file?
    (message "Trying to load module elisp at %s" fullpath)
    (load-file fullpath)

    ;; can we fire up the init function to ready the module?
    (setq state
	  (if args
	      (funcall (intern funcname) path args)
	    (funcall (intern funcname) path)))

    ;; we've run the init now, so lets store the starter function name
    ;; for later
    (oset state startfunc starter)

    state
  )

)

(defun emx/start-module (state)
  "invoke the specified game modes init function, if it can be found"

  ;;(oref state startfunc)
  (funcall (intern (oref state startfunc)) state)

)

(defun emx/register-mapscript-from-file (modpath jspath)

  (let* ( (scriptpath (f-join modpath jspath))
	  (js (json-read-file scriptpath))
	  detail
	  elpath
	)
    
    ;;(print js)

    (setq detail (list (cons "path" jspath)
		       (cons "name" (cdr (assoc 'name js)))
		       (cons "id" (cdr (assoc 'id js)))
		       (cons "elisp" (cdr (assoc 'elisp js)))
		       (cons "start" (cdr (assoc 'start js)))
		       ))

    (setq elpath (f-join modpath (cdr (assoc 'elisp js))))
    (message "Attempting to load mapscript %s" elpath)
    (load-file elpath)
    
    detail

  ) ; let
  
)

(defun emx/invoke_mapscript (msalist state)
  (let* ( (funcname (cdr (assoc "start" msalist)))
	)

    (message "Calling mapscript function %s" funcname)
    (funcall (intern funcname) state)
  
  )
)
