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

;;;; why isn't this a mappable function, instead of a imperative style function? :P
(defun emx/inhale-module-pieces (pathbase pathext)
  "Scan the path, consuming json files there, returning a hash of string id's to the artwork"

  (let* ( (path (concat pathbase "/" pathext))
	  (pfound (f-entries path))
	  (plist (make-hash-table :test 'equal))
	  js
	  apath
	  path
	)

    (message "Looking for pieces in %s" path)
    ;;(print pfound)

    (while pfound

      (progn

	(setq apath (car pfound))
	(message "\tInspecting file %s" apath)

	(when (and (cl-search ".json" apath)
		   (not (cl-search "~" apath)))

	  ;; echo
	  (message "\t\tExamining piece: %s (%s)" (f-filename apath) apath)

	  ;; try loading game module .. to string
	  (setq js (json-read-file apath))

	  (when (assoc 'name js) ; assume its valid if it has the 'name' key

	    (message "\t\t\t-> Identifies as piece '%s'" (cdr (assoc 'name js)))

	    (let (detail asspath)

	      ;; try to load the associated image
	      (setq asspath (cdr (assoc 'icon (cdr (assoc 'rest (cdr (assoc 'stance js)))))))
	      (message "\t\t\t\tArtwork asset path appears to be %s" (concat pathbase "/" asspath))
	      (push (cons '_icon (create-image (concat pathbase "/" asspath))) (cdr (assoc 'rest (cdr (assoc 'stance js)))))

	      ;;(push (create-image (concat pathbase "/" asspath)) (assoc 'rest (cdr (assoc 'stance js))))

	      ;; make an object of it
	      (setq detail (list (cons "path" apath)
				 (cons "name" (cdr (assoc 'name js)))
				 (cons "id" (cdr (assoc 'id js)))
				 (cons "stance" (cdr (assoc 'stance js))) ))

	      ;; store to the game 'list'
	      (puthash (cdr (assoc "id" detail)) detail plist)
	      
	      ) ; let

	    ) ; when 'name' key exists within

	  
	) ; when

	;; remove first item from the list so we can end eventually
	(setq pfound (cdr pfound))

      ) ; progn

    ) ; while

    ;; return the identified games
    plist
  ) ; let
)

(defun emx/get-pieces-image (piececode)
  "Return the image .. or nil if no image found"

  (let (asset cachedimage)

    (setq asset (gethash piececode artcache))
    ;;(message "%d %d %s" x y id)

    ;; an item in the map .. look it up in the hash?
    (if asset
	(setq cachedimage (cdr (assoc-string "_icon" (assoc-string "rest" (assoc-string "stance" asset)))))
      nil
    )
  ) ; let

)
