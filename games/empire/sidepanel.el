
;; hydra seems to work nice here too..
;; hydra-posframe could be nice, but seems unsupported and broken project currently
;; ==> so we'll roll our own lame little panel

;;
;; experiment - posframe looks like it could work, but persists across buffer switches
;; .. so we'd have to hide during switch, and unhide on return, which could work
;;

(defvar *emx/side-panel-visible?* nil "should side panel be visible?")
(defvar *emx/side-panel-content* nil "content to display in side panel")
(defvar *emx/side-panel-timer* nil "current rimer object")

(defun emx/side-panel-hide-if-required ()
  "about to buffer change, so do we need to show or hide a side panel?"
  (if (string= (buffer-name) *emx/bufname*)
      (emx/hide-side-panel)
    (emx/show-restore-side-panel)
  )
)

(defun emx/showhide-side-panel ()
  "if side panel should be showing, show it"

  ;;(message "tick %s" (current-time-string))
  ;;(message "tick %s %s -> %s" (buffer-name) *emx/bufname* *emx/side-panel-visible?*)

  ;; don't hide when we're just in the minibuffer
  (unless (eq major-mode 'minibuffer-inactive-mode)
    (if (string= (buffer-name) *emx/bufname*)
	(if *emx/side-panel-visible?*
	    ;; if in module buffer, and should be visible.. show it
	    (progn
	      (emx/refresh-side-panel *emx/side-panel-content*)
	      (emx/show-side-panel)
	    )
	  ;; in module buffer, but should not be shown .. hide it
	  (emx/hide-side-panel))
      ;; not in module buffer, hide it
      (emx/hide-side-panel)
    )
  )
)

(defun emx/side-panel-init ()
  "Set up hooks for the side panel"
  
  ;;(remove-hook 'buffer-list-update-hook 'emx/side-panel-hide-if-required)
  ;;(add-hook 'buffer-list-update-hook 'emx/side-panel-hide-if-required)

  (emx/show-side-panel) ; create the panel

  (when *emx/side-panel-timer*
    (cancel-timer *emx/side-panel-timer*)
  )
  (setq *emx/side-panel-timer* (run-with-timer 0.5 0.5 #'emx/showhide-side-panel))

)

(defun emx/show-side-panel ()
  "Display the side panel (use -refresh- to populate it)"

  ;; show posframe detail panel
  (when (posframe-workable-p)
    (posframe-show *emx/panelname*
		   ;; :string emx/side-panel-content
		   :position '(900 . 10)
		   :min-width 30
		   :min-height 10
		   :lines-truncate t
		   ;; :background-color "brown" :foreground-color "white"
		   :internal-border-width 5 :internal-border-color "black")

    (setq *emx/side-panel-visible?* t)

  )

  ;;(posframe-delete-all)

)

(defun emx/refresh-side-panel ( text )
  "Populate the side panel"

  (setq *emx/side-panel-content* text)

  (with-current-buffer *emx/panelname*
    (erase-buffer)
    (insert *emx/side-panel-content*)
    (posframe-refresh *emx/panelname*)
  )

  ;;(posframe-delete-all)

)

(defun emx/intend-hide-side-panel ()
  "Flag the side panel to be hidden soon as can be"
  (setq *emx/side-panel-visible?* nil)
)

(defun emx/hide-side-panel ()
  "_Actually_ hide the side panel for now"
  (when (get-buffer *emx/panelname*)
    (posframe-hide-all)
  )
)

(defun emx/kill-side-panel ()
  "Kill the side panel entirely"

  (setq *emx/side-panel-content* nil)
  (setq *emx/side-panel-visible?* nil)
  
  (when (get-buffer *emx/panelname*)
    (posframe-delete-frame *emx/panelname*)
  )
  (when (get-buffer *emx/panelname*)
    (kill-buffer *emx/panelname*)
  )
)

;;
;; experiment - just render panel into the buffer? or split the buffer and render on side..
;;
