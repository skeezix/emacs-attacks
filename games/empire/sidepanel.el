
(defvar *emx/panelname* "*EmacsAttacksPanel*" "Emacs Attacks! - buffer name for side panel")

(defun emx/show-side-panel ()
  "Open the side panel"

  (display-buffer-in-side-window
   (get-buffer-create *emx/panelname*) (list '(side . right) '(slot . 0)))

)

(defun emx/refresh-side-panel ( text )
  "Populate the side panel"

  (with-current-buffer *emx/panelname*
    (erase-buffer)
    (insert text)
    (setq cursor-type nil)
    (setq buffer-read-only t)
  )

)

(defun emx/kill-side-panel ()
  "Kill the side panel entirely"

  (when (get-buffer *emx/panelname*)
    (kill-buffer *emx/panelname*)
  )

)
