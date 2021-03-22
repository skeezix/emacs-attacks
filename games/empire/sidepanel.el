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

(defvar *emx/panelname* "*EmacsAttacksPanel*" "Emacs Attacks! - buffer name for side panel")

(defun emx/show-side-panel ()
  "Open the side panel"

  (display-buffer-in-side-window
   (get-buffer-create *emx/panelname*) (list '(side . right) '(slot . 0)))

)

(defun emx/refresh-side-panel ( text )
  "Populate the side panel"

  (with-current-buffer *emx/panelname*
    (let ( (inhibit-read-only t) )
      (erase-buffer)
      (insert text)
      (setq cursor-type nil)
      (setq buffer-read-only t)
    ) ; let
  )

)

(defun emx/kill-side-panel ()
  "Kill the side panel entirely"

  (when (get-buffer *emx/panelname*)
    (kill-buffer *emx/panelname*)
  )

)

(defun emx/side-panel-separator ()
  (let ( (text "") )
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    (setq text (concat text "\n"))
    text
  )
)
