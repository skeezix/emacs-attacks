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

(defun emx/point-right ()
  (interactive)
  (forward-char)
  (message "right")
)

(when t
  (defvar emattacks-mode-map nil "keymap for Emacs Attacks!")
  (setq emattacks-mode-map (make-sparse-keymap))
  (set-keymap-parent emattacks-mode-map special-mode-map)
  
  (define-key emattacks-mode-map (kbd "j") 'next-line)
  (define-key emattacks-mode-map (kbd "k") 'previous-line)
  (define-key emattacks-mode-map (kbd "l") 'emx/point-right)
  (define-key emattacks-mode-map (kbd "h") 'backward-char)
  ;; viewport
  (define-key emattacks-mode-map (kbd "4") 'emx/a-viewport-left)
  (define-key emattacks-mode-map (kbd "6") 'emx/a-viewport-right)
  (define-key emattacks-mode-map (kbd "8") 'emx/a-viewport-up)
  (define-key emattacks-mode-map (kbd "2") 'emx/a-viewport-down)

)
