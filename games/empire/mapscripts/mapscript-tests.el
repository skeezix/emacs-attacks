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

(defun emx/map-script-test-1 (state)

  (let* ( (board (emx/a-state-board state))
	)

    (emx/a-map-set-all-tiles board "watf")               ; fill with water
    (emx/a-map-set-tiles board 1 1 8 8 "grsf" t)         ; top left grass blob
    (emx/a-map-set-tiles board 13 10 8 8 "grsf" t)       ; middle bottom plains blob
    (emx/a-map-set-tiles board 16 13 4 4 "plnf" t)       ; middle bottom plains blob
    (emx/a-map-set-tiles board 13 10 8 8 "mowf" nil)     ; middle bottom plains blob
    (emx/a-map-set-tile board 1 1 "tref")
    (emx/a-map-set-tile board 2 2 "tref")
    (emx/a-map-set-tile board 3 3 "tnkb")
    (emx/a-map-set-tile board 4 4 "plnf")
    (emx/a-map-set-tile board 5 5 "mowf")
    (emx/a-map-set-tile board 10 10 "plnf")
    (emx/a-map-set-tile board 15 11 "tnkr")

    t

  ) ; let

)
