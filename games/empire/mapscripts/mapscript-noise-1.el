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

;; a classic 'noise' based map generator
;; a low level threshold -> water
;; as you go up level threshholds .. water, plains, grassland, mountain .. something like that

;; continents cookie cutter option .. drop some blob minimum size, grow outwards from there until
;; some depth value is found? drop cutouts onto an all-water layer?

;; hmm, write a perlkin noise function, or just invoke external python job .. hmmm..
