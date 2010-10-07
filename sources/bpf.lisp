;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; This file is part of omchreode.

;;; Copyright (c) 2010, Kilian Sprotte. All rights reserved.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :om)

(defun interpolate-bpf (bpf x)
  (let ((pos (position x (om::x-points bpf) :test #'<)))
    (unless pos (setq pos (- (length (om::x-points bpf)) 1)))
    (decf pos)
    (let* ((x-tail (nthcdr pos (om::x-points bpf)))
	   (x1 (first x-tail))
	   (x2 (second x-tail))
	   (y-tail (nthcdr pos (om::y-points bpf)))
	   (y1 (first y-tail))
	   (y2 (second y-tail)))
      (om::interpol-segment x x1 x2 y1 y2))))


(defmethod! bpf-lookup ((x number) bpf xmin xmax ymin ymax)
  :doc "Allows to find the corresponding y value for a given x.
The bpf is treated as if its ranges were as given by xmin, xmax, ymin, ymax."
  (if (<= xmin x xmax)
      (om-scale
       (interpolate-bpf
	bpf
	(om-scale x
		  (list-min (x-points bpf))
		  (list-max (x-points bpf))
		  xmin xmax))
       ymin ymax
       (list-min (y-points bpf))
       (list-max (y-points bpf)))
      nil))
