#!/usr/bin/env clisp

(defun estimate-chemical-x (topography bat-computer-location)
    (defvar *topography* topography)
    (defvar *bat-computer-location* bat-computer-location)
    (loop for i from 1 to (length topography) until (= (height-of-chemical-x i) 0) sum (- (height-of-chemical-x i) (height-of-column i))))

(defun height-of-column (column)
    (parse-integer (subseq *topography* (- column 1) column)))

(defun height-of-chemical-x (column)
    (cond ; Off the map. Always 0.
          ((> column (length *topography*)) 0)

          ; Left of or at the bat computer. Take the highest available
          ((<= column *bat-computer-location*) (max (+ (height-of-column column) 1)
                                                    (height-of-chemical-x (+ column 1))))

          ; Right of the bat computer and lower than the level needed to flood the computer
          ((<= (height-of-column column) (height-of-column *bat-computer-location*)) (+ (height-of-column *bat-computer-location*) 1))

          ; else
	  (t 0)))

(if (= 2 (length *args*)) 
    (format t "Estimate: ~A~%" (estimate-chemical-x (car *args*) (parse-integer (cadr *args*))))
    (format t "Usage: ~A <topography> <bat_computer_column>" *load-pathname*))
