#!/usr/bin/env clisp

(setq *numbers* (loop for i from 0 to 9999 append (list (format nil "~4,'0d" i))))

(defun calculate-overlap (a b)
  (cond ((string= (subseq a 1 4) (subseq b 0 3)) 3)
        ((string= (subseq a 2 4) (subseq b 0 2)) 2)
        ((string= (subseq a 3 4) (subseq b 0 1)) 1)
        (t 0)))

(defun find-largest-overlap-position (num numbers)
  (setq found nil)
  (loop for i from 3 downto 0 while (null found) do
    (setq found (position-if (lambda (n) (= (calculate-overlap num n) i)) numbers)))
  found)

(defun write-code ()
  (setq *last-number* (first *numbers*))
  (setq *numbers* (delete (first *numbers*) *numbers* :test #'equal))
  (format t "~a" *last-number*)

  (loop until (= (length *numbers*) 0) do
    (let ((next-item (nth (find-largest-overlap-position *last-number* *numbers*) *numbers*)))
        (format t "~a" (subseq next-item (calculate-overlap *last-number* next-item)))
        (setq *numbers* (delete next-item *numbers* :test #'equal))
        (setq *last-number* next-item))))


(write-code)
