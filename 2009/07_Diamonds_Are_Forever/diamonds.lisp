#!/usr/bin/clisp
; Character manipulation ------------------------------------------------------
(defun add-char (x y) (code-char (+ (char-code x) y)))
(defun prev-char (x) (add-char x -1))
(defun next-char (x) (add-char x 1))

; Diamond formatters ----------------------------------------------------------
(defun format-diamond-line (x y) 
    (format nil "~A~A~[~A~A~]~%" 
                (outer-space x y) x (if (eql x #\A) 1 0) (inner-space x y) x))

(defun format-diamond (x y)
    (if (eql x (car (last y)))
        (format-diamond-line x y)
        (concatenate 'string (format-diamond-line x y)
                             (format-diamond (next-char x) y)
                             (format-diamond-line x y))))

; Util functions --------------------------------------------------------------
(defun outer-space (x y) (create-whitespace (- (length y) (position x y))))
(defun inner-space (x y) (create-whitespace (- (* (position x y) 2) 1)))
(defun create-whitespace (x) (format nil (format nil "~~~DA" x) #\Space))

(defun command-arg-char ()
    (if (and (eql 1 (length *args*)) 
             (eql 1 (length (car *args*))) 
             (alpha-char-p (char (car *args*) 0)))
        (char-upcase (char (car *args*) 0))
        (error "~A~%" "Usage: diamonds.lisp [one letter]")))

(defun all-chars (x) 
    (if (eql x #\A) 
        (list x) 
        (append (all-chars (prev-char x)) (list x))))

; Main script -----------------------------------------------------------------
(format t "~A" (format-diamond #\A (all-chars (command-arg-char))))
