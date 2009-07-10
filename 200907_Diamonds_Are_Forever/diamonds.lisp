#!/usr/bin/clisp
; ----------------------------------------------------------------------------
; Character manipulation
(defun add-char (x y) (code-char (+ (char-code x) y)))
(defun prev-char (x) (add-char x -1))
(defun next-char (x) (add-char x 1))

; ----------------------------------------------------------------------------
; Character list generators
(defun all-chars (x) 
    (if (eql x #\A) 
        (list x) 
        (append (all-chars (prev-char x)) (list x))
    )
)

; ----------------------------------------------------------------------------
; Diamond formatters
(defun format-diamond-line (x y) 
    (if (eql x #\A)
        (format nil (format nil "~~~dd~~d~~%" (length y)) #\Space #\A)
        (format nil (format nil "~~~dd~~d~~~dd~~d~~%" (outer-space x y) 
                                                      (inner-space x y)) 
                    #\Space (string x) #\Space (string x))
    )
)

(defun format-diamond (x y)
    (if (eql x (car (last y)))
        (format-diamond-line x y)
        (concatenate 'string (format-diamond-line x y)
                             (format-diamond (next-char x) y)
                             (format-diamond-line x y))
    )
)

; ----------------------------------------------------------------------------
; Util functions
(defun outer-space (x y) (- (length y) (position x y)))
(defun inner-space (x y) (- (* (- (length y) (outer-space x y)) 2) 1))

(defun command-arg-char ()
    (if (and (eql 1 (length *args*)) 
             (eql 1 (length (car *args*))) 
             (alpha-char-p (char (car *args*) 0)))
        (char-upcase (char (car *args*) 0))
        (error "~A~%" "Usage: diamonds.lisp [one letter].")
    )
)

; ----------------------------------------------------------------------------
; Main script
(format t "~A" (format-diamond #\A (all-chars (command-arg-char))))
