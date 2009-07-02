; Solium Entry for July 2009. Written in Lisp.
; Written with the CLISP Lisp interpreter, and uses
; its extensions for command line arguments

; ----------------------------------------------------------------------------
; Character manipulation
(defun prev-char (x) (code-char (- (char-code x) 1)))

(defun next-char (x) (code-char (+ (char-code x) 1)))

; ----------------------------------------------------------------------------
; Character list generators
(defun all-chars-rev (x) 
    (if (eql x #\A) 
        (list x) 
        (cons x (all-chars-rev (prev-char x)))
    )
)

(defun all-chars (x) (reverse (all-chars-rev x)))

(defun whitespace (x)
    (if (eql x 0) 
        ""
        (concatenate 'string " " (whitespace (- x 1)))
    )
)
    
(defun outter-space (x y) (- (- (length y) (position x y)) 1))

(defun inner-space (x y)
    (- (- (* 2 (- (length y) 1)) (* 2(outter-space x y))) 1)
)


; ----------------------------------------------------------------------------
; Diamond formatters
(defun format-diamond-line (x y) 
    (format nil "~A~%"
        (if (eql x #\A)
            (concatenate 'string (whitespace (- (length y) 1)) 
                                 (string x) 
                                 (whitespace (- (length y) 1)))
            (concatenate 'string (whitespace (outter-space x y)) 
                                 (string x) 
                                 (whitespace (inner-space x y)) 
                                 (string x) 
                                 (whitespace (outter-space x y)))
        )
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
; Main script
(format t "~A" (format-diamond #\A (all-chars #\D)))
