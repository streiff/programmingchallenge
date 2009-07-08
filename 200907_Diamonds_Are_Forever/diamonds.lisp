; Solium Entry for July 2009. Written in Lisp.
; Written with the CLISP Lisp interpreter, and uses its way of accessing
; command line arguments.
;
; No variables were harmed (or changed) in the creation of this program.

; ----------------------------------------------------------------------------
; Character manipulation
(defun add-char (x y) (code-char (+ (char-code x) y)))
(defun prev-char (x) (add-char x -1))
(defun next-char (x) (add-char x 1))

; ----------------------------------------------------------------------------
; Character list generators
(defun all-chars (x) (reverse (all-chars-rev x)))
(defun all-chars-rev (x) 
    (if (eql x #\A) 
        (list x) 
        (cons x (all-chars-rev (prev-char x)))
    )
)

(defun whitespace (x)
    (if (eql x 0) 
        ""
        (concatenate 'string " " (whitespace (- x 1)))
    )
)
    
(defun outer-space (x y) (- (length y) (position x y) 1))
(defun inner-space (x y) (- (* 2 (- (length y) 1)) (* 2 (outer-space x y)) 1))

; ----------------------------------------------------------------------------
; Diamond formatters
(defun format-diamond-line (x y) 
    (format nil "~{~A~}~%"
        (if (eql x #\A)
            (layer (list (whitespace (- (length y) 1))
                            (string x)))
            (layer (list (whitespace (outer-space x y))
                            (string x)
                            (list (whitespace (inner-space x y)))))
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
; Util functions
(defun layer (l)
  (if (eq (length l) 1)
      (car l)
      (concatenate 'list (car l) (layer (cdr l)) (car l))
  )
)

(defun usage-error ()
    (error "~A~%" "Requires one command line argument with a single character.")
)

(defun is-letter (x)
    (if (and (char>= (char-upcase x) #\A) (char<= (char-upcase x) #\Z))
        1
        nil
    )
)

(defun command-arg-char ()
    (if (and 
            (eql 1 (length *args*)) 
            (eql 1 (length (car *args*))) 
            (is-letter (char (car *args*) 0)))
        (char-upcase (char (car *args*) 0))
        (usage-error)
    )
)

; ----------------------------------------------------------------------------
; Main script
(format t "~A" (format-diamond #\A (all-chars (command-arg-char))))
