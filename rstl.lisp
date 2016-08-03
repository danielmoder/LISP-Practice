; Daniel Moder, July 2016
; This program converts from Ruby S-expressions to LISP for any arithmetic expression

; Sample argument:
; (:program ((:binary (:binary (:@int "4" (1 0)) :/ (:@int "2" (1 2))) :+ (:binary (:@int "9" (1 4)) :/ (:@int "3" (1 6))))))
; (Ruby S-expression for "4/2+9/3")


; Format operations appropriately for LISP
(defun op-clean (op)
  (cond ((eql op :+) '+)
	((eql op :-) '-)
	((eql op :*) '*)
	((eql op :/) '/)))


; Takes Ruby S-expression of an arithmetic expression
; Returns the same arithmetic expression in LISP syntax

(defun ruby-sexp-to-lisp (exp)
  (cond ((null exp) null)						; If empty, return empty
	((atom exp) exp)						; If atom, return it
	(t								; Else:
	
	 (let ((tag (car exp))						; prefixed tag describes operation
	       (args (cdr exp)))					; car = tag, cdr = args
	       
	   (cond ((eql tag :program) (ruby-sexp-to-lisp (caar args)))
	   	 ; ":program" appears at beginning --> return the remainder of the S-exp.
		 ((eql tag :@int) (parse-integer (nth 0 args)))
		 ; ":@int" takes only a single integer argument: these are leaf nodes --> return the integer
		 ((eql tag :binary)
		 ; ":binary" operators! this is the good stuff

		  (let ((op (nth 1 args))
			(arg1 (nth 0 args))
			(arg2 (nth 2 args)))
		        ; read in Ruby's infixed-operators...

		    (list (op-clean op) (ruby-sexp-to-lisp arg1) (ruby-sexp-to-lisp arg2)))))))))
                    ; and reorder for LISP!
                    ; (...and recurse)
