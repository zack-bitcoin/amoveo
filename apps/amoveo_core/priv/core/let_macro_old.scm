; let is a common tool for lisp.
(import (core/cond.scm core/eqs.scm))


;first a version that happens at compile time
;it is a way of making a short term macro that wont pollute the macro name space anywhere else.
(macro let_macro (pairs code)
       (cond
	(((= pairs ()) code)
	 (true (let_macro (let2 (car pairs) (cdr pairs))
		 (let2 (car pairs) code))))))
(macro let2 (pair code)
       (let3 (car pair)
	     (car (cdr pair))
	     code))

(macro let3 (ID Value Code)
       (cond (((= () Code) ())
	      ((= ID (car Code))
	       '(cons Value ,(let3 ID Value (cdr Code))))
	      ((is_list (car Code))
	       '(cons ,(let3 ID Value (car Code))
		     ,(let3 ID Value (cdr Code))))
	      (true '(cons ,(car Code)
			  ,(let3 ID Value (cdr Code)))))))


;now here is the run-time version

; uses the r-stack to store the memory locations
; for the input of the functions we are currently
; processing. So the r-stack is used as a function
; call stack, one additional thing is added to r every
; time a function is called, and one thing is
; removed from r every time a function returns.


;(>r 500) ;start storing inputs to functions at 50, that way 1-49 are available for smart contract developers.

(macro _pointer (X)
       ; look up a pointer to the xth variable being stored for the current function being processed
       (cond (((= X 0) '(r@))
	      (true '(+ r@ X)))))
;(_load_inputs (a) 0)
(macro _load_inputs (V N)
       ; store the inputs of the function into variables,
       ; the top of the r stack points to these variables.
       (cond (((= V ()) ())
	      ((= 0 N) '(nop r@ !
			  ,(_load_inputs (cdr V) 1)))
	      (true '(nop ,(_pointer N) !
			  ,(_load_inputs (cdr V) (+ N 1)))))))
;1 2 3
;(_variable* zack '(nop 0 zack) 1)
(macro _variable* (Var Code N)
                                        ;Replace each Var in Code with the input to the function
       ;'(nop
         (cond (((= Code ()) ())
                 (true
                  (cons
                   (cond
                    (((is_list (car Code))
                      (_variable* Var (car Code) N))
                     ((= (car Code) Var)
                      '(@ ,(_pointer N)))
                     (true (car Code))))
                   (_variable* Var (cdr Code) N))))))
(macro _variables (Vars Code N)
;       '(nop
       ; repeatedly use _variable* to replace
       ; each Var in Code with the inputs to the function,
                                        ; which are stored in the vm as variables.
;       '(;(write Code)
;l         nop
       (cond (
              ;((not (is_list Code))
               ;(car (_variables Vars (Code) N)))
              ((= Vars ()) Code)
               (true (_variables
                      (cdr Vars)
                      (_variable* (car Vars) Code N)
                      (+ N 1))))))
;(write (_variables (reverse (a b c))
;                   '(nop (+ a b)) 0))
;0
(macro _call_stack* (Many Code)
       ; functions need to be able to call other functions.
       ; if function A calls function B, then when our
       ; program returns from function B, we need to
       ; remember the inputs for all the variables in
       ; function A, so we can process the rest of
       ; function A correctly.
       (cond
	(((= Many 0) Code)
	 ;if a function has no inputs, then the call
	 ; stack remains unchange.
	 ((= Code ()) ())
	 ((is_list (car Code))
	  (cons (_call_stack* Many (car Code))
		(_call_stack* Many (cdr Code))))
         ((= (car Code) call)
	   ;'(nop ,(cdr Code) (+ r@ Many) >r
	   ;'(nop ,(_call_stack* Many (cdr Code)) (+ r@ Many) >r
	   '(nop ,(_call_stack* Many (cdr Code)) ,(_pointer Many) >r
                 call
                 r> drop))
                                        ;,(_call_stack* Many (cdr Code))))
         ;((= (car Code) let*) (write (let_macro here)))
         (true 
          (cons (car Code)
		 (_call_stack* Many (cdr Code)))))))
(macro _length (X)
       ;returns the length of list X at compile-time
       (cond (((= X ()) 0)
	      (true (+ (_length (cdr X)) 1)))))

;this version of let gets it's inputs from the stack.
(macro let_stack (Vars Code)
       '(nop
;         ,(write (inside let macro))
;         (write ,(reverse Vars))
;         (write ,(_load_inputs Vars 0))
;         (write ,(Code))
;         (write ,(_variables (reverse Vars)
;                            (Code)
;                            0))
;          ,(_variables (reverse Vars)
;                      Code
;                      0))
         ,(_load_inputs Vars 0)
         ,(_call_stack*
           (_length Vars)
           (_variables (reverse Vars)
                       (Code)
                       0))))
;4 5
;(let_stack (x y) (* (+ x x) y))

;2 3 (let_stack (x y) (+ x (- y 1)))
;,(write '(let macro here))
;(write (let ((x 2) (y 3)) (+ x (- y 1))))
;0
