
;This is a library for having a context to store local variables in, that way you don't need to use global variables.

(export (deflet3 let_stack))

(macro init ()
       '(nop 500 >r))
(init)

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
;(write (_variables (a) ((c a)) 0))
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
         (true 
          (cons (car Code)
		 (_call_stack* Many (cdr Code)))))))
(macro _length (X)
       ;returns the length of list X at compile-time
       (cond (((= X ()) 0)
	      (true (+ (_length (cdr X)) 1)))))

;this version of let gets it's inputs from the stack.


;this version of let accepts a list of pairs, which are variable definitions. 
;n is how deep in memory to store variables so we don't over-write any currently in use by the surrounding context.
(macro let*2 (pairs code n)
         (cond (((= pairs ()) code)
                 (true '(() ,(car (cdr (car pairs)))
                         r@ n + !
                         ,(let*2
                           (_variable*
                            (car (car pairs))
                            (cdr pairs)
                            n)
                           (_variable*
                            (car (car pairs))
                            code
                            n)
                           (+ n 1)))))))
;(let*2 ((a 4)(b (+ a 1))) ((+ a b)) 0)
;(write (let*2 (_variables (a) ((c a)) 0)
;              (_variables (a) (c) 0)
;              2))
; )


;,(write '(let macro here))
;(write (let ((x 2) (y 3)) (+ x (- y 1))))
;0


(macro deflet3 (vars pairs code);we should use this to define let and define.
       (deflet4 vars pairs code (_length vars)
         (+ (_length vars)
            (_length pairs))
         (reverse vars)))

(macro deflet4 (vars pairs code m n rv)
       '(nop
         ,(_load_inputs vars 0)
         ,(let*2 (_call_stack* n (_variables rv pairs 0))
                 ((_call_stack* n (_variables rv code 0)))
                 m)))
                                        ;(deflet3 () () () 0 ())


(macro let_stack (Vars Code)
       '(nop
         ,(_load_inputs Vars 0)
         ,(_call_stack*
           (_length Vars)
           (_variables (reverse Vars)
                       (Code)
                       0))))
;4 5
;(let_stack (x y) (* (+ x x) y))
;2 3 (let_stack (x y) (+ x (- y 1)))



