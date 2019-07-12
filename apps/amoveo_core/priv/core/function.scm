(import (core/immutable_variables.scm))
(export (execute lambda define execute2 deflet))

;this is a library for making functions at run-time.

(macro lambda (Vars Code)
      ; define a new function
       (deflet2 Vars () (Code)))
;(macro lambda_old (Vars Code)
;       '(nop
;         def
         ;,(write (function lambda))
         ;(write Vars)
         ;(write Code)
;         ,(let_stack Vars (Code))
;         end_fun))
;(ex (lambda (a b c) '(nop b))
;(macro define_old (Name Vars Code)
;       '(! ,(lambda Vars Code) Name))
(macro define (Name Vars Code)
      ;make a new function and give it a name.
       '(deflet Name Vars () Code))
(macro execute (F V)
       '(call ,(cons nop V) F))
       ;'(call V F))
(macro execute2 (Vars)
                                        ;'(execute (@ ,(car Vars)) ,(cdr Vars)))
       ;'(nop
         ;(write ,(car Vars))
       '(execute (@ ,(car Vars)) ,(cdr Vars)))

(macro deflet (name vars pairs code)
       ;store the function pointer in a variable
       '(! ,(deflet2 vars pairs (code)) name))
(macro deflet2 (vars pairs code) ;ths is the new version of lambda.
       ;wrap the function definition in `def` and `end_fun` to mark it as a function.
       '(nop
         def
         ,(deflet3 vars pairs code )
         end_fun))


;(lambda (x y) (+ 1 (+ x y)))
;(lambda (x y z) (+ x (+ z y )))
;(_load_inputs (x y z) 0)
;(_length (x y z))
;(_variables (z y x) '(+ z (+ y x)) 0)

;(_length (1 1 5 1 1 1 1))
;(_pointer 3) ; 4

;4 3 (_load_inputs (a b) 0)
;(_variable* a '(+ a 1) 0) ;900 @ 5 + @
;(_variable* a (_variable* b (a b) 0) 1)
;(_variables (a b) '(+ a (+ b 2)) 0)
;(_call_stack* 3 '(+ (+ a b) c))

;3 (_load_inputs (x) 0)

