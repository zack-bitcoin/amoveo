; let is a common tool for lisp.
(import (core/immutable_variables.scm))

;let is how lisp offers local variables. That way you don't pollute the global namespace

;this let library provides the most standard syntax of let for lisp. example
;(let ((x 2)(y 3)(z 5)) (+ (- z y) x)) -> 2

;you can use expressions as inputs to the let expression:
;(let ((x (@ Height))(y 3)) (+ (- x x) y)) -> (- y (* 2 (@ height)))

; the (@ Height) computation executes only once, the result is stored in a variable, and read from that variable for every time you use x.

(macro seconds (ps)
  (cond (((= () ps) ())
         (true
          (cons ()
                (cons (car (cdr (car ps)))
                      (seconds (cdr ps))))))))
(macro firsts (ps)
  (cond (((= () ps) ())
         (true 
          (cons (car (car ps))
                (firsts (cdr ps)))))))
;(write (firsts '((1 2)(3 4))))
;0
;(seconds '((x 2)(3 y)))
                                        ;1
;(macro mcons (x y) (cons x y));for some reason I had to do this to get the compiler-time version of cons to run.
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

;let doesn't know about the surrounding context. if you want access to some data, it needs to be included in the pairs.
(macro let (pairs code)
       '(() (>r (+ @r 30));hopefully the parent functions has less than 30 input variables. there is probably a better way to do this.
         (seconds pairs)
         (let_stack (firsts pairs)
          ;(reverse (firsts pairs))
          code)
         (drop r>)
         ))
;(let ((a 3)(b 5)) (+ a b))
;(write (let_stack '(a b) (+ a b)))
; should become ->
;r@ 30 + r> 3 r@ ! 5 r@ 1 + ! r@ @ r@ 1 + @ +

;let* doesn't know about the surrounding context. if you want access to some data, it needs to be included in the pairs.
;you can't put let* into a function, instead use the letdef macro.
(macro let* (pairs code)
       '(() (>r (+ @r 30))
         (let*2 pairs (code) 0)
         (drop r>)
         ))
(macro test_let ()
       (let* ((a 7)
              (b (+ a 123));130
              (c 0)
              (d 1))
         '(nop (+ d (- b a)))))
                                        ;(test_let)
;(let* ((a 7)) (+ 5 a))
;(test)
;(let_stack (a) (+ a 2))
;(let* ((a 0)(x 2)(y 5)(z 11)) (+ y z))
;(let ((a 0)) (let ((b a)(x 2)) (+ b x)))
;(let ((a 0)) (+ 100 a))

                                        ;(let ((a 5)) (let ((b (+ 10 a))(c (+ 1 a))) (+ c b)))

;2 5 7 (let_stack (x y z) (+ x y))
;(seconds '((x 20)(y 10)))
;(let_stack (reverse (firsts '((x 20)(y 10))))
;           (+ x (- 1 y)))

;(! 5 Z )
;(let ((x 20)
;      (y (@ Z))
;      (z (* 1 (@ Z))))
;  (+ y (- y (* y (* y 0)))))
;drop
