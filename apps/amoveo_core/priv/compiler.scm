(export (compile))

(macro init ()
       '(nop 500 >r))
(init)
(macro length (l)
       (cond (((= l ()) 0)
              (true (+ 1
                       (length
                        (cdr l)))))))
(macro load_inputs (many mbv)
       (cond (((= many 0) ())
              ((= mbv 0)
               '(nop r@ !
                     ,(load_inputs (- many 1)
                                   (+ mbv 1))))
              (true
               '(nop (+ r@ mbv) !
                     ,(load_inputs (- many 1)
                                   (+ mbv 1)))))))
(macro replace_many (pairs code)
       (cond (((= pairs ()) code)
              (true (replace_many
                     (cdr pairs)
                     (replace_one (car pairs)
                                  code))))))
(macro replace_one (pair code)
       (cond (((= code ()) ())
              ((= code (car pair)) (car (cdr pair)))
              ((not (is_list code)) code)
              (true (cons (replace_one pair (car code))
                          (replace_one pair (cdr code)))))))
(macro function_internal (Vars code mbv env funs)
       '(end_fun
         def
         ,(load_inputs (length Vars) mbv)
         ,(replace_many (function_internal_pairs Vars mbv)
                       'code)))
(macro function_internal_pairs (l mbv)
       (cond (((= l ()) ())
              (true
               (cons
                (cons (car l)
                      (cons (@ (+ r@ mbv))
                            ()))
                (function_internal_pairs
                 (cdr l)
                 (+ mbv 1)))))))
(macro cond_internal (mbv X env funs)
       (cond (((= X ()) '(()))
	      ((= (car (car X)) true)
	       (cdr (car X)))
	      (true '(nop
		      ,(compile2 mbv (car (car X)) env funs)
		      if
		      ,(compile2 mbv (car (cdr (car X))) env funs)
		      else
		      (cond_internal mbv ,(cdr X) env funs)
		      then)))))
(macro =_i (A B) '(nop A B === tuck drop drop))
(macro tree_internal (T)
       (cond (((= T ()) '(nil))
	      ((is_list (car T))
	       '(cons ,(tree_internal (car T))
		      ,(tree_internal (cdr T))))
	      (true '(cons ,(car T)
			   ,(tree_internal (cdr T)))))))
(macro let_setup_inputs (mbv pairs env funs)
       '(nop
         (compile2 (+ mbv 1)
                   (car (cdr (car pairs)))
                   env
                   funs)
         (let_setup_inputs2 (length (car (car pairs)))
                            mbv)))
(macro let_setup_inputs2 (many_in mbv)
       (cond (((= many_in 0) ())
              (true '(() (nop r@ mbv + !)
                       (let_setup_inputs2
                        (- many_in 1)
                        (+ mbv 1)))))))
(macro let_setup_env (mbv pairs code env funs)
       (let_internal
        (+ mbv (length (car (car pairs))))
        (cdr pairs)
        code
        (let_setup_env2 env mbv (reverse (car (car pairs))))
        funs))
(macro let_setup_env2 (env mbv vars)
       (cond (((= vars ()) env)
              (true (let_setup_env2
                     (lambda (internal_arg)
                       (cond (((= internal_arg (car vars))
                               (@ (+ r@ mbv)))
                              (true (env internal_arg)))))
                     (+ 1 mbv)
                     (cdr vars))))))
                             
(macro let_internal (mbv pairs code env funs)
       (cond (((= pairs ()) (compile2 mbv code env funs))
              ((is_list (car (car pairs))) ;(write '(let internal pairs two)))
               '((),(let_setup_inputs mbv pairs env funs)
                 ,(let_setup_env mbv pairs code env funs)))
                ; ))
              (true
               '((! (compile2 (+ mbv 1)
                              (car (cdr (car pairs)))
                              env
                              funs)
                    (+ r@ mbv))
                ,(let_internal
                  (+ mbv 1)
                  (cdr pairs)
                  code
                  (lambda (y)
                    (cond (((= y (car (car pairs)))
                            (@ (+ r@ mbv)))
                           (true (env y)))))
                  funs))))))
(macro execute_helper (fun code mbv)
       (cond
        (((= mbv 0)
          (call code fun))
         (true
          '((call code fun
             (>r (+ r@ mbv)))
            (drop r>))))))
(macro
    compile2 (mbv expr env funs)
    (cond
     (((is_number expr) expr)
      ((is_atom expr) (env expr))
      ((null? expr) ())
      ((null? (car expr))
       (compile2 mbv (cdr expr) env funs))
      ((= let (car expr))
       (let_internal mbv
                     (car (cdr expr))
                     (car (cdr (cdr expr)))
                     env
                     funs))
;      ((= (car expr) quote) (cdr expr))
      ((= cond (car expr))
       (cond_internal mbv (car (cdr expr)) env funs))
      ((= = (car expr))
       (=_i (compile2 mbv (car (cdr expr)) env funs)
            (compile2 mbv (car (cdr (cdr expr))) env funs)))
      ((= tree (car expr))
       (tree_internal (car (cdr expr))))
      ((= set! (car expr))
       (! (compile2 mbv (car (cdr (cdr expr))) env funs)
          (compile2 mbv (car (cdr expr)) env funs)))
      ((funs (car expr))
       (execute_helper
          (@ (compile2 0 (car expr) env funs))
          (compile2 0 (cons nop (cdr expr)) env funs)
          mbv))
      ((is_atom (car expr))
       (cons (env (car expr))
              (compile2 mbv (cdr expr) env funs)))
      ((is_number (car expr))
       (cons (car expr)
             (compile2 mbv (cdr expr) env funs)))
      ((= def (car (car expr)))
       ((! (function_internal
            (reverse (compile2
             mbv
                      (car (cdr (cdr (car expr)))) env funs))
            (compile2
                      (+ (length (car (cdr (cdr (car expr)))))
                         mbv)
;             mbv
                      (car (cdr (cdr (cdr (car expr))))) env funs)
            mbv env funs)
           (compile2 mbv (car (cdr (car expr))) env funs))
        (compile2
         mbv
         (cdr expr)
         env
          ;update this boolean function to return 'true' if we lookup whether this variable is storing a function.
         (lambda (y) (cond (((= (compile2 mbv y env funs)
                                (compile2 mbv (car (cdr (car expr))) env funs))
                             1)
                            (true (funs y))))))))
      ((is_list (car expr))
       (cons (compile2 mbv (car expr) env funs)
             (compile2 mbv (cdr expr) env funs)))
      (true (()
             ,(write '(undefined syntax))
             ,(write (car expr)))))))

(macro compile (expr)
       (compile2
        0 ;how many bound variables in the current runtime environment.
        expr
        (lambda (x) x);
        (lambda (_) 0)))

(macro test ()
       '(()
       ,(! 5 N)
       ,(compile '(= 5 (@ N)))
       (=_i (cons 3 nil)
            ,(compile '(cons 3 nil)))
       (=_i 15 ,(compile '(+ (@ N) 10)))
       (=_i (+ 3 6)
            ,(compile '(+ 4 5)))
       ,(compile '(= 5 (cond (((> 5 4) (@ N))
                                     ((= 1 2) 6)
                                     (true 3)))))
       (=_i (tree_internal (1 2 3 4))
            ,(compile '(tree (1 2 3 4))))
       ,(compile '((def f1 (x y z) (- x y));defining a function
                   (= 4 (f1 7 3 1))
                   (= 10 (f1 15 (f1 (f1 11 1 0) 5 0) 0))
                   ))
       ,(compile '((set! A 50);global variables
                   (= 50 (@ A))))
       ,(compile '(= 21 (let ((foo 20);local variables using r-stack
                              (bar (- foo 2)))
                          ((+ bar 3)))))
       ,(compile '((def f (x) (+ 2 (* x x)))
                   (= 11 (let ((a (f 1))
                               (b (f a)))
                           b))))
       and and and and and and and and and and
       ,(compile '(= 100 (let (((a b) (car@ (cons 100 (cons 101 nil)))));binding variables to functions that have more than one output.
                    (a))))
       ,(compile '(= 102 (let (((x y z) (nop 101 102 103)))
                           y)))
       and and
       ;loading variables from the stack into local variable space
       ,(compile '(= 23 (nop 11 12 (let (((b c) ()))
                                     (+ b c)))))
       and
       ))
;(test)
;0
