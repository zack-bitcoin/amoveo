; this is the run time version of cond.
; case.scm tests this out.

(macro cond (X)
       (cond (((= X ()) '(()))
	      ((= (car (car X)) true)
	       (cdr (car X)))
	      (true '(nop
		      ,(car (car X))
		      if
		      ,(car (cdr (car X)))
		      else
		      (cond ,(cdr X))
		      then)))))
