(import (basics.scm))
(export global)

; the map is happening completely at run-time

(define map (F X)
  (cond (((= nil X) nil)
	 (true (cons
		(execute F ((car X)))
		;,(F '(car X))
		(recurse F ((cdr X))))))))

; this map is happening completely at compile-time
(macro map_ct (f l)
;       (write f))
;(macro next ()
       (cond (((= () l) ())
	      (true
               '(cons ,(execute f ((car l)))
                      ,(map_ct f (cdr l)))))))

			      
