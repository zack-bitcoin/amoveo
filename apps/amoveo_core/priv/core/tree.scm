;This macro converts nested lists from a compile-time data structure to a run-time data structure.

(macro tree (T)
       (cond (((= T ()) '(nil))
	      ((is_list (car T))
	       '(cons ,(tree (car T))
		      ,(tree (cdr T))))
	      (true '(cons ,(car T)
			   ,(tree (cdr T)))))))
