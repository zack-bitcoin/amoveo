(import (basics.scm))
(export (filter))

(define filter (F X) 
  (cond (((= nil X) nil)
         ((execute F ((car X)))
          (cons (car X)
                (recurse F (cdr X))))
         (true (recurse F (cdr X))))))

;(define filter1 (F X);does not work
;  (cond (((= nil X) nil)
;         (true (filter2 F X)))))
;(deflet filter2 (F X) ((carx (car X))
;                       (fr ((filter1 F (cdr X)))))
;  (((execute F (carx))
;   (cons carx fr))
;  (true fr)))
