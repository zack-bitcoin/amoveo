(import (basics.scm))

(define (fold F A L)
  (cond (((= nil L) A)
         (true (recurse F
                        (execute F (A (car L)))
                        (cdr L))))))


