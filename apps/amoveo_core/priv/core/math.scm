(import (basics.scm))


(define max (a b)
  (cond (((> a b) a)
         (true b))))
(define min (a b)
  (cond (((< a b) a)
         (true b))))
;(max 100 5)
