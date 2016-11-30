( By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number? )

: prime2 dupnil== if drop true else 
  car N @ 2dup swap rem integer 0 == if drop drop drop false else
  swap dup * < if drop true else 
  recurse call then then then ;

macro prime? dup N ! dup * Limit ! prime2 call; ( List Number -- true/false )

macro grow
  2dup prime? if 
  integer 1 C +! 
  dup integer 1000 > if drop else
    nil cons swap ++ 
  then   
  else drop then ; ( List Number -- NewList )

( [integer 2, integer 3, integer 5, integer 7, integer 11] integer 13 prime?   )
( [integer 2, integer 3, integer 5, integer 7, integer 11] integer 12 grow call )

: doit2 N @ grow integer 2 N +! C @ L @ == if drop N @ integer 2 - else recurse call then ;
macro doit [integer 2, integer 3] integer 5 N ! integer 10001 L ! integer 2 C ! doit2 call;

 doit integer 104743 ==