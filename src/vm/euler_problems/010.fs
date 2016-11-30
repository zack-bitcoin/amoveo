(
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
)
(a minute and 20 seconds to add all the primes below 200,000)
: prime2 dupnil== if drop true else 
  car N @ swap rem integer 0 == if drop false else
  recurse call then then ;
macro prime? dup N ! dup * Limit ! prime2 call; ( List Number -- true/false )
macro grow
  2dup prime? if 
  dup C +! 
  dup integer 1414 > if drop else
    nil cons swap ++ 
  then   
  else drop then ; ( List Number -- NewList )
: doit2 N @ grow integer 2 N +! N @ L @ > if drop C @ else recurse call then ;
macro doit [integer 2, integer 3] integer 5 N ! integer 200000 L ! integer 5 C ! doit2 call;
 doit 
