( If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
%Find the sum of all the multiples of 3 or 5 below 1000. )

: f r@ integer 0 == if else
  r@ integer 3 rem integer 0 ==
  r@ integer 5 rem integer 0 ==
  or if r@ + else then
  r> integer 1 - >r recurse call 
then ;
integer 999 >r integer 0 f call