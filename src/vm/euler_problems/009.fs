(
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
)

macro square dup * ;
: pythag? square tuck square swap square + == ;
: eq1000? + + integer 1000 swap rem integer 0 == ;
: increasing? swap dup tuck > tuck < and ;
(integer 3 integer 4 integer 5 pythag? call)
(integer -1 integer 1 integer 2 increasing? call)
macro totrip dup integer 20 rem >r integer 20 /
             dup integer 20 rem >r integer 20 /
	     r> r> ;
(integer 4344 totrip)
macro restart not if integer 1 + recurse call else ;
:doit2 dup totrip increasing? call restart
      dup totrip eq1000?     call restart
      dup totrip pythag?     call restart
then then then;

macro doit
integer 0 doit2 call dup totrip + + integer 1000 swap / >r totrip 
r@ * tuck 
r@ * tuck 
r@ * tuck
;

doit