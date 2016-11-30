( 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder. )
( What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? )
: gcd dup integer 0 == if drop else
	dup tuck rem recurse call
	then ;
: main dup integer 1 == if drop else
	2dup gcd call swap dup rot / rot
	* swap integer 1 - recurse call
	then ;
: doit integer 1 integer 20 main call ;

doit call

( integer 20 integer 28 gcd call )