
( the main function recursively adds 0's to the stack. )
: main dup integer 0 > if integer 1 - integer 0 swap recurse call else drop then ;

( the test macro adds 5 zeros, and checks to make sure that 5 zeros were added. )
macro test
integer 5 main call 
integer 0 == swap 
integer 0 == and swap 
integer 0 == and swap 
integer 0 == and swap 
integer 0 == and
;