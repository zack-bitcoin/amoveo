: format integer 1 == if
	hash integer -12 == swap dup integer -9 match rot and or_die call else
	integer -10 drop
    then ;
: doit integer 1 == if
	hash binary 6DIFJeegWoFCARdzPWKgFaMGniG5vD8Qh+WgPZBb5HQ=  == swap dup format match rot and or_die call else
	integer 555 drop
    then ;

macro test 
integer -10 integer 0 format doit check integer 555 == ;