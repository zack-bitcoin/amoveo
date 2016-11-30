( The goal here is to have a contract that if unlocked, results in a slightly different version of the same contract with a higher nonce. )

: format integer 1 == if
	hash integer -12 == swap dup integer -9 match rot and or_die call else
	integer -10
    then ;
: doit2 integer 1 == if
	hash binary qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY= == swap dup format match rot and or_die call else
	integer 27
    then ;
: doit integer 1 == if
	hash binary 2J54tzk6WXTncb03djRc/Tjlw0/0M25ZTmyuyaCRkGw= print == swap dup format match rot and or_die call else
	integer 555
    then ;

macro test integer 0 doit2 binary qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY= integer 1 doit call integer 27 == ;