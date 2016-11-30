( Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640 )
( Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum. )

( Start Limit Total -- T )
: sum_squares 2dup == if drop drop else
	dup dup * tuck integer 1 + >r >r + r> r>
	recurse call
    then ;

    integer 0 integer 101 integer 0 sum_squares call integer 5050 dup * swap - 