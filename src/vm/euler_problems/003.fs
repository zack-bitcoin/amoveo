%The prime factors of 13195 are 5,7, 13 and 29.
%What is the largest prime factor of the number 600851475143 ?
%problem003s(A, B) when ((B rem A) == 0) ->
%    problem003s(A, B div A);
%problem003s(A, B) when (A * A) > B -> B;
%problem003s(A, B) -> problem003s(A+2, B).

: f 
  2dup swap rem integer 0 == if
    swap dup tuck / recurse call
  else
   2dup swap dup * < if swap drop
     else
     swap integer 2 + swap recurse call
     then
  then
;
integer 3 integer 600851475143 f call