int 1024 oracle_max !
% int 1024 UL ! %needs to be <= oracle_max.
% int 0 LL ! % needs to be <= oracle_max, and >= 0.


%use `scalar_market:test().` to run this code

macro plus_32 ( Binary32 integer4 -- BinaryB32 )
      swap int 28 split tuck + ++
;

macro helper ( ProofStructure N -- Int )
%first unwrap the oracle data from the proof structure.
%car drop
>r
car swap int 5 == if
  drop drop
else
  fail
then
car swap MarketID @ r> plus_32 == if
  drop drop % check that it is the same as MarketID + N
else
  fail
then
car drop

int 32 split drop
int 1 split swap drop

%convert from 8 bit to 32 bit.
binary 3 AAAA swap  ++
;
: multi_helper_f ( Accumulator [ProofStructure ... ] N -- [Int ...])
  >r ( Acc L )
  nil == 
  if
    drop drop reverse r> drop
  else drop % [ProofStructure ...]
    car swap r@ helper
    rot cons swap r> int 1 + recurse call
  then
;
macro multi_helper ( [ProofStructure ... ] -- [Int ...])
  nil swap int 0 multi_helper_f call
;
    
: bad_oracle ( L -- Bool )
  nil ==
  if
    drop drop int 0
  else drop
    car swap int 3 ==
    if
      drop drop drop int 1
    else
      drop drop recurse call
    then
  then
;
: unresolved_oracle ( L -- Bool )
  nil ==
  if
    drop drop int 0
  else drop
    car swap int 0 ==
    if
      drop drop drop int 1
    else
      drop drop recurse call
    then
  then
;
: twotozerofun ( Acc L -- Acc2 )
  nil ==
  if
    drop drop reverse
  else drop
    car swap int 2 ==
    if
      drop drop int 0
    else
      drop drop int 1
    then
    rot cons swap recurse call
  then
;    
macro twotozero ( L -- L2 )
      nil swap twotozerofun call
;

: binary_convert2 ( L N -- N2 )
  swap nil == if
    drop drop
  else drop
    car swap rot int 2 * + recurse call
  then
;
macro binary_convert ( L -- N )
  int 0 binary_convert2 call
;
macro min2 ( A B -- M )
      2dup > if swap drop else drop then
;

macro bet2 ()
     LL @ int 1024 * oracle_max @ /
     2dup
     > if % no negative amounts.
       -
     else
       drop drop int 0
     then (oracle_amount)
     oracle_max @ *
     UL @ LL @ - /
     %((output of oracle range 1024) - ((LL * 1024) / OM)) * OM / (UL - LL)
     %imagine we have lower limit 200 and upper limit 400, and oracle_max of 900.
%((output of oracle range 1024) - ((200 * 1024) / 900)) * 900 / (400-200)
     int 10000 * int 1023 / (Amount)
     int 10000 min2
     int 10000 swap - bet_amount @ swap -
     int 0 swap int 3 swap (delay nonce amount)
;
macro bet ( [ProofStructure p2 p3 p4 p5 p6 p7 p8 p9 p10] -- delay nonce amount)
      multi_helper
      dup
      bad_oracle call
      if %some oracle had a 3
        int 0 int 3 int 10000 MaxPrice @ -
      else
        dup unresolved_oracle call
	if %some oracle had a 0
          int 1 int 1 int 10000 MaxPrice @ -
        else
	     twotozero %-for each one convert to a binary bit. 2->0
	     binary_convert %convert to decimal
	     bet2
	then
      then
;

macro doit
bet return
;

macro [ nil ;
macro , swap cons ;
macro ] swap cons reverse ;

macro test
      %binary 32 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= int 3 plus_32
      %[ int 1 , int 2, int 1, int 2] bad_oracle call %0
      %[ int 1 , int 3, int 1, int 2] bad_oracle call %1
      %[ int 1 , int 3, int 1, int 2] unresolved_oracle call %0
      %[ int 0 , int 3, int 0, int 2] unresolved_oracle call %1
      %[ int 0, int 0, int 1, int 1, int 1] binary_convert %7
      %[ int 2, int 2, int 2, int 1] twotozero %[0,0,0,1]
      int 512 bet2
;