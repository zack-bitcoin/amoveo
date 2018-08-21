%use `scalar_market:test().` to run this code

macro helper ( ProofStructure -- Int )
%first unwrap the oracle data from the proof structure.
car drop
car swap drop
car swap drop
car drop

int 32 split drop
int 1 split swap drop

%convert from 8 bit to 32 bit.
binary 3 AAAA swap  ++
;

macro shsc ( PS L -- L2 )
      swap helper swap cons
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

macro bet ( ProofStructure p2 p3 p4 p5 p6 p7 p8 p9 p10 -- delay nonce amount)
      %unpack the 10 things into a list. Use helper on each.
      helper nil cons
      shsc shsc shsc shsc shsc
      shsc shsc shsc shsc
      dup
      bad_oracle call
      if %some oracle had a 3
        int 0 int 3 int 10000 MaxPrice @ -
      else drop
        dup unresolved_oracle call
	if %some oracle had a 0
          int 1 int 1 int 10000 MaxPrice @ -
        else drop
	     twotozero %-for each one convert to a binary bit. 2->0
	     binary_convert %convert to decimal
	     int 10000 * int 255 / (Amount)
	     int 0 swap int 3 swap (delay nonce amount)
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
      [ int 1 , int 2, int 1, int 2] bad_oracle call %0
      [ int 1 , int 3, int 1, int 2] bad_oracle call %1
      [ int 1 , int 3, int 1, int 2] unresolved_oracle call %0
      [ int 0 , int 3, int 0, int 2] unresolved_oracle call %1
      [ int 0, int 0, int 1, int 1, int 1] binary_convert %7
      [ int 2, int 2, int 2, int 1] twotozero %[0,0,0,1]
;