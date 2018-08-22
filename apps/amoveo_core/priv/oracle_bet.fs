%use `market:test().` to run this code

macro helper ( ProofStructure -- Int )
%first unwrap the oracle data from the proof structure.
% [ [5, short, long] ]
car drop
car swap int 5 == if %should equal 5 because of oracle type
  drop drop
else
  fail
then
car swap MarketID @ == if
  drop drop
else
  fail
then %should equal expected key.
car drop

int 32 split drop
int 1 split swap drop

%convert from 8 bit to 32 bit.
binary 3 AAAA swap  ++
;

macro bet ( ProofStructure -- delay nonce amount ) % no reason to return a nonce. only delay and amount are sufficient.
helper 
%1 means that the oracle returned true
int 1 == if drop drop 
     %delay, nonce, amount
     %delay has to be 1 or 0. it is multiplied by a bigger number to possibly set the delay to 0.
     int 0 int 3 bet_amount @ else

%if bet on false, bet_amount is 0.
%if bet_amount is 0, we ned to flip amount

%2 is false
drop int 2 == if drop drop 
     int 0 int 3 int 10000 bet_amount @ - else

% 3 means that the oracle decided that the question was a bad question to ask that does not have a clear binary answer.
drop int 3 == if drop drop 
     int 0 int 3 int 10000 MaxPrice @ - else

%0 means the oracle is still unresolved 
drop int 0 == if drop drop
     % fail else
     int 1 int 1 int 10000 MaxPrice @ - else

then then then then
;

macro doit
bet return
;		 
