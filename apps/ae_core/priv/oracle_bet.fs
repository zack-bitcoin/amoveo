%use `market:test().` to run this code

macro helper
%first unwrap the oracle data from the proof structure.
car drop
car swap drop
car swap drop
car drop

%next extract the result from the oracle
%    <<(X#oracle.id):256, %32 bytes
%      (X#oracle.result):8, % 1 byte
%      _/binary>>.

%int 32 split MarketID == or_die drop drop drop drop ( make sure the marketid is the same )
int 32 split drop
int 1 split swap drop

%for testing that result=1 works.
%binary 1 AQ==

%convert from 8 bit to 32 bit.
binary 3 AAAA swap  ++
;

macro bet ( ProofStructure -- delay nonce amount )
helper 
%1 means that the oracle returned true
int 1 == if drop drop 
     %delay, nonce, amount
     %delay has to be 1 or 0. it is multiplied by a bigger number to possibly set the delay to 0.
     int 0 int 3 bet_amount @ else

%2 is false
drop int 2 == if drop drop 
     int 0 int 3 int 10000 bet_amount @ - else

% 3 means that the oracle decided that the question was a bad question to ask that does not have a clear binary answer.
drop int 3 == if drop drop 
     int 0 int 3 int 10000 MaxPrice @ - else

%0 means the oracle is still unresolved %or maybe it means your bet never got matched?
drop int 0 == if drop drop 
     % fail else
     int 1 int 1 int 10000 MaxPrice @ - else

then then then then
;

macro doit
bet crash
;		 
