%use `market:test().` to run this code

%This makes case-statement-like syntax possible.
macro ncet nil print crash else then ;
macro case= == if drop drop ;

macro oracle_bet ( ProofStructure -- exit )
%first unwrap the oracle data from the proof structure.
car drop
car swap drop
car swap drop
car drop

%next extract the result from the oracle
%    <<(X#oracle.id):48, %6 bytes
%      (X#oracle.result):8, % 1 byte
%      _/binary>>.
int 6 split drop
int 1 split swap drop

%for testing that result=1 works.
%binary 1 AQ==

%convert from 8 bit to 32 bit.
binary 3 AAAA swap  ++

print

%1 means that the oracle returned true
int 1 case=
     %delay, nonce, amount
     int 0 int 3 int 10000 ncet

%2 is false
drop int 2 case=
     int 0 int 3 int 0 ncet

% 3 means that the oracle decided that the question was a bad question to ask that does not have a clear binary answer.
drop int 3 case=
     int 0 int 3 int 5000 ncet

%0 means the oracle is still unresolved
drop int 0 case=
     int 8000 int 1 int 5000 ncet
;
oracle_bet