%use `market:test().` to run this code

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
print int 1 split swap drop
print binary 3 AAAA swap  ++
print



int 1 int 1 int 1 nil 