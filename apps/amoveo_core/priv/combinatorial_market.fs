
% the market maker has to publish the price periodically, every trade is matched at the earliest price possible.


int 200 die_number !
macro or_die int 0 == if
  die_number @ fail
else
  die_number @ int 1 + die_number !
then drop drop ;

% price declaration format
% <<height:32, price00:32, price01:32, price10:32, price11:32,
%	      portionMatched00:32, portionMatched01:32,
%	      portionMatched01:32, portionMatched11:32,
%	      market_id:256, signature/binary>>

macro extract4
int 4 split r> cons >r
;
macro extractlist
nil >r
extract4
extract4
extract4
extract4
r>
;

macro extract
int 66 split dup tuck Pubkey @ verify_sig or_die

% height
int 4 split >r

% price array
extractlist >r

% portion matched array
extractlist >r

MarketID @ == or_die drop drop
r> r> r> ( portionMatched Price height )
dup Height @ < not or_die
% `Height @` is when the smart contract was written.
tuck swap
;

macro max ( A B -- M )
      2dup > if drop else swap drop then
;
macro min ( A B -- M )
      2dup > if swap drop else drop then
;
macro diff ( A B -- D ) %calculates absolute value of A-B.
      2dup ( A B A B )
      min >r max r> ( Max Min )
      -
;