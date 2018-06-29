
% the market maker has to publish the price periodically, every trade is matched at the earliest price possible.


int 200 die_number !
macro or_die int 0 == if
  die_number @ fail
else
  die_number @ int 1 + die_number !
then drop drop ;

macro mil int 1000000 ;

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

macro extract ( signed_price_declaration -- height price portionMatched )
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

macro no_publish( -- delay nonce amount )
      Period @
      dup % dup + ( period 2period )
      height swap / ( period, height / 2period  )
      int 0
;

macro contradictory_prices ( signed_price_declaration signed_price_declaration2 -- delay nonce amount )
  extract PM1 ! >r >r
  extract PM2 !
  swap r> diff Period @ int 2 / < or_die %check if heights are within half a period of each other or less.
  r> == not tuck drop drop %price unequal
  PM1 @ PM2 @ == not tuck drop drop %portion_matched unequal
  or or_die
  int 0 mil mil + int 0
;

macro evidence ( signed_price_declaration -- delay nonce amount )
  extract height Period @ - > or_die %require that the SPD was made in the most recent Period
  drop ( declaration_height )
  Period @ / int 1 + ( nonce )
  r> Expires @ hight - ( delay )
  r> ( delay nonce )
  int 10000 MaxPrice @ -
;

macro unmatched ( OracleProof1 OracleProof2 -- delay nonce amount )
  helper
  int 0 == if
    drop drop
    Expires @ Period @ + int 2000 +
    int 0
    int 10000 MaxPrice @ -
  else
    drop drop
    helper
    int 0 == if
      drop drop
      Expires @ Period @ + int 2000 +
      int 0
      int 10000 MaxPrice @ -
    else
      drop drop
      Period @
      int 1
      int 10000 MaxPrice @ -
    then
  then
;
: vector ( List Int -- Element )
  dup int 0 ==
  if
    drop drop car drop
  else
    drop int 1 - swap car swap drop swap recurse call
  then
;
macro match_order ( signed_price_declaration -- delay nonce amount )
  extract ( SPD height price portion_matched )
  Type @ vector call
  PM !
  Type @ vector call
  dup PRICE ! ( SPD height price )
  dup MaxPrice @ check_size or_die %make sure it is better than the agreed upon price.
  >r HEIGHT !
  % working here
         
;

macro main
swap
      int 0 == if drop drop no_publish else drop
      int 1 == if drop drop swap match_order else drop
      int 2 == if drop drop drop contradictory_prices else drop
      int 3 == if drop drop drop evidence else drop
      int 4 == if drop drop unmatched else drop
      then then then then then
      return
;
% main
