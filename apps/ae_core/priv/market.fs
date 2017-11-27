%the bet is encoded in the macro `bet`
%we need a smart contract for the trust-free markets for financial derivatives.
%It acts as an order book.
%the market maker has to publish the price periodically, every trade is matched at the earliest price possible.
int 200 die_number !
macro mil int 1000000 ;
macro or_die int 0 == if
  die_number @ fail
else
  die_number @ int 1 + die_number !
then drop drop ;

%<<height:32, price:16, market_id:16, signature/binary>>
%sig data pub
 ( signed_price_declaration -- height price portion_matched)
 macro extract
int 10 split dup tuck Pubkey @ verify_sig or_die
int 4 split swap
int 2 split binary 2 AAA= swap ++ swap
int 2 split binary 2 AAA= swap ++ swap
            binary 2 AAA= swap ++ 
MarketID @ == or_die drop drop 
;

macro max ( A B -- M )
      2dup > if drop else swap drop then
;
macro min ( A B -- M )
      2dup > if swap drop else drop then
;
macro diff ( A B -- D )
      2dup ( A B A B )
      min >r max r> ( Max Min )
      -
;
macro minus_zero ( A B -- D ) % if A is bigger, returns A-B, else returns 0.
      2dup > if - else drop drop int 0 then
;
  %If the market maker publishes contradictory prices at the same time, with the same market id, he loses all the money in every bet. 
macro contradictory_prices ( signed_price_declaration signed_price_declaration2 -- delay nonce amount ) 
extract PM1 ! >r >r extract PM2 !
swap r> diff Period @ int 2 / < or_die %height equal %instead we should check if heights are within half a period of each other or less.
      
     r> == not swap drop swap drop %price unequal
     PM1 @ PM2 @ == not swap drop swap drop %portion_matched unequal
     or or_die
     int 0 mil mil + int 0
;

%if the market maker fails to publish by a certain point in time, then everyone can take all the money from the channels.
%delay = medium-low, nonce = (lower than if the market maker had published )
macro no_publish ( -- delay nonce amount )
      Period @
      mil height + Period @ -
      int 0
;

%If you try doing a no_publish while the server is publishing, this is how the server stops you from stealing money.
macro evidence ( signed_price_declaration -- delay nonce amount )
      extract drop drop ( declaration_height )
	      drop
      Expires @ height -  ( delay )
      mil height + Period @ int 2 / - ( delay nonce )
      int 10000 MaxPrice @ -
;
      

%The bet gets matched at the earliest price_declaration possible.
%So earlier price_declarations return lower nonces.
% runs bet and raises the nonce.

macro match_order ( signed_price_declaration -- delay nonce amount )
        extract ( SPD height price portion_matched )
	PM ! dup PRICE ! ( SPD height price )
	dup MaxPrice @ check_size or_die %make sure it is better than the agreed upon price.
	    %The biggest price means the most money goes to the server. So a trade that can get matched has a price that  is lower than the price we asked for.
	>r height > not or_die
	bet ( delay nonce amount )
            %if price_declaration_maker.price is better than the price we requested, then change delay from 1 to 0.
        rot Expires @ height minus_zero * tuck ( delay2 nonce amount )
	height swap ( delay nonce height amount )
	>r ( delay nonce height ) 
	swap mil + ( delay height big_nonce ) 
	% swap - r> ( delay new_nonce amount )
	swap drop r> ( delay new_nonce amount )
	PRICE @ flip MaxPrice @ ==
	if
	  drop drop PM @ * int 10000 / %first include the money that got matched in the order book 
	  int 10000 MaxPrice @ - int 10000 PM @ -
	  * int 10000 / +
%we add on some more money for how much refund we get from the unmatched portion.
	else % since the prices don't match, we might get a partial refund. If we were willing to pay a higher price than was actually matched.
          swap - >r
          int 0 == % if it is 0, that means we won the bet, so there is no additional refund.
          if
            drop r> drop
          else
            drop r> -
          then
	then	
;
macro unmatched ( OracleProof -- delay nonce amount )
        helper
	int 0 == if
     		Expires @ Period @ + height - int 100 +
        	int 100000
	 	int 10000 MaxPrice @ -
      	else
                %maybe we should get rid of this ending.
		int 51 int 500000 int 10000 MaxPrice @ -
      	then
;
macro main
swap
      int 0 == if drop drop no_publish else drop
      int 1 == if drop drop swap match_order else drop
      int 2 == if drop drop drop contradictory_prices else drop
      int 3 == if drop drop drop evidence else drop
      int 4 == if drop drop unmatched else drop
      then then then then then
      crash
;
main

