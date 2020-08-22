-module(market_swap_tx).
-export([go/4, make_dict/6]).
-include("../../records.hrl").
-record(market_swap_tx, {from, fee, nonce, mid, give, take, direction}).

make_dict(From, MID, Give, Take, Direction, Fee) ->
    Acc = trees:get(accounts, From),
    #market_swap_tx{
           from = From,
           nonce = Acc#acc.nonce + 1,
           mid = MID,
           fee = Fee,
           give = Give,
           take = Take,
           direction = Direction}.

go(Tx, Dict, NewHeight, _) ->
    #market_swap_tx{
    from = From,
    nonce = Nonce,
    mid = MID,
    fee = Fee,
    give = Give,
    take = Take,
    direction = Direction
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    M = markets:dict_get(MID, Dict2),

    %maintain constant product in the market.


    %take away one kind of currency from acc, give to market.
    %take away one kind of currency from market, give to account. Pay a governance_fee less than the total so that liquidity shares can grow.


    ok.
    
