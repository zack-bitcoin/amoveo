-module(market_liquidity_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
-record(market_liquidity_tx, {from, fee, nonce, mid, amount}).

make_dict(From, MID, Amount, Fee) ->
    Acc = trees:get(accounts, From),
    #market_liquidity_tx{
           from = From,
           nonce = Acc#acc.nonce + 1,
           mid = MID,
           fee = Fee,
           amount = Amount}.

go(Tx, Dict, NewHeight, _) ->
    #market_liquidity_tx{
    from = From,
    nonce = Nonce,
    mid = MID,
    fee = Fee,
    amount = Amount
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    M = markets:dict_get(MID, Dict2),

    %take away money from the account in the 2 subcurrency types in the same ratio as currently exists in the market.

    %add money to the 2 amounts in the market according to what was taken from the account.

    %give the account liquidity shares according to the fraction of the liquidity that they had provided.

    ok.
