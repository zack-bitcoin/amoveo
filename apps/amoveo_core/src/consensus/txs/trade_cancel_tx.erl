-module(trade_cancel_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").

make_dict(From, Nonce, Fee, Salt) ->
    #trade_cancel_tx{
           acc = From, nonce = Nonce,
           fee = Fee, salt = Salt}.

go(Tx, Dict, NewHeight, _) ->
    #trade_cancel_tx{
    acc = From,
    nonce = Nonce,
    fee = Fee,
    salt = Salt
   } = Tx,
    true = NewHeight > forks:get(44),
    TID = swap_tx:trade_id_maker(From, Salt),
    Trade0 = trades:dict_get(TID, Dict),
    F45 = NewHeight > forks:get(45),
    Trade = case {F45, Trade0} of
                {true, empty} -> 
                    trades:new(1, TID);
                _ -> Trade0
            end,
    #trade{
            height = Height
          } = Trade,
    true = Height < Nonce,
    NewTrade =
        Trade#trade{
          height = Nonce
         },
    Dict2 = trades:dict_write(NewTrade, Dict),
    Dict3 = swap_tx:fee_helper(
              Fee, From, Dict2),
    Dict3.
    

    
