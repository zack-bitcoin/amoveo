-module(market_liquidity_tx).
-export([go/4, make_dict/4, send_stuff/5]).
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
    amount = Amount %amount is the larger of 2 amounts that could be deposited. 
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    M = markets:dict_get(MID, Dict2),
    #market{
             cid1 = CID1,
             type1 = Type1,
             amount1 = Amount1, 
             cid2 = CID2,
             type2 = Type2,
             amount2 = Amount2,
             shares = Shares
           } = M,

    {BuyAmount1, BuyAmount2} = 
        if
            Amount1 > Amount2 ->
                {Amount, Amount * Amount2 div Amount1};
            true ->
                {Amount * Amount1 div Amount2, Amount}
        end,

    SharesBought = Shares * BuyAmount1 div Amount1,
    
    %take away money from the account in the 2 subcurrency types in the same ratio as currently exists in the market.
    Dict3 = send_stuff(From, CID1, Type1, Dict2, -BuyAmount1),
    Dict4 = send_stuff(From, CID2, Type2, Dict3, -BuyAmount2),

    %add money to the 2 amounts in the market according to what was taken from the account, also add the new shares.
    M2 = M#market{
           amount1 = Amount1 + BuyAmount1,
           amount2 = Amount2 + BuyAmount2,
           shares = Shares + SharesBought 
          },
    Dict5 = markets:dict_write(M2, Dict4),

    %give the account liquidity shares according to the fraction of the liquidity that they had provided.
    Dict6 = send_stuff(From, MID, 0, Dict5, Shares),
    Dict6.

send_stuff(_, _, _, Dict, 0) -> Dict;
send_stuff(Pub, CID, Type, Dict, Amount) ->
    case CID of
        <<0:256>> ->
            Type = 0,
            Acc = case accounts:dict_get(Pub, Dict) of
                empty ->
                          true = (Amount) >= 0,
                          accounts:new(Pub, Amount);
                _ ->
                          accounts:dict_update(Pub, Dict, Amount, none)
                  end,
            accounts:dict_write(Acc, Dict);
        _ -> 
            Key = sub_accounts:make_key(Pub, CID, Type),
            SubAcc = case sub_accounts:dict_get(Key, Dict) of
                         empty ->
                             true = (Amount) >= 0,
                             sub_accounts:new(Pub, Amount, CID, Type);
                         _ ->
                             sub_accounts:dict_update(Key, Dict, Amount, none)
                     end,
            sub_accounts:dict_write(SubAcc, Dict)
    end.
 
