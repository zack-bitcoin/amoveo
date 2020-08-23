-module(market_new_tx).
-export([go/4, make_dict/8]).
-include("../../records.hrl").
-record(market_new_tx, {from, fee, cid1, cid2, type1, type2, amount1, amount2}).

make_dict(From, CID1, Type1, Amount1, CID2, Type2, Amount2, Fee) ->
    #market_new_tx{from = From,
                   fee = Fee,
                   cid1 = CID1,
                   type1 = Type1,
                   amount1 = Amount1,
                   cid2 = CID2, 
                   type2 = Type2,
                   amount2 = Amount2}.

go(Tx, Dict, NewHeight, _) ->
    #market_new_tx{
    from = From,
    fee = Fee,
    cid1 = CID1,
    type1 = Type1,
    amount1 = Amount1,
    cid2 = CID2,
    type2 = Type2,
    amount2 = Amount2} = Tx,
    false = ({CID1, Type1} == {CID2, Type2}),%dont make a market with the same asset on both sides.

    true = NewHeight > forks:get(34),
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    Market = markets:new(CID1, Type1, Amount1, 
                         CID2, Type2, Amount2),
    MID = markets:make_id(Market),
    empty = markets:dict_get(MID, Dict2),
    Dict3 = markets:dict_write(Market, Dict2),
    Dict4 = spend_stuff(From, CID1, Type1, Dict3, Amount1),
    Dict5 = spend_stuff(From, CID2, Type2, Dict4, Amount2),
    %This is how we encode the market liquidity shares.
    MKey = sub_accounts:make_key(From, MID, 0),
    MSAcc = sub_accounts:new(From, Market#market.shares, MID, 0),
    sub_accounts:dict_write(MSAcc, Dict5).
    
        
spend_stuff(Pub, CID, Type, Dict, Amount) ->
    %this is only for losing money, so it assumes that the account already exists.
    case CID of
        <<0:256>> ->
            Type = 0,
            Acc = accounts:dict_update(Pub, Dict, -Amount, none),
            accounts:dict_write(Acc, Dict);
        _ -> 
            Key = sub_accounts:make_key(Pub, CID, Type),
            SubAcc = sub_accounts:dict_update(Key, Dict, -Amount, none),
            sub_accounts:dict_write(SubAcc, Dict)
    end.
            
