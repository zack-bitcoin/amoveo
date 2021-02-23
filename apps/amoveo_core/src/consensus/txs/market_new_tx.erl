-module(market_new_tx).
-export([go/4, make_dict/8]).
-include("../../records.hrl").

make_dict(From, CID1, Type1, Amount1, CID2, Type2, Amount2, Fee) ->
%    <<N1:256>> = CID1,
%    <<N2:256>> = CID2,
%    if
%            ((N1+Type1) =< (N2 + Type2)) -> 
    #market_new_tx{
           from = From,
           fee = Fee,
           cid1 = CID1,
           type1 = Type1,
           amount1 = Amount1,
           cid2 = CID2, 
           type2 = Type2,
           amount2 = Amount2}.
%            true ->
%            make_dict(From, CID2, Type2, Amount2, CID1, Type1, Amount1, Fee)
%    end.


go(Tx, Dict, NewHeight, _) ->
    #market_new_tx{
    from = From,
    fee = Fee,
    cid1 = CID10,
    type1 = Type10,
    amount1 = Amount10,
    cid2 = CID20,
    type2 = Type20,
    amount2 = Amount20} = Tx,
    false = ({CID10, Type10} == {CID20, Type20}),%dont make a market with the same asset on both sides.
    <<N1:256>> = CID10,
    <<N2:256>> = CID20,
%This way we can't make 2 markets that each are using the same pair of currencies.
    {CID1, Type1, Amount1,
     CID2, Type2, Amount2} = 
        if
            ((N1+Type10) =< (N2+Type20)) ->
                {CID10, Type10, Amount10,
                 CID20, Type20, Amount20};
            true ->
                {CID20, Type20, Amount20,
                 CID10, Type10, Amount10}
        end,
    true = NewHeight > forks:get(35),
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    Market = markets:new(CID1, Type1, Amount1, 
                         CID2, Type2, Amount2),
    MID = markets:make_id(Market),
    empty = markets:dict_get(MID, Dict2),
    Dict3 = markets:dict_write(Market, Dict2, NewHeight),
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
            
