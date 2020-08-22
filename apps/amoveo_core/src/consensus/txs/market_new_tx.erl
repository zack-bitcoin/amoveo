-module(market_new_tx).
-export([go/4, make_dict/6]).
-include("../../records.hrl").
-record(market_new_tx, {from, fee, cid1, cid2, type1, type2}).

make_dict(From, CID1, Type1, CID2, Type2, Fee) ->
    #market_new_tx{from = From,
                   fee = Fee,
                   cid1 = CID1,
                   type1 = Type1,
                   cid2 = CID2, 
                   type2 = Type2}.

go(Tx, Dict, NewHeight, _) ->
    #market_new_tx{
    from = From,
    fee = Fee,
    cid1 = CID1,
    type1 = Type1,
    cid2 = CID2,
    type2 = Type2} = Tx,
    true = NewHeight > forks:get(34),
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    Market = markets:new(CID1, Type1, CID2, Type2),
    MID = markets:make_id(Market),
    empty = markets:dict_get(MID, Dict2),
    markets:dict_write(Market, Dict2).
        
