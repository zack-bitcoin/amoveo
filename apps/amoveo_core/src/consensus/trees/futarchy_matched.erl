-module(futarchy_matched).
-export([key_to_int/1, dict_get/2,
         dict_write/2, dict_write/3, dict_get/3,
         maker_id/1, maker_id/2, maker_id/4, taker_id/3,
         maker_id_with_tx_pool/1
        ]).

-include("../../records.hrl").
key_to_int(#futarchy_matched{id = <<X:256>>}) -> X;
key_to_int(<<X:256>>) -> X.
%make_id(Pub, Salt) ->
%    <<_:256>> = Salt,
%    Pub2 = case size(Pub) of
%               33 -> Pub;
%               65 -> trees2:compress_pub(Pub)
%           end,
%    B = <<0, Pub2/binary, Salt/binary>>,
%    hash:doit(B).
    
dict_get(ID, Dict, _) ->
    dict_get(ID, Dict).
dict_get(ID, Dict) ->
    case csc:read({?MODULE, ID}, Dict) of
        error -> error;
        {empty, _, _} -> empty;
        {ok, ?MODULE, Val} -> Val
    end.
dict_write(Job, Dict) ->
    dict_write(Job, 0, Dict).
dict_write(Job, _Meta, Dict) ->
    #futarchy_matched{
            id = ID, win_amount = WA, revert_amount = RA
                     } = Job,
    csc:update({?MODULE, ID}, Job, Dict).

taker_id(FID, TxNonce, Owner) ->
    true = is_integer(TxNonce),
    true = TxNonce > 0,
    <<_:256>> = FID,
    Pub = trees2:compress_pub(Owner),
    hash:doit(<<3,7,3,8,2,4,5,TxNonce:32,
                FID/binary, Pub/binary>>).
maker_id_with_tx_pool(TID) ->
    FU = trees:get(futarchy_unmatched, TID),
    maker_id2(FU).
maker_id(TID) ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    FU = trees:get(futarchy_unmatched, TID, dict:new(), Trees),
    maker_id2(FU).
maker_id(TID, Dict) ->
    FU = futarchy_unmatched:dict_get(TID, Dict),
    maker_id2(FU).

maker_id2(FU) ->
    #futarchy_unmatched
        {
          owner = Owner,
          futarchy_id = FID,
          nonce = FUNonce,
          id = TID
        } = FU,
    maker_id(FID, FUNonce, TID, Owner).
    
    

maker_id(FID, Nonce, UnmatchedID, Owner) ->
    %this nonce is from inside the futarchy_unmatched element.
    %makers use the id of their own unmatched trade here.
    %takers use the tx nonce.
    true = is_integer(Nonce),
    <<_:256>> = FID,
    <<_:256>> = UnmatchedID,
    true = Nonce >= 0,
    Pub = trees2:compress_pub(Owner),
    hash:doit(<<3,7,3,8,2,4,5,Nonce:32, FID/binary, 
                UnmatchedID/binary, Pub/binary>>).


