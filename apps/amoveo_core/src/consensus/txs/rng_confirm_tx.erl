-module(rng_confirm_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
%-record(rng_confirm_tx, {pubkey, nonce, fee, sortition_id, result_id}).

make_dict(Creator, SID, RID, Fee) ->
    Acc = trees:get(accounts, Creator),
    #rng_confirm_tx{pubkey = Creator, nonce = Acc#acc.nonce + 1, sortition_id = SID, result_id = RID, fee = Fee}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #rng_confirm_tx{
    pubkey = From,
    nonce = Nonce,
    fee = Fee,
    sortition_id = SID,
    result_id = RID
   } = Tx,
    Reward = governance:dict_get_value(rng_result_tx, Dict),
    A2 = accounts:dict_update(From, Dict, Reward-Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),
    RR = rng_result:dict_get(RID, Dict2),
    #rng_result{
                 value = RNGV,
                 impossible = 0
               } = RR,
    false = (RNGV == <<0:256>>),
    Dict3 = rng_result:dict_delete(RID, Dict2),
    S = sortition:dict_get(SID, Dict3),
    #sortition{
                rng_end = RE,
                top_rng = RID
              } = S,
    true = RE < NewHeight,
    S2 = S#sortition{rng_value = RNGV},
    sortition:dict_write(S2, Dict3).
    
