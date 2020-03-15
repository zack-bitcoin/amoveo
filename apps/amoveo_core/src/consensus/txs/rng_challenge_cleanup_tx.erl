-module(rng_challenge_cleanup_tx).
-export([go/4, make_dict/2]).
-include("../../records.hrl").


make_dict(From, CID) ->
    Acc = trees:get(accounts, From),
    Challenge = trees:get(rng_challenge, CID),
    SID = Challenge#rng_challenge.sortition_id,
    #rng_challenge_cleanup_tx{from = From, nonce = Acc#acc.nonce + 1, challenge_id = CID, sortition_id = SID, fee = 0}.

go(Tx, Dict, NewHeight, _) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #rng_challenge_cleanup_tx{
    from = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = 0,
    challenge_id = CID
   } = Tx,
    RC = rng_challenge:dict_get(CID, Dict),
    #rng_challenge{
                    sortition_id = SID
                  } = RC,
    S = sortition:dict_get(SID, Dict),
    #sortition{
                closed = 1
              } = S,
    Dict2 = rng_challenge:dict_delete(CID, Dict).
    %A2 = accounts:dict_update(From, Dict2, 0, Nonce),
    %accounts:dict_write(A2, Dict2).
    
