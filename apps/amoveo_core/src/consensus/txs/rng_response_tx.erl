-module(rng_response_tx).
-export([go/4, make_dict/6]).
-include("../../records.hrl").

make_dict(Creator, ID, SID, RID, Hashes, Fee) ->
    Acc = trees:get(accounts, Creator),
    %RC = trees:get(rng_challenge, ID),
    #rng_response_tx{pubkey = Creator, 
                     nonce = Acc#acc.nonce + 1, 
                     id = ID, 
                     fee = Fee,
                     sortition_id = SID,
                     result_id = RID,%RC#rng_challenge.result_id,
                     hashes = Hashes
                    }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #rng_response_tx{nonce = Nonce,
                     fee = Fee,
                     pubkey = From,
                     sortition_id = SID,
                     id = ID,
                     hashes = Hashes,
                     result_id = RID
                  } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    RC = rng_challenge:dict_get(ID, Dict2),
    #rng_challenge{
                    result_id = RID,
                    hashes = OldHashesRoot,
                    start_hash = StartHash,
                    end_hash = EndHash,
                    many = Many
                  } = RC,
    true = (<<0:256>> == OldHashesRoot),%they didn't already respond
    Result = rng_result:dict_get(RID, Dict2),
    #rng_result{
                 sortition_id = SID,
                 impossible = 0,
                 pubkey = From%verifies that they have permission to make this response.
               } = Result,
    S = sortition:dict_get(SID, Dict),
    #sortition{
                rng_value = <<0:256>>%verifies that this sortition has not decided on the rng yet.
              } = S,
    [StartHash|_] = Hashes,
    [EndHash|_] = lists:reverse(Hashes),
    {HashesRoot, _, _} = rng_result_tx:merklize(Hashes),
    RC2 = rng_challenge:dict_update(RC, NewHeight, 0, HashesRoot),
    rng_challenge:dict_write(RC2, Dict2).
    
    
    
