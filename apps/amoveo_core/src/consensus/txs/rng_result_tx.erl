-module(rng_result_tx).
-export([go/4, make_dict/6]).
-include("../../records.hrl").
%-record(rng_result_tx, {pubkey, nonce, fee, id, sortition_id, challenge_id, hashes}).

make_dict(Creator, ID, SID, CID, Hashes, Fee) ->
    Acc = trees:get(accounts, Creator),
    #rng_result_tx{pubkey = Creator, 
                   nonce = Acc#acc.nonce + 1, 
                   id = ID, 
                   fee = Fee,
                   sortition_id = SID,
                   challenge_id = CID,
                   hashes = Hashes
                  }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    %From = Tx#rng_result_tx.pubkey,
    %Fee = Tx#rng_result_tx.fee,
    #rng_result_tx{nonce = Nonce,
                   fee = Fee,
                   pubkey = From,
                   sortition_id = SID,
                   challenge_id = CID,
                   id = ID
                  } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    empty = rng_response:dict_get(ID, Dict),
    #sortition{} = sortition:dict_get(SID, Dict),
    case CID of
        <<0:256>> -> ok;
        _ -> #rng_challenge{} = rng_challenge:dict_get(CID, Dict)
    end,
    R = rng_response:new(),
    rng_response:dict_write(R, Dict2).
