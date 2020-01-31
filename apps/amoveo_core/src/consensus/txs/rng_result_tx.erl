-module(rng_result_tx).
-export([go/4, make_dict/5]).
-include("../../records.hrl").
make_dict(Creator, ID, SID, Hashes, Fee) ->
    Acc = trees:get(accounts, Creator),
    #rng_result_tx{
                    pubkey = Creator, 
                    nonce = Acc#acc.nonce + 1, 
                    fee = Fee,
                    id = ID, 
                    sortition_id = SID,
                    hashes = Hashes
                  }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #rng_result_tx{nonce = Nonce,
                   fee = Fee,
                   pubkey = From,
                   sortition_id = SID,
                   hashes = Hashes,
                   id = ID
                  } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    empty = rng_result:dict_get(ID, Dict),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
      closed = 0,
      top_rng = TRNG,
      bottom_rng = BRNG
     } = S,
    HashesRoot = merklize(Hashes),%TODO
    NewR = rng_result:new(ID, SID, From, HashesRoot),
    Dict3 = rng_result:dict_write(NewR, Dict2),
    {S2, Dict4} = 
        case BRNG of
            <<0:256>> ->
                {S#sortition{top_rng = ID},
                 Dict3};
             _ ->
                OldR = rng_result:dict_get(BRNG, Dict3),
                #rng_result{
                  confirmed = 0,
                  impossible = 0
                 } = OldR,
                OldR2 = rng_result:dict_update(OldR, 0, 0, ID),
                Dict5 = rng_result:dict_write(OldR2, Dict3),
                {S, Dict5}
            end,
    S3 = S2#sortition{
           bottom_rng = ID
          },
    sortition:dict_write(S3, Dict4).
        
merklize(Checkpoints) ->
    %every pair of adjacent checkpoints should be stored.
    %store under the integers 0-127
    Size = 64,
    KeyLength = 2,
    M = mtree:new_empty(KeyLength, Size, 0),
    CFG = mtree:cfg(M),
    L = merklize_make_leaves(0, Checkpoints, CFG),
    {Root, M2} = mtree:store_batch(L, 1, M),
    mtree:root_hash(Root, M2).
merklize_make_leaves(_, [X], _) -> [];
merklize_make_leaves(N, [H1|[H2|T]], CFG) -> 
    Leaf = leaf:new(N, <<H1/binary, H2/binary>>, 0, CFG),
    [Leaf|merklize_make_leaves(N+1, [H2|T], CFG)].
