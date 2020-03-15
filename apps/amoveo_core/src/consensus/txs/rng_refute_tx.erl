-module(rng_refute_tx).
-export([go/4, make_dict/8]).
-include("../../records.hrl").

make_dict(Creator, SID, CID, RID, Proof, SH, EH, Fee) ->
    Acc = trees:get(accounts, Creator),
    #rng_refute_tx{pubkey = Creator, nonce = Acc#acc.nonce + 1, fee = Fee, sortition_id = SID, challenge_id = CID, result_id = RID, proof = Proof, start_hash = SH, end_hash = EH}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #rng_refute_tx{
    pubkey = From,
    nonce = Nonce,
    fee = Fee,
    sortition_id = SID,
    challenge_id = CID,
    result_id = RID,
    start_hash = StartHash,
    end_hash = EndHash,
    proof = Proof
   } = Tx,
    S = sortition:dict_get(SID, Dict),
    RC = rng_challenge:dict_get(CID, Dict),
    RR = rng_result:dict_get(RID, Dict),
    #rng_challenge{
                    hashes = Hashes,
                    many = Many,
                    result_id = RID,
                    timestamp = T
                  } = RC,
    #sortition{
                top_rng = RID,%only allow refuting the top priority RNG possibility.
                bottom_rng = Bottom,
                rng_response_delay = RRD,
                closed = 0
              } = S,
    #rng_result{
                 next_result = NextR
               } = RR,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict1_2 = accounts:dict_write(A2, Dict),
    true = (NewHeight - T) > RRD,
    RR2 = rng_result:dict_update(RR, 0, 1, <<0:256>>),
    Dict2 = rng_result:dict_write(RR2, Dict1_2),
    NewBottom = 
        if
            Bottom == RID -> <<0:256>>;
            true -> Bottom
        end,
    S2 = sortition:dict_update(S, NewHeight, 0, NextR, NewBottom),%remove rng_result from the queue of potential results.
    %maybe we should delete it too?
    %io:fwrite(packer:pack(S2)),
    sortition:dict_write(S2, Dict2).
    

