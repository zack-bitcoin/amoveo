-module(rng_refute_tx).
-export([go/4, make_dict/9]).
-include("../../records.hrl").

make_dict(Creator, SID, CID, RID, N, Proof, SH, EH, Fee) ->
    Acc = trees:get(accounts, Creator),
    #rng_refute_tx{pubkey = Creator, nonce = Acc#acc.nonce + 1, fee = Fee, sortition_id = SID, challenge_id = CID, result_id = RID, n = N, proof = Proof, start_hash = SH, end_hash = EH}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #rng_refute_tx{
    pubkey = From,
    nonce = Nonce,
    fee = Fee,
    sortition_id = SID,
    challenge_id = CID,
    result_id = RID,
    n = N,
    start_hash = StartHash,
    end_hash = EndHash,
    proof = Proof
   } = Tx,
    %A = accounts:dict_get(From, Dict),
    S = sortition:dict_get(SID, Dict),
    RC = rng_challenge:dict_get(CID, Dict),
    RR = rng_result:dict_get(RID, Dict),
    #rng_challenge{
                    %start_hash = SH,
                    %end_hash = EH,
                    hashes = Hashes,
                    many = Many,
                    result_id = RID,
                    timestamp = T
                  } = RC,
    #sortition{
                top_rng = RID,%only allow refuting the top priority RNG possibility.
                bottom_rng = Bottom,
                rng_response_delay = RRD
              } = S,
    #rng_result{
                 next_result = NextR
               } = RR,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict1_2 = accounts:dict_write(A2, Dict),
    true = 
        if
            (N < 0) -> false;
            (N < 129) ->
                Size = 64,
                KeyLength = 2,
                M = mtree:new_empty(KeyLength, Size, 0),
                CFG = mtree:cfg(M),
                HashPair = <<StartHash/binary, EndHash/binary>>,
                Leaf = leaf:new(0, HashPair, 0, CFG),
    %io:fwrite(packer:pack({Hashes, Leaf, Proof})),
    %io:fwrite("\n"),
                true = verify:proof(Hashes, Leaf, Proof, CFG),
                <<1:9, Mantissa:7>> = <<Many:16>>,%check that the gap is small enough
                not(hash_match(StartHash, EndHash, Mantissa)); %check that the hashes don't match
            (N > 128) -> %if the challenge didn't get a response for too long a period of time.
                (NewHeight - T) > RRD
    end, 
    RR2 = rng_result:dict_update(RR, 0, 1, <<0:256>>),
    Dict2 = rng_result:dict_write(RR2, Dict1_2),
    NewBottom = 
        if
            Bottom == RID -> <<0:256>>;
            true -> Bottom
        end,
    S2 = sortition:dict_update(S, NewHeight, 0, NextR, Bottom),%remove rng_result from the queue of potential results.
    %maybe we should delete it too?
    %io:fwrite(packer:pack(S2)),
    sortition:dict_write(S2, Dict2).
    

hash_match(A, A, 0) -> true;
hash_match(_, _, 0) -> false;
hash_match(S, E, N) ->
    hash_match(hash:doit(S), E, N-1).
