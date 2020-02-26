-module(rng_challenge_tx).
-export([go/4, make_dict/10]).
-include("../../records.hrl").
%-record(rng_challenge_tx, {pubkey, nonce, fee, id, sortition_id, parent_id, n}).
%-record(rng_challenge, {id, result_id, parent_id, pubkey, hashes, timestamp, refunded, n}).

make_dict(From, ID, SID, PID, PT, N, StartHash, EndHash, Proof, Fee) ->
    Acc = trees:get(accounts, From),
    #rng_challenge_tx{pubkey = From, 
                      nonce = Acc#acc.nonce + 1, 
                      fee = Fee,
                      id = ID,
                      sortition_id = SID,
                      parent_id = PID,
                      parent_type = PT,
                      start_hash = StartHash,
                      end_hash = EndHash,
                      proof = Proof,
                      n = N
                     }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #rng_challenge_tx{nonce = Nonce,
                      fee = Fee,
                      pubkey = From,
                      sortition_id = SID,
                      parent_id = PID,
                      parent_type = PT,
                      start_hash = StartHash,
                      end_hash = EndHash,
                      id = ID,
                      proof = Proof,
                      n = N
                     } = Tx,
    32 = size(StartHash),
    32 = size(EndHash),
    Type = case PT of
               0 -> rng_result;
               1 -> rng_challenge
           end,
    empty = rng_challenge:dict_get(ID, Dict),
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                rng_value = <<0:256>>%verifies that this sortition has not decided on the rng yet
     } = S,
    X = Type:dict_get(PID, Dict2),
    Hashes = Type:hashes(X),
    Size = 64,
    KeyLength = 2,
    M = mtree:new_empty(KeyLength, Size, 0),
    CFG = mtree:cfg(M),
    HashPair = <<StartHash/binary, EndHash/binary>>,
    Leaf = leaf:new(0, HashPair, 0, CFG),
    true = verify:proof(Hashes, Leaf, Proof, CFG),
    {RID, Many} = 
        case Type of
            rng_result -> 
                GM = governance:dict_get_value(rng_many, Dict2),
                {PID, GM};
            rng_challenge ->
                RC = rng_challenge:dict_get(PID, Dict2),
                #rng_challenge{result_id = R_0,
                              hashes = H_0,
                              many = ManyLeft} = RC,
                false = (<<0:256>> == H_0),%a result needs to have been provided that can be challenged.
                {R_0, ManyLeft}
        end,
    <<Radix:9, Mantissa:7>> = <<Many:16>>,
    <<Many2:16>> = <<(Radix-1):9, Mantissa:7>>,
    false = (Radix == 1), %if radix is 0, handle with rng_cleanup
    Result = rng_result:dict_get(RID, Dict2),
    %io:fwrite(Result),
    SID = Result#rng_result.sortition_id,
    <<HashStart:256, HashEnd:256>> = HashPair,
    NRC = rng_challenge:new(ID, PID, RID, From, NewHeight, N, <<HashStart:256>>, <<HashEnd:256>>, Many2, SID),
    rng_challenge:dict_write(NRC, Dict2).
