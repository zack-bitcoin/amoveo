-module(rng_challenge).
-export([new/9,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/4, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0
]).
-define(id, rng_challenge).
-include("../../records.hrl").

new(ID, PID, RID, Pub, Time, N, Start, End, Many) ->
    #rng_challenge{
     id = ID,
     result_id = RID,
     parent_id = PID,
     pubkey = Pub,
     hashes = <<0:256>>,
     start_hash = Start,
     end_hash = End,
     many = Many,
     timestamp = Time,
     refunded = 0,
     n = N
    }.

id(X) -> X#rng_challenge.id.
result_id(X) -> X#rng_challenge.result_id.
parent_id(X) -> X#rng_challenge.parent_id.
pubkey(X) -> X#rng_challenge.pubkey.
hashes(X) -> X#rng_challenge.hashes.
timestamp(X) -> X#rng_challenge.timestamp.
refunded(X) -> X#rng_challenge.refunded.
n(X) -> X#rng_challenge.n.
         

write(C, Root) ->
    Key = C#rng_challenge.id,
    SC = serialize(C),
    ID = key_to_int(Key),
    trie:put(ID, SC, 0, Root, ?id).

get(Key, Candidate) ->
    ID = key_to_int(Key),
    {RH, Leaf, Proof} = trie:get(ID, Candidate, ?id),
    S = case Leaf of
            empty -> empty;
            Leaf ->
                deserialize(leaf:value(Leaf))
        end,
    {RH, S, Proof}.

delete(Key, Sortition) ->
    ID = key_to_int(Key),
    trie:delete(ID, Sortition, ?id).

dict_update(R, Time, Refunded, Hashes) ->
    R#rng_challenge{timestamp = Time, 
                    refunded = Refunded,
                    hashes = Hashes}.

dict_delete(Key, Dict) ->
    dict:store({rng_challenge, Key}, 0, Dict).

dict_write(C, Dict) ->
    K = id(C),
    dict:store({rng_challenge, K},
               serialize(C), 
               Dict).

dict_get(Key, Dict) ->
    case dict:find({rng_challenge, Key}, Dict) of
	error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.

verify_proof(RootHash, Key, Value, Proof) ->

    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).

key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.

deserialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    HEI = constants:height_bits(),
    <<
      ID:HS,
      RID:HS,
      PID:HS,
      Pub:PS,
      Hashes:HS,
      StartHash:HS,
      EndHash:HS,
      T:HEI,
      Refund:1,
      N:7,
      Many:16
    >> = B,
    #rng_challenge{
           id = <<ID:HS>>,
           result_id = <<RID:HS>>,
           parent_id = <<PID:HS>>,
           pubkey = <<Pub:PS>>,
           hashes = <<Hashes:HS>>,
           start_hash = <<StartHash:HS>>,
           end_hash = <<EndHash:HS>>,
           timestamp = T,
           refunded = Refund,
           n = N,
           many = Many
          }.

serialize(R) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    HEI = constants:height_bits(),
    ID = R#rng_challenge.id,
    RID = R#rng_challenge.result_id,
    PID = R#rng_challenge.parent_id,
    Pub = R#rng_challenge.pubkey,
    Hashes = R#rng_challenge.hashes,
    StartHash = R#rng_challenge.start_hash,
    EndHash = R#rng_challenge.end_hash,
    HS = size(ID),
    HS = size(RID),
    HS = size(PID),
    PS = size(Pub),
    HS = size(Hashes),
    HS = size(StartHash),
    HS = size(EndHash),
%-record(rng_challenge, {id, result_id, parent_id, pubkey, hashes, timestamp, refunded, n}).
    <<
      ID/binary,
      RID/binary,
      PID/binary,
      Pub/binary,
      Hashes/binary,
      StartHash/binary,
      EndHash/binary,
      (R#rng_challenge.timestamp):HEI,
      (R#rng_challenge.refunded):1,
      (R#rng_challenge.n):7,
      (R#rng_challenge.many):16
    >>.

all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Sortition = trees:?MODULE(Trees),
    All = trie:get_all(Sortition, ?MODULE),
    lists:map(fun(X) ->
                      deserialize(leaf:value(X))
              end, All).

test() ->
    %make a new.
    {Pub, _Priv} = testnet_sign:new_key(),
    ID = hash:doit(1),
    PID = hash:doit(2),
    RID = hash:doit(3),
    SH = hash:doit(4),
    EH = hash:doit(5),
    <<Many:16>> = <<2:6, 100:10>>,
    S = new(ID, PID, RID, Pub, 200, 3, SH, EH, Many),
    S1 = deserialize(serialize(S)),
    S = S1,
    Root0 = trees:empty_tree(rng_challenge),
    NewLoc = write(S, Root0),
    {Root, S, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(S), Proof),
    {Root2, empty, Proof2} = get(ID, Root0),
    true = verify_proof(Root2, ID, 0, Proof2),
    success.
