-module(rng_result).
-export([new/5,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/4, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0,
         hashes/1
]).
-define(id, rng_result).
-include("../../records.hrl").


new(ID, SID, Pub, Hashes, Value) ->
    #rng_result{
     id = ID,
     sortition_id = SID,
     pubkey = Pub,
     hashes = Hashes,
     value = Value,
     next_result = <<0:256>>,
     impossible = 0, 
     confirmed = 0
    }.

id(X) -> X#rng_result.id.
sortition_id(X) -> X#rng_result.sortition_id.
%challenge_id(X) -> X#rng_result.challenge_id.
pubkey(X) -> X#rng_result.pubkey.
%timestamp(X) -> X#rng_result.timestamp.
%refunded(X) -> X#rng_result.refunded.
possible(X) -> X#rng_result.impossible.
confirmed(X) -> X#rng_result.confirmed.
hashes(X) -> X#rng_result.hashes.


write(C, Root) ->
    Key = C#rng_result.id,
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

dict_update(R, Confirmed, Impossible, NR) ->
    R#rng_result{confirmed = Confirmed,
                 impossible = Impossible,
                 next_result = NR}.

dict_delete(Key, Dict) ->
    dict:store({rng_result, Key}, 0, Dict).

dict_write(C, Dict) ->
    K = id(C),
    dict:store({rng_result, K},
               serialize(C), 
               Dict).

dict_get(Key, Dict) ->
    case dict:find({rng_result, Key}, Dict) of
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
      SID:HS,
      Pub:PS,
      Hashes:HS,
      Value:HS,
      NextResult:HS,
      Possible:1,
      Confirmed:1,
      0:6
    >> = B,
    #rng_result{
           id = <<ID:HS>>,
           sortition_id = <<SID:HS>>,
           pubkey = <<Pub:PS>>,
           hashes = <<Hashes:HS>>,
           value = <<Value:HS>>,
           next_result = <<NextResult:HS>>,
           impossible = Possible,
           confirmed = Confirmed
          }.

%-record(rng_result, {id, sortition_id, pubkey, hashes, impossible, confirmed, next_result}).
serialize(R) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    HEI = constants:height_bits(),
    ID = R#rng_result.id,
    SID = R#rng_result.sortition_id,
    Pub = R#rng_result.pubkey,
    Hashes = R#rng_result.hashes,
    Value = R#rng_result.value,
    NextResult = R#rng_result.next_result,
    HS = size(ID),
    HS = size(SID),
    HS = size(Hashes),
    HS = size(Value),
    HS = size(NextResult),
    PS = size(Pub),
    <<
      ID/binary,
      SID/binary,
      Pub/binary,
      Hashes/binary,
      Value/binary,
      NextResult/binary,
      (R#rng_result.impossible):1,
      (R#rng_result.confirmed):1,
      0:6
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
    SID = hash:doit(2),
    Hashes = hash:doit(4),
    Value = hash:doit(5),
    S = new(ID, SID, Pub, Hashes, Value),
    S1 = deserialize(serialize(S)),
    S = S1,
    Root0 = trees:empty_tree(rng_result),
    NewLoc = write(S, Root0),
    {Root, S, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(S), Proof),
    {Root2, empty, Proof2} = get(ID, Root0),
    true = verify_proof(Root2, ID, 0, Proof2),
    success.

