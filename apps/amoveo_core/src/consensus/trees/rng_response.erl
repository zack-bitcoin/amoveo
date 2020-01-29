-module(rng_response).
-export([new/8,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/4, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0
]).
-define(id, rng_response).
-include("../../records.hrl").
%-record(rng_response, {id, sortition_id, challenge_id, pubkey, timestamp, refunded, possible, hashes}).


new(ID, SID, CID, Pub, T, Refunded, Possible, Hashes) ->
    #rng_response{
     id = ID,
     sortition_id = SID,
     challenge_id = CID,
     pubkey = Pub,
     timestamp = T,
     refunded = Refunded,
     impossible = Possible, 
     hashes = Hashes
    }.

id(X) -> X#rng_response.id.
sortition_id(X) -> X#rng_response.sortition_id.
challenge_id(X) -> X#rng_response.challenge_id.
pubkey(X) -> X#rng_response.pubkey.
timestamp(X) -> X#rng_response.timestamp.
refunded(X) -> X#rng_response.refunded.
possible(X) -> X#rng_response.impossible.
hashes(X) -> X#rng_response.hashes.


write(C, Root) ->
    Key = C#rng_response.id,
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

dict_update(R, Time, Refunded, Possible) ->
    R#rng_response{timestamp = Time, 
                    refunded = Refunded,
                  impossible = Possible}.

dict_delete(Key, Dict) ->
    dict:store({rng_response, Key}, 0, Dict).

dict_write(C, Dict) ->
    K = id(C),
    dict:store({rng_response, K},
               serialize(C), 
               Dict).

dict_get(Key, Dict) ->
    case dict:find({rng_response, Key}, Dict) of
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
      CID:HS,
      Pub:PS,
      T:HEI,
      Refund:1,
      Possible:1,
      0:6,
      Hashes:HS
    >> = B,
    #rng_response{
           id = <<ID:HS>>,
           sortition_id = <<SID:HS>>,
           challenge_id = <<CID:HS>>,
           pubkey = <<Pub:PS>>,
           timestamp = T,
           refunded = Refund,
           impossible = Possible,
           hashes = <<Hashes:HS>>
          }.

serialize(R) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    HEI = constants:height_bits(),
    ID = R#rng_response.id,
    SID = R#rng_response.sortition_id,
    CID = R#rng_response.challenge_id,
    Pub = R#rng_response.pubkey,
    Hashes = R#rng_response.hashes,
    HS = size(ID),
    HS = size(SID),
    HS = size(CID),
    HS = size(Hashes),
    PS = size(Pub),
    <<
      ID/binary,
      SID/binary,
      CID/binary,
      Pub/binary,
      (R#rng_response.timestamp):HEI,
      (R#rng_response.refunded):1,
      (R#rng_response.impossible):1,
      0:6,
      Hashes/binary
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
    RID = hash:doit(3),
    Hashes = hash:doit(4),
    S = new(ID, SID, RID, Pub, 200, 0, 0, Hashes),
    S1 = deserialize(serialize(S)),
    S = S1,
    Root0 = trees:empty_tree(rng_response),
    NewLoc = write(S, Root0),
    {Root, S, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(S), Proof),
    {Root2, empty, Proof2} = get(ID, Root0),
    true = verify_proof(Root2, ID, 0, Proof2),
    success.

