-module(existence).
-export([new/2,hash/1, %custom stuff
         get/2,write/2,%update tree stuff
         dict_get/2, dict_write/2,%update dict stuff
         verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,test/0]).%common tree stuff
%for accessing the proof of existence tree
-record(exist, {hash, height}).
-define(name, existence).
-include("../../records.hrl").
hash(X) -> X#exist.hash.
new(Hash, Height) ->
    HS = constants:hash_size(),
    HS = size(Hash),
    #exist{hash = Hash, height = Height}.
serialize(E) ->
    HB = constants:height_bits(),
    Hash = E#exist.hash,
    HS = size(Hash),
    HS = constants:hash_size(),
    <<(E#exist.height):HB,
     Hash/binary>>.
deserialize(B) ->
    HS = constants:hash_size()*8,
    HB = constants:height_bits(),
    <<Height:HB,
     Hash:HS>> = B,
    #exist{hash = <<Hash:HS>>, height = Height}.
dict_get(Hash, Dict) ->
    true = is_binary(Hash),
    X = dict:find({existence, Hash}, Dict),
    case X of
	error -> empty;
        {ok, 0} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
key_to_int(X) ->
    <<Y:256>> = hash:doit(X),
    Y.
get(Hash, Tree) ->
    true = is_binary(Hash),
    Key = key_to_int(Hash),
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> 
		Y = leaf:value(L),
		deserialize(Y)
	end,
    {X, V, Proof}.
dict_write(C, Dict) ->
    Hash = C#exist.hash,
    dict:store({existence, Hash},
               serialize(C),
               Dict).
write(E, Tree) ->
    Hash = E#exist.hash,
    Key = key_to_int(Hash),
    X = serialize(E),
    trie:put(Key, X, 0, Tree, ?name).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
test() ->
    Height = (tx_pool:get())#tx_pool.height,
    Hash = hash:doit(2),
    C = new(Hash, Height),
    Root0 = constants:root0(),
    %C = hash:doit(2),
    {_, empty, _} = get(Hash, Root0),
    NewLoc = write(C, Root0),
    C2 = new(hash:doit(4), Height),
    NewLoc2 = write(C2, NewLoc),
    {Root1, C, Path1} = get(Hash, NewLoc2),
    {Root2, empty, Path2} = get(Hash, Root0),
    true = verify_proof(Root1, Hash, serialize(C), Path1),
    true = verify_proof(Root2, Hash, 0, Path2),
    success.
