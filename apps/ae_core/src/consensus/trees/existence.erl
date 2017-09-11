-module(existence).
-export([get/2,write/2,new/1,hash2int/1,root_hash/1,hash/1, 
	 serialize/1, verify_proof/4, dict_get/2, dict_write/2,
         make_leaf/3, key_to_int/1,
	 test/0]).
%for accessing the proof of existence tree
-record(exist, {hash}).
-define(name, existence).
hash(X) ->
    X#exist.hash.
new(Hash) ->
    HS = constants:hash_size(),
    HS = size(Hash),
    #exist{hash = Hash}.
serialize(E) ->
    HS = constants:hash_size(),
    Hash = E#exist.hash,
    HS = size(Hash),
    Hash.
deserialize(B) ->
    HS = constants:hash_size()*8,
    <<Hash:HS>> = B,
    #exist{hash = <<Hash:HS>>}.

dict_get(Hash, Dict) ->
    true = is_binary(Hash),
    X = dict:fetch({existence, Hash}, Dict),
    case X of
        0 -> empty;
        _ -> deserialize(X)
    end.
key_to_int(X) ->
    hash2int(X).
get(Hash, Tree) ->
    %io:fwrite("existence get hash is "),
    %io:fwrite(Hash),
    %io:fwrite("\n"),
    true = is_binary(Hash),
    Key = hash2int(Hash),
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
    Key = hash2int(Hash),
    X = serialize(E),
    trie:put(Key, X, 0, Tree, ?name).
	     
hash2int(X) -> 
    S = size(X),
    S = constants:hash_size(),
    hash2int(X, 0).
hash2int(<<>>, N) -> N;
hash2int(<<X, Y/binary>>, N) ->
    M = (N*256) + X,
    hash2int(Y, M).
root_hash(Root) ->
    trie:root_hash(?name, Root).
make_leaf(Key, V, CFG) ->
    leaf:new(trees:hash2int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).


test() ->
    Hash = testnet_hasher:doit(2),
    C = new(Hash),
    Root0 = constants:root0(),
    %C = testnet_hasher:doit(2),
    {_, empty, _} = get(Hash, Root0),
    NewLoc = write(C, Root0),
    C2 = new(testnet_hasher:doit(4)),
    NewLoc2 = write(C2, NewLoc),
    {Root1, C, Path1} = get(Hash, NewLoc2),
    {Root2, empty, Path2} = get(Hash, Root0),
    true = verify_proof(Root1, Hash, serialize(C), Path1),
    true = verify_proof(Root2, Hash, 0, Path2),
    success.
