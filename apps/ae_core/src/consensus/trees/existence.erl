-module(existence).
-export([get/2,write/2,new/2,hash2int/1,root_hash/1,hash/1, 
	 serialize/1, verify_proof/4, dict_get/2, dict_write/2,
         make_leaf/3, key_to_int/1,
	 test/0]).
%for accessing the proof of existence tree
-record(exist, {hash, height}).
-define(name, existence).
height(X) ->
    X#exist.height.
hash(X) ->
    X#exist.hash.
new(Hash, Height) ->
    HS = constants:hash_size(),
    HS = size(Hash),
    #exist{hash = Hash, height = Height}.
serialize(E) ->
    HS = constants:hash_size(),
    HB = constants:height_bits(),
    Hash = E#exist.hash,
    HS = size(Hash),
    io:fwrite(packer:pack(E#exist.height)),
    io:fwrite("\n"),
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
    X = dict:fetch({existence, Hash}, Dict),
    case X of
        0 -> empty;
        _ -> deserialize(X)
    end.
key_to_int(X) ->
    <<Y:256>> = hash:doit(X),
    Y.
get(Hash, Tree) ->
    %io:fwrite(Hash),
    true = is_binary(Hash),
    Key = key_to_int(Hash),
    io:fwrite("get key"),
    io:fwrite(packer:pack(<<Key:256>>)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(Hash)),
    io:fwrite("\n"),
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
    io:fwrite("write key is "),
    io:fwrite(packer:pack(<<Key:256>>)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(Hash)),
    io:fwrite("\n"),
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
    leaf:new(key_to_int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).


test() ->
    {_, Height, _} = tx_pool:data(),
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
