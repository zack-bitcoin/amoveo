-module(burn).
-export([test/0, new/1, get/2, write/2, address/1, 
	 amount/1, root_hash/1, serialize/1, key_to_int/1,
	 verify_proof/4, make_leaf/3]).
%The proof of burn tree stores by address. It stores the number of AE tokens that this address has burned.
-record(burn, {address, amount = 0}).
-define(name, burn).
address(B) -> B#burn.address.
amount(B) -> B#burn.amount.
new(Address) -> #burn{address = Address}.
serialize(E) ->
    HS = constants:hash_size(),
    BAL = constants:balance_bits(),
    %A = testnet_sign:address2binary(E#burn.address),
    A = E#burn.address,
    HS = size(A),
    <<(E#burn.amount):BAL,
      A/binary>>.
deserialize(B) ->
    HS = constants:hash_size() * 8,
    BAL = constants:balance_bits(),
    <<I:BAL, A:HS>> = B,
    #burn{address = <<A:HS>>,
	  amount = I}.
key_to_int(X) ->
    existence:hash2int(X).
get(B, Tree) ->
    %B = testnet_sign:address2binary(Address),
    Key = existence:hash2int(B),
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
write(A, Tree) ->
    Address = A#burn.address,
    %Binary = testnet_sign:address2binary(Address),
    Key = existence:hash2int(Address),
    X = serialize(A), 
    trie:put(Key, X, 0, Tree, ?name).
root_hash(Root) ->
    trie:root_hash(?name, Root).
make_leaf(Key, V, CFG) ->
    leaf:new(trees:hash2int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
    
test() ->
    X = testnet_hasher:doit(<<>>),
    %Address = constants:master_address(),
    C = new(X),
    Root0 = constants:root0(),
    {_, empty, _} = get(X, Root0),
    NewLoc = write(C, Root0),
    {Root1, C, Path1} = get(X, NewLoc),
    {Root2, empty, Path2} = get(X, Root0),
    true = verify_proof(Root1, X, serialize(C), Path1),
    true = verify_proof(Root2, X, 0, Path2),
    success.
    
    
    
