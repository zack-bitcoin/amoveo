-module(burn).
-export([test/0, new/1, get/2, write/2, address/1, 
	 amount/1, root_hash/1, serialize/1]).
%The proof of burn tree stores by address. It stores the number of AE tokens that this address has burned.
-record(burn, {address, amount = 0}).
-define(name, burn).
address(B) -> B#burn.address.
amount(B) -> B#burn.amount.
new(Address) -> #burn{address = Address}.
serialize(E) ->
    HS = constants:hash_size(),
    BAL = constants:balance_bits(),
    A = testnet_sign:address2binary(E#burn.address),
    HS = size(A),
    <<(E#burn.amount):BAL,
      A/binary>>.
deserialize(B) ->
    HS = constants:hash_size() * 8,
    BAL = constants:balance_bits(),
    <<I:BAL, A:HS>> = B,
    #burn{address = testnet_sign:binary2address(<<A:HS>>),
	  amount = I}.
get(Address, Tree) ->
    B = testnet_sign:address2binary(Address),
    Key = existence:hash2int(B),
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
write(A, Tree) ->
    Address = A#burn.address,
    Binary = testnet_sign:address2binary(Address),
    Key = existence:hash2int(Binary),
    X = serialize(A), 
    trie:put(Key, X, 0, Tree, ?name).
root_hash(Root) ->
    trie:root_hash(?name, Root).
    
test() ->
    Address = constants:master_address(),
    C = new(Address),
    {_, empty, _} = get(Address, 0),
    NewLoc = write(C, 0),
    {_, C, _} = get(Address, NewLoc),
    {_, empty, _} = get(Address, 0),
    success.
    
    
    
