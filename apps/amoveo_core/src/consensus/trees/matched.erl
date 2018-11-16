-module(matched).
-export([new/4,
         get/2,write/2,%update tree stuff
         dict_get/2, dict_write/2,%update dict stuff
         verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,test/0]).%common tree stuff
%for accessing the proof of existence tree
-record(matched, {account, %pubkey of the account
		  oracle, %oracle id
		  true = 0, 
		  false = 0, 
		  bad = 0}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle

-define(name, matched).
-include("../../records.hrl").
account(X) -> X#matched.account.
oracle(X) -> X#matched.oracle.
true(X) -> X#matched.true.
false(X) -> X#matched.false.
bad(X) -> X#matched.bad.
new(Account, Oracle, Type, Amount) ->
    HS = constants:hash_size(),
    HS = size(oracle),
    AS = constants:pubkey_size(),
    AS = size(Account),
    {T, F, B} = 
	case Type of
	    1 -> {Amount, 0, 0};
	    2 -> {0, Amount, 0};
	    3 -> {0, 0, Amount}
	end, 
    #matched{account = Account, oracle = Oracle,
	    true = T, false = F, bad = B}.







serialize(E) -> ok.
deserialize(B) -> ok.
dict_get(Hash, Dict) ->
    true = is_binary(Hash),
    X = dict:find({matched, Hash}, Dict),
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
    Hash = ok,
    dict:store({matched, Hash},
               serialize(C),
               Dict).
write(E, Tree) ->
    Hash = ok,
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
    C = new(Hash, Height, 0, 0),
    Root0 = constants:root0(),
    %C = hash:doit(2),
    {_, empty, _} = get(Hash, Root0),
    NewLoc = write(C, Root0),
    C2 = new(hash:doit(4), Height, 0, 0),
    NewLoc2 = write(C2, NewLoc),
    {Root1, C, Path1} = get(Hash, NewLoc2),
    {Root2, empty, Path2} = get(Hash, Root0),
    true = verify_proof(Root1, Hash, serialize(C), Path1),
    true = verify_proof(Root2, Hash, 0, Path2),
    success.
