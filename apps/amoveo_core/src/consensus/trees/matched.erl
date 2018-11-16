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
    HS = size(Oracle),
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
serialize(X) -> 
    HS = constants:hash_size()*8,
    BAL = constants:balance_bits(),
    PS = constants:pubkey_size(),
    PS = size(X#matched.account),
    <<_:HS>> = X#matched.oracle,
    <<(X#matched.account)/binary,
     (X#matched.oracle)/binary,
     (X#matched.true):BAL,
     (X#matched.false):BAL,
     (X#matched.bad):BAL>>.
deserialize(B) -> 
    HS = constants:hash_size()*8,
    BAL = constants:balance_bits(),
    PS = constants:pubkey_size()*8,
    <<Acc:PS, Oracle:HS, True:BAL, False:BAL, Bad:BAL>> = B,
    #matched{true = True, false = False, bad = Bad, oracle = <<Oracle:HS>>, account = <<Acc:PS>>}.
dict_get({key, Account, Oracle}, Dict) ->
    true = is_binary(Account),
    true = is_binary(Oracle),
    HS = constants:hash_size(),
    HS = size(Oracle),
    PS = constants:pubkey_size(),
    PS = size(Account),
    X = dict:find({matched, {key, Account, Oracle}}, Dict),
    case X of
	error -> empty;
        {ok, 0} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
key_to_int({key, Account, Oracle}) ->
    <<Y:256>> = hash:doit(<<Account/binary, Oracle/binary>>),
    Y.
get(K, Tree) ->
    Key = key_to_int(K),
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> 
		Y = leaf:value(L),
		deserialize(Y)
	end,
    {X, V, Proof}.
dict_write(C, Dict) ->
    Account = C#matched.account,
    Oracle = C#matched.oracle,
    dict:store({matched, {key, Account, Oracle}},
               serialize(C),
               Dict).
write(E, Tree) ->
    K = {key, E#matched.account, E#matched.oracle},
    Key = key_to_int(K),
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
    C = new(keys:pubkey(), Hash, 1, 10000),
    Root0 = constants:root0(),
    %C = hash:doit(2),
    K = {key, keys:pubkey(), Hash},
    {_, empty, _} = get(K, Root0),
    NewLoc = write(C, Root0),
    C2 = new(keys:pubkey(), hash:doit(4), 1, 10200),
    K2 = {key, keys:pubkey(), hash:doit(4)},
    NewLoc2 = write(C2, NewLoc),
    {Root1, C2, Path1} = get(K2, NewLoc2),
    {Root2, empty, Path2} = get(K, Root0),
    true = verify_proof(Root1, K2, serialize(C2), Path1),
    true = verify_proof(Root2, K, 0, Path2),
    success.
