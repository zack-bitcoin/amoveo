-module(oracle_bets).
-export([test/0, new/3, increase/3, id/1,
	 true/1, false/1, bad/1,
	 write/2, get/2, root_hash/1, add_bet/4,
	 reward/3, delete/2, verify_proof/4,
         dict_add_bet/5, dict_get/2, dict_delete/2,
         serialize/1, make_leaf/3, key_to_int/1,
         deserialize/1]).
%Each account has a tree of oracle bets. Oracle bets are not transferable. Once an oracle is settled, the bets in it can be converted to shares.
-record(bet, {id, true, false, bad}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle
-define(name, oracle_bets).
reward(Bet, Correct, NewHeight) ->
    %returns {Shares, Tokens}
    ID = Bet#bet.id,
    {Positive, _Negative} = 
	case Correct of
	    1->{Bet#bet.true,Bet#bet.false+Bet#bet.bad};
	    2->{Bet#bet.false,Bet#bet.true+Bet#bet.bad};
	    3->{Bet#bet.bad,Bet#bet.true+Bet#bet.false}
	end,
    %[shares:new(ID, Positive, NewHeight), shares:new(ID, -Negative, NewHeight)],
    Positive.
id(X) ->
    X#bet.id.
true(X) ->
    X#bet.true.
false(X) ->
    X#bet.false.
bad(X) ->
    X#bet.bad.
increase(X, Type, A) ->
    case Type of
	1 -> X#bet{true = X#bet.true + A};
	2 -> X#bet{false = X#bet.false + A};
	3 -> X#bet{bad = X#bet.bad + A}
    end.
new(ID, Type, Amount) ->
    {A, B, C} = 
	case Type of
	    1 -> {Amount, 0, 0};
	    2 -> {0, Amount, 0};
	    3 -> {0, 0, Amount}
	end, 
    new(ID, A, B, C).
    
new(OracleID, True, False, Bad) ->
    %{_, X, _} = active_oracles:read(OracleID, AORoot),
    %false = X == empty,
    #bet{id = OracleID, true = True, false = False,
	 bad = Bad}.
serialize(X) ->
    KL = constants:key_length()*8,
    BAL = constants:balance_bits(),
    <<(X#bet.id):KL,
      (X#bet.true):BAL,
      (X#bet.false):BAL,
      (X#bet.bad):BAL>>.
deserialize(B) ->
    KL = constants:key_length()*8,
    BAL = constants:balance_bits(),
    <<ID:KL, True:BAL, False:BAL, Bad:BAL>> = B,
    #bet{true = True, false = False, bad = Bad, id = ID}.
dict_write(X, Pub, Dict) ->
    dict:store({oracle_bets, {key, Pub, X#bet.id}},
               serialize(X),
               Dict).
write(X, Tree) ->
    Key = X#bet.id,
    Z = serialize(X),
    trie:put(Key, Z, 0, Tree, ?name).
dict_get(Key, Dict) ->
    X = dict:fetch({oracle_bets, Key}, Dict),
    case X of
        0 -> empty;
        _ -> deserialize(X)
    end.
key_to_int({key, _, X}) -> X.
get(ID, Tree) ->
    {X, Leaf, Proof} = trie:get(ID, Tree, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
dict_delete(Key, Dict) ->
    dict:store({oracle_bets, Key}, 0, Dict).
delete(ID, Tree) ->
    trie:delete(ID, Tree, ?name).
dict_add_bet(Pub, OID, Type, Amount, Dict) ->
    io:fwrite("dict add bet \n"),
    X = dict_get({key, Pub, OID}, Dict),
    io:fwrite("dict got was \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    Y = case X of
            empty -> new(OID, Type, Amount);
            Bet -> increase(Bet, Type, Amount)
        end, 
    io:fwrite("about to write "),
    io:fwrite(packer:pack(Y)),
    io:fwrite("\n"),
    dict_write(Y, Pub, Dict).
    
add_bet(Id, Type, Amount, Tree) ->
    {_, X, _} = get(Id, Tree),
    Y = case X of
	    empty -> new(Id, Type, Amount);
	    Bet -> increase(Bet, Type, Amount)
	end,
    write(Y, Tree).
root_hash(A) ->
    trie:root_hash(?name, A).
make_leaf(Key, V, CFG) ->
    leaf:new(Key, V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

test() ->
    C = new(1, 3, 100),
    ID = C#bet.id,
    Root0 = constants:root0(),
    {_, empty, _} = get(ID, Root0),
    Root = write(C, Root0),
    {Root1, C, Path1} = get(ID, Root),
    {Root2, empty, Path2} = get(ID, Root0),
    Tree2 = add_bet(ID, 1, 100, Root),
    {_, Bet2, _} = get(ID, Tree2),
    Bet2 = increase(C, 1, 100),

    true = verify_proof(Root1, ID, serialize(C), Path1),
    true = verify_proof(Root2, ID, 0, Path2),
    test2().
test2() ->
    OID = 1,
    C = new(OID, 3, 100),
    ID = C#bet.id,
    CFG = trie:cfg(oracle_bets),
    Dict0 = dict:new(),
    Pub = keys:pubkey(),
    Key = {key, Pub, ID},
    Dict1 = dict_write(C, Pub, Dict0),
    C = dict_get(Key, Dict1),
    Dict2 = dict_add_bet(Pub, OID, 1, 100, Dict1),
    Bet2 = dict_get(Key, Dict2),
    Bet2 = increase(C, 1, 100),
    success.
    
    

