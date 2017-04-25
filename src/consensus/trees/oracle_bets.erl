-module(oracle_bets).
-export([test/0, new/3, increase/3, id/1,
	 true/1, false/1, bad/1,
	 write/2, get/2, root_hash/1, add_bet/4,
	 to_shares/3, delete/2,
	 remove_bet/3]).
%Each account has a tree of oracle bets. Oracle bets are not transferable. Once an oracle is settled, the bets in it can be converted to shares.
-record(bet, {id, true, false, bad}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle
-define(name, oracle_bets).
to_shares(Bet, Correct, NewHeight) ->
    %returns {Shares, Tokens}
    ID = Bet#bet.id,
    {Positive, Negative} = 
	case Correct of
	    1->{Bet#bet.true,Bet#bet.false+Bet#bet.bad};
	    2->{Bet#bet.false,Bet#bet.true+Bet#bet.bad};
	    3->{Bet#bet.bad,Bet#bet.true+Bet#bet.false}
	end,
    [shares:new(ID, Positive, NewHeight), shares:new(ID, -Negative, NewHeight)].
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
write(X, Tree) ->
    Key = X#bet.id,
    Z = serialize(X),
    trie:put(Key, Z, 0, Tree, ?name).
get(ID, Tree) ->
    {X, Leaf, Proof} = trie:get(ID, Tree, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
delete(ID, Tree) ->
    trie:delete(ID, Tree, ?name).
add_bet(Id, Type, Amount, Tree) ->
    {_, X, _} = get(Id, Tree),
    Y = case X of
	    empty -> new(Id, Type, Amount);
	    Bet -> increase(Bet, Type, Amount)
	end,
    write(Y, Tree).
remove_bet(_, _, _) ->
    ok.
	    
root_hash(A) ->
    trie:root_hash(?name, A).

test() ->
    C = new(1, 3, 100),
    ID = C#bet.id,
    {_, empty, _} = get(ID, 0),
    Root = write(C, 0),
    {_, C, _} = get(ID, Root),
    {_, empty, _} = get(ID, 0),
    Tree2 = add_bet(ID, true, 100, Root),
    {_, Bet2, _} = get(ID, Tree2),
    Bet2 = increase(C, true, 100),
    success.
    
    
