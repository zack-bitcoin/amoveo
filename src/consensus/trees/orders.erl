-module(orders).
-export([match/2, add/2, root_hash/1, id/1, amount/1,
	 pointer/1, new/3, get/2, empty_book/0,
	 remove/2, update_amount/2, set_amount/2,
	 available_id/1, many/1, aid/1, head_get/1,
	 significant_volume/2,
	 test/0]).
-define(name, orders).
-record(order, {id, aid, amount, pointer}).
significant_volume(Root, Trees) ->
    ManyOrders = many(Root),
    if
	ManyOrders == 0 -> false;
	ManyOrders > 2 -> true;
	true -> 
	    {Head, _} = orders:head_get(Root),
	    {_, Order0, _} = orders:get(Head, Root),
	    Governance = trees:governance(Trees),
	    OIL = governance:get_value(oracle_initial_liquidity, Governance),
	    (orders:amount(Order0) > constants:oracle_initial_liquidity())
    end.
    
many(Root) ->
    {_, Many} = head_get(Root),
    Many.
aid(X) -> X#order.aid.
id(X) -> X#order.id.
amount(X) -> X#order.amount.
pointer(X) -> X#order.pointer.
update_pointer(X, P) ->
    X#order{pointer = P}.
set_amount(X, A) ->
    X#order{amount = A}.
update_amount(X, A) ->
    B = X#order.amount + A,
    true = B>0,
    X#order{amount = B}.
available_id(Root) ->
    available_id(Root, 1).
available_id(Root, N) ->
    {_, A, _} = get(N, Root),
    case A of
	empty -> N;
	_ -> available_id(Root, N+1)
    end.
new(ID, AID, Amount) ->
    true = ID > 1,
    #order{id = ID, aid = AID, amount = Amount, pointer = 0}.
serialize_head(Head, Many) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    Y = OL+OL,
    AB = KL+BAL,
    <<Head:Y, Many:AB>>.
deserialize_head(X) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    Y = OL+OL,
    AB = KL+BAL,
    <<Head:Y, Many:AB>> = X,
    {Head, Many}.
    
serialize(A) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    <<(A#order.id):OL,
      (A#order.aid):KL,
      (A#order.amount):BAL,
      (A#order.pointer):OL>>.
deserialize(B) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    <<ID:OL, AID:KL,
      Amount:BAL, P:OL>> = B,
    #order{id = ID, aid = AID, amount = Amount,
	   pointer = P}.
write(X, Root) -> 
    V = serialize(X),
    Key = id(X),
    trie:put(Key, V, 0, Root, ?name).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
empty_book() ->
    X = serialize_head(0, 0),
    trie:put(1, X, 0, 0, ?name).
head_get(Root) ->
    {_, L, _} = trie:get(1, Root, ?name),
    deserialize_head(leaf:value(L)).
head_update(Head, Root) ->
    {_, Many} = head_get(Root),
    head_put(Head, Many, Root).
many_update(Many, Root) ->
    {Head, _} = head_get(Root),
    head_put(Head, Many, Root). 
head_put(Head, Many, Root) ->
    Y = serialize_head(Head, Many),
    trie:put(1, Y, 0, Root, ?name).
add(Order, Root) ->
    X = id(Order),
    {_, empty, _} = get(X, Root),
    %make the end of the list point to the new order.
    {Head, Many} = head_get(Root),
    case Head of
	0 ->
	    Root2 = head_put(X, Many+1, Root),
	    write(Order, Root2);
	Y ->
	    Root2 = head_put(Head, Many+1, Root),
	    add2(Order, Root2, Y)
    end.
add2(Order, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    case N of
	0 ->
	    L2 = update_pointer(L, id(Order)),
	    Root2 = write(L2, Root),
	    0 = Order#order.pointer,
	    write(Order, Root2);
	M ->
	    add2(Order, Root, M)
    end.
remove(ID, Root) ->
    {Head, Many} = head_get(Root),
    {_,Order,_} = get(Head, Root),
    Q = Order#order.id,
    if 
	ID == Q -> 
	    head_put(Order#order.pointer, Many-1, Root);
	true ->
	    Root2 = head_put(Head, Many-1, Root),
	    remove2(ID, Root2, Head)
    end.
remove2(ID, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    case N of
	ID ->
	    {_, L2, _} = get(ID, Root),
	    N = update_pointer(L, id(L2)),
	    Root2 = delete(N, Root),
	    write(N, Root2);
	X -> remove2(ID, Root, X)
    end.
delete(ID, Root) ->
    trie:delete(ID, Root, ?name).
match(Order, Root) ->
    {Head, Many} = head_get(Root),
    {Switch, NewRoot, Matches1, Matches2} = match2(Order, Root, Head, [], []),
    {Many2, Switch2} = case Switch of
	       same_exact -> {Many - length(Matches2), same};
	       switch -> {1, switch};
	       same -> {Many - length(Matches2) + 1, same}
	   end,
    Root2 = many_update(Many2, NewRoot),

    {Matches1, Matches2, Switch2, Root2}.
match2(Order, Root, T, Matches1, Matches2) ->
    {_, La, _} = get(T, Root),
    case La of
	empty -> 
	    P = Order#order.id,
	    Root2 = head_update(P, Root),
	    NewRoot = write(Order, Root2),
	    {switch, NewRoot, Matches1, Matches2};
	L ->
	    OldA = L#order.amount,
	    NewA = Order#order.amount,
	    P = L#order.pointer,
	    if
		NewA > OldA ->
		    Root2 = head_update(P, Root),
		    Order2 = update_amount(Order, -OldA),
		    Root3 = delete(id(L), Root2),
		    Order3 = set_amount(Order, OldA),
		    match2(Order2, Root3, P, [Order3|Matches1], [L|Matches2]);
		NewA == OldA ->
		    {same_exact, head_update(P, Root), [Order|Matches1], [L|Matches2]};
		NewA < OldA ->
		    Order2 = update_amount(L, -NewA),
		    L3 = set_amount(L, NewA),
		    {same, write(Order2, Root), [Order|Matches1], [L3|Matches2]}
	    end
    end.

root_hash(Root) ->
    trie:root_hash(?name, Root).

test() ->
    Root0 = empty_book(),
    ID1 = 5,
    ID2 = 3,
    Order1 = new(ID1, 2, 100),
    Order2 = new(ID2, 2, 100),
    Root1 = add(Order1, Root0),
    Root2 = add(Order2, Root1),
    Order3 = new(6, 5, 110),
    {Matches1, Matches2, same, Root3} = match(Order3, Root2),
    {_, empty, _} = get(ID1, 0),
    {_, {order, 5, 2, 100, _}, _} = get(ID1, Root2),
    {_, {order, 3, 2, 100, _}, _} = get(ID2, Root2),
    {_, empty, _} = get(ID1, Root3),

    Root4 = add(Order1, Root0),
    {Matches3, Matches4, switch, Root5} = match(Order3, Root4),
    {_, empty, _} = get(ID1, Root5), 
    {_, {order, 6, 5, 10, 0}, _} = get(6, Root5),
    {Matches1, Matches2, Matches3, Matches4},
    success.
    
