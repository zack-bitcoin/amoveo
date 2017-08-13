-module(orders).
-export([match/2, add/2, root_hash/1, amount/1,
	 pointer/1, new/2, get/2, empty_book/0,
	 remove/2, update_amount/2, set_amount/2,
	 many/1, head_get/1, aid/1,
	 significant_volume/2, verify_proof/4,
         deserialize/1, all/1,
	 test/0]).
-define(name, orders).
-record(order, {aid, amount, pointer}).
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
	    (orders:amount(Order0) > OIL)
    end.
    
many(Root) ->
    {_, Many} = head_get(Root),
    Many.
aid(X) -> X#order.aid.
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
new(AID, Amount) ->
    PS = constants:pubkey_size() * 8,
    #order{aid = AID, amount = Amount, pointer = <<0:PS>>}.
serialize_head(Head, Many) ->
    %KL = constants:key_length(),
    %HS = constants:hash_size()*8,
    PS = constants:pubkey_size() * 8,
    BAL = constants:balance_bits(),
    Y = PS,
    AB = PS+BAL,
    <<Head2:Y>> = Head,
    <<Head2:Y, Many:AB>>.
deserialize_head(X) ->
    %KL = constants:key_length(),
    PS = constants:pubkey_size() * 8,
    %HS = constants:hash_size()*8,
    BAL = constants:balance_bits(),
    Y = PS,
    AB = PS+BAL,
    <<Head:Y, Many:AB>> = X,
    {<<Head:Y>>, Many}.
    
serialize(A) ->
    BAL = constants:balance_bits(),
    true = size(A#order.aid) == constants:pubkey_size(),
    true = size(A#order.pointer) == constants:pubkey_size(),
    <<(A#order.amount):BAL,
      (A#order.pointer)/binary,
      (A#order.aid)/binary>>.
deserialize(B) ->
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    PS = constants:pubkey_size() * 8,
    <<Amount:BAL, P:PS,
     AID:PS>> = B,
    #order{aid = <<AID:PS>>, amount = Amount,
	   pointer = <<P:PS>>}.
write(X, Root) -> 
    V = serialize(X),
    Pubkey = aid(X),
    HP = accounts:ensure_decoded_hashed(Pubkey),
    HPID = trees:hash2int(HP),
    trie:put(HPID, V, 0, Root, ?name).
get(Pub, Root) ->
    HP = accounts:ensure_decoded_hashed(Pub),
    HPID = trees:hash2int(HP),
    {RH, Leaf, Proof} = trie:get(HPID, Root, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
empty_book() ->
    PS = constants:pubkey_size() * 8,
    X = serialize_head(<<0:PS>>, 0),
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
all(Root) ->
    {Head, _Many} = head_get(Root),
    all2(Head, Root).
all2(X, Root) ->
    PS = constants:pubkey_size() * 8,
    case X of
        <<0:PS>> -> [<<0:PS>>];
        Pub -> 
            {_, Order, _} = get(Pub, Root),
            [Pub|all2(Order#order.pointer, Root)]
    end.
            
add(Order, Root) ->
    X = aid(Order),
    {_, OldOrder, _} = get(X, Root),
    PS = constants:pubkey_size() * 8,
    %make the end of the list point to the new order.
    case OldOrder of
        empty ->
            {Head, Many} = head_get(Root),
            case Head of
                <<0:PS>> -> %adding an element to an empty list
                    Root2 = head_put(X, Many+1, Root),
                    write(Order, Root2);
                Y ->
                    Root2 = head_put(Head, Many+1, Root),
                    add2(Order, Root2, Y)
            end;
        Old ->
            New = Old#order{amount = Old#order.amount + Order#order.amount},
            write(New, Root)
    end.
add2(Order, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    PS = constants:pubkey_size() * 8,
    case N of
	<<0:PS>> ->
	    L2 = update_pointer(L, aid(Order)),
	    Root2 = write(L2, Root),
	    <<0:PS>> = Order#order.pointer,
	    write(Order, Root2);
	M ->
	    add2(Order, Root, M)
    end.
remove(ID, Root) ->
    {Head, Many} = head_get(Root),
    {_,Order,_} = get(Head, Root),
    Q = Order#order.aid,
    if 
	ID == Q -> 
	    %io:fwrite("remove path 1\n"),
	    Root2 = head_put(Order#order.pointer, Many-1, Root),
	    delete(ID, Root2);
	true ->
	    %io:fwrite("remove path 2\n"),
	    Root2 = head_put(Head, Many-1, Root),
	    remove2(ID, Root2, Head)
    end.
remove2(ID, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    case N of
	ID ->
	    %io:fwrite("remove path 3\n"),
	    {_, L2, _} = get(ID, Root),
	    L3 = update_pointer(L, aid(L2)),
	    Root2 = delete(N, Root),
	    write(L3, Root2);
	X -> 
	    %io:fwrite("remove path 4\n"),
	    remove2(ID, Root, X)
    end.
delete(Pub, Root) ->
    HP = accounts:ensure_decoded_hashed(Pub),
    HPID = trees:hash2int(HP),
    trie:delete(HPID, Root, ?name).
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
	    P = Order#order.aid,
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
		    Root3 = delete(aid(L), Root2),
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

cfg() ->
    PS = constants:pubkey_size(),
    HashSize = constants:hash_size(),
    BB = constants:balance_bits(),
    KL = constants:key_length(), 
    PathSize = constants:hash_size()*8,
    cfg:new(PathSize, ((BB div 8) + (PS * 2)), 
            orders, 0, HashSize).
verify_proof(RootHash, Key, Value, Proof) ->
    CFG = cfg(),
    V = case Value of
	    0 -> empty;
	    X -> serialize(X)
	end,
    verify:proof(RootHash, 
		 leaf:new(trees:hash2int(accounts:ensure_decoded_hashed(Key)), 
                          V, 0, CFG), 
		 Proof, CFG).

test() ->
    Root0 = empty_book(),
    {Pub1,_} = testnet_sign:new_key(),
    {Pub2,_} = testnet_sign:new_key(),
    Order1 = new(Pub2, 100),
    Order2 = new(Pub2, 100),
    Root1 = add(Order1, Root0),
    Root2 = add(Order2, Root1),
    Order3 = new(Pub1, 110),
    {Matches1, Matches2, same, Root3} = match(Order3, Root2),
    {_, empty, _} = get(Pub1, 0),
    %{_, {order, Pub1, 110, _}, _} = get(Pub1, Root2),
    {_, {order, Pub2, 200, _}, _} = get(Pub2, Root2),
    {_, empty, _} = get(Pub1, Root3),

    Root4 = add(Order1, Root0),
    {Matches3, Matches4, switch, Root5} = match(Order3, Root4),
    {_, empty, _} = get(Pub2, Root5), 
    PS = constants:pubkey_size() * 8,
    {_, {order, Pub1, 10, <<0:PS>>}, _} = get(Pub1, Root5),
    {Matches1, Matches2, Matches3, Matches4},
    %io:fwrite("TEST orders, about to remove \n"),
    Root6 = remove(Pub2, Root2),
    {_, empty, _} = get(Pub1, Root6),
    {Root8, empty, Path1} = get(Pub2, Root6),
    {Root9, {order, Pub2, 200, Pointer2}, Path2} = get(Pub2, Root2),
    %Root7 = remove(Pub2, Root2),
    %{Root8, empty, Path1} = get(Pub2, Root7),
    %{Root9, {order, Pub2, 100, Pointer2}, Path2} = get(Pub1, Root7),
    true = verify_proof(Root8, Pub2, 0, Path1),
    true = verify_proof(Root9, Pub2, {order, Pub2, 200, Pointer2}, Path2),
    success.
    
