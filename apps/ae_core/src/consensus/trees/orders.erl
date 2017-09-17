-module(orders).
-export([match/2, add/2, root_hash/1, amount/1,
         pointer/1, new/2, get/2, empty_book/0,
         remove/2, update_amount/2, set_amount/2,
         many/1, head_get/1, aid/1,
         significant_volume/2, verify_proof/4,
         serialize/1, deserialize/1, all/1,
         dict_significant_volume/3, dict_match/3,
         dict_write/3, dict_get/2, dict_head_get/2,
         dict_add/3, dict_remove/3, make_leaf/3,
         key_to_int/1, write/2, delete/2,
         test/0]).
-define(name, orders).
-record(orders, {aid, amount, pointer}).
dict_significant_volume(Dict, OID, OIL) ->
    ManyOrders = dict_many(Dict, OID),
    io:fwrite("dict sig volume many is "),
    io:fwrite(integer_to_list(ManyOrders)),
    io:fwrite("\n"),
    if 
        ManyOrders == 0 -> false;
        ManyOrders > 2 -> true;
        true ->
            io:fwrite("dict complex\n"),
            {Head, _} = dict_head_get(Dict, OID),
            Order0 = dict_get({key, Head, OID}, Dict),
            amount(Order0) > OIL
    end.
            
significant_volume(Root, Trees) ->
    ManyOrders = many(Root),
    io:fwrite("sig volume many is "),
    io:fwrite(integer_to_list(ManyOrders)),
    io:fwrite("\n"),
        if
            ManyOrders == 0 ->
                 false;
            ManyOrders > 2 -> true;
            true -> 
                io:fwrite("complex\n"),
                {Head, _} = head_get(Root),
                {_, Order0, _} = get(Head, Root),
                Governance = trees:governance(Trees),
                OIL = governance:get_value(oracle_initial_liquidity, Governance),
                (orders:amount(Order0) > OIL)
    end.
dict_many(Dict, OID) -> 
    {_, Many} = dict_head_get(Dict, OID),
    Many.
many(Root) ->
    {_, Many} = head_get(Root),
    Many.
aid(X) -> X#orders.aid.
amount(X) -> X#orders.amount.
pointer(X) -> X#orders.pointer.
update_pointer(X, P) ->
    X#orders{pointer = P}.
set_amount(X, A) ->
    X#orders{amount = A}.
update_amount(X, A) ->
    B = X#orders.amount + A,
    true = B>0,
    X#orders{amount = B}.
new(AID, Amount) ->
    PS = constants:pubkey_size() * 8,
    #orders{aid = AID, amount = Amount, pointer = <<0:PS>>}.
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
    true = size(A#orders.aid) == constants:pubkey_size(),
    true = size(A#orders.pointer) == constants:pubkey_size(),
    <<(A#orders.amount):BAL,
      (A#orders.pointer)/binary,
      (A#orders.aid)/binary>>.
deserialize(B) ->
    %OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    PS = constants:pubkey_size() * 8,
    <<Amount:BAL, P:PS,
     AID:PS>> = B,
    #orders{aid = <<AID:PS>>, amount = Amount,
              pointer = <<P:PS>>}.
dict_write(Order, OID, Dict) ->
    Pub = aid(Order), 
    Key = {key, Pub, OID},
    dict:store({orders, Key},
               serialize(Order),
               Dict).
write(X, Root) -> 
    V = serialize(X),
    Pubkey = aid(X),
    HPID = key_to_int2(Pubkey),
    trie:put(HPID, V, 0, Root, ?name).
dict_get(Key, Dict) ->
    X = dict:fetch({orders, Key}, Dict),
    case X of
        0 -> empty;
        _ -> deserialize(X)
    end.
    
key_to_int({key, Pubkey, _}) ->
    key_to_int2(Pubkey).
key_to_int2(Pubkey) ->
    HP = accounts:ensure_decoded_hashed(Pubkey),
    trees:hash2int(HP).
get(Pub, Root) ->
    HPID = key_to_int2(Pub),
    {RH, Leaf, Proof} = trie:get(HPID, Root, ?name),
    V = case Leaf of
                empty -> empty;
                L -> deserialize(leaf:value(L))
                         end,
    {RH, V, Proof}.
empty_book() ->
    PS = constants:pubkey_size() * 8,
    X = serialize_head(<<0:PS>>, 0),
    trie:put(1, X, 0, constants:root0(), ?name).
dict_head_get(Dict, OID) ->
    PS = constants:pubkey_size() * 8,
    Key = {key, <<0:PS>>, OID},
    X = dict:fetch({orders, Key}, Dict),
    case X of
        0 -> {<<0:PS>>, 0};
        _ ->
            deserialize_head(X)
    end.
head_get(Root) ->
    {_, L, _} = trie:get(1, Root, ?name),
    deserialize_head(leaf:value(L)).
dict_head_update(Head, OID, Dict) ->
    {_, Many} = dict_head_get(Dict, OID),
    dict_head_put(Head, Many, OID, Dict).
head_update(Head, Root) ->
    {_, Many} = head_get(Root),
    head_put(Head, Many, Root).
dict_many_update(Many, OID, Dict) ->
    {Head, _} = dict_head_get(Dict, OID),
    dict_head_put(Head, Many, OID, Dict).
many_update(Many, Root) ->
    {Head, _} = head_get(Root),
    head_put(Head, Many, Root). 
dict_head_put(Head, Many, OID, Dict) ->
    Y = serialize_head(Head, Many),
    PS = constants:pubkey_size() * 8,
    Key = {key, <<0:PS>>, OID},
    dict:store({orders, Key},
               Y,
               Dict).
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
            [Pub|all2(Order#orders.pointer, Root)]
    end.
dict_add(Order, OID, Dict) ->
    X = aid(Order),
    OldOrder = dict_get({key, X, OID}, Dict),
    PS = constants:pubkey_size() * 8,
    case OldOrder of
        empty ->
            {Head, Many} = dict_head_get(Dict, OID),
            case Head of
                <<0:PS>> ->
                    Dict2 = dict_head_put(X, Many+1, OID, Dict),
                    dict_write(Order, OID, Dict2);
                Y ->
                    Dict2 = head_put(Head, Many+1, Dict),
                    dict_add2(Order, Dict2, Y, OID)
            end;
        Old ->
            New = Old#orders{amount = Old#orders.amount + Order#orders.amount},
            dict_write(New, OID, Dict)
    end.
add(Order, Root) ->
    X = aid(Order),
    {_, OldOrder, _} = get(X, Root),
    PS = constants:pubkey_size() * 8,
    %make the end of the list point to the new orders.
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
            New = Old#orders{amount = Old#orders.amount + Order#orders.amount},
            write(New, Root)
    end.
dict_add2(Order, Dict, P, OID) ->
    L = dict_get({key, P, OID}, Dict),
    N = L#orders.pointer,
    PS = constants:pubkey_size() * 8,
    case N of
        <<0:PS>> ->
            L2 = L#orders{pointer = aid(Order)},
            Dict2 = dict_write(L2, OID, Dict),
            <<0:PS>> = Order#orders.pointer,
            dict_write(Order, OID, Dict2);
        M -> dict_add2(Order, Dict, M, OID)
    end.
add2(Order, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#orders.pointer,
    PS = constants:pubkey_size() * 8,
    case N of
        <<0:PS>> ->
                L2 = update_pointer(L, aid(Order)),
                Root2 = write(L2, Root),
                <<0:PS>> = Order#orders.pointer,
                write(Order, Root2);
        M ->
                add2(Order, Root, M)
    end.
dict_remove(ID, OID, Dict) ->
    {Head, Many} = dict_head_get(Dict, OID),
    Order = dict_get({key, ID, OID}, Dict),
    Q = Order#orders.aid,
    if
        ID == Q ->
            Dict2 = dict_head_put(Order#orders.pointer, Many-1, OID, Dict),
            dict_delete(ID, OID, Dict2);
        true ->
            Dict2 = dict_head_put(Head, Many - 1, OID, Dict),
            dict_remove2(ID, OID, Dict2, Head)
    end.
remove(ID, Root) ->
    {Head, Many} = head_get(Root),
    {_,Order,_} = get(Head, Root),
    Q = Order#orders.aid,
    if 
        ID == Q -> 
                %io:fwrite("remove path 1\n"),
                Root2 = head_put(Order#orders.pointer, Many-1, Root),
                delete(ID, Root2);
        true ->
                %io:fwrite("remove path 2\n"),
                Root2 = head_put(Head, Many-1, Root),
                remove2(ID, Root2, Head)
    end.
dict_remove2(ID, OID, Dict, P) ->
    L = dict_get({key, ID, OID}, Dict),
    N = L#orders.pointer,
    case N of
        ID ->
            L2 = dict_get({key, ID, OID}, Dict),
            L3 = L#orders{pointer = aid(L2)},
            Dict2 = dict_delete(N, OID, Dict),
            dict_write(L3, OID, Dict2);
        X ->
            dict_remove2(ID, OID, Dict, X)
    end.
remove2(ID, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#orders.pointer,
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
dict_delete(Pub, OID, Dict) ->
    Key = {key, Pub, OID},
    dict:store({orders, Key}, 0, Dict).
delete(Pub, Root) ->
    HP = accounts:ensure_decoded_hashed(Pub),
    HPID = trees:hash2int(HP),
    trie:delete(HPID, Root, ?name).
dict_match(Order, OID, Dict) ->
    {Head, Many} = dict_head_get(Dict, OID),
    {Switch, Dict2, Matches1, Matches2} = 
        dict_match2(Order, OID, Dict, Head, [], []),
    {Many2, Switch2} = 
        case Switch of
            same_exact -> {Many - length(Matches2), same};
            switch -> {1, switch};
            same -> {Many - length(Matches2) + 1, same}
                                                 end,
    Root2 = dict_many_update(Many2, OID, Dict2),
    {Matches1, Matches2, Switch2, Root2}.
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
dict_match2(Order, OID, Dict, T, Matches1, Matches2) ->
    La = dict_get({key, T, OID}, Dict),
    case La of
        empty ->
            P = Order#orders.aid,
            Dict2 = dict_head_update(P, OID, Dict),
            Dict3 = dict_write(Order, OID, Dict2),
            {switch, Dict3, [Order|Matches1], Matches2};
        L ->
            OldA = L#orders.amount,
            NewA = Order#orders.amount,
            P = L#orders.pointer,
            if
                NewA > OldA ->
                    Dict2 = dict_head_update(P, OID, Dict),
                    Order2 = update_amount(Order, -OldA),
                    Dict3 = dict_delete(aid(L), OID, Dict2),
                    Order3 = update_amount(Order, OldA),
                    dict_match2(Order2, OID, Dict3, P, [Order3|Matches1], [L|Matches2]);
               NewA == OldA ->
                    {same_exact, dict_head_update(P, OID, Dict), [Order|Matches1], [L|Matches2]};
                NewA < OldA ->
                    Order2 = update_amount(L, -NewA),
                    L3 = set_amount(L, NewA),
                    {same, dict_write(Order2, OID, Dict), 
                     [Order|Matches1], [L3|Matches2]}
            end
    end.
match2(Order, Root, T, Matches1, Matches2) ->
    %io:fwrite(packer:pack({match2, Order})),
    %io:fwrite("\n"),
    {_, La, _} = get(T, Root),
    case La of
        empty -> 
            %io:fwrite("was empty\n"),
            P = Order#orders.aid,
            Root2 = head_update(P, Root),
            NewRoot = write(Order, Root2),
            {switch, NewRoot, [Order|Matches1], Matches2};
        L ->
            OldA = L#orders.amount,
            NewA = Order#orders.amount,
            P = L#orders.pointer,
            if
                NewA > OldA ->
                                                %io:fwrite("new bigger\n"),
                    Root2 = head_update(P, Root),
                    Order2 = update_amount(Order, -OldA),
                    Root3 = delete(aid(L), Root2),
                    Order3 = set_amount(Order, OldA),
                    match2(Order2, Root3, P, [Order3|Matches1], [L|Matches2]);
                NewA == OldA ->
                    %io:fwrite("same\n"),
                    {same_exact, head_update(P, Root), [Order|Matches1], [L|Matches2]};
                NewA < OldA ->
                    Order2 = update_amount(L, -NewA),
                    L3 = set_amount(L, NewA),
                    {same, write(Order2, Root), [Order|Matches1], [L3|Matches2]}
            end
    end.

root_hash(Root) ->
    trie:root_hash(?name, Root).
make_leaf(Key, V, CFG) ->
    leaf:new(trees:hash2int(accounts:ensure_decoded_hashed(Key)), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

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
    {_, empty, _} = get(Pub1, constants:root0()),
    %{_, {orders, Pub1, 110, _}, _} = get(Pub1, Root2),
    {_, {orders, Pub2, 200, _}, _} = get(Pub2, Root2),
    {_, empty, _} = get(Pub1, Root3),

    Root4 = add(Order1, Root0),
    {Matches3, Matches4, switch, Root5} = match(Order3, Root4),
    {_, empty, _} = get(Pub2, Root5), 
    PS = constants:pubkey_size() * 8,
    {_, {orders, Pub1, 10, <<0:PS>>}, _} = get(Pub1, Root5),
    {Matches1, Matches2, Matches3, Matches4},
    %io:fwrite("TEST orders, about to remove \n"),
    Root6 = remove(Pub2, Root2),
    {_, empty, _} = get(Pub1, Root6),
    {Root8, empty, Path1} = get(Pub2, Root6),
    {Root9, {orders, Pub2, 200, Pointer2}, Path2} = get(Pub2, Root2),
    %Root7 = remove(Pub2, Root2),
    %{Root8, empty, Path1} = get(Pub2, Root7),
    %{Root9, {orders, Pub2, 100, Pointer2}, Path2} = get(Pub1, Root7),
    true = verify_proof(Root8, Pub2, 0, Path1),
    true = verify_proof(Root9, Pub2, serialize({orders, Pub2, 200, Pointer2}), Path2),
    test2().
test2()->
    Root0 = empty_book(),
    OID = 1,
    {Pub,_} = testnet_sign:new_key(),
    Order1 = new(Pub, 100),
    CFG = trie:cfg(orders),
    Dict0 = dict:new(),
    Key = {key, Pub, OID},
    Dict1 = dict_write(Order1, OID, Dict0),
    Order1 = dict_get(Key, Dict1),
    success.
    
