-module(orders).%
-export([root_hash/1, amount/1,%
         new/2, new/3,%
         get/2, empty_book/0,%
         set_amount/2,%
         many/1, head_get/1, %
         aid/1,%
         verify_proof/4,%
         serialize/1, deserialize/1, all/1,%
         dict_significant_volume/3, dict_match/4,%
         dict_write/3, dict_get/2, dict_head_get/2,%
         dict_add/3, dict_remove/3, make_leaf/3,%
         key_to_int/1, write/2, delete/2,%
         deserialize_head/1, head_put/3,%
	 serialize_head/2,%
         test/0]).%
-define(name, orders).%
-define(Null, 0).%
-define(Header, 1).%
-record(orders, {aid, amount, pointer}).%
dict_significant_volume(Dict, OID, OIL) ->%
    ManyOrders = dict_many(Dict, OID),%
        if %
            ManyOrders == 0 ->%
                false;%
            ManyOrders > 2 -> true;%
            true ->%
                {Head, _} = dict_head_get(Dict, OID),%
                Order0 = dict_get({key, Head, OID}, Dict),%
                amount(Order0) > OIL%
        end.%
            %
dict_many(Dict, OID) -> %
    {_, Many} = dict_head_get(Dict, OID),%
    Many.%
many(Root) ->%
    {_, Many} = head_get(Root),%
    Many.%
aid(X) -> X#orders.aid.%
amount(X) -> X#orders.amount.%
set_amount(X, A) ->%
    X#orders{amount = A}.%
update_amount(X, A) ->%
    B = X#orders.amount + A,%
    true = B>0,%
    X#orders{amount = B}.%
new(AID, _, Amount) ->%
    new(AID, Amount).%
new(AID, Amount) ->%
    PS = constants:pubkey_size() * 8,%
    #orders{aid = AID, amount = Amount, pointer = <<?Null:PS>>}.%
serialize_head(Head, Many) ->%
    %HS = constants:hash_size()*8,%
    PS = constants:pubkey_size() * 8,%
    BAL = constants:balance_bits(),%
    AB = PS+BAL,%
    <<Head2:PS>> = Head,%
    <<Head2:PS, Many:AB>>.%
deserialize_head(X) ->%
    PS = constants:pubkey_size() * 8,%
    %HS = constants:hash_size()*8,%
    BAL = constants:balance_bits(),%
    Y = PS,%
    AB = PS+BAL,%
    <<Head:Y, Many:AB>> = X,%
    {<<Head:Y>>, Many}.%
    %
serialize(A) ->%
    BAL = constants:balance_bits(),%
    true = size(A#orders.aid) == constants:pubkey_size(),%
    true = size(A#orders.pointer) == constants:pubkey_size(),%
    <<(A#orders.amount):BAL,%
      (A#orders.pointer)/binary,%
      (A#orders.aid)/binary>>.%
deserialize(B) ->%
    %OL = constants:orders_bits(),%
    BAL = constants:balance_bits(),%
    PS = constants:pubkey_size() * 8,%
    <<Amount:BAL, P:PS,%
     AID:PS>> = B,%
    #orders{aid = <<AID:PS>>, amount = Amount,%
              pointer = <<P:PS>>}.%
dict_write(Order, OID, Dict) ->%
    Pub = aid(Order), %
    Key = {key, Pub, OID},%
    dict:store({orders, Key},%
               serialize(Order),%
               Dict).%
write(X, Root) -> %
    V = serialize(X),%
    Pubkey = aid(X),%
    HPID = key_to_int(Pubkey),%
    trie:put(HPID, V, 0, Root, ?name).%
dict_get(Key, Dict) ->%
    X = dict:fetch({orders, Key}, Dict),%
    case X of%
        0 -> empty;%
        _ -> deserialize(X)%
    end.%
key_to_int(Pubkey) ->%
    accounts:key_to_int(Pubkey).%
get(Pub, Root) ->%
    HPID = key_to_int(Pub),%
    {RH, Leaf, Proof} = trie:get(HPID, Root, ?name),%
    V = case Leaf of%
                empty -> empty;%
                L -> deserialize(leaf:value(L))%
                         end,%
    {RH, V, Proof}.%
empty_book() ->%
    PS = constants:pubkey_size() * 8,%
    X = serialize_head(<<?Null:PS>>, 0),%
    ID = key_to_int(<<?Header:PS>>),%
    trie:put(ID, X, 0, constants:root0(), ?name).%
dict_head_get(Dict, OID) ->%
    PS = constants:pubkey_size() * 8,%
    Key = {key, <<?Header:PS>>, OID},%
    X = dict:fetch({orders, Key}, Dict),%
    case X of%
        0 -> {<<?Null:PS>>, 0};%
        _ ->%
            deserialize_head(X)%
    end.%
head_get(Root) ->%
    false = Root == 0,%
    PS = constants:pubkey_size() * 8,%
    ID = key_to_int(<<?Header:PS>>),%
    {_, L, _} = trie:get(ID, Root, ?name),%
    deserialize_head(leaf:value(L)).%
dict_head_update(Head, OID, Dict) ->%
    {_, Many} = dict_head_get(Dict, OID),%
    dict_head_put(Head, Many, OID, Dict).%
head_update(Head, Root) ->%
    {_, Many} = head_get(Root),%
    head_put(Head, Many, Root).%
dict_many_update(Many, OID, Dict) ->%
    {Head, _} = dict_head_get(Dict, OID),%
    dict_head_put(Head, Many, OID, Dict).%
dict_head_put(Head, Many, OID, Dict) ->%
    Y = serialize_head(Head, Many),%
    PS = constants:pubkey_size() * 8,%
    Key = {key, <<?Header:PS>>, OID},%
    dict:store({orders, Key},%
               Y,%
               Dict).%
head_put(Head, Many, Root) ->%
    PS = constants:pubkey_size() * 8,%
    Y = serialize_head(Head, Many),%
    ID = key_to_int(<<?Header:PS>>),%
    trie:put(ID, Y, 0, Root, ?name).%
all(Root) ->%pubkeys of everyone who made bets.%
    {Head, _Many} = head_get(Root),%
    all2(Head, Root).%
all2(X, Root) ->%
    PS = constants:pubkey_size() * 8,%
    case X of%
        <<?Null:PS>> -> [<<?Header:PS>>];%
	%
        Pub -> %
            {_, Order, _} = get(Pub, Root),%
            [Pub|all2(Order#orders.pointer, Root)]%
    end.%
dict_add(Order, OID, Dict) ->%
    X = aid(Order),%
    OldOrder = dict_get({key, X, OID}, Dict),%
    PS = constants:pubkey_size() * 8,%
    case OldOrder of%
        empty ->%
            {Head, Many} = dict_head_get(Dict, OID),%
            case Head of%
                <<?Null:PS>> ->%
                    Dict2 = dict_head_put(X, Many+1, OID, Dict),%
                    dict_write(Order, OID, Dict2);%
                Y ->%
                    Dict2 = dict_head_put(Head, Many+1, OID, Dict),%
                    dict_add2(Order, Dict2, Y, OID)%
            end;%
        Old ->%
            New = Old#orders{amount = Old#orders.amount + Order#orders.amount},%
            dict_write(New, OID, Dict)%
    end.%
dict_add2(Order, Dict, P, OID) ->%
    L = dict_get({key, P, OID}, Dict),%
    N = L#orders.pointer,%
    PS = constants:pubkey_size() * 8,%
    case N of%
        <<?Null:PS>> ->%
            L2 = L#orders{pointer = aid(Order)},%
            Dict2 = dict_write(L2, OID, Dict),%
            <<?Null:PS>> = Order#orders.pointer,%
            dict_write(Order, OID, Dict2);%
        M -> dict_add2(Order, Dict, M, OID)%
    end.%
dict_remove(ID, OID, Dict) ->%
    {Head, Many} = dict_head_get(Dict, OID),%
    %Order = dict_get({key, ID, OID}, Dict),%
    Order = dict_get({key, Head, OID}, Dict),%
    Q = Order#orders.aid,%
    if%
        ID == Q ->%
            Dict2 = dict_head_put(Order#orders.pointer, Many-1, OID, Dict),%
            dict_delete(ID, OID, Dict2);%
        true ->%
            Dict2 = dict_head_put(Head, Many - 1, OID, Dict),%
            dict_remove2(ID, OID, Dict2, Head)%
    end.%
dict_remove2(ID, OID, Dict, P) ->%
    L = dict_get({key, P, OID}, Dict),%
    N = L#orders.pointer,%
    case N of%
        ID ->%
            L2 = dict_get({key, ID, OID}, Dict),%
            L3 = L#orders{pointer = aid(L2)},%
            Dict2 = dict_delete(N, OID, Dict),%
            dict_write(L3, OID, Dict2);%
        X ->%
            dict_remove2(ID, OID, Dict, X)%
    end.%
dict_delete(Pub, OID, Dict) ->%
    Key = {key, Pub, OID},%
    dict:store({orders, Key}, 0, Dict).%
delete(Pub, Root) ->%
    ID = key_to_int(Pub),%
    trie:delete(ID, Root, ?name).%
dict_match(Order, OID, Dict, _) ->%
    %Match1 is orders that are still open.%
    %Match2 is orders that are already closed. We need to pay them their winnings.%
    {Head, Many} = dict_head_get(Dict, OID),%
    {Switch, Dict2, Matches1, Matches2} = %
        dict_match2(Order, OID, Dict, Head, [], []),%
    {Many2, Switch2} = %
        case Switch of%
            same_exact -> {Many - length(Matches2), same};%
            switch -> {1, switch};%
            same -> {Many - length(Matches2) + 1, same}%
        end,%
    Root2 = dict_many_update(Many2, OID, Dict2),%
    {Matches1, Matches2, Switch2, Root2}.%
dict_match2(Order, OID, Dict, T, Matches1, Matches2) ->%
    PS = constants:pubkey_size() * 8,%
    case T of%
        <<?Null:PS>> ->%
            P = Order#orders.aid,%
            Dict2 = dict_head_update(P, OID, Dict),%
            Dict3 = dict_write(Order, OID, Dict2),%
            {switch, Dict3, [Order|Matches1], Matches2};%
        _ ->%
            La = dict_get({key, T, OID}, Dict),%
            case La of%
                empty ->%
                    %throw(orders_check_if_path_needed),%
                    P = Order#orders.aid,%
                    Dict2 = dict_head_update(P, OID, Dict),%
                    Dict3 = dict_write(Order, OID, Dict2),%
                    {switch, Dict3, [Order|Matches1], Matches2};%
                L ->%
                    OldA = L#orders.amount,%
                    NewA = Order#orders.amount,%
                    P = L#orders.pointer,%
                    if%
                        NewA > OldA ->%
                            %throw(check),%
                            Dict2 = dict_head_update(P, OID, Dict),%
                            Order2 = update_amount(Order, -OldA),%
                            Dict3 = dict_delete(aid(L), OID, Dict2),%
                            Order3 = update_amount(Order, OldA),%
                            dict_match2(Order2, OID, Dict3, P, [Order3|Matches1], [L|Matches2]);%
                        NewA == OldA ->%
                            {same_exact, dict_head_update(P, OID, Dict), [Order|Matches1], [L|Matches2]};%
                        NewA < OldA ->%
                            Order2 = update_amount(L, -NewA),%
                            L3 = L#orders{amount = NewA},%
                            {same, dict_write(Order2, OID, Dict), %
                             [Order2|Matches1], [L3|Matches2]}%
                    end%
            end%
    end.%
%
root_hash(Root) ->%
    trie:root_hash(?name, Root).%
make_leaf(Key, V, CFG) ->%
    leaf:new(accounts:key_to_int(Key), %
             V, 0, CFG).%
verify_proof(RootHash, Key, Value, Proof) ->%
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).%
%
test() ->%
    Root0 = empty_book(),%
    {Pub1,_} = testnet_sign:new_key(),%
    {Pub2,_} = testnet_sign:new_key(),%
    Order1 = new(Pub2, 100),%
    Order2 = new(Pub2, 100),%
    Order3 = new(Pub1, 110),%
    {_, empty, _} = get(Pub1, constants:root0()),%
    PS = constants:pubkey_size() * 8,%
    test2().%
test2()->%
    Root0 = empty_book(),%
    OID = 1,%
    {Pub,_} = testnet_sign:new_key(),%
    Order1 = new(Pub, 100),%
    CFG = trie:cfg(orders),%
    Dict0 = dict:new(),%
    Key = {key, Pub, OID},%
    Dict1 = dict_write(Order1, OID, Dict0),%
    Order1 = dict_get(Key, Dict1),%
    success.%
