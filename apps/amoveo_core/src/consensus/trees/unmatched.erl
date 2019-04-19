-module(unmatched).
-export([new/3,
         get/2,write/2,%update tree stuff
         dict_get/2, dict_write/2,%update dict stuff
         verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,
         dict_significant_volume/3, dict_match/4,
         dict_head_get/2,
         dict_add/3, dict_remove/3, make_leaf/3,
	 delete/2, 
         deserialize_head/1, head_put/4,
         deserialize/1,
	 serialize_head/2,
	 all/2,
	 amount/1,
	 account/1,
	 aid/1,
	 dict_empty_book/2,
	 test/0]).%common tree stuff
%for accessing the proof of existence tree
-record(unmatched, {account, %pubkey of the account
		    oracle, %oracle id
		    amount,
		    pointer}).

-define(name, unmatched).
-define(Null, 0).
-define(Header, 1).
-include("../../records.hrl").
dict_significant_volume(Dict, OID, OIL) ->
    ManyOrders = dict_many(Dict, OID),
        if 
            ManyOrders == 0 ->
                %io:fwrite("unmatched dict_significant_volume, invalid oracle because of zero orders.\n"),
                false;
            ManyOrders > 2 -> true;
            true ->
                {Head, _} = dict_head_get(Dict, OID),
                Order0 = dict_get({key, Head, OID}, Dict),
                %io:fwrite("unmatched dict_significant_volume, "),
                %io:fwrite(packer:pack([amount(Order0), OIL, Order0])),
                %io:fwrite("\n"),
                %true = is_integer(OIL),
                amount(Order0) > OIL
        end.
dict_many(Dict, OID) -> 
    {_, Many} = dict_head_get(Dict, OID),
    Many.
%many(Root, OID) ->
%    {_, Many} = head_get(Root, OID),
%    Many.
amount(#unmatched{amount = A}) -> A.
set_amount(X, A) ->
    X#unmatched{amount = A}.
update_amount(X, A) ->
    B = X#unmatched.amount + A,
    true = B>0,
    X#unmatched{amount = B}.
account(X) -> X#unmatched.account.
aid(X) -> account(X).
oracle(X) -> X#unmatched.oracle.
new(Account, Oracle, Amount) ->
    HS = constants:hash_size(),
    HS = size(Oracle),
    AS = constants:pubkey_size(),
    AS = size(Account),
    PS = AS * 8,
    #unmatched{account = Account, oracle = Oracle,
	       amount = Amount, pointer = <<?Null:PS>>}.
serialize(X) -> 
    PS = constants:pubkey_size(),
    PS = size(X#unmatched.account),
    PS = size(X#unmatched.pointer),
    HS = constants:hash_size(),
    HS = size(X#unmatched.oracle),
    BAL = constants:balance_bits(),
    <<(X#unmatched.account)/binary,
     (X#unmatched.oracle)/binary,
     (X#unmatched.amount):BAL,
     (X#unmatched.pointer)/binary>>.
deserialize(B) -> 
    HS = constants:hash_size()*8,
    BAL = constants:balance_bits(),
    PS = constants:pubkey_size()*8,
    <<Acc:PS, Oracle:HS, Amount:BAL, Pointer:PS>> = B,
    #unmatched{amount = Amount, pointer = <<Pointer:PS>>, oracle = <<Oracle:HS>>, account = <<Acc:PS>>}.
serialize_head(Head, Many) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size() * 8,
    BAL = constants:balance_bits(),
    AB = PS+HS+BAL,
    <<Head2:PS>> = Head,
    <<Head2:PS, Many:AB>>.
deserialize_head(X) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size() * 8,
    BAL = constants:balance_bits(),
    AB = PS+HS+BAL,
    <<Head:PS, Many:AB>> = X,
    {<<Head:PS>>, Many}.


dict_get({key, Account, Oracle}, Dict) ->
    true = is_binary(Account),
    true = is_binary(Oracle),
    HS = constants:hash_size(),
    HS = size(Oracle),
    PS = constants:pubkey_size(),
    PS = size(Account),
    X = dict:find({unmatched, {key, Account, Oracle}}, Dict),
    case X of
	error -> empty;
        {ok, 0} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
key_to_int({key, Account, Oracle}) ->
    true = is_binary(Account),
    true = is_binary(Oracle),
    HS = constants:hash_size(),
    HS = size(Oracle),
    PS = constants:pubkey_size(),
    PS = size(Account),
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
    Account = C#unmatched.account,
    Oracle = C#unmatched.oracle,
    dict:store({unmatched, {key, Account, Oracle}},
               serialize(C),
               Dict).
write(E, Tree) ->
    K = {key, E#unmatched.account, E#unmatched.oracle},
    Key = key_to_int(K),
    X = serialize(E),
    trie:put(Key, X, 0, Tree, ?name).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
dict_empty_book(OID, Dict) ->
    PS = constants:pubkey_size() * 8,
    Head = <<?Null:PS>>,
    Many = 0,
    dict_head_put(Head, Many, OID, Dict).
%    X = serialize_head(<<?Null:PS>>, 0),
%    dict:store({unmatched, {key, <<?Null:PS>>, OID}},
%	       X, Dict).
empty_book(OID, Tree) ->
    PS = constants:pubkey_size() * 8,
    X = serialize_head(<<?Null:PS>>, 0),
    trie:put(OID, X, 0, Tree, ?name).
dict_head_get(Dict, OID) ->
    PS = constants:pubkey_size() * 8,
    Key = {key, <<?Header:PS>>, OID},
    X = dict:fetch({unmatched, Key}, Dict),
    case X of
        0 -> {<<?Null:PS>>, 0};
        _ ->
            deserialize_head(X)
    end.
head_get(Root, OID) ->
    false = Root == 0,
    PS = constants:pubkey_size() * 8,
    ID = key_to_int({key, <<?Header:PS>>, OID}),
    {_, L, _} = trie:get(ID, Root, ?name),
    false = empty == L,
    deserialize_head(leaf:value(L)).
dict_head_update(Head, OID, Dict) ->
    {_, Many} = dict_head_get(Dict, OID),
    dict_head_put(Head, Many, OID, Dict).
head_update(Head, OID, Root) ->
    {_, Many} = head_get(Root, OID),
    head_put(Head, OID, Many, Root).
dict_many_update(Many, OID, Dict) ->
    {Head, _} = dict_head_get(Dict, OID),
    dict_head_put(Head, Many, OID, Dict).
dict_head_put(Head, Many, OID, Dict) ->
    Y = serialize_head(Head, Many),
    PS = constants:pubkey_size() * 8,
    Key = {key, <<?Header:PS>>, OID},
    dict:store({unmatched, Key},
               Y,
               Dict).
head_put(Head, Many, OID, Root) ->
    PS = constants:pubkey_size() * 8,
    Y = serialize_head(Head, Many),
    ID = key_to_int({key, <<?Header:PS>>, OID}),
    trie:put(ID, Y, 0, Root, ?name).
all(Root, OID) ->%pubkeys of everyone who made bets.
    {Head, _Many} = head_get(Root, OID),
    all2(Head, Root, OID).
all2(X, Root, OID) ->
    PS = constants:pubkey_size() * 8,
    case X of
        <<?Null:PS>> -> [<<?Header:PS>>];
	
        Pub -> 
            {_, Order, _} = get({key, Pub, OID}, Root),
            [Pub|all2(Order#unmatched.pointer, Root, OID)]
    end.
dict_add(Order, _, Dict) ->
    X = Order#unmatched.account,
    OID = Order#unmatched.oracle,
    OldOrder = dict_get({key, X, OID}, Dict),
    PS = constants:pubkey_size() * 8,
    case OldOrder of
        empty ->
            {Head, Many} = dict_head_get(Dict, OID),
            case Head of
                <<?Null:PS>> ->
                    Dict2 = dict_head_put(X, Many+1, OID, Dict),
                    dict_write(Order, Dict2);
                Y ->
                    Dict2 = dict_head_put(Head, Many+1, OID, Dict),
                    dict_add2(Order, Dict2, Y, OID)
            end;
        Old ->
            New = Old#unmatched{amount = Old#unmatched.amount + Order#unmatched.amount},
            dict_write(New, Dict)
    end.
dict_add2(Order, Dict, P, OID) ->
    L = dict_get({key, P, OID}, Dict),
    N = L#unmatched.pointer,
    PS = constants:pubkey_size() * 8,
    case N of
        <<?Null:PS>> ->
            L2 = L#unmatched{pointer = Order#unmatched.account},
            Dict2 = dict_write(L2, Dict),
            <<?Null:PS>> = Order#unmatched.pointer,
            dict_write(Order, Dict2);
        M -> dict_add2(Order, Dict, M, OID)
    end.
dict_remove(ID, OID, Dict) ->
    {Head, Many} = dict_head_get(Dict, OID),
    %Order = dict_get({key, ID, OID}, Dict),
    Order = dict_get({key, Head, OID}, Dict),
    Q = Order#unmatched.account,
    if
        ID == Q ->
            Dict2 = dict_head_put(Order#unmatched.pointer, Many-1, OID, Dict),
            dict_delete(ID, OID, Dict2);
        true ->
            Dict2 = dict_head_put(Head, Many - 1, OID, Dict),
            dict_remove2(ID, OID, Dict2, Head)
    end.
dict_remove2(ID, OID, Dict, P) ->
    L = dict_get({key, P, OID}, Dict),
    N = L#unmatched.pointer,
    case N of
        ID ->
            L2 = dict_get({key, ID, OID}, Dict),
            L3 = L#unmatched{pointer = L2#unmatched.account},
            Dict2 = dict_delete(N, OID, Dict),
            dict_write(L3, Dict2);
        X ->
            dict_remove2(ID, OID, Dict, X)
    end.
dict_delete(Pub, OID, Dict) ->
    Key = {key, Pub, OID},
    dict:store({unmatched, Key}, 0, Dict).
delete(Pub, Root) ->
    ID = key_to_int(Pub),
    trie:delete(ID, Root, ?name).
dict_match(Order, OID, Dict, Height) ->
    %Match1 is unmatched that are still open.
    %Match2 is unmatched that are already closed. We need to pay them their winnings.
    {Head, Many} = dict_head_get(Dict, OID),
    {Switch, Dict2, Matches1, Matches2} = 
        dict_match2(Order, OID, Dict, Head, [], [], Height),
    {Many2, Switch2} = 
        case Switch of
            same_exact -> {Many - length(Matches2), same};
            switch -> {1, switch};
            same -> {Many - length(Matches2) + 1, same}
        end,
    Dict3 = dict_many_update(Many2, OID, Dict2),
    {Matches1, Matches2, Switch2, Dict3}.
dict_match2(Order, OID, Dict, T, Matches1, Matches2, Height) ->
    PS = constants:pubkey_size() * 8,
    case T of
        <<?Null:PS>> ->
            P = Order#unmatched.account,
            Dict2 = dict_head_update(P, OID, Dict),
            Dict3 = dict_write(Order, Dict2),
            {switch, Dict3, [Order|Matches1], Matches2};
        _ ->
            La = dict_get({key, T, OID}, Dict),
            case La of
                empty ->
                    %throw(unmatched_check_if_path_needed),
                    P = Order#unmatched.account,
                    Dict2 = dict_head_update(P, OID, Dict),
                    Dict3 = dict_write(Order, Dict2),
                    {switch, Dict3, [Order|Matches1], Matches2};
                L ->
                    OldA = L#unmatched.amount,
                    NewA = Order#unmatched.amount,
                    P = L#unmatched.pointer,
                    if
                        NewA > OldA ->
                            %throw(check),
                            Dict2 = dict_head_update(P, OID, Dict),
                            Order2 = update_amount(Order, -OldA),
                            Dict3 = dict_delete(L#unmatched.account, OID, Dict2),
                            Order3 = update_amount(Order, OldA),
                            dict_match2(Order2, OID, Dict3, P, [Order3|Matches1], [L|Matches2], Height);
                        NewA == OldA ->
                            F15 = forks:get(15),
                            Dict4 = if
                                        Height > F15 -> dict_delete(L#unmatched.account, OID, Dict);
                                        Height > 62208 -> 1=2;%can be deleted once hard fork 15 activates.
                                        true -> Dict
                                    end,
                            {same_exact, dict_head_update(P, OID, Dict4), [Order|Matches1], [L|Matches2]};
                        NewA < OldA ->
                            Order2 = update_amount(L, -NewA),
                            L3 = L#unmatched{amount = NewA},
                            {same, dict_write(Order2, Dict), 
                             [Order2|Matches1], [L3|Matches2]}
                    end
            end
    end.

root_hash(Root) ->
    trie:root_hash(?name, Root).

test2() ->
    success.

test() ->
    Height = (tx_pool:get())#tx_pool.height,
    Hash = hash:doit(2),
    C = new(keys:pubkey(), Hash, 10000),
    Root0 = constants:root0(),
    %C = hash:doit(2),
    K = {key, keys:pubkey(), Hash},
    {_, empty, _} = get(K, Root0),
    NewLoc = write(C, Root0),
    C2 = new(keys:pubkey(), hash:doit(4), 10200),
    K2 = {key, keys:pubkey(), hash:doit(4)},
    NewLoc2 = write(C2, NewLoc),
    {Root1, C2, Path1} = get(K2, NewLoc2),
    {Root2, empty, Path2} = get(K, Root0),
    true = verify_proof(Root1, K2, serialize(C2), Path1),
    true = verify_proof(Root2, K, 0, Path2),

    OID = <<1:256>>,
    Head = <<0:520>>,
    Many = 10,
    RootA = head_put(Head, Many, OID, Root0),
    {Head, Many} = head_get(RootA, OID),

    success.
