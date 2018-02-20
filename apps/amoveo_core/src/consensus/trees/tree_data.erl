-module(tree_data).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	dict_update_trie/2, prune/2, garbage/2]).
-include("../../records.hrl").
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({garbage, Trash, Keep}, _, _) -> 
    internal(Trash, Keep, fun(A, B, C) -> trie:garbage(A, B, C) end),
    {reply, ok, []};
handle_call({prune, Trash, Keep}, _, _) -> 
    1=2,
    internal(Trash, Keep, fun(A, B, C) -> trie:prune(A, B, C) end),
    {reply, ok, []};
handle_call({update, Trees, Dict}, _From, _) -> 
    Y = internal_dict_update_trie(Trees, Dict),
    {reply, Y, []};
handle_call(_, _From, X) -> {reply, X, X}.

dict_update_trie(Trees, Dict) ->
    gen_server:call(?MODULE, {update, Trees, Dict}).
garbage(Trash, Keep) ->
    gen_server:call(?MODULE, {garbage, Trash, Keep}).
prune(Trash, Keep) ->
    gen_server:call(?MODULE, {prune, Trash, Keep}).
internal(PruneBlock, KeepBlock, F) ->
    Trees = [accounts, oracles, channels, existence, governance],
    [A, O, _, _, _] = 
	lists:map(fun(T) ->
			  T1 = PruneBlock#block.trees,
			  T2 = KeepBlock#block.trees,
			  A1 = trees:T(T1),
			  A2 = trees:T(T2),
			  F(A1, A2, T)
		  end, Trees),
    ok.
dont_doit(O, A, F) ->
    lists:map(fun({L1, L2}) ->
		      CFG = trie:cfg(oracles),
		      Leaf1 = leaf:get(L1, CFG),
		      Orders1 = leaf:meta(Leaf1),
		      Leaf2 = leaf:get(L2, CFG),
		      Orders2 = leaf:meta(Leaf2),
		      F(Orders1, Orders2, orders)
	      end, O),
    lists:map(fun({L1, L2}) ->
		      CFG = trie:cfg(accounts),
		      Leaf1 = leaf:get(L1, CFG),
		      Bets1 = leaf:meta(Leaf1),
		      Leaf2 = leaf:get(max(1, L2), CFG),
		      Bets2 = leaf:meta(Leaf2),
		      F(Bets1, Bets2, oracle_bets)
	      end, A),
    ok.

internal_dict_update_trie(Trees, Dict) ->
    %do the orders and oracle_bets last, then insert their state roots into the accounts and oracles.
    %pointers are integers, root hashes are binary.
    Keys = dict:fetch_keys(Dict),
    {Orders0, Keys2} = get_things(orders, Keys),
    PS = constants:pubkey_size()*8,
    Orders = lists:sort(fun({orders, Keya}, {orders, Keyb}) -> 
				<<A:PS>> = Keya#key.pub,
				<<B:PS>> = Keyb#key.pub,
				B < A
			end, Orders0),
    {OracleBets, Keys3} = get_things(oracle_bets, Keys2),
    {Accounts, Keys4} = get_things(accounts, Keys3),
    
    {Oracles, Keys5} = get_things(oracles, Keys4),
    OrdersLeaves = dict_update_trie_orders(Trees, Orders, Dict, []),
    %{leaf, key, val, meta}
    Dict2 = orders_batch_update(OrdersLeaves, Dict, trees:oracles(Trees)),%Dict20 should be the same as Dict2, but we don't use orders:head_put or orders:write to calculate it.
    OBLeaves = dict_update_trie_oracle_bets(Trees, OracleBets,Dict2, []),
    Dict3 = oracle_bets_batch_update(OBLeaves, Dict2, trees:accounts(Trees)),
    AccountLeaves = dict_update_trie_account(Trees, Accounts, Dict3, []),
    AT = trees:accounts(Trees),
    AT2 = trie:put_batch(AccountLeaves, AT, accounts),
    Trees4 = trees:update_accounts(Trees, AT2),

    OracleLeaves = dict_update_trie_oracles(Trees, Oracles, Dict3, []),
    OT = trees:oracles(Trees4),
    OT2 = trie:put_batch(OracleLeaves, OT, oracles),
    Trees5 = trees:update_oracles(Trees4, OT2),
    %We need to sort the keys to update each trie one at a time.

    {Channels, Keys6} = get_things(channels, Keys5),
    CT = trees:channels(Trees5),
    ChannelsLeaves = keys2leaves(Channels, channels, Dict3),
    CT2 = trie:put_batch(ChannelsLeaves, CT, channels),
    Trees6 = trees:update_channels(Trees5, CT2),

    {Ex, Keys7} = get_things(existence, Keys6),
    ET = trees:existence(Trees6),
    ExistenceLeaves = keys2leaves(Ex, existence, Dict3),
    ET2 = trie:put_batch(ExistenceLeaves, ET, existence),
    Trees7 = trees:update_existence(Trees6, ET2),

    {Gov, []} = get_things(governance, Keys7),
    GT = trees:governance(Trees7),
    GovernanceLeaves = keys2leaves(Gov, governance, Dict3),
    GT2 = trie:put_batch(GovernanceLeaves, GT, governance),
    trees:update_governance(Trees7, GT2).

keys2leaves([], _, _) -> [];
keys2leaves([H|T], Type, Dict) ->
    {Type, Key} = H,
    New = Type:dict_get(Key, Dict),
    I = Type:key_to_int(Key),
    L = case New of
	    empty -> leaf:new(I, empty, 0, trie:cfg(Type));
	    _ ->
		Value = Type:serialize(New),
		leaf:new(I, Value, 0, trie:cfg(Type))
	end,
    [L|keys2leaves(T, Type, Dict)].
dict_update_trie_oracles(_, [], _, X) -> X;
dict_update_trie_oracles(Trees, [H|T], Dict, X) ->
    X2 = dict_update_account_oracle_helper(oracles, H, orders, Trees, orders:empty_book(), set_orders, Dict, X),
    dict_update_trie_oracles(Trees, T, Dict, X2).
dict_update_trie_account(_, [], _, X) -> X;
dict_update_trie_account(Trees, [H|T], Dict, X) ->
    X2 = dict_update_account_oracle_helper(accounts, H, bets, Trees, constants:root0(), update_bets, Dict, X),
    dict_update_trie_account(Trees, T, Dict, X2).

dict_update_account_oracle_helper(Type, H, Type2, Trees, EmptyType2, UpdateType2, Dict, Leaves) ->
    {_, Key} = H,
    New0 = Type:dict_get(Key, Dict),
    Tree = trees:Type(Trees),
    Leaves2 = 
        case New0 of
            empty -> 
		L = leaf:new(Type:key_to_int(Key), empty, 0, trie:cfg(Type)),
                [L|Leaves];
            _ -> 
                ABN = Type:Type2(New0),
                {_, Old, _} = Type:get(Key, trees:Type(Trees)),
                New = if
                          Old == empty -> 
                              Type:UpdateType2(New0, EmptyType2);
                          true ->
                              ABO = Type:Type2(Old),%pointer to bets/orders
                              if
                                  ABO == 0 -> 
                                      throw("dict update trie account oracle. Meta should not be 0 now."),
                                      New0;
                                  0 == ABN -> 
                                      Type:UpdateType2(New0, Type:Type2(Old));
                                  true -> New0
                              end
                      end,
		Meta = Type:meta_get(New),
		L = leaf:new(Type:key_to_int(Key), Type:serialize(New), Meta, trie:cfg(Type)),
                [L|Leaves]
    end,
    %Update = list_to_atom("update_" ++ atom_to_list(Type)),
    Leaves2.
dict_update_trie_orders(_, [], D, L) -> L;
dict_update_trie_orders(Trees, [H|T], Dict, L) ->
    {orders, Key} = H,
    {key, Pub, OID} = Key,
    PS = constants:pubkey_size()*8,
    case Pub of
        <<0:PS>> -> throw(dict_update_trie_orders_error);
        _ -> ok
    end,
    Leaf = 
        case Pub of
            <<1:PS>> ->
                %update the header.
                S = dict:fetch(H, Dict),
		PS = constants:pubkey_size() * 8,
		ID = orders:key_to_int(<<1:PS>>),%1 is Header constant from orders.erl
		leaf:new(ID, S, 0, trie:cfg(orders));
            _ ->
                New2 = 
                    case orders:dict_get(Key, Dict) of
                        empty -> empty;
                        New -> orders:serialize(New)
                    end,
		ID = orders:key_to_int(Pub),
		leaf:new(ID, New2, 0, trie:cfg(orders))
        end,
    dict_update_trie_orders(Trees, T, Dict, [{OID, Leaf}|L]).
dict_update_trie_oracle_bets(_, [], _, L) -> L;
dict_update_trie_oracle_bets(Trees, [H|T], Dict, L) ->
    {oracle_bets, Key} = H,
    {key, Pub, OID} = Key,
    New = oracle_bets:dict_get(Key, Dict),
    ID = oracle_bets:key_to_int(OID),
    New2 = case New of
	       empty -> empty;
	       _ -> oracle_bets:serialize(New)
	   end,
    Leaf = leaf:new(ID, New2, 0, trie:cfg(oracle_bets)),
    dict_update_trie_oracle_bets(Trees, T, Dict, [{Pub, Leaf}|L]).
get_things(Key, L) ->
    get_things(Key, L, [], []).
get_things(Key, [], A, B) -> {A, B};
get_things(Key, [{Key, X}|L], A, B) ->
    get_things(Key, L, [{Key, X}|A], B);
get_things(Key, [{Key2, X}|L], A, B) ->
    get_things(Key, L, A, [{Key2, X}|B]).
oracle_bets_batch_update([], Dict, _) -> Dict;
oracle_bets_batch_update([X|T], Dict, Accounts) ->
    {ID, L} = X,
    {B, R} = lists:partition(
	       fun({ID2, L2}) -> ID2 == ID end,
	       [X|T]),
    Dict2 = oracle_bets_batch_update2(ID, B, Dict, Accounts),
    oracle_bets_batch_update(R, Dict2, Accounts).
oracle_bets_batch_update2(ID, B, Dict, Accounts) ->
    Acc = accounts:dict_get(ID, Dict),
    Bets = case Acc#acc.bets of
	       0 ->
		   {_, Acc2, _} = accounts:get(ID, Accounts),
		   Acc2#acc.bets;
	       G -> G
	   end,
    false = Bets == 0,
    B2 = lists:map(fun({_, X}) -> X end, B),
    OracleBets2 = trie:put_batch(B2, Bets, oracle_bets),
    accounts:dict_write(Acc, OracleBets2, Dict).
orders_batch_update([], Dict, _) -> Dict;
orders_batch_update([X|T], Dict, Oracles) ->
    {OID, L} = X,
    %Val = orders:deserialize(leaf:value(L)),
    {B, R} = lists:partition(
	       fun({OID2, L2}) -> OID2 == OID end, 
	       [X|T]),
    Dict2 = orders_batch_update2(OID, B, Dict, Oracles),
    orders_batch_update(R, Dict2, Oracles).
orders_batch_update2(OID, L, Dict, Oracles) ->
    %all these orders have the same pubkey
    DO = oracles:dict_get(OID, Dict),
    Orders = case DO#oracle.orders of
		 0 -> 
		     {_, Oracle, _}=oracles:get(OID, Oracles),
		     Oracle#oracle.orders;
		 G -> G
	     end,
    %Orders = DO#oracle.orders,
    false = Orders == 0,
    L2 = lists:map(fun({_, X}) -> X end, L),
    Orders2 = trie:put_batch(L2, Orders, orders),
    oracles:dict_write(DO, Orders2, Dict).
   



    
