-module(tree_data).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         dict_update_trie/5, dict_update_root/5, 
         garbage/2, remove_before/2]).
-include("../../records.hrl").
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({remove_before, Blocks, Work}, _, _) -> 
    B2 = remove_before_internal(Blocks, Work),
    {reply, B2, []};
handle_call({garbage, Trash, Keep}, _, _) -> 
    internal(Trash, Keep, fun(A, B, C) -> trie:garbage(A, B, C) end),
    {reply, ok, []};
%handle_call({prune, Trash, Keep}, _, _) -> 
%    internal(Trash, Keep, fun(A, B, C) -> trie:prune(A, B, C) end),
%    {reply, ok, []};
handle_call({update_root, Trees, Dict, Height, ProofTree, RootHash}, _, _) ->
    Y = internal_dict_update_root(
          Trees, Dict, Height, ProofTree, RootHash),
    {reply, Y, []};
handle_call({update, Trees, Dict, Height, ProofTree, RootHash}, 
            _From, _) -> 
    Y = internal_dict_update_trie(
          Trees, Dict, Height, ProofTree, RootHash),
    {reply, Y, []};
handle_call(_, _From, X) -> {reply, X, X}.

remove_before(Blocks, Work) ->
    gen_server:call(?MODULE, {remove_before, Blocks, Work}).
dict_update_root(Trees, Dict, Height, ProofTree, RootHash) ->
    gen_server:call(?MODULE, {update_root, Trees, Dict, Height, ProofTree, RootHash}).
dict_update_trie(Trees, Dict, Height, ProofTree, RootHash) ->
    gen_server:call(?MODULE, {update, Trees, Dict, Height, ProofTree, RootHash}).
garbage(Trash, Keep) ->
    gen_server:call(?MODULE, {garbage, Trash, Keep}).
internal(PruneBlock, KeepBlock, F) ->
    TA = [accounts, oracles, channels, existence, governance],
    TB = TA ++ [matched, unmatched],
    TC = TB ++ [sub_accounts, contracts, trades],
    TD = TC ++ [markets],
    TE = TD ++ [receipts, stablecoins],
    T1 = PruneBlock#block.trees,
    T2 = KeepBlock#block.trees,
    Trees = case element(1, T1) of%
		trees -> TA;%
		trees2 -> TB;
                trees3 -> TC;
                trees4 -> TD;
                trees5 -> TE
	    end,%
    _ = 
	lists:map(fun(T) ->
			  T1 = PruneBlock#block.trees,
			  T2 = KeepBlock#block.trees,
			  A1 = trees:T(T1),
			  A2 = trees:T(T2),
			  F(A1, A2, T)
		  end, Trees),
    ok.
order_sorter({orders, Keya}, {orders, Keyb}) ->%
    PS = constants:pubkey_size()*8,%
    <<A:PS>> = Keya#key.pub,%
    <<B:PS>> = Keyb#key.pub,%
    B < A.%
internal_dict_update_trie(Trees, Dict, _, _, _) when (element(1, Trees) == trees) ->%
    %do the orders and oracle_bets last, then insert their state roots into the accounts and oracles.
    %pointers are integers, root hashes are binary.

    %we updated dict. it used to be liked Pubkey -> account_object.
    %now it is like hash_of_pubkey -> #consensus_state object.

    Keys = dict:fetch_keys(Dict),%
    {Orders0, Keys2} = get_things(orders, Keys, Dict),%
    Orders = lists:sort(fun(A, B) -> order_sorter(A, B) end,%
			Orders0),%
    {OracleBets, Keys3} = get_things(oracle_bets, Keys2, Dict),%
    {Accounts, Keys4} = get_things(accounts, Keys3, Dict),%
    {Oracles, Keys5} = get_things(oracles, Keys4, Dict),%
    OrdersLeaves = dict_update_trie_orders(Trees, Orders, Dict, []),%
    %{leaf, key, val, meta}
    Dict2 = orders_batch_update(OrdersLeaves, Dict, trees:oracles(Trees)),%Dict20 should be the same as Dict2, but we don't use orders:head_put or orders:write to calculate it.
    OBLeaves = dict_update_trie_oracle_bets(Trees, OracleBets,Dict2, []),%
    Dict3 = oracle_bets_batch_update(OBLeaves, Dict2, trees:accounts(Trees)),%
    AccountLeaves = dict_update_trie_account(Trees, Accounts, Dict3, []),%
    AT = trees:accounts(Trees),%
    AT2 = trie:put_batch(AccountLeaves, AT, accounts),%
    Trees4 = trees:update_accounts(Trees, AT2),%

    OracleLeaves = dict_update_trie_oracles(Trees, Oracles, Dict3, []),%
    OT = trees:oracles(Trees4),%
    OT2 = trie:put_batch(OracleLeaves, OT, oracles),%
    Trees5 = trees:update_oracles(Trees4, OT2),%
    %We need to sort the keys to update each trie one at a time.
    {Channels, Keys6} = get_things(channels, Keys5, Dict),%
    CT = trees:channels(Trees5),%
    ChannelsLeaves = keys2leaves(Channels, channels, Dict3),%
    CT2 = trie:put_batch(ChannelsLeaves, CT, channels),%
    Trees6 = trees:update_channels(Trees5, CT2),%

    {Ex, Keys7} = get_things(existence, Keys6, Dict),%
    ET = trees:existence(Trees6),%
    ExistenceLeaves = keys2leaves(Ex, existence, Dict3),%
    ET2 = trie:put_batch(ExistenceLeaves, ET, existence),%
    Trees7 = trees:update_existence(Trees6, ET2),%

    {Gov, _Keys8} = get_things(governance, Keys7, Dict),%
    GT = trees:governance(Trees7),%
    GovernanceLeaves = keys2leaves(Gov, governance, Dict3),%
    GT2 = trie:put_batch(GovernanceLeaves, GT, governance),%
    trees:update_governance(Trees7, GT2);%
internal_dict_update_trie(Trees, Dict, H, ProofTree, RootHash) ->
    F52 = forks:get(52), 
    if
        H > F52 -> verkle_dict_update_trie(Trees, Dict, ProofTree, RootHash, H);
        true ->
            Types2 = [accounts, oracles, channels, existence, governance, matched, unmatched],
            Types3 = Types2 ++ [sub_accounts, contracts, trades],
            Types4 = Types3 ++ [markets],
            Types5 = Types4 ++ [receipts, stablecoins],
            Types = 
                case element(1, Trees) of
                    trees2 -> Types2;
                    trees3 -> Types3;
                    trees4 -> Types4;
                    trees5 -> Types5
                end,
            Keys = dict:fetch_keys(Dict),
            idut2(Types, Trees, Dict, Keys)
    end.
internal_dict_update_root(Trees, Dict, ProofTree, RootHash, Height) ->
    F52 = forks:get(52), 
    if
        Height > F52 -> verkle_dict_update_root(Dict, ProofTree, RootHash, Height);
        true ->
            Trees2 = internal_dict_update_trie(Trees, Dict, ProofTree, RootHash, Height),
            RootHash2 = trees:root_hash(Trees2),
            RootHash == RootHash2
    end.
    
verkle_dict_update_root(Dict, ProofTree, RootHash, Height) ->
    Keys = dict:fetch_keys(Dict),
    Leaves = 
        lists:map(
          fun(Key) -> 
                  V = case dict:fetch(Key, Dict) of
                          {V2, _Meta}  -> V2;
                          V3 -> V3
                      end,
                  V
          end, Keys),
    Leaves2 = lists:filter(
                fun(X) -> not(X == 0) end, Leaves),
    Leaves2b = lists:map(
                 fun(#consensus_state{val = X}) -> X;
                    (Y = {unmatched_head, _, _, _}) -> Y end, Leaves2),
    ProofTreeB = trees2:update_proof(Leaves2b, ProofTree),
    RootHash == stem_verkle:hash_point(hd(ProofTreeB)).
verkle_dict_update_trie(Trees, Dict, ProofTree, RootHash, Height) ->
    true = is_integer(Trees),
    Keys = dict:fetch_keys(Dict),
    Leaves = 
        lists:map(
          fun(Key) -> 
                  V = case dict:fetch(Key, Dict) of
                          {V2, _Meta}  -> V2;
                          V3 -> V3
                      end,
                  V
          end, Keys),
    Leaves2 = lists:filter(
                fun(X) -> not(X == 0) end, Leaves),
    %we never delete from the verkle tree, so we can remove anything that seems empty.
    Leaves2b = lists:map(
                 fun(#consensus_state{val = X}) -> X;
                    (Y = {unmatched_head, _, _, _}) -> Y end, Leaves2),
    if
        true -> ok;
        ((Height == 161186)) ->
            io:fwrite({Leaves2b});
        true -> ok;
        ((Height == 8) and (not(ProofTree == unknown))) ->
            io:fwrite({Keys});
        true -> ok;
        ((Height == 13) and (not(ProofTree == unknown))) ->
            io:fwrite({Leaves2b});
        true -> ok
    end,
    Trees3 = case ProofTree of
                 unknown -> 
                     %io:fwrite({Leaves2}),
                     Trees4 = trees2:store_things(Leaves2b, Trees),
                     Trees4;
                 _ ->
                     %ProofTree = dict:fetch(proof, Dict),
                     %io:fwrite("about to update the proof in tree_data\n"),
                     %io:fwrite(integer_to_list(Height)),
                     %io:fwrite("\n"),
                     ProofTreeB = trees2:update_proof(Leaves2b, ProofTree),
                     %io:fwrite("updated the proof in tree_data\n"),
                     RootHash = stem_verkle:hash_point(hd(ProofTreeB)),%verify that the new state root matches what was written on the block.
                     trees2:store_verified(Trees, ProofTreeB)
             end,
    %todo. before we can write the data to the tree, we need to somehow verify that the new verkle root match. or if the roothash is unknown, skip the check.
    %todo. we should calculate the hash first, then store the batch. Writing to the hard drive like this is falsely assuming that we are

    Trees3.

    
%internal_dict_update_trie(Trees, Dict) when (element(1, Trees) == trees2) ->
%    Types = [accounts, oracles, channels, existence, governance, matched, unmatched],
%    Keys = dict:fetch_keys(Dict),
%    idut2(Types, Trees, Dict, Keys);
%internal_dict_update_trie(Trees, Dict) when (element(1, Trees) == trees3)->
%    Types = [accounts, oracles, channels, existence, governance, matched, unmatched, sub_accounts, contracts, trades],
%    Keys = dict:fetch_keys(Dict),
%    idut2(Types, Trees, Dict, Keys);
%internal_dict_update_trie(Trees, Dict) when (element(1, Trees) == trees4)->
%    Types = [accounts, oracles, channels, existence, governance, matched, unmatched, sub_accounts, contracts, trades, markets],
%    Keys = dict:fetch_keys(Dict),
%    idut2(Types, Trees, Dict, Keys).

idut2([], Trees, _, _) -> Trees;
idut2([H|Types], Trees, Dict, Keys) ->
    %{sharding, full_node},
    {A, Keys2} = get_things(H, Keys, Dict),
    T = trees:H(Trees),
    Leaves = keys2leaves(A, H, Dict),
    {ok, ShardMode} = application:get_env(amoveo_core, sharding),
    case ShardMode of
        full_node ->
            %io:fwrite("tree_data idut2 "),
            %io:fwrite("\n"),
            T2 = trie:put_batch(Leaves, T, H),%returns a pointer to the root of the trie for the new block.
            U = list_to_atom("update_" ++ atom_to_list(H)),
            Trees2 = trees:U(Trees, T2),
            idut2(Types, Trees2, Dict, Keys2);
        light_node -> 
            Trees;
        _ -> 
            io:fwrite("shard mode is: "),
            io:fwrite(ShardMode),
            io:fwrite("\n"),
            1=2
    end.
					       

keys2leaves([], _, _) -> [];
keys2leaves([H|T], Type, Dict) ->
    %only used for the merkle tree, not verkle
    %{Type, Key} = H,
    Key = H,
    L = case csc:read(Key, Dict) of
            {empty, Type, UnhashedKey} ->
                I0 = Type:key_to_int(UnhashedKey),
                leaf:new(I0, empty, 0, trie:cfg(Type));
            {ok, Type, Val} ->
                Val2 = case Val of
                           {X, _} -> X;
                           Y -> Y
                       end,
                I1 = trees2:val2int(Val2),
                SV = Type:serialize(Val2),
                leaf:new(I1, SV, 0, trie:cfg(Type))
             end,
    [L|keys2leaves(T, Type, Dict)].

%    New = Type:dict_get(Key, Dict),
%    I = Type:key_to_int(Key),
%    L = case New of
%	    error -> leaf:new(I, empty, 0, trie:cfg(Type));
%	    empty -> leaf:new(I, empty, 0, trie:cfg(Type));
%	    _ ->
%		Value = Type:serialize(New),
%		leaf:new(I, Value, 0, trie:cfg(Type))
%	end,
%    [L|keys2leaves(T, Type, Dict)].
dict_update_trie_oracles(_, [], _, X) -> X;%
dict_update_trie_oracles(Trees, [H|T], Dict, X) ->%
    X2 = dict_update_account_oracle_helper(oracles, H, orders, Trees, orders:empty_book(), set_orders, Dict, X),%
    dict_update_trie_oracles(Trees, T, Dict, X2).%
dict_update_trie_account(_, [], _, X) -> X;%
dict_update_trie_account(Trees, [H|T], Dict, X) ->%
    R = trees:empty_tree(oracle_bets),
    %H is hash_of_pubkey, not pubkey.
    X2 = dict_update_account_oracle_helper(accounts, H, bets, Trees, R, update_bets, Dict, X),%
    dict_update_trie_account(Trees, T, Dict, X2).%

dict_update_account_oracle_helper(Type, H, Type2, Trees, EmptyType2, UpdateType2, Dict, Leaves) ->%
    %Key = H,
    {ok, #consensus_state{
       empty = false, val = New00, 
       unhashed_key = Key
      }} = csc:read2(H, Dict),
    New0 = case New00 of
               error -> error;
               empty -> error;
               {Val, Meta2} -> Val#acc{bets = Meta2};
               _ -> New00
           end,
    %New0 = Type:dict_get(H, Dict),%
    Tree = trees:Type(Trees),%
    Leaves2 = %
        case New0 of%
            error -> %
		L = leaf:new(Type:key_to_int(Key), empty, 0, trie:cfg(Type)),%
                [L|Leaves];%
            _ -> %
                ABN = Type:Type2(New0),%
                %{_, Old, _} = Type:get(Key, trees:Type(Trees)),%
                %Old = trees:get(Type, Key, dict:new(), Trees),%
                Old = trees:get(Type, H, dict:new(), Trees),%
                New = if%
                          Old == empty -> %
                              Type:UpdateType2(New0, EmptyType2);%
                          true ->%
                              ABO = Type:Type2(Old),%pointer to bets/orders
                              if%
                                  ABO == 0 -> %
                                      throw("dict update trie account oracle. Meta should not be 0 now."),%
                                      New0;%
                                  0 == ABN -> %
                                      Type:UpdateType2(New0, Type:Type2(Old));%
                                  true -> New0%
                              end%
                      end,%
		Meta = Type:meta_get(New),%
		L = leaf:new(Type:key_to_int(Key), Type:serialize(New), Meta, trie:cfg(Type)),%
                [L|Leaves]%
    end,%
    Leaves2.%
dict_update_trie_orders(_, [], D, L) -> L;%
dict_update_trie_orders(Trees, [H|T], Dict, L) ->%
    {orders, Key} = H,%
    {key, Pub, OID} = Key,%
    PS = constants:pubkey_size()*8,%
    case Pub of%
        <<0:PS>> -> throw(dict_update_trie_orders_error);%
        _ -> ok%
    end,%
    Leaf = %
        case Pub of%
            <<1:PS>> ->%
                %update the header.%
                PS = constants:pubkey_size() * 8,%
                ID = orders:key_to_int(<<1:PS>>),%1 is Header constant from orders.erl%
                case csc:read(H, Dict) of
                    error -> 1=2;
                    {empty, _, _} -> 1=2;
                    {ok, orders, {orders_head, Head, Many, _OID}} ->
                        S = orders:serialize_head(Head, Many),
                        leaf:new(ID, S, 0, trie:cfg(orders));
                    {ok, orders, {Head, Many}} ->
                        %S = dict:fetch(H, Dict),
                        S = orders:serialize_head(Head, Many),
                        leaf:new(ID, S, 0, trie:cfg(orders))
                end;
            _ ->%
                New2 = %
                    case orders:dict_get(Key, Dict) of%
                        error -> empty;%
                        empty -> empty;%
                        New -> orders:serialize(New)%
                    end,%
		ID = orders:key_to_int(Pub),%
		leaf:new(ID, New2, 0, trie:cfg(orders))%
        end,%
    dict_update_trie_orders(Trees, T, Dict, [{OID, Leaf}|L]).%
dict_update_trie_oracle_bets(_, [], _, L) -> L;%
dict_update_trie_oracle_bets(Trees, [H|T], Dict, L) ->%
    {oracle_bets, Key} = H,%
    {key, Pub, OID} = Key,%
    New = oracle_bets:dict_get(Key, Dict),%
    ID = oracle_bets:key_to_int(OID),%
    New2 = case New of%
	       error -> empty;%
	       empty -> empty;%
	       _ -> oracle_bets:serialize(New)%
	   end,%
    Leaf = leaf:new(ID, New2, 0, trie:cfg(oracle_bets)),%
    dict_update_trie_oracle_bets(Trees, T, Dict, [{Pub, Leaf}|L]).%

-record(r, {empty = true, val, key, unhashed_key, type}).

get_things(Type0, L, Dict) ->
    get_things(Type0, L, [], [], Dict).
get_things(_, [], Found, Rest, _) -> 
    {Found, Rest};
get_things(Type0, [H|T], A, LeftoverKeys, Dict) ->
    Type = case csc:read(H, Dict) of
               {ok, X, _} -> X;
               {empty, X, _} -> X
           end,
    %{_, Type, _}  = csc:read(H, Dict),
    {A2, LeftoverKeys2} = 
        case Type of
            Type0 -> {[H|A], 
                      LeftoverKeys};
            _ -> {A, [H|LeftoverKeys]}
        end,
    get_things(Type0, T, A2, 
               LeftoverKeys2, Dict).
                

        


get_things(Key, [], Found, Rest) -> {Found, Rest};
get_things(Key, [{Key, X}|L], A, B) ->
    get_things(Key, L, [{Key, X}|A], B);
get_things(Key, [{Key2, X}|L], A, B) ->
    get_things(Key, L, A, [{Key2, X}|B]).
oracle_bets_batch_update([], Dict, _) -> Dict;%
oracle_bets_batch_update([X|T], Dict, Accounts) ->%
    {ID, L} = X,%
    {B, R} = lists:partition(%
	       fun({ID2, L2}) -> ID2 == ID end,%
	       [X|T]),%
    Dict2 = oracle_bets_batch_update2(ID, B, Dict, Accounts),%
    oracle_bets_batch_update(R, Dict2, Accounts).%
oracle_bets_batch_update2(ID, B, Dict, Accounts) ->%
    Acc = accounts:dict_get(ID, Dict),%
    case Acc of%
	empty -> %
	    io:fwrite("tree data oracle bets batch update, account does not exist\n"),%
	    io:fwrite(base64:encode(ID)),%
	    io:fwrite("\n"),%
	    Dict;%
	error -> %
	    io:fwrite("tree data oracle bets batch update, account does not exist\n"),%
	    io:fwrite(base64:encode(ID)),%
	    io:fwrite("\n"),%
	    Dict;%
	_ ->%
	    Bets = case Acc#acc.bets of%
		       0 ->%
			   {_, Acc2, _} = accounts:get(ID, Accounts),%
			   Acc2#acc.bets;%
		       G -> G%
		   end,%
	    false = Bets == 0,%
	    B2 = lists:map(fun({_, X}) -> X end, B),%
	    OracleBets2 = trie:put_batch(B2, Bets, oracle_bets),%
	    accounts:dict_write(Acc, OracleBets2, Dict)%
    end.%
orders_batch_update([], Dict, _) -> Dict;%
orders_batch_update([X|T], Dict, Oracles) ->%
    {OID, L} = X,%
    %Val = orders:deserialize(leaf:value(L)),
    {B, R} = lists:partition(%
	       fun({OID2, L2}) -> OID2 == OID end, %
	       [X|T]),%
    Dict2 = orders_batch_update2(OID, B, Dict, Oracles),%
    orders_batch_update(R, Dict2, Oracles).%
orders_batch_update2(OID, L, Dict, Oracles) ->%
    %all these orders have the same pubkey%
    DO = oracles:dict_get(OID, Dict),%
    Orders = case DO#oracle.orders of%
		 0 -> %
		     {_, Oracle, _}=oracles:get(OID, Oracles),%
		     Oracle#oracle.orders;%
		 G -> G%
	     end,%
    %Orders = DO#oracle.orders,%
    false = Orders == 0,%
    L2 = lists:map(fun({_, X}) -> X end, L),%
    Orders2 = trie:put_batch(L2, Orders, orders),%
    oracles:dict_write(DO, Orders2, Dict).%
   
remove_before_internal([], _) -> [];
remove_before_internal([{Hash, TotalWork}|T], X) when TotalWork < X ->
    KeepBlock = block:get_by_hash(Hash),
    Height = KeepBlock#block.height,
    if
	Height < 2 -> ok;
	true ->
	    H = KeepBlock#block.prev_hash,
	    OldBlock = block:get_by_hash(H),
	    internal(OldBlock, KeepBlock, fun(A, B, C) -> trie:garbage(A, B, C) end)
	    %tree_data:garbage(OldBlock, KeepBlock)
    end,
    remove_before_internal(T, X);
remove_before_internal([H|T], X) -> [H|remove_before_internal(T, X)].



    
