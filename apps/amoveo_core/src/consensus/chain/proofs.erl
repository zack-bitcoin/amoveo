-module(proofs).
-export([prove/2, test/0, hash/1, facts_to_dict/2, txs_to_querys/3, 
         root/1, tree/1, path/1, value/1, governance_to_querys/1,
         key/1]).
-define(Header, 1).
-record(proof, {tree, value, root, key, path}).
-include("../../records.hrl").

root(X) -> X#proof.root.
key(X) -> X#proof.key.
value(X) -> X#proof.value.
path(X) -> X#proof.path.
tree(X) -> int_to_tree(X#proof.tree).

%we use a deterministic merge sort that removes repeats while sorting.
tree_to_int(accounts) -> 1;
tree_to_int(channels) -> 2;
tree_to_int(existence) -> 3;
tree_to_int(oracles) -> 5;
tree_to_int(governance) -> 6;
tree_to_int(oracle_bets) -> 7;%
tree_to_int(orders) -> 8;%
tree_to_int(multi_tx) -> 9;
tree_to_int(matched) -> 10;
tree_to_int(unmatched) -> 11.

int_to_tree(1) -> accounts;
int_to_tree(2) -> channels;
int_to_tree(3) -> existence;
int_to_tree(5) -> oracles;
int_to_tree(6) -> governance;
int_to_tree(7) -> oracle_bets;%
int_to_tree(8) -> orders;%
int_to_tree(9) -> multi_tx;
int_to_tree(10) -> matched;
int_to_tree(11) -> unmatched.
    

%deterministic merge-sort    
compare({Ta, Ka}, {Tb, Kb}) ->
    T0 = tree_to_int(Ta),
    T1 = tree_to_int(Tb),
    if
        T0 < T1 -> true;
	T0 > T1 -> false;
	Ka < Kb -> true;
	Ka > Kb -> false;
	true -> repeat
    end.
merge([], []) -> [];
merge([], T) -> T;
merge(T, []) -> T;
merge([A|B], [S|T]) ->
    C = compare(A, S),
    case C of
	repeat -> [A|merge(B, T)];
	true -> [A|merge(B, [S|T])];
	false -> [S|merge([A|B], T)]
    end.
det_improve([]) -> [];
det_improve([A]) -> [A];
det_improve([A|[B|T]]) ->
    [merge(A, B)|det_improve(T)].
det_helper([]) -> [];
det_helper([A]) -> A;
det_helper(L) ->
    det_helper(det_improve(L)).
to_lists([]) -> [];
to_lists([A|T]) -> [[A]|to_lists(T)].
det_order(Querys) ->    
    F = to_lists(Querys),
    det_helper(F).
%finished defining merge-sort.       

prove(Querys, Trees) ->
    F2 = det_order(Querys),
    prove2(F2, Trees).
prove2([], _) ->
    [];
prove2([{orders, Key}|T], Trees) ->%
    Oracles = trees:oracles(Trees),%
    {_, Data0, _} = oracles:get(Key#key.id, Oracles),%
    OrdersTree = %
        if%
            Data0 == empty ->%
                orders:empty_book();%
            true ->%
                oracles:orders(Data0)%
        end,%
    {Root, Data, Path} = orders:get(Key#key.pub, OrdersTree),%
    Data2 = case Data of%
		empty -> 0;%
		_ -> orders:serialize(Data)%
	    end,%
    Proof = #proof{root = Root,%
		   key = Key,%
		   path = Path,%
		   value = Data2,%
		   tree = tree_to_int(orders)},%
    true = orders:verify_proof(Root, Key#key.pub, Data2, Path),%
    [Proof|prove2(T, Trees)];%
prove2([{oracle_bets, Key}|T], Trees) ->%
    Accounts = trees:accounts(Trees),%
    {_, Data0, _} = accounts:get(Key#key.pub, Accounts),%
    OrdersTree = Data0#acc.bets,%
    {Root, Data, Path} = oracle_bets:get(Key#key.id, OrdersTree),%
    Data2 = case Data of%
		empty -> 0;%
		_ -> oracle_bets:serialize(Data)%
	    end,%
    Proof = #proof{root = Root,%
		   key = Key,%
		   path = Path,%
		   value = Data2,%
		   tree = tree_to_int(oracle_bets)},%
    true = oracle_bets:verify_proof(Root, Key#key.id, Data2, Path),%
    [Proof|prove2(T, Trees)];%
prove2([{Tree, Key}|T], Trees) ->
    Branch = trees:Tree(Trees),
    {Root, Data, Path} = Tree:get(Key, Branch),
    Data2 = case Data of
		empty -> 0;
		_ -> Tree:serialize(Data)
	    end,
    Proof = #proof{root = Root,
		  key = Key,
		  path = Path, 
		  value = Data2,
		  tree = tree_to_int(Tree)},
    true = Tree:verify_proof(Root, Key, Data2, Path),
    [Proof|prove2(T, Trees)].
facts_to_dict([], D) -> D;
facts_to_dict([F|T], D) ->
    Tree = int_to_tree(F#proof.tree),
    Key2 = 
        case Tree of%
            orders -> F#proof.key#key.pub;%
            oracle_bets -> F#proof.key#key.id;%
	    _ -> F#proof.key
	end,
    true = 
        Tree:verify_proof(
          F#proof.root,
          Key2,
          F#proof.value,
          F#proof.path),
    Key = F#proof.key,
    Value0 = F#proof.value,
    Value3 = case Tree of
                 accounts -> {Value0, 0};
                 oracles -> {Value0, 0};
                 _ -> Value0
            end,
    D2 = dict:store({Tree, Key}, Value3, D),
    facts_to_dict(T, D2).
hash(F) ->
    hash:doit(F).
governance_to_querys(Gov) ->
    Leaves = trie:get_all(Gov, governance),
    Keys = leaves_to_querys(Leaves).
leaves_to_querys([]) -> [];
leaves_to_querys([L|T]) ->
    Q = {governance, leaf:key(L)},
    [Q|leaves_to_querys(T)].
-define(n2i(X), governance:name2number(X)).
txs_to_querys([C|T], Trees, Height) -> 
    case element(1, C) of
        coinbase ->
            [
             {governance, ?n2i(block_reward)},
             {governance, ?n2i(developer_reward)},
             {accounts, constants:master_pub()},
             {accounts, coinbase_tx:from(C)}
            ] ++
                txs_to_querys2(T, Trees, Height);
        signed -> txs_to_querys2([C|T], Trees, Height)
    end.
txs_to_querys2([], _, _) -> [];
txs_to_querys2([STx|T], Trees, Height) ->
    F10 = Height > forks:get(10),
    Tx = testnet_sign:data(STx),
    PS = constants:pubkey_size() * 8,
    L = case element(1, Tx) of
	    multi_tx ->
		txs_to_querys_multi(multi_tx:txs(Tx)) ++
		    [ 
		      {accounts, multi_tx:from(Tx)}
		     ];
	    create_acc_tx -> 
                [
                 {governance, ?n2i(create_acc_tx)},
                 {accounts, create_account_tx:pubkey(Tx)},
                 {accounts, create_account_tx:from(Tx)}
                ];
	    spend -> 
                [
                 {governance, ?n2i(spend)},
                 {accounts, spend_tx:from(Tx)},
                 {accounts, spend_tx:to(Tx)}
                ];
	    delete_acc_tx -> 
                [
                 {governance, ?n2i(delete_acc_tx)},
                 {accounts, delete_account_tx:from(Tx)},
                 {accounts, delete_account_tx:to(Tx)}
                  ];
            nc_accept ->
                io:fwrite(packer:pack(Tx)),
                io:fwrite("\n"),
                [
                 {governance, ?n2i(nc)},
                 {accounts, new_channel_tx2:acc1(Tx)},
                 {accounts, new_channel_tx2:acc2(Tx)},
                 {channels, new_channel_tx2:cid(Tx)}
                ];
            nc ->
                [
                 {governance, ?n2i(nc)},
                 {accounts, new_channel_tx:acc1(Tx)},
                 {accounts, new_channel_tx:acc2(Tx)},
                 {channels, new_channel_tx:cid(Tx)}
                ];
	    gc -> 
                [
                 {governance, ?n2i(gc)},
                 {accounts, grow_channel_tx:acc1(Tx)},
                 {accounts, grow_channel_tx:acc2(Tx)},
                 {channels, grow_channel_tx:id(Tx)}
                ];
	    ctc -> 
                [
                 {governance, ?n2i(ctc)},
                 {accounts, channel_team_close_tx:aid1(Tx)},
                 {accounts, channel_team_close_tx:aid2(Tx)},
                 {channels, channel_team_close_tx:id(Tx)}
                ];
	    csc -> 
                [
                 {governance, ?n2i(csc)},
                 {governance, ?n2i(time_gas)},
                 {governance, ?n2i(space_gas)},
                 {governance, ?n2i(fun_limit)},
                 {governance, ?n2i(var_limit)},
                 {accounts, channel_solo_close:from(Tx)},
                 {channels, channel_solo_close:id(Tx)}
                ];
	    timeout -> 
                [
                 {governance, ?n2i(timeout)},
                 {accounts, channel_timeout_tx:spk_aid1(Tx)},
                 {accounts, channel_timeout_tx:spk_aid2(Tx)},
                 {channels, channel_timeout_tx:cid(Tx)}
                ];
	    cs -> 
                [
                 {governance, ?n2i(time_gas)},
                 {governance, ?n2i(space_gas)},
                 {governance, ?n2i(fun_limit)},
                 {governance, ?n2i(var_limit)},
                 {governance, ?n2i(cs)},
                 {accounts, channel_slash_tx:from(Tx)},
                 {channels, channel_slash_tx:id(Tx)}
                ];
	    ex -> 
                [
                 {governance, ?n2i(ex)},
                 {accounts, existence_tx:from(Tx)},
                 {existence, existence_tx:commit(Tx)}
                ];
	    oracle_new -> 
                OID = oracle_new_tx:id(Tx),
                AID = oracle_new_tx:from(Tx),
		N2IOIL = ?n2i(oracle_initial_liquidity),
                G = case oracle_new_tx:governance(Tx) of
                        0 -> 
			    FH5 = forks:get(5),%
			    B = FH5 < Height,%
			    %B = false,%
			    OILK = if%
				       B -> ?n2i(oracle_question_liquidity);
				       true -> N2IOIL%
				   end,%
			    [{governance, OILK}];
                        N -> [{governance, N2IOIL},
			      {governance, N}]
                    end,
		U = if
			F10 -> 
			    PS = constants:pubkey_size() * 8,
			    [{unmatched, {key, <<1:PS>>, OID}}];
			true -> []
		    end,
                [
                 {governance, ?n2i(oracle_new)},
                 {governance, ?n2i(governance_change_limit)},
                 {governance, ?n2i(maximum_question_size)},
                 {governance, ?n2i(minimum_oracle_time)},
                 {accounts, AID},
                 {oracles, OID}
                ] ++ G;
	    oracle_bet -> 
                OID = oracle_bet_tx:id(Tx),
                Pubkeys = [oracle_bet_tx:from(Tx)|
                           oracle_bet_tx:to_prove(OID, Trees)],
                Pubkeys2 = remove(<<?Header:PS>>, Pubkeys),
                Prove = tagify(accounts, Pubkeys) ++ 
                    make_oracle_bets(Pubkeys2, OID, F10) ++
                    make_orders(Pubkeys, OID, F10),
		U = if%
			F10 -> {unmatched, #key{pub = <<?Header:PS>>, id = OID}};
		    true -> {orders, #key{pub = <<?Header:PS>>, id = OID}}%
		    end,%
		[
		 {governance, ?n2i(oracle_bet)},
		 {governance, ?n2i(minimum_oracle_time)},
		 {governance, ?n2i(oracle_initial_liquidity)},
		 {oracles, OID}] ++ [U] ++ Prove;
	    oracle_close -> 
                AID = oracle_close_tx:from(Tx),
                OID = oracle_close_tx:oracle_id(Tx),
                Oracles = trees:oracles(Trees),
                {_, Oracle, _} = oracles:get(OID, Oracles),
                Gov = Oracle#oracle.governance,
                G = case Gov of
                        0 -> [];
                        _ -> [{governance, Gov}]
                    end,
                From = oracle_close_tx:from(Tx),
                Pubkeys = [From|
                           oracle_bet_tx:to_prove(OID, Trees)],
                Pubkeys2 = remove(<<?Header:PS>>, Pubkeys),
                Prove = tagify(accounts, Pubkeys) ++ %this should probably be Pubkeys2
                    make_oracle_bets(Pubkeys2, OID, F10) ++
                    make_orders(Pubkeys, OID, F10),
		U = if%
			F10 -> [{unmatched, #key{pub = <<?Header:PS>>, id = OID}},
				{matched, #key{pub = Oracle#oracle.creator, id = OID}}];
			true ->%
			    [{orders, #key{pub = <<?Header:PS>>, id = OID}},%
			     {oracle_bets, #key{pub = Oracle#oracle.creator, id = OID}}]%
		    end,%
		[
                 {governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(maximum_oracle_time)},
                 {governance, ?n2i(oracle_close)},
                 {governance, ?n2i(oracle_initial_liquidity)},
                 {governance, ?n2i(oracle_bet)},
                 {oracles, OID}
                ] ++ U ++ Prove ++ G;
	    unmatched -> 
                OID = oracle_unmatched_tx:oracle_id(Tx),
                From = oracle_unmatched_tx:from(Tx),
		U = if %
			F10 -> [{unmatched, #key{pub = <<?Header:PS>>, id = OID}},
				{unmatched, #key{pub = From, id = OID}}];
			true -> [{orders, #key{pub = <<?Header:PS>>, id = OID}},%
				 {orders, #key{pub = From, id = OID}}]%
		    end,%
		[
                 {governance, ?n2i(unmatched)},
                 {accounts, From},
                 {oracles, OID}
                ] ++ U;
	    oracle_winnings -> 
                OID = oracle_winnings_tx:oracle_id(Tx),
                From = oracle_winnings_tx:from(Tx),
		U = if%
			F10 -> [{matched, #key{pub = From, id = OID}}];
			true ->[{oracle_bets, #key{pub = From, id = OID}}]%
		    end,%
		[{governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(oracle_winnings)},
                 {accounts, From},
                 {oracles, OID}
                ] ++ U;
	    coinbase_old -> 
                [
                 {governance, ?n2i(block_reward)},
                 {governance, ?n2i(developer_reward)},
                 {accounts, constants:master_pub()},
                 {accounts, coinbase_tx:from(Tx)}
                ]
	end,
    L ++ txs_to_querys2(T, Trees, Height).
txs_to_querys_multi([]) -> [];
txs_to_querys_multi([Tx|T]) ->
    PS = constants:pubkey_size() * 8,
    L = case element(1, Tx) of
	    create_acc_tx -> 
                [
                 {governance, ?n2i(create_acc_tx)},
                 {accounts, create_account_tx:pubkey(Tx)}
                ];
	    spend -> 
                [
                 {governance, ?n2i(spend)},
                 {accounts, spend_tx:to(Tx)}
                ]
	end,
    L ++ txs_to_querys_multi(T).

	    
    
remove(_, []) -> [];
remove(X, [X|A]) -> remove(X, A);
remove(X, [Y|A]) -> [Y|remove(X, A)].
tagify(_, []) -> [];
tagify(X, [H|T]) ->
    [{X, H}|tagify(X, T)].
make_oracle_bets([], _, _) -> [];
make_oracle_bets([H|T], OID, F10) ->
    A = if%
	    F10 -> {matched, #key{pub = H, id = OID}};
	    true -> {oracle_bets, #key{pub = H, id = OID}}%
	end,%
    [A|make_oracle_bets(T, OID, F10)].
make_orders([], _, _) -> [];
make_orders([H|T], OID, F10) ->
    A = if%
	    F10 -> {unmatched, #key{pub = H, id = OID}};
	    true -> {orders, #key{pub = H, id = OID}}%
	end,%
     [A|make_orders(T, OID, F10)].
test() ->
    headers:dump(),
    %block:initialize_chain(),
    tx_pool:dump(),
    test_txs:mine_blocks(2),
    timer:sleep(150),
    Question = <<>>,
    OID = <<2:256>>,
    Fee = 20 + constants:initial_fee(),
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), OID, 0, 0),
    %{Tx, _} = oracle_new_tx:make(constants:master_pub(), Fee, Question, 1, OID, 0, 0, Trees0),
    tx_pool_feeder:absorb(keys:sign(Tx)),
    test_txs:mine_blocks(1),
    timer:sleep(200),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Pub2 = <<"BL6uM2W6RVAI341uFO7Ps5mgGp4VKZQsCuLlDkVh5g0O4ZqsDwFEbS9GniFykgDJxYv8bNGJ+/NdrFjKV/gJa6c=">>,
    Pub3 = <<"BIG0bGOtCeH+ik2zxohHNOHyydjzIfi2fhKwFCZ0TFh99y+C8eiwHWwWkFrfGtEL7HcKP+5jdQmRc6wfnG32wlc=">>,
    {Pub55, _} = testnet_sign:new_key(),
    PS = constants:pubkey_size() * 8,
    Querys = [{accounts, keys:pubkey()},
	      {accounts, keys:pubkey()},%repeats are ignored
	      {accounts, base64:decode(Pub2)},%empty account
              {accounts, Pub55},
              {accounts, Pub3},
              {accounts, <<297:520>>},
              {accounts, <<744:520>>},
	      {governance, block_reward},
	      {governance, 1},
	      {channels, <<1:256>>},
	      {existence, hash:doit(1)},
	      {oracles, OID},
	      {oracles, <<1:256>>},
	      {unmatched, #key{pub = keys:pubkey(), id = OID}},
              {matched, #key{pub = keys:pubkey(), id = OID}}
	      %{orders, #key{pub = keys:pubkey(), id = OID}},
              %{oracle_bets, #key{pub = keys:pubkey(), id = OID}}
	     ],% ++
        %governance_to_querys(trees:governance(Trees)),
    Facts = prove(Querys, Trees),
    ProofRoot = hash(Facts),
    Dict = facts_to_dict(Facts, dict:new()), %when processing txs, we use this dictionary to look up the state.
    Querys2 = dict:fetch_keys(Dict),
    Facts = prove(Querys2, Trees),
    Dict,
    
    ETxs = "g2wAAAAEaARkAAZzaWduZWRoBmQAAmNhbQAAAEEEhVmGzXqC2hD+5Qy6OXlpK62kiYLi9rwx7CAK96HowS4OOgO+1CphnkV5hxSFj9AuOkIGteOq9O0WI3iWLQ2GOmEBYRRtAAAAQQRHXAXlfMl3JIv7Ni5NmiaAhuff/NsmnCCnWElvuaemWoQ2aCFJzogO/dHY9yrDUsIHaqtS+iD1OW3KuPrpBgoCYjuaygBtAAAAYE1FVUNJUUR5Q0p1Y2h6TlEzUXBkbTk4VjFkWGNxQklEUjVlNDFoRWtlMGRvUkVNd2hBSWdKbjcza3hISzhNUXZDVUttcGEzbzRSWkJYR3FoMXNWV2NZZXNyQ3NRVlo4PWpoBGQABnNpZ25lZGgGZAACY2FtAAAAQQSFWYbNeoLaEP7lDLo5eWkrraSJguL2vDHsIAr3oejBLg46A77UKmGeRXmHFIWP0C46Qga146r07RYjeJYtDYY6YQJhFG0AAABBBFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+FiO5rKAG0AAABgTUVVQ0lRRG1naWwvSkxGRVJaN05LUEpZMHZFQ21nZUlsNFdkdU5SbmlzWkw2R25ZVFFJZ1dBOExUazNENEVva3EvWUY4U3d4SnljR1Ixd2RLejlRMWpJUmpyeEFzSDQ9amgEZAAGc2lnbmVkaApkAAJuY20AAABBBIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0NhjptAAAAQQRUY7goLnUk0VO/UlaAgb1ouefjdUHrYb0COiylAqBDx7yH3nZEC8Z7g9lSR6qC61C5l8nsUVrNrDk8Mi7Hci/hYTJhA2IAACcQYgAAJxFhAmEEYQFtAAAAYE1FWUNJUUQ4U1hNeUYxQmRnbWRaRVdHbWFFR3JncXRxTXUvRGZJYmZVMnE1eE94ZUdnSWhBTTU3L21wcmFucDdiVTBSK2RoMS9wZjBOeHViVWJIU256UEFrcFY5b1gwNW0AAABgTUVVQ0lIeTdhenJyYmxIdzdSdEVmRVRMcU5ERTdCUUhmb1Rnd29CVHlZV0JKcHd0QWlFQWxPcnRhY1k1NVFSNUZUVUpoVFltbW5TWldtSGZ4cFUvbmExbjJsSVhJdm89aARkAAZzaWduZWRoCmQAAm5jbQAAAEEER1wF5XzJdySL+zYuTZomgIbn3/zbJpwgp1hJb7mnplqENmghSc6IDv3R2Pcqw1LCB2qrUvog9Tltyrj66QYKAm0AAABBBFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+FhMmEBYgAAJxBiAAAnEWECYQRhAm0AAABgTUVRQ0lCZHlWUUhxRlZyQWFGMTVsN0NmajlyckU5THI3RFFUWVJrc3c5d3dMek1nQWlBOGZrMXpIVVgwdlN6b0dVQ05JTGRmRER5Y2lNMnlWVldLb0pnTGNUbUZhdz09bQAAAGBNRVFDSUJvV3pJQU9oUExqTXJjN0tnV3ZFOUxhWmdXdllqYTY0Mk10YzE0S3RFdXNBaUFhRktDTmNhQUFSck9NUVNCUmZMKzdPV054aHduaWdwRUZBc1JaL0c3MmVBPT1q",
    %Txs = binary_to_term(base64:decode(ETxs)),
    {Pub30, Priv30} = testnet_sign:new_key(),
    {Pub4, _} = testnet_sign:new_key(),
    NewTx = create_account_tx:make_dict(Pub30, 10, 10, keys:pubkey()),
    NewTx2 = create_account_tx:make_dict(Pub4, 10, 10, keys:pubkey()),
    CID = <<7:256>>,
    NewTx3 = new_channel_tx:make_dict(CID, keys:pubkey(), Pub3, 1, 1, 1, 1),
    Txs = [keys:sign(NewTx),
           keys:sign(NewTx2),
           testnet_sign:sign_tx(NewTx3, Pub3, Priv30)],
    Q2 = txs_to_querys2(Txs, Trees, 1),
    prove(Q2, Trees),
    success.
    
    

