-module(proofs).
-export([prove/2, test/0, hash/1, facts_to_dict/2, txs_to_querys/2, 
         root/1, tree/1, governance_to_querys/1]).
-record(proof, {tree, value, root, key, path}).
-record(key, {pub, id}). %used for shared, oracle_bets, and orders

root(X) -> X#proof.root.
tree(X) -> int_to_tree(X#proof.tree).

%we use a deterministic merge sort that removes repeats while sorting.
tree_to_int(accounts) -> 1;
tree_to_int(channels) -> 2;
tree_to_int(existence) -> 3;
tree_to_int(burn) -> 4;
tree_to_int(oracles) -> 5;
tree_to_int(governance) -> 6;
tree_to_int(oracle_bets) -> 7;
tree_to_int(orders) -> 8;
tree_to_int(shares) -> 9.

int_to_tree(1) -> accounts;
int_to_tree(2) -> channels;
int_to_tree(3) -> existence;
int_to_tree(4) -> burn;
int_to_tree(5) -> oracles;
int_to_tree(6) -> governance;
int_to_tree(7) -> oracle_bets;
int_to_tree(8) -> orders;
int_to_tree(9) -> shares.
    

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
prove2([{orders, Key}|T], Trees) ->
    Oracles = trees:oracles(Trees),
    {_, Data0, _} = oracles:get(Key#key.id, Oracles),
    OrdersTree = oracles:orders(Data0),%%%%
    {Root, Data, Path} = orders:get(Key#key.pub, OrdersTree),
    Data2 = case Data of
		empty -> 0;
		_ -> orders:serialize(Data)
	    end,
    Proof = #proof{root = Root,
		   key = Key,
		   path = Path,
		   value = Data2,
		   tree = tree_to_int(orders)},
    true = orders:verify_proof(Root, Key#key.pub, Data2, Path),
    [Proof|prove2(T, Trees)];
prove2([{oracle_bets, Key}|T], Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Data0, _} = accounts:get(Key#key.pub, Accounts),
    OrdersTree = accounts:bets(Data0),%%%%
    {Root, Data, Path} = oracle_bets:get(Key#key.id, OrdersTree),
    Data2 = case Data of
		empty -> 0;
		_ -> oracle_bets:serialize(Data)
	    end,
    Proof = #proof{root = Root,
		   key = Key,
		   path = Path,
		   value = Data2,
		   tree = tree_to_int(oracle_bets)},
    true = oracle_bets:verify_proof(Root, Key#key.id, Data2, Path),
    [Proof|prove2(T, Trees)];
    
prove2([{Tree, Key}|T], Trees) ->
    Branch = trees:Tree(Trees),
    {Root, Data, Path} = Tree:get(Key, Branch),
    Data2 = case Data of
		empty -> 0;
		_ -> Tree:serialize(Data)
	    end,
    %SD = Tree:serialize(Data),
    Proof = #proof{root = Root,
		  key = Key,
		  path = Path, 
		  value = Data2,
		  tree = tree_to_int(Tree)},
    true = Tree:verify_proof(Root, Key, Data2, Path),
    [Proof|prove2(T, Trees)].
facts_to_dict([], D) -> D;
facts_to_dict([F|T], D) ->
%-record(proof, {tree, value, root, key, path}).
    %CFG is different for each trie
    Tree = int_to_tree(F#proof.tree),
    Key2 = 
        case Tree of
            orders -> 
                K = F#proof.key,
                Pub = K#key.pub,
                ID = K#key.id,
                Oracle = dict:fetch({oracles, ID}, D),
                RH = oracles:orders_hash(oracles:deserialize(Oracle)),
                RH = F#proof.root,
                Pub;
            oracle_bets -> 
                K = F#proof.key,
                Pub = K#key.pub,
                ID = K#key.id,
                Account = dict:fetch({accounts, Pub}, D),
                RH = accounts:bets_hash(accounts:deserialize(Account)),
                RH = F#proof.root,
                ID;
	_ ->
            F#proof.key
    end,
    true = 
        Tree:verify_proof(
          F#proof.root,
          Key2,
          F#proof.value,
          F#proof.path),
    Key = F#proof.key,
    Value = F#proof.value,
    Value2 = case Value of
	0 -> empty;
	_ -> Value
    end,
    D2 = dict:store({Tree, Key}, Value, D),
    facts_to_dict(T, D2).
hash(F) ->
    testnet_hasher:doit(F).
governance_to_querys(Gov) ->
    Leaves = trie:get_all(Gov, governance),
    Keys = leaves_to_querys(Leaves).
leaves_to_querys([]) -> [];
leaves_to_querys([L|T]) ->
    Q = {governance, leaf:key(L)},
    [Q|leaves_to_querys(T)].
-define(n2i(X), governance:name2number(X)).
txs_to_querys([], _) -> [];
txs_to_querys([STx|T], Trees) ->
    Tx = testnet_sign:data(STx),
    L = case element(1, Tx) of
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
            nc -> 
                [
                 {governance, ?n2i(nc)},
                 {governance, ?n2i(channel_closed_time)},%what is this for??
                 {governance, ?n2i(create_channel_fee)},
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
                PS = constants:pubkey_size() * 8,
                OID = oracle_new_tx:id(Tx),
                AID = oracle_new_tx:from(Tx),
                [
                 {governance, ?n2i(oracle_new)},
                 {governance, ?n2i(governance_change_limit)},
                 {governance, ?n2i(governance_delay)},
                 {governance, ?n2i(maximum_question_size)},
                 {governance, ?n2i(question_delay)},
                 {governance, ?n2i(oracle_initial_liquidity)},
                 {governance, ?n2i(oracle_future_limit)},
                 {governance, ?n2i(minimum_oracle_time)},
                 %{oracles, oracle_new_tx:recent_price(Tx)},
                 %{oracle_bets, {key, AID, OID}},
                 {accounts, AID},
                 {oracles, OID}
                ];
	    oracle_bet -> 
                OID = oracle_bet_tx:id(Tx),
                Pubkeys = [oracle_bet_tx:from(Tx)|
                           oracle_bet_tx:to_prove(Tx, Trees)],
                PS = constants:pubkey_size() * 8,
                Pubkeys2 = remove(<<0:PS>>, Pubkeys),
                Prove = tagify(accounts, Pubkeys2) ++ 
                    make_oracle_bets(Pubkeys2, OID) ++
                    make_orders(Pubkeys, OID),
                 [
                  {orders, #key{pub = <<0:PS>>, id = OID}},
                  {governance, ?n2i(oracle_bet)},
                  {governance, ?n2i(minimum_oracle_time)},
                  {governance, ?n2i(oracle_initial_liquidity)},
                  {oracles, oracle_bet_tx:id(Tx)}] ++
                    Prove;
	    oracle_close -> 
                PS = constants:pubkey_size() * 8,
                OID = oracle_close_tx:oracle_id(Tx),
                Oracles = trees:oracles(Trees),
                {_, Oracle, _} = oracles:get(OID, Oracles),
                [
                             %whichever governance variable is being updated.
                 {governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(maximum_oracle_time)},
                 {governance, ?n2i(oracle_close)},
                 {governance, ?n2i(oracle_initial_liquidity)},
                 {orders, #key{pub = <<0:PS>>, id = OID}},
                 {oracle_bets, #key{pub = oracles:creator(Oracle), id = OID}},
                 {accounts, oracle_close_tx:from(Tx)},
                 {oracles, OID}
                ];
	    unmatched -> 
                PS = constants:pubkey_size() * 8,
                OID = oracle_unmatched_tx:oracle_id(Tx),
                From = oracle_unmatched_tx:from(Tx),
                [
                 {governance, ?n2i(unmatched)},
                 {orders, #key{pub = <<0:PS>>, id = OID}},
                 {orders, #key{pub = From, id = OID}},
                 {accounts, From},
                 {oracles, OID}
                ];
	    oracle_shares -> 
                OID = oracle_shares_tx:oracle_id(Tx),
                From = oracle_shares_tx:from(Tx),
                [
                 {governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(oracle_shares)},
                 {oracle_bets, #key{pub = From, id = OID}},
                 {accounts, From},
                 {oracles, OID}
                ];
	    coinbase -> 
                [
                 {governance, ?n2i(block_reward)},
                 {governance, ?n2i(developer_reward)},
                 {accounts, coinbase_tx:from(Tx)}
                ]
	end,
    L ++ txs_to_querys(T, Trees).
remove(_, []) -> [];
remove(X, [X|A]) -> remove(X, A);
remove(X, [Y|A]) -> [Y|remove(X, A)].
tagify(_, []) -> [];
tagify(X, [H|T]) ->
    [{X, H}|tagify(X, T)].
make_oracle_bets([], _) -> [];
make_oracle_bets([H|T], OID) ->
    [{oracle_bets, #key{pub = H, id = OID}}|
     make_oracle_bets(T, OID)].
make_orders([], _) -> [];
make_orders([H|T], OID) ->
    [{orders, #key{pub = H, id = OID}}|
     make_orders(T, OID)].
test() ->
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    {Trees0, _, _} = tx_pool:data(),
    Question = <<>>,
    OID = 2,
    Fee = 20,
    {Tx, _} = oracle_new_tx:make(constants:master_pub(), Fee, Question, 1, OID, constants:initial_difficulty(), 0, 0, 0, Trees0),
    tx_pool_feeder:absorb(keys:sign(Tx)),
    test_txs:mine_blocks(1),
    timer:sleep(200),
    {Trees, _, _} = tx_pool:data(),
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
	      {channels, 1},
	      {existence, testnet_hasher:doit(1)},
	      {oracles, OID},
	      {oracles, 1},
	      {burn, testnet_hasher:doit(1)},
	      {orders, #key{pub = keys:pubkey(), id = OID}},
              {oracle_bets, #key{pub = keys:pubkey(), id = OID}}
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
    %io:fwrite(Txs),
    {Pub30, Priv30} = testnet_sign:new_key(),
    {Pub4, _} = testnet_sign:new_key(),
    {NewTx, _} = create_account_tx:new(Pub30, 10, 10, keys:pubkey(), Trees),
    {NewTx2, _} = create_account_tx:new(Pub4, 10, 10, keys:pubkey(), Trees),
    CID = 7,
    {NewTx3, _} = new_channel_tx:make(CID, Trees, keys:pubkey(), Pub3, 1, 1, 1, 1, 1),
    Txs = [keys:sign(NewTx),
           keys:sign(NewTx2),
           testnet_sign:sign_tx(NewTx3, Pub3, Priv30)],
    %io:fwrite(Txs),
    Q2 = txs_to_querys(Txs, Trees),
    prove(Q2, Trees),
    success.
    
    

