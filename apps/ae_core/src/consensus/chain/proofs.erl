-module(proofs).
-export([prove/2, test/0, hash/1, facts_to_dict/2, txs_to_querys/1]).
-record(proof, {tree, value, root, key, path}).
-record(key, {pub, id}). %used for shared, oracle_bets, and orders

%we use a deterministic merge sort that removes repeats while sorting.
tree_to_int(governance) -> 0;
tree_to_int(accounts) -> 1;
tree_to_int(channels) -> 2;
tree_to_int(existence) -> 3;
tree_to_int(burn) -> 4;
tree_to_int(oracles) -> 5;
tree_to_int(oracle_bets) -> 6;
tree_to_int(orders) -> 7;
tree_to_int(shares) -> 8.

int_to_tree(0) -> governance;
int_to_tree(1) -> accounts;
int_to_tree(2) -> channels;
int_to_tree(3) -> existence;
int_to_tree(4) -> burn;
int_to_tree(5) -> oracles;
int_to_tree(6) -> oracle_bets;
int_to_tree(7) -> orders;
int_to_tree(8) -> shares.
    

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
prove2([{shares, Key}|T], Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Data0, _} = shares:get(Key, Accounts),
    SharesTree = accounts:shares(Data0),
    {Root, Data, Path} = shares:get(Key#key.id, SharesTree),
    Proof = #proof{root = Root,
		   key = Key,
		   path = Path,
		   value = Data,
		   tree = tree_to_int(shares)},
    [Proof|prove2(T, Trees)];
    
prove2([{Tree, Key}|T], Trees) ->
    Branch = trees:Tree(Trees),
    {Root, Data, Path} = Tree:get(Key, Branch),
    Data2 = case Data of
		empty -> 0;
		_ -> Data
	    end,
    %SD = Tree:serialize(Data),
    Proof = #proof{root = Root,
		  key = Key,
		  path = Path, 
		  value = Data2,
		  tree = tree_to_int(Tree)},
    [Proof|prove2(T, Trees)].
facts_to_dict([], D) -> D;
facts_to_dict([F|T], D) ->
    Tree = int_to_tree(F#proof.tree),
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
txs_to_querys([]) -> [];
txs_to_querys([STx|T]) ->
    Tx = testnet_sign:data(STx),
    L = case element(1, Tx) of
	    ca -> [{accounts, create_account_tx:from(Tx)},
		   {accounts, create_account_tx:pubkey(Tx)}];
	    spend -> [];
	    da -> [];
	    gc -> [];
	    ctc -> [];
	    csc -> [];
	    timeout -> [];
	    cs -> [];
	    ex -> [];
	    oracle_new -> [{accounts, oracle_new_tx:from(Tx)}];
	    oracle_bet -> [];
	    oracle_close -> [];
	    unmatched -> [];
	    oracle_shares -> [];
	    coinbase -> [];
	    _ -> [] 
	end,
    L ++ txs_to_querys(T).

test() ->
    {Trees, _, _} = tx_pool:data(),
    Pub2 = <<"BL6uM2W6RVAI341uFO7Ps5mgGp4VKZQsCuLlDkVh5g0O4ZqsDwFEbS9GniFykgDJxYv8bNGJ+/NdrFjKV/gJa6c=">>,
    Querys = [{accounts, keys:pubkey()},
	      {shares, #key{pub = keys:pubkey(), id = 1}},
	      {accounts, keys:pubkey()},
	      {accounts, base64:decode(Pub2)},%empty account
	      {governance, block_reward}],
    Facts = prove(Querys, Trees),
    %true = verify_facts(Trees, Facts),
    ProofRoot = hash(Facts),
    Dict = facts_to_dict(Facts, dict:new()),
    Querys2 = dict:fetch_keys(Dict),
    Facts = prove(Querys2, Trees),
    Dict.
    %success.
