-module(proofs).
-export([prove/2, test/0]).
-record(proof, {root, key, path, value, tree}).

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
det_helper([A]) -> A;
det_helper(L) ->
    det_helper(det_improve(L)).
to_lists([]) -> [];
to_lists([A|T]) -> [[A]|to_lists(T)].
det_order(Querys) ->    
    F = to_lists(Querys),
    det_helper(F).
       
	    

prove(Querys, Trees) ->
    F2 = det_order(Querys),
    prove2(F2, Trees).
prove2([], _) ->
   [];
prove2([{Tree, Key}|T], Trees) ->
    Branch = trees:Tree(Trees),
    {Root, Data, Path} = Tree:get(Key, Branch),
    SD = Tree:serialize(Data),
    Proof = #proof{root = Root,
		  key = Key,
		  path = Path, 
		  value = SD,
		  tree = Tree},
    [Proof|prove2(T, Trees)].
facts_to_dict([], D) -> D;
facts_to_dict([F|T], D) ->
    Tree = F#proof.tree,
    Key = F#proof.key,
    Value = F#proof.value,
    D2 = dict:store({Tree, Key}, Value, D),
    facts_to_dict(T, D2).

test() ->
    {Trees, _, _} = tx_pool:data(),
    Querys = [{accounts, keys:pubkey()},
	     {accounts, keys:pubkey()},
	     {governance, block_reward}],
    Facts = prove(Querys, Trees),
    Dict = facts_to_dict(Facts, dict:new()),
    Querys2 = dict:fetch_keys(Dict),
    Facts = prove(Querys2, Trees),
    Dict.
    %success.
