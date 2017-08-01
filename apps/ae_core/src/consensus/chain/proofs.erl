-module(proofs).
-export([prove/2, test/0, hash/1, facts_to_dict/2, txs_to_querys/1]).
-record(proof, {tree, value, root, key, path}).
-record(key, {pub, id}). %used for shared, oracle_bets, and orders

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
prove2([{shares, Key}|T], Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Data0, _} = accounts:get(Key#key.pub, Accounts),
    SharesTree = accounts:shares(Data0),
    {Root, Data, Path} = shares:get(Key#key.id, SharesTree),
    Data2 = case Data of
		empty -> 0;
		_ -> Data
	    end,
    Proof = #proof{root = Root,
		   key = Key,
		   path = Path,
		   value = Data2,
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
%-record(proof, {tree, value, root, key, path}).
    %CFG is different for each trie
    Tree = int_to_tree(F#proof.tree),
    io:fwrite(packer:pack({to_dict, Tree, F})),
    io:fwrite("\n"),
    case Tree of
	shares -> 
	    K = F#proof.key,
	    Pub = K#key.pub,
	    ID = K#key.id,
	    Account = dict:fetch({accounts, Pub}, D),
	    Shares = accounts:shares(Account),
	    RH = shares:root_hash(Shares),
	    RH = F#proof.root,
	    true = 
		Tree:verify_proof(
		  F#proof.root,
		  ID,
		  F#proof.value,
		  F#proof.path);
	_ ->
	    true = 
		Tree:verify_proof(
		  F#proof.root,
		  F#proof.key,
		  F#proof.value,
		  F#proof.path)
    end,
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
	    spend -> [{accounts, spend_tx:from(Tx)},
		      {accounts, spend_tx:to(Tx)}];
	    da -> [{accounts, delete_account_tx:from(Tx)},
		   {accounts, delete_account_tx:to(Tx)}];
	    gc -> [{accounts, grow_channel_tx:acc1(Tx)},
		   {accounts, grow_channel_tx:acc2(Tx)},
		   {channels, grow_channel_tx:id(Tx)}];
	    ctc -> [{accounts, channel_team_close_tx:aid1(Tx)},
		    {accounts, channel_team_close_tx:aid2(Tx)},
		    {channels, channel_team_close_tx:id(Tx)}];
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
	      {shares, #key{pub = keys:pubkey(), id = 2}},
	      {accounts, keys:pubkey()},
	      {accounts, base64:decode(Pub2)},%,%empty account
	      {governance, block_reward},
	      {channels, 1},
	      {existence, testnet_hasher:doit(1)}
	     ],
    Facts = prove(Querys, Trees),
    io:fwrite(packer:pack({prove_facts, Facts})),
    ProofRoot = hash(Facts),
    Dict = facts_to_dict(Facts, dict:new()),
    Querys2 = dict:fetch_keys(Dict),
    Facts = prove(Querys2, Trees),
    Dict.
    %success.

%["prove_facts",[-6,["proof",1,["acc",1080000000000,0,0,"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",0,1],"OpYNdZnsFT8WXXHehd0L/hl7Mq8z0p701BxnXGpOSLk=","BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",[-6,[-7,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","81OTmtuiaO7i5W48IEmf8pG3WQsvX5/5MZnozSc1Riw=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="]]],["proof",1,0,"OpYNdZnsFT8WXXHehd0L/hl7Mq8z0p701BxnXGpOSLk=","BL6uM2W6RVAI341uFO7Ps5mgGp4VKZQsCuLlDkVh5g0O4ZqsDwFEbS9GniFykgDJxYv8bNGJ+/NdrFjKV/gJa6c=",[-6,[-7,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","81OTmtuiaO7i5W48IEmf8pG3WQsvX5/5MZnozSc1Riw=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="]]],["proof",2,0,"B2onx55azio9R/ndLoPk/26ohys8Ihj2bJK4m1XzZWA=",1,[-6,[-7,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="]]],["proof",6,["gov",1,1800,0],"cLq/gC1tK0aZakdMlK/q25vfgGX4LPEZBQJ678pU3/I=","block_reward",[-6,[-7,"u+GsDrSJqi++s8tlJVSAYMKfFvhx+p7KgsPKb2f2L7s=","3nwrzTuk6cAN38yzMEJ/MtOdBy2d3m3cThi1F0YQAhY=","KPs0sTJysNQ70AQC0meK7wYFs5WVvitneKzEZ8yqUBg=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="],[-7,"IZRiPjeHHdkWkIK0/taQBy1FseH832DRFBZw4b5W19A=","qsVFAjWhSALUUxLt/Ee0lY9zmoSqkX0YHKrgme7R1no=","QfqYpbBDS12QWVeO+gT7QwqToFKwG1ABD71n2zqVKEg=","fZGeglV4LHILKKsAWQCnSQR0nledB7WhSG3k/k9s3kA=","inNs/nI/ejO64/LMlVx8AOJA8jO1XuiQ+v00388auhw=","x4rEwUY//VPkxGsMqeiTS2f4vIDfWA1IRSyDsoBpFtY=","8X/ckE4N8i830Rc8IE3G9yoP74h5j3Xy0Impkexkdgw=","zKE/sBD1r+h+fZl8oxWP4Nj+ksmgFlx7+/jp9DHDajc=","12GvIcqtsmWUkvikuqclZv9tKPoyGjs177DwpvSwQMQ=","Z3F8LFsRJlA6iHhbwVUSNPx0KcdZ/M0p7vgav3mo9PM=","MK/uhrelqpxAoWCYb5+uiw0LqFg6jHXgIx4hMVUmiHU=","INYL4mF5HFbNGRx3NjOdLo6vfiKEQq5GBDDu0Su/FwM=","hW2Yy8E50+OuE18HGv7ygNuBxzAwplnN0+pr0JHH3dI=","ZB1pbxc4XFWh29Hhcw0Yiaq/ivnLBCPeIUz6MpvdsOc=","P6NmCntVmzTFjNv4CIInEd7+DVfZkEkJSJwHG6nYzaw=","pe7Wbu+ktsOPuAaMg239Uz+67PDzPOGSVNGgf7d62ZY="]]],["proof",9,["share",1,100,0],"I00UBtfsIomvNGvnL7OJrEtifeND2PWdpIhmsYIsEl8=",["key","BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",1],[-6,[-7,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","FV4JqyX1iNSGtf6UnFfYn/adRIkEOuCdaIN312w1hq0=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="]]]]]** exception error: no match of right hand side value 2
