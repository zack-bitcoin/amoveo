-module(block_new).
-export([block_to_header/1, test/0,
	 height/1, prev_hash/1, txs/1, trees_hash/1, time/1, difficulty/1, comment/1, version/1, pow/1, trees/1, prev_hashes/1

	]).
-record(block, {height, 
		prev_hash, 
		txs, 
		trees_hash, 
		time, 
		difficulty, 
		comment = <<>>,
		version,
		pow, 
		trees, 
		prev_hashes = {prev_hashes}}).%tries: txs, channels, census, 
height(B) -> B#block.height.
prev_hash(B) -> B#block.prev_hash.
txs(B) -> B#block.txs.
trees_hash(B) -> B#block.trees_hash.
time(B) -> B#block.time.
difficulty(B) -> B#block.difficulty.
comment(B) -> B#block.comment.
version(B) -> B#block.version.
pow(B) -> B#block.pow.
trees(B) -> B#block.trees.
prev_hashes(B) -> B#block.prev_hashes.
 

txs_hash(X) ->
    testnet_hasher:doit(X).
block_to_header(B) ->
    POW = B#block.pow,
    Nonce = pow:nonce(POW),
    io:fwrite("make header\n"),
    headers:make_header(B#block.prev_hash,
			B#block.height,
			B#block.time,
			B#block.version,
			B#block.trees,
			txs_hash(B#block.txs),
			Nonce,
			B#block.difficulty).
hash(B) when is_binary(B) ->
    A = size(B) == constants:hash_size(),
    if
	A -> B;
	true -> testnet_hasher:doit(B)
    end;
hash(B) when element(1, B) ==  header ->
    hash(headers:serialize(B));
hash(B) when is_record(B, block) ->
    hash(block_to_header(B)).

calculate_prev_hashes(Parent) ->
    H = height(Parent),
    PH = hash(Parent),
    calculate_prev_hashes([PH], H, 2).
calculate_prev_hashes([PH|Hashes], Height, N) -> 
    NHeight = Height - N,
    if
	NHeight < 1 -> list_to_tuple([prev_hashes|lists:reverse([PH|Hashes])]);
	true ->
	    B = read_int(NHeight, PH),
	    calculate_prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.
binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    "blocks/" ++C++".db".
read(H) ->    
    Hash = hash(H),
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    case Z of
	[] -> empty;
	A -> binary_to_term(zlib:uncompress(A))
    end.
lg(X) ->
    true = X > 0,
    true = is_integer(X),
    lgh(X, 0).
lgh(1, X) -> X;
lgh(N, X) -> lgh(N div 2, X+1).
read_int(N, BH) ->
    Block = read(BH),
    M = height(Block),
    D = M-N,
    if 
	D<0 -> 
	    empty;
	D == 0 -> Block;
	true ->
	    read_int(N, prev_hash(lg(D), Block))
    end.
prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) ->%N=0 should be the same as prev_hash(BP)
    element(N+1, BP#block.prev_hashes).
%instead of limiting it to one coinbase, we could require the coinbase to be the first tx in the list. This way we don't have to search the list for a coinbase tx.
one_coinbase([], _) -> false.
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins(), 0),
    Accounts = accounts:write(0, First),
    GovInit = governance:genesis_state(),
    Trees = trees:new(Accounts, 0, 0, 0, 0, GovInit),
    TreeRoot = trees:root_hash(Trees),
    #block{height = 0,
	   prev_hash = 0,
	   txs = [],
	   trees_hash = TreeRoot,
	   time = 0,
	   difficulty = constants:initial_difficulty(),
	   version = constants:version(),
	   pow = {pow, <<>>, 0, 0},
	   trees = Trees
	  }.
block_reward(Trees, Height, ID, PH) -> 
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    BlocksAgo = Height - BCM,
    Txs = txs(read_int(BlocksAgo, PH)),
    TransactionFees = txs:fees(Txs),
    TransactionCosts = tx_costs(Txs, Governance, 0),
    BlockReward = governance:get_value(block_reward, Governance),
    Amount = BlockReward + TransactionFees - TransactionCosts,
    NM = case accounts:get(ID, OldAccounts) of
	     {_, empty,_} ->  accounts:new(ID, Amount, Height);
	     _ -> accounts:update(ID, Trees, Amount, none, Height)
	 end,
    accounts:write(OldAccounts, NM).
tx_costs([], _, Out) -> Out;
tx_costs([STx|T], Governance, Out) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    tx_costs(T, Governance, Cost+Out).
   
make(Header, Txs0, Trees, Pub) ->
    {CB, Proofs} = coinbase_tx:make(Pub, Trees),
    Txs = [keys:sign(CB)|Txs0],
    Height = headers:height(Header),
    NewTrees0 = txs:digest(Txs, Trees, Height+1),
    NewTrees = block_reward(NewTrees0, Height+1, Pub, hash(Header)), 
    Block = #block{height = Height + 1,
		   prev_hash = hash(Header),
		   txs = Txs,
		   trees_hash = trees:root_hash(NewTrees),
		   time = time_now(),
		   difficulty = headers:difficulty_should_be(read(headers:prev_hash(Header))),
		   version = constants:version(),
		   pow = {pow, 0,0,0},
		   trees = NewTrees,
		   prev_hashes = prev_hashes(Header)
		  }.
mine(Block, Times) ->
    PH = Block#block.prev_hash,
    Difficulty = Block#block.difficulty,
    ParentPlus = read(PH),
    Trees = ParentPlus#block.trees,
    Governance = trees:governance(Trees),
    BlockReward = governance:get_value(block_reward, Governance),
    MineDiff = (Difficulty * BlockReward) div constants:initial_block_reward(),
    case pow:pow(hash(Block), MineDiff, Times, constants:hash_size()) of
	false -> fail;
	Pow -> Block#block{pow = Pow}
    end.

 
test() ->
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins(), 0),
    Accounts = accounts:write(0, First),
    GovInit = governance:genesis_state(),
    Trees = trees:new(Accounts, 0, 0, 0, 0, GovInit),
    %TreeRoot = trees:root_hash(Trees),
    Header0 = block_to_header(genesis_maker()),
    Pub = keys:pubkey(),
    Block = make(Header0, [], Trees, Pub).
    
