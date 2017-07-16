-module(block_new).
-export([block_to_header/1, test/0,
	 height/1, prev_hash/1, txs/1, trees_hash/1, time/1, difficulty/1, comment/1, version/1, pow/1, trees/1, prev_hashes/1, 
	 read_int/2, hash/1

	]).
-record(block, {height, 
		prev_hash, 
		txs, 
		trees_hash, 
		time, 
		difficulty, 
		comment = <<>>,
		version,
		nonce = 0, 
		trees, 
		prev_hashes = {prev_hashes},
		proofs = []}).
%proofs is for this
%If the attacker sends a valid block with a valid header,
% but makes one of the proofs of the state tree wrong.
%The node getting attacked would have to verify all the proofs until it found a bad one.
%I think this might be making it too affordable to DDOS our nodes.
%So I am thinking of adding something more to the header.
%A hash of a datastructure containing all the proofs for this block.
%So when you are loading the block,
%you can know if a proof has been manipulated immediately.
height(B) -> B#block.height.
prev_hash(B) -> B#block.prev_hash.
txs(B) -> B#block.txs.
trees_hash(B) -> B#block.trees_hash.
time(B) -> B#block.time.
difficulty(B) -> B#block.difficulty.
comment(B) -> B#block.comment.
version(B) -> B#block.version.
pow(B) -> B#block.nonce.
trees(B) -> B#block.trees.
prev_hashes(B) -> B#block.prev_hashes.
proofs(B) -> B#block.proofs.
 

txs_and_proof_hash(Txs, Proofs) ->
    testnet_hasher:doit({Txs, Proofs}).
block_to_header(B) ->
    Nonce = B#block.nonce,
    headers:make_header(B#block.prev_hash,
			B#block.height,
			B#block.time,
			B#block.version,
			trees:root_hash(B#block.trees),
			txs_and_proof_hash(B#block.txs, B#block.proofs),
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
    H = headers:height(Parent),
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
    TreesRoot = trees:root_hash(Trees),
    #block{height = 0,
	   prev_hash = <<0:(constants:hash_size()*8)>>,
	   txs = [],
	   trees_hash = TreesRoot,
	   time = 0,
	   difficulty = constants:initial_difficulty(),
	   version = constants:version(),
	   trees = Trees
	  }.
block_reward(Trees, Height, ID, PH) -> 
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    BlocksAgo = Height - BCM,
    if
	BlocksAgo > 0 ->
	    Txs = txs(read_int(BlocksAgo, PH)),
	    TransactionFees = txs:fees(Txs),
	    TransactionCosts = tx_costs(Txs, Governance, 0),
	    BlockReward = governance:get_value(block_reward, Governance),
	    Amount = BlockReward + TransactionFees - TransactionCosts,
	    NM = case accounts:get(ID, OldAccounts) of
		     {_, empty,_} ->  accounts:new(ID, Amount, Height);
		     _ -> accounts:update(ID, Trees, Amount, none, Height)
		 end,
	    NewAccounts = accounts:write(OldAccounts, NM),
	    trees:update_accounts(Trees, NewAccounts);
	true -> Trees
    end.
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
		   difficulty = headers:difficulty_should_be(Header),
		   version = constants:version(),
		   trees = NewTrees,
		   prev_hashes = {block}%calculate_prev_hashes(Header)
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
	Pow -> 
	    Nonce = pow:nonce(Pow),
	    Block#block{nonce = Nonce}
    end.
do_save(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = hash(BlockPlus),
    BF = binary_to_file(Hash),
    ok = db:save(BF, Z).
    

 
test() ->
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins(), 0),
    Accounts = accounts:write(0, First),
    GovInit = governance:genesis_state(),
    Trees = trees:new(Accounts, 0, 0, 0, 0, GovInit),
    GB = genesis_maker(),
    do_save(GB),
    Header0 = block_to_header(GB),
    GH = hash(Header0),
    gen_server:call(headers, {add, GH, Header0, 0}),
    Pub = keys:pubkey(),
    Block1 = make(Header0, [], Trees, Pub),
    Header1 = block_to_header(Block1).


    %H2 = hash(Header1),
    %gen_server:call(headers, {add, H2, Header1, 0}),
    %make(Header1, [], Block1#block.trees, Pub).
