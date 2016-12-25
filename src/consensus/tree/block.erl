-module(block).
-export([hash/1,check2/1,test/0,mine_test/0,genesis/0,
	 make/3,mine/2,height/1,accounts/1,channels/1,
	 accounts_hash/1,channels_hash/1,save/1,
	 absorb/1,read/1,binary_to_file/1,block/1,
	 prev_hash/1,read_int/1,check1/1,pow_block/1,
	 mine_blocks/2, mine_blocks/1]).
-record(block, {height, prev_hash = 0, txs, channels, accounts, mines_block, time, difficulty}).%tries: txs, channels, census, 
-record(block_plus, {block, accounts, channels, accumulative_difficulty = 0}).%The accounts and channels in this structure only matter for the local node. they are pointers to the locations in memory that are the root locations of the account and channel tries on this node.
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
block(P) when element(1, P) == pow ->
    pow:data(P);
block(BP) when is_record(BP, block_plus) ->
    block(BP#block_plus.block);
block(B) when is_record(B, block) ->
    B.
pow_block(B) when element(1, B) == pow ->
    B;
pow_block(BP) when is_record(BP, block_plus) ->
    pow_block(BP#block_plus.block).

channels(Block) ->
    Block#block_plus.channels.
channels_hash(BP) when is_record(BP, block_plus) ->
    channels_hash(pow:data(BP#block_plus.block));
channels_hash(Block) -> Block#block.channels.
accounts(BP) ->
    BP#block_plus.accounts.
accounts_hash(BP) when is_record(BP, block_plus) ->
    accounts_hash(pow:data(BP#block_plus.block));
accounts_hash(Block) ->
    Block#block.accounts.
height(X) ->
    B = block(X),
    B#block.height.
%height(BP) when is_record(BP, block_plus) ->
%    height(pow:data(BP#block_plus.block));
%height(Block) when is_record(Block, block)->
%    Block#block.height;
%height(X) ->
%    io:fwrite("error. should be a block, "),
%    io:fwrite(X).
prev_hash(X) -> 
    B = block(X),
    B#block.prev_hash.
%jprev_hash(BP) when is_record(BP, block_plus) ->
 %   prev_hash(pow:data(BP#block_plus.block));
%prev_hash(Block) ->
%    Block#block.prev_hash.

hash(X) -> hash:doit(term_to_binary(block(X))).
%hash(BP) when is_record(BP, block_plus) ->
%    hash(pow:data(BP#block_plus.block));
%hash(Block) when is_record(Block, block)->
%    B2 = term_to_binary(Block),
%    hash:doit(B2).

time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis() ->
    %the pointer to an empty trie is 0.
    Address = constants:master_address(),
    ID = 1,
    First = account:new(ID, Address, constants:initial_coins(), 0),
    Accounts = account:write(0, First),
    AccRoot = account:root_hash(Accounts),
    ChaRoot = channel:root_hash(0),

    %Block = 
    %#block{height = 0,
	       %txs = [],
	       %channels = ChaRoot,
	       %accounts = AccRoot,
	       %mines_block = ID,
	       %time = 0,
	       %difficulty = constants:initial_difficulty()},
    Block = {pow,{block,0,0,[], ChaRoot, AccRoot,
		  %<<1,223,2,81,223,207,12,158,239,5,219,253>>,
		  %<<108,171,180,35,202,56,178,151,11,85,188,193>>,
		  1,0,4080},
	     4080,44358461744572027408730},

    #block_plus{block = Block, channels = 0, accounts = Accounts}.
absorb_txs(PrevPlus, MinesBlock, Height, Txs) ->
    OldAccounts = PrevPlus#block_plus.accounts,
    NewMiner = account:update(MinesBlock,
			      OldAccounts,
			      constants:block_reward(),
			      none,
			      Height),
    NewAccounts = account:write(OldAccounts, NewMiner),
    txs:digest(Txs, 
	       PrevPlus#block_plus.channels,
	       NewAccounts,
	       Height).
    
make(PrevHash, Txs, ID) ->%ID is the user who gets rewarded for mining this block.
    ParentPlus = read(PrevHash),
    Parent = pow:data(ParentPlus#block_plus.block),
    Height = Parent#block.height + 1,
    {NewChannels, NewAccounts} = absorb_txs(ParentPlus, ID, Height, Txs),
    %{NewChannels, NewAccounts} = 
	%txs:digest(Txs, 
	%	   ParentPlus#block_plus.channels, 
	%	   ParentPlus#block_plus.accounts,
	%	   Height),
    CHash = trie:root_hash(channels, NewChannels),
    AHash = account:root_hash(NewAccounts),
    NextDifficulty = next_difficulty(PrevHash),
    #block_plus{
       block = 
	   #block{height = Height,
		  prev_hash = PrevHash,
		  txs = Txs,
		  channels = CHash,
		  accounts = AHash,
		  mines_block = ID,
		  time = time_now()-5,
		  difficulty = NextDifficulty},
       accumulative_difficulty = next_acc(ParentPlus, NextDifficulty),
       channels = NewChannels, 
       accounts = NewAccounts
      }.
next_acc(Parent, ND) ->
    Parent#block_plus.accumulative_difficulty + pow:sci2int(ND).
    %We need to reward the miner for his POW.
    %We need to reward the miner the sum of transaction fees.
mine(BP, Times) when is_record(BP, block_plus) ->
    Block = BP#block_plus.block,
    case mine(Block, Times) of
	false -> false;
	Mblock -> BP#block_plus{block = Mblock}
    end;
mine(Block, Times) ->
    Difficulty = Block#block.difficulty,
    pow:pow(Block, Difficulty, Times).

next_difficulty(PrevHash) ->
    ParentPlus = read(PrevHash),
    Parent = pow:data(ParentPlus#block_plus.block),
    Height = Parent#block.height + 1,
    RF = constants:retarget_frequency(),
    X = Height rem RF,
    OldDiff = Parent#block.difficulty,
    if
	Height == 1 -> constants:initial_difficulty(); 
	Height < (RF+1) -> OldDiff;
	X == 0 -> retarget(PrevHash, Parent#block.difficulty);
	true ->  OldDiff
    end.
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
    
retarget(PrevHash, Difficulty) ->    
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget2(PrevHash, F, []),
    {Times2, _} = retarget2(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,
    %io:fwrite([Ratio, Difficulty]),%10/2, 4096
    ND = pow:recalculate(Difficulty, constants:block_time(), T),
    max(ND, constants:initial_difficulty()).
retarget2(Hash, 0, L) -> {L, Hash};
retarget2(Hash, N, L) -> 
    BP = read(Hash),
    B = BP#block_plus.block,
    T = B#block.time,
    H = B#block.prev_hash,
    retarget2(H, N-1, [T|L]).
   
check1(BP) -> 
    %check1 makes no assumption about the parent's existance.
    BH = hash(BP),
    GH = hash(genesis()),
    if
	BH == GH -> {BH, 0};
	true ->
	    PowBlock = pow_block(BP),
	    Block = block(PowBlock),
	    Difficulty = Block#block.difficulty,
	    true = Difficulty >= constants:initial_difficulty(),
	    pow:above_min(PowBlock, Difficulty),
 
	    true = Block#block.time < time_now(),
	    {BH, Block#block.prev_hash}
    end.


check2(BP) ->
    %check that the time is later than the median of the last 100 blocks.

    %check2 assumes that the parent is in the database already.
    PowBlock = pow_block(BP),
    Block = block(PowBlock),
    Difficulty = Block#block.difficulty,
    PH = Block#block.prev_hash,
    Difficulty = next_difficulty(PH),
    PrevPlus = read(PH),
    Prev = block(PrevPlus),
    true = (Block#block.height-1) == Prev#block.height,
    {CH, AH} = {Block#block.channels, Block#block.accounts},
    {CR, AR} = absorb_txs(PrevPlus, Block#block.mines_block, Block#block.height, Block#block.txs),
    CH = channel:root_hash(CR),
    AH = account:root_hash(AR),
    #block_plus{block = PowBlock, channels = CR, accounts = AR, accumulative_difficulty = next_acc(PrevPlus, Block#block.difficulty)}.

absorb(BP) ->
    BH = hash(BP),
    case block_hashes:check(BH) of
	true -> ok;%If we have seen this block before, then don't process it again.
	false ->
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    io:fwrite("absorb block "),
	    io:fwrite(packer:pack(BP)),
	    io:fwrite("\n"),
	    check1(BP),
	    BP2 = check2(BP),
	    save(BP2)
    end.   
save(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    Hash = hash(BlockPlus),
    BF = binary_to_file(Hash),
    db:save(BF, Z),
    top:add(BlockPlus),
    Hash.
binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    H = C,
    "blocks/"++H++".db".
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    case Z of
	[] -> empty;
	A -> binary_to_term(zlib:uncompress(A))
    end.
    
read_int(N) ->%currently O(n), needs to be improved to O(lg(n))
    true = N >= 0,
    read_int(N, top:doit()).
read_int(N, BH) ->
    Block = read(BH),
    case height(Block) of
	N -> Block;
	_ -> read_int(N, prev_hash(Block))
    end.
	    
    
    


test() ->
    block:read(top:doit()),
    PH = top:doit(),
    BP = read(PH),
    Accounts = accounts(BP),
    %Accounts = BP#block_plus.accounts,
    _ = account:get(1, Accounts),
    %{block_plus, Block, _, _, _} = make(PH, [], 1),
    Block = make(PH, [], 1),
    MBlock = mine(Block, 100000000),
    check2(MBlock),
    success.
mine_test() ->
    PH = top:doit(),
    {block_plus, Block, _, _, _} = make(PH, [], keys:id()),
    PBlock = mine(Block, 1000000000),
    absorb(PBlock),
    mine_blocks(10, 1000000000),
    success.
mine_blocks(N) ->
    mine_blocks(N, 1000000).
   
mine_blocks(0, _) -> success;
mine_blocks(N, Times) -> 
    spawn(fun() -> easy:sync() end),
    PH = top:doit(),
    {_,_,_,Txs} = tx_pool:data(),
    {block_plus, Block, _, _, _} = make(PH, Txs, keys:id()),
    
    io:fwrite("mining attempt #"),
    io:fwrite(integer_to_list(N)),
    io:fwrite(" time "),
    io:fwrite(integer_to_list(time_now())),
    io:fwrite(" diff "),
    io:fwrite(integer_to_list(Block#block.difficulty)),
    %erlang:system_info(logical_processors_available)
    Cores = erlang:system_info(logical_processors_available),
    io:fwrite(" using "),
    io:fwrite(integer_to_list(Cores)),
    io:fwrite(" CPU"),
    io:fwrite("\n"),
    F = fun() ->
		case mine(Block, Times) of
		    false -> false;
		    PBlock -> 
			io:fwrite("FOUND A BLOCK !!!!!!!!!!!!!!!!\n"),
			absorb(PBlock)
		end
	end,
    spawn_many(Cores-1, F),
    F(),
    mine_blocks(N-1, Times).
    
spawn_many(0, _) -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
