-module(block).
-export([hash/1,check2/1,test/0,mine_test/0,genesis/0,
	 make/3,mine/2,height/1,
	 read/1,binary_to_file/1,block/1,prev_hash/2,
	 prev_hash/1,read_int/1,check1/1,
	 mine_blocks/2, mine_blocks/3, hashes/1, 
	 block_to_header/1,
	 median_last/2, trees/1, trees_hash/1,
	 guess_number_of_cpu_cores/0, difficulty/1,
	 txs/1, genesis_maker/0, new_id/1, read_many/2
	]).

-record(block, {height, prev_hash, txs, trees, 
		mines_block, time, 
		difficulty, comment = <<>>,
		magic = constants:magic()}).%tries: txs, channels, census, 
-record(block_plus, {block, pow, trees, accumulative_difficulty = 0, prev_hashes = {}}).%The accounts and channels in this structure only matter for the local node. they are pointers to the locations in memory that are the root locations of the account and channel tries on this node.
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
txs(X) ->
    X#block.txs.
trees_hash(X) ->
    X#block.trees.
block_to_header(B) ->
    Block = block(B),
    Height = Block#block.height,
    PH = Block#block.prev_hash,
    Trees = Block#block.trees,
    Miner = case Block#block.mines_block of
		{ID, _} -> ID;
		ID -> ID
	    end,
    Time = Block#block.time,
    Diff = Block#block.difficulty,
    Magic = Block#block.magic,
    TxHash = testnet_hasher:doit(Block#block.txs),
    <<PH/binary,
      Height:(constants:height_bits()),
      Miner:(constants:acc_bits()),
      Time:(constants:time_bits()),
      Diff:(constants:difficulty_bits()),
      Magic:(constants:magic_bits()),
      Trees/binary,
      TxHash/binary>>.
      
hashes(BP) ->
    BP#block_plus.prev_hashes.
difficulty(C) -> 
    B = block(C),
    B#block.difficulty.
%block({Block, _Pow}) ->
%    Block;
%block(P) when element(1, P) == pow ->
%    pow:data(P);
block(BP) when is_record(BP, block_plus) ->
    block(BP#block_plus.block);
block(B) when is_record(B, block) -> B.
%pow_block(B) when element(1, B) == pow -> B;
%pow_block(BP) when is_record(BP, block_plus) ->
%    pow_block(BP#block_plus.block).
trees(Block) ->
    Block#block_plus.trees.
height(X) ->
    B = block(X),
    B#block.height.
prev_hashes(PH) ->
    H = height(read(PH)),
    prev_hashes([PH], H, 2).
prev_hashes([PH|Hashes], Height, N) ->
    NHeight = Height - N,
    if
	NHeight < 1 -> list_to_tuple(lists:reverse([PH|Hashes]));
	true ->
	    B = read_int(NHeight, PH),
	    prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.

   
prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) ->%N=0 should be the same as prev_hash(BP)
    element(N, BP#block_plus.prev_hashes).
prev_hash(X) -> 
    B = block(X),
    B#block.prev_hash.
hash(X) -> 
    testnet_hasher:doit(block_to_header(block(X))).
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    %the pointer to an empty trie is 0.
    Address = constants:master_address(),
    ID = 1,
    First = accounts:new(ID, Address, constants:initial_coins(), 0),
    io:fwrite("gensis maker accounts write \n"),
    Accounts = accounts:write(0, First),
    GovInit = governance:genesis_state(),
    Trees = trees:new(Accounts, 0, 0, 0, 0, GovInit),
    TreeRoot = trees:root_hash(Trees),
Block = {block_plus,{block,0,
		     <<0:(8*constants:hash_size())>>,
                   %<<0,0,0,0,0,0,0,0,0,0,0,0>>,
                   [],
		     TreeRoot,
                   %<<86,31,143,142,73,28,203,208,227,116,25,154>>,
                   1,0,4080,<<>>,1},
		     %constants:magic()}
            {pow,<<>>,4080,44358461744572027408730},
	 Trees,
            %{trees,1,0,0,0,0,38},
            0,
            {prev_hashes}},
    %Block = {pow,{block,0,<<0:(8*constants:hash_size())>>,[], TreeRoot,
	%	  1,0,4080, <<>>, constants:magic()},
	%     4080,44358461744572027408730},
    Block.
    %#block_plus{block = Block, trees = Trees}.
genesis() ->
{block_plus,{block,0,
                   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
                   [],
                   <<86,31,143,142,73,28,203,208,227,116,25,154>>,
                   1,0,4080,<<>>,1},
            {pow,<<>>,4080,44358461744572027408730},
            {trees,1,0,0,0,0,72},
            0,
            {prev_hashes}}.
block_reward(Trees, Height, ID) -> 
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    BlocksAgo = Height - BCM,
    Txs = block:txs(block:block(block:read_int(BlocksAgo))),
    TransactionFees = txs:fees(Txs),
    TransactionCosts = tx_costs(Txs, Governance, 0),
    BlockReward = governance:get_value(block_reward, Governance),
    NM = accounts:update(ID, Trees, BlockReward + TransactionFees - TransactionCosts, none, Height),
    %NM2 = accounts:update(1, Trees, ((BlockReward * 8) div 100), none, Height),
    accounts:write(
      %accounts:write(OldAccounts, NM2),
      OldAccounts,
      NM).
tx_costs([], _, Out) -> Out;
tx_costs([STx|T], Governance, Out) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    tx_costs(T, Governance, Cost+Out).
absorb_txs(PrevPlus, Height, Txs, {MinerID, MinerAddress}) -> 
    %this part reserves your ID.
    Trees = PrevPlus#block_plus.trees,
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    Rent = governance:get_value(account_rent, Governance),
    Acc2 = accounts:new(MinerID, MinerAddress, Rent * BCM * 13 div 10, Height),
    Accounts2 = accounts:write(OldAccounts, Acc2),
    Trees2 = trees:update_accounts(Trees, Accounts2),
    absorb_txs2(Trees2, Height, BCM, Txs);
absorb_txs(PrevPlus, Height, Txs, MinerID) -> 
    Trees = PrevPlus#block_plus.trees,
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    Rent = governance:get_value(account_rent, Governance),
    %create Miner's account
    Acc1 = accounts:update(MinerID, Trees, Rent * BCM * 13 div 10, none, Height),%gives 30% more than the amount of money you need to keep the account open until you get your reward.
    Accounts2 = accounts:write(OldAccounts, Acc1),
    Trees2 = trees:update_accounts(Trees, Accounts2),
    absorb_txs2(Trees2, Height, BCM, Txs).

absorb_txs2(Trees2, Height, BCM, Txs) ->
    %this part gives a block reward
    Governance = trees:governance(Trees2),
    Accounts2 = trees:accounts(Trees2),
    BlocksAgo = Height - BCM,
    Trees3 = 
	if
	    BlocksAgo > 0 ->
		%block reward
		MinesBlock = mine_block_ago(BlocksAgo),
		case MinesBlock of
		    -1 -> Trees2;
		    ID ->
			BR = governance:get_value(block_reward, Governance),
			Acc2 = accounts:update(ID, Trees2, BR, none, Height),
			Accounts3 = accounts:write(Accounts2, Acc2),
			trees:update_account(Trees2, Accounts3)
		end;
	    true -> Trees2
	end,
    txs:digest(Txs, 
	       Trees3,
	       Height).
    
absorb_txs_old(PrevPlus, Height, Txs) ->
    Trees = PrevPlus#block_plus.trees,
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    %BlockReward = governance:get_value(block_reward, Governance),
    BCM = governance:get_value(block_creation_maturity, Governance),
    BlocksAgo = Height - BCM,
    MinesBlock = mine_block_ago(BlocksAgo),
    NewAccounts = 
	case MinesBlock of
	    -1 ->
		OldAccounts;
	    {ID, Address} -> %for miners who don't yet have an account.
		{_, Acc, _} = accounts:get(ID, OldAccounts),
		Trees3 = 
		    case Acc of
			empty ->
			    NM = accounts:new(ID, Address, 0, Height),
			    io:fwrite("absorb_txs accounts write \n"),
			    Accounts2 = accounts:write(OldAccounts, NM),
			    trees:update_accounts(Trees, Accounts2);
			_ ->
			    Trees
		    end,
		    block_reward(Trees3, Height, ID);
	    MB -> %If you already have an account.
		{_, OldAcc, _} = accounts:get(MB, OldAccounts),
		MB2 = 
		    case OldAcc of
		    empty -> 1;
		    _ -> MB
		end,
		block_reward(Trees, Height, MB2)
	end,
    NewTrees = trees:update_accounts(Trees, NewAccounts),
    txs:digest(Txs, 
	       NewTrees,
	       Height).
    
make(PrevHash, Txs, ID) ->%ID is the user who gets rewarded for mining this block.
    ParentPlus = read(PrevHash),
    Parent = block(ParentPlus),
    Height = Parent#block.height + 1,
    NewTrees = absorb_txs(ParentPlus, Height, Txs, ID),
    NextDifficulty = next_difficulty(ParentPlus),
    #block_plus{block = 
		#block{height = Height,
		       prev_hash = PrevHash,
		       txs = Txs,
		       trees = trees:root_hash(NewTrees),
		       mines_block = ID,
		       time = time_now()-5,
		       difficulty = NextDifficulty},
		accumulative_difficulty = next_acc(ParentPlus, NextDifficulty),
		trees = NewTrees,
		prev_hashes = prev_hashes(PrevHash)
      }.
next_acc(Parent, ND) ->
    Parent#block_plus.accumulative_difficulty + pow:sci2int(ND).
    %We need to reward the miner the sum of transaction fees.
mine(BP, Times) when is_record(BP, block_plus) ->
    Block = BP#block_plus.block,
    case mine2(Block, Times) of
	false -> false;
	Pow -> BP#block_plus{pow = Pow}
    end.
mine2(Block, Times) ->
    PH = Block#block.prev_hash,
    ParentPlus = read(PH),
    Trees = ParentPlus#block_plus.trees,
    Difficulty = Block#block.difficulty,
    Governance = trees:governance(Trees),
    BlockReward = governance:get_value(block_reward, Governance),
    MineDiff = (Difficulty * BlockReward) div constants:initial_block_reward(),
    Pow = pow:pow(hash(Block), MineDiff, Times, constants:hash_size()),
    Pow.
next_difficulty(ParentPlus) ->
    Trees = ParentPlus#block_plus.trees,
    Parent = block(ParentPlus),
    Height = Parent#block.height + 1,
    RF = constants:retarget_frequency(),
    Governance = trees:governance(Trees),
    RP = governance:get_value(retarget_period, Governance),
    X = Height rem RP,
    OldDiff = Parent#block.difficulty,
    PrevHash = hash(ParentPlus),
    if
	Height == 1 -> constants:initial_difficulty(); 
	Height < (RF+1) -> OldDiff;
	X == 0 -> 
	    retarget(PrevHash, Parent#block.difficulty, Trees);
	true ->  OldDiff
    end.
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
    
retarget(PrevHash, Difficulty, Trees) ->    
    Governance = trees:governance(Trees),
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget2(PrevHash, F, []),
    {Times2, _} = retarget2(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,
    %io:fwrite([Ratio, Difficulty]),%10/2, 4096
    BT = governance:get_value(block_time, Governance),
    ND = pow:recalculate(Difficulty, BT, max(1, T)),
    max(ND, constants:initial_difficulty()).
retarget2(Hash, 0, L) -> {L, Hash};
retarget2(Hash, N, L) -> 
    BP = read(Hash),
    B = block(BP),
    T = B#block.time,
    H = B#block.prev_hash,
    retarget2(H, N-1, [T|L]).
check1(BP) ->    
    BH = hash(BP),
    GH = hash(genesis()),
    if
	BH == GH ->
	    {BH, 0};
	true ->    
	    Block = block(BP),
	    Difficulty = Block#block.difficulty,
	    true = Difficulty >= constants:initial_difficulty(),
	    true = check_pow(BP),
	    PowBlock = BP#block_plus.pow,
	    Header = hash(Block),
	    Header = pow:data(PowBlock),
	    true = Block#block.time < time_now(),
	    true = one_tx_per_account(Block),
	    {BH, Block#block.prev_hash}
    end.

%check2(BP) when is_record(BP, block_plus) ->
%    check2(pow_block(BP));
check_pow(BP) ->
    Pow = BP#block_plus.pow,
    A = pow:check_pow(Pow, constants:hash_size()),
    B = hash(BP) == pow:data(Pow),
    A and B.

check2(BP) ->
    %check that the time is later than the median of the last 100 blocks.

    %check2 assumes that the parent is in the database already.
    Block = block(BP),
    true = Block#block.magic == constants:magic(),
    Difficulty = Block#block.difficulty,
    PH = Block#block.prev_hash,
    ParentPlus = read(PH),
    Trees0 = ParentPlus#block_plus.trees,
    Difficulty = next_difficulty(ParentPlus),
    true = is_record(ParentPlus, block_plus),
    Prev = block(ParentPlus),
    Governance = trees:governance(Trees0),
    CL = governance:get_value(comment_limit, Governance),
    Comment = Block#block.comment,
    true = is_binary(Comment),
    true = size(Comment) < CL,
    BlockReward = governance:get_value(block_reward, Governance),
    MineDiff = (Difficulty * BlockReward) div constants:initial_block_reward(),
    Pow = BP#block_plus.pow,
    Header = pow:data(Pow),
    Header = hash(Block),
    true = pow:above_min(Pow, MineDiff, constants:hash_size()),
    B = size(term_to_binary(Block#block.txs)),
    MaxBlockSize = governance:get_value(max_block_size, Governance),
    true = B < MaxBlockSize,
    
    BTAM = governance:get_value(block_time_after_median, Governance),
    ML = median_last(PH, BTAM),
    true = Block#block.time > ML,
    Height = Block#block.height,
    true = (Height-1) == Prev#block.height,
    GP = free_constants:garbage_period(),
    case Height rem GP of
       0 -> 
	    RD = free_constants:revert_depth(),
	    MRDGP = max(RD, GP),
	    if 
		Height > MRDGP ->
		    io:fwrite("check2 garbage "),
		    block_absorber:garbage();
		    %trees:garbage();
		true -> ok
	    end;
       _ -> ok
    end,
    TreeHash = Block#block.trees,
    MinerID = Block#block.mines_block,
    Trees = absorb_txs(ParentPlus, Height, Block#block.txs, MinerID),
    TreeHash = trees:root_hash(Trees),
    MyAddress = keys:address(),
    MTB = Block#block.mines_block,
    case MTB of
	{ID, MyAddress} ->
	    keys:update_id(ID);
	    %because of hash_check, this function is only run once per block. 
	_ -> ok
    end,
    BP#block_plus{block = Block, trees = Trees, accumulative_difficulty = next_acc(ParentPlus, Block#block.difficulty), prev_hashes = prev_hashes(hash(Prev))}.

mine_block_ago(Height) when Height < 1 ->
    -1;
mine_block_ago(Height) ->
    BP = block:read_int(Height),
    Block = block(BP),
    Block#block.mines_block.

median_last(BH, N) ->
    median(block_times(BH, N)).
block_times(_, 0) -> [];
block_times(<<0:96>>, N) ->
    list_many(N, 0);
block_times(H, N) ->
    BP = block:read(H),
    Block = block(BP),
    BH2 = Block#block.prev_hash,
    T = Block#block.time,
    [T|block_times(BH2, N-1)].
list_many(0, _) -> [];
list_many(N, X) -> [X|list_many(N-1, X)].

binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    %File1 = "blocks/"++[G],
    %os:cmd("mkdir " ++ File1),
    %File2 = File1 ++ "/" ++ [H],
    %os:cmd("mkdir " ++ File2),
    "blocks/" ++C++".db".
read(Hash) ->
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
read_many(N, Many) ->
    io:fwrite(packer:pack({read_many, N, Many})),
    X = read_int(N),
    PH = prev_hash(X),
    [X|read_many2(PH, Many)].
read_many2(H, 1) ->
    read(H);
read_many2(H, M) ->
    X = read(H),
    case X of
	empty -> [];
	_ ->
	    PH = prev_hash(X),
	    [read(H)|read_many2(PH, M-1)]
    end.
read_int(N) ->%currently O(n), needs to be improved to O(lg(n))
    true = N >= 0,
    read_int(N,top:doit()).
read_int(N, BH) ->
    Block = read(BH),
    M = height(Block),
    D = M-N,
    if 
	D<0 -> 
	    empty;
	    %io:fwrite("D is "),
	       %io:fwrite(integer_to_list(D)),
	       %D = 5;
	D == 0 -> Block;
	true ->
	    read_int(N, prev_hash(lg(D), Block))
    end.
one_tx_per_account(Block) ->
    %make sure that every account and channel only gets updated once per block.
    true.
	    
    
    


test() ->
    io:fwrite("top, \n"),
    block:read(top:doit()),
    PH = top:doit(),
    BP = read(PH),
    Trees = trees(BP),
    Accounts = trees:accounts(Trees),
    %Accounts = BP#block_plus.accounts,
    _ = accounts:get(1, Accounts),
    %{block_plus, Block, _, _, _} = make(PH, [], 1),
    Block = make(PH, [], 1),
    io:fwrite(packer:pack(Block)),
    io:fwrite("top 2, \n"),
    MBlock = mine(Block, 100000000),
    io:fwrite(packer:pack(MBlock)),
    io:fwrite("top 3, \n"),
    check2(MBlock),
    success.
new_id(N) -> 
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    ID = new_id2(N, Accounts),
    H = height(read(top:doit())),
    M = max(0, H-BCM),
    new_id3(keys:pubkey(), M, H, ID, Accounts).
new_id3(_, N, H, ID, _) when N > H -> ID;
new_id3(Address, N, H, ID, Accounts) ->
    BP = read_int(N),
    B = block(BP),
    case B#block.mines_block of
	{ID, Address} -> ID;
	{ID, _} -> new_id3(Address, 
			   N+1,
			   H,
			   new_id2(ID+1, Accounts),
			   Accounts);
	_ -> new_id3(Address, N+1, H, ID, Accounts)
    end.
new_id2(N, Accounts) ->
   case accounts:get(N, Accounts) of
       {_, empty, _} -> N;
       _ -> new_id2(N+1, Accounts)
   end.
	   
mine_test() ->
    PH = top:doit(),
    BP = make(PH, [], keys:id()),
    PBlock = mine(BP, 1000000000),
    block_absorber:doit_ask(PBlock),
    mine_blocks(10, 100000),
    success.
mine_blocks(A, B) -> 
    Cores = guess_number_of_cpu_cores(),
    mine_blocks(A,B,Cores).
mine_blocks(0, _, _) -> 
    block_absorber:garbage(),
    %trees:garbage(), 
    success;
mine_blocks(N, Times, Cores) -> 
    io:fwrite("mine blocks\n"),
    PH = top:doit(),
    {_,_,Txs} = tx_pool:data(),
    ID = case {keys:pubkey(), keys:id()} of
	     {[], X} -> io:fwrite("you need to make an account before you can mine. look at docs/new_account.md"),
			X = 294393793232;
	     {_, -1} ->
		 NewID = new_id(1),
		 {NewID, keys:address()};
	     {_, Identity} -> Identity
	 end,
    BP = make(PH, Txs, ID),
    
    %io:fwrite("mining attempt #"),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite(" time "),
    %io:fwrite(integer_to_list(time_now())),
    %io:fwrite(" diff "),
    %io:fwrite(integer_to_list(Block#block.difficulty)),
    %erlang:system_info(logical_processors_available)
    %io:fwrite(" using "),
    %io:fwrite(integer_to_list(Cores)),
    %io:fwrite(" CPU"),
    %io:fwrite("\n"),
    F = fun() ->
		case mine(BP, Times) of
		    false -> false;
		    PBlock -> 
			io:fwrite("FOUND A BLOCK !\n"),
			H = height(PBlock) rem 10,
			case H of
			    0 ->
				block_absorber:garbage();
			    _ -> ok
			end,
			block_absorber:doit_ask(PBlock)
			%timer:sleep(250)
		end
	end,
    spawn_many(Cores-1, F),
    F(),
    %timer:sleep(100),
    mine_blocks(N-1, Times, Cores).
    
spawn_many(N, _) when N < 1 -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
guess_number_of_cpu_cores() ->    
    case free_constants:test_mode() of
	true -> 1;
	false ->
	    X = erlang:system_info(logical_processors_available),
	    Y = if
		    X == unknown ->
						% Happens on Mac OS X.
			erlang:system_info(schedulers);
		    is_integer(X) -> 
						%ubuntu
			X;
		    true -> io:fwrite("number of CPU unknown, only using 1"), 1
		end,
	    min(Y, free_constants:cores_to_mine())
    end.
