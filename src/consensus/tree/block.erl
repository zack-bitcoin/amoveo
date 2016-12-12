-module(block).
-export([hash/1,check/1,test/0,genesis/0,make/3,mine/2,height/1,accounts/1,channels/1,accounts_hash/1,channels_hash/1,save/1,absorb/1,read/1,binary_to_file/1]).
-record(block, {height, prev_hash, txs, channels, accounts, mines_block, time, difficulty}).%tries: txs, channels, census, 
-record(block_plus, {block, accounts, channels}).%The accounts and channels in this structure only matter for the local node. they are pointers to the locations in memory that are the root locations of the account and channel tries on this node.
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
channels(Block) ->
    Block#block_plus.channels.
channels_hash(BP) when is_record(BP, block_plus) ->
    channels_hash(BP#block_plus.block);
channels_hash(Block) -> Block#block.channels.
accounts(BP) ->
    BP#block_plus.accounts.
accounts_hash(BP) when is_record(BP, block_plus) ->
    accounts_hash(BP#block_plus.block);
accounts_hash(Block) ->
    Block#block.accounts.
height(BP) when is_record(BP, block_plus) ->
    height(BP#block_plus.block);
height(Block) when is_record(Block, block)->
    Block#block.height;
height(X) ->
    io:fwrite("error. should be a block, "),
    io:fwrite(X).

hash(BP) when is_record(BP, block_plus) ->
    hash(BP#block_plus.block);
hash(Block) ->
    B2 = term_to_binary(Block),
    hash:doit(B2).

time_now() ->
    (os:system_time() div 1000000000) - 1480952170.
genesis() ->
    Address = constants:master_address(),
    ID = 1,
    First = account:new(ID, Address, constants:initial_coins(), 0),
    %io:fwrite(First),
    Accounts = account:write(0, First, ID),
    AccRoot = account:root_hash(Accounts),
    io:fwrite("AccRoot "),
    io:fwrite(AccRoot),
    io:fwrite("\n"),
    ChaRoot = trie:root_hash(channels, 0),
    Block = 
	#block{height = 0,
	       txs = [],
	       channels = ChaRoot,
	       accounts = AccRoot,
	       mines_block = <<"9zpTqk93izqvN76Z">>,
	       time = 0,
	       difficulty = constants:initial_difficulty()},
    #block_plus{block = Block, channels = 0, accounts = Accounts}.
make(PrevHash, Txs, ID) ->%ID is the user who gets rewarded for mining this block.
    ParentPlus = read(PrevHash),
    Parent = ParentPlus#block_plus.block,
    Height = Parent#block.height + 1,
    {NewChannels, NewAccounts} = 
	txs:digest(Txs, 
		   ParentPlus#block_plus.channels, 
		   ParentPlus#block_plus.accounts,
		   Height),
    CHash = trie:root_hash(channels, NewChannels),
    AHash = account:root_hash(NewAccounts),
    #block_plus{
       block = 
	   #block{height = Height,
		  prev_hash = PrevHash,
		  txs = Txs,
		  channels = CHash,
		  accounts = AHash,
		  mines_block = ID,
		  time = time_now()-5,
		  difficulty = Parent#block.difficulty},
       channels = NewChannels, 
       accounts = NewAccounts}.
mine(Block, Times) ->
    Difficulty = Block#block.difficulty,
    pow:pow(Block, Difficulty, Times).
    
    
check(PowBlock) ->
    %check that the time is later than the median of the last 100 blocks. 
    Block = pow:data(PowBlock),
    Difficulty = Block#block.difficulty,
    pow:above_min(PowBlock, Difficulty),
    PH = Block#block.prev_hash,
    PrevPlus = read(PH),
    Prev = PrevPlus#block_plus.block,
    Difficulty = constants:initial_difficulty(),
    true = (Block#block.height-1) == Prev#block.height,
    true = Block#block.time < time_now(),
    {CH, AH} = {Block#block.channels, Block#block.accounts},
    {CR, AR} = txs:digest(Block#block.txs, 
		   PrevPlus#block_plus.channels,
		   PrevPlus#block_plus.accounts,
		   Block#block.height),
    CH = trie:root_hash(channels, CR),
    AH = account:root_hash(AR),
    #block_plus{block = Block, channels = CR, accounts = AR}.

%next_difficulty(_Block) ->
    %take the median time on the last 2000 blocks, subtract it from the current time, divide by 1000. This is the current blockrate. Adjust the difficulty to make the rate better.
%    constants:initial_difficulty().
absorb(PowBlock) ->
    BlockPlus = check(PowBlock),
    save(BlockPlus),
    top:add(BlockPlus#block_plus.block).
binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    %C = base64:encode(B),
    %H = binary_to_list(C),
    H = C,
    "blocks/"++H++".db".
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    binary_to_term(zlib:uncompress(Z)).
save(BlockPlus) ->
    Block = BlockPlus#block_plus.block,
    Z = zlib:compress(term_to_binary(BlockPlus)),
    Hash = hash(Block),
    BF = binary_to_file(hash(Block)),
    db:save(BF, Z),
    Hash.


test() ->
    block:read(top:doit()),
    PH = top:doit(),
    BP = read(PH),
    Accounts = BP#block_plus.accounts,
    _ = account:get(1, Accounts),
    {block_plus, Block, _, _} = make(PH, [], 0),
    PBlock = mine(Block, 1000000000),
    absorb(PBlock),
    success.

