-module(block).
-export([hash/1,check/1,test/0,genesis/0,make_block/3,height/1]).
-record(block, {height, prev_hash, txs, channels, accounts, mines_block, time, difficulty}).%tries: txs, channels, census, 
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
height(Block) ->
    Block#block.height.
hash(Block) ->
    B2 = term_to_binary(Block),
    hash:doit(B2).

time_now() ->
    (os:system_time() div 1000000000) - 1480952170.
genesis() ->
    #block{height = 0,
	   txs = [],
	   channels = stem:hash(stem:new_empty(), trie:cfg(channels)),
	   accounts = stem:hash(stem:new_empty(), trie:cfg(accounts)),
	   mines_block = <<"9zpTqk93izqvN76Z">>,
	   time = 0,
	   difficulty = constants:initial_difficulty()}.
make_block(PrevHash, Txs, ID) ->
    Parent = block_tree:read(PrevHash),
    Height = Parent#block.height + 1,
    {NewChannels, NewAccounts} = 
	txs:digest(Txs, 
		   Parent#block.channels, 
		   Parent#block.accounts,
		   Height),
    #block{height = Height,
	   prev_hash = PrevHash,
	   txs = Txs,
	   channels = NewChannels,
	   accounts = NewAccounts,
	   mines_block = ID,
	   time = time_now(),
	   difficulty = constants:initial_difficulty()
	  }.
    
    
check(PowBlock) ->
    %check that the time is later than the median of the last 100 blocks. 
    Block = pow:data(PowBlock),
    Difficulty = Block#block.difficulty,
    pow:above_min(PowBlock, Difficulty),
    PH = Block#block.prev_hash,
    Prev = block_tree:read(PH),
    Difficulty = constants:initial_difficulty(),
    true = (Block#block.height-1) == Prev#block.height,
    true = Block#block.time < time_now(),
    X = {Block#block.channels, Block#block.accounts},
    X = txs:digest(Block#block.txs, 
		   Prev#block.channels,
		   Prev#block.accounts,
		   Block#block.height),
    true.

%next_difficulty(_Block) ->
    %take the median time on the last 2000 blocks, subtract it from the current time, divide by 1000. This is the current blockrate. Adjust the difficulty to make the rate better.
%    constants:initial_difficulty().


test() ->
    
    ok.
