-module(block_absorber).
-behaviour(gen_server).
-include("../../records.hrl").
-export([%prune/0, %% delete unneeded things from trees asynchronously
	 %synch_prune/1,
         enqueue/1, %% async request
	 save/1,    %% returns after saving
	 do_save/1]). %% run without gen_server
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    io:fwrite("block absorber died! \n"),
    ok.
handle_info(_, X) -> {noreply, X}.
%handle_cast(prune, X) ->
%    trees:prune(),
%    {noreply, X};
handle_cast({doit, BP}, X) ->
    absorb_internal(BP),
    {noreply, X}.
%handle_call({prune, Blocks}, _, X) ->
%    B = Blocks ++ trees:hash2blocks(recent_blocks:read()),
%    trees:prune(B),
%    {reply, ok, X};
handle_call({doit, BP}, _From, X) -> 
    Y = absorb_internal(BP),
    {reply, Y, X}.
%prune() -> gen_server:cast(?MODULE, prune).
%synch_prune(Blocks) -> gen_server:call(?MODULE, {prune, Blocks}, 10000).
enqueue([]) -> ok;
enqueue([B|T]) -> enqueue(B), enqueue(T);
enqueue(B) -> gen_server:cast(?MODULE, {doit, B}).
%save([]) -> ok;
save([T]) -> save(T);
save([B|T]) -> save(B), save(T);
save(B) -> gen_server:call(?MODULE, {doit, B}, 10000).
absorb_internal(Block) ->
    BH = block:hash(Block),
    NextBlock = Block#block.prev_hash,
    Height = Block#block.height,
    BHC = block_hashes:check(BH),
    if
        Height == 0 -> 
	    {ok, Header00} = headers:read(BH),
	    Header00;
        BHC -> 3; %we already have this block
	true ->
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    false = empty == block:get_by_hash(NextBlock), %check that previous block was valid
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
            TH = headers:read(BH),
            Header = case TH of
                         {ok, H} -> 
			     headers:absorb_with_block([H]),
			     H;
                         error -> 
                             H = block:block_to_header(Block),
                             headers:absorb([H]),
			     headers:absorb_with_block([H]),
                             H
                     end,
	    {true, Block2} = block:check(Block),
	    BH = block:hash(Block2),
	    do_save(Block2),
	    if 
		(Height == 1) ->
		    {ok, RD} = application:get_env(amoveo_core, revert_depth),
		    HH = (headers:top())#header.height,
		    if
			HH - RD < Height -> sync_mode:normal();
			true -> ok
		    end;
		true -> ok
	    end,
	    case sync_mode:check() of
		normal -> 
		    Txs = (tx_pool:get())#tx_pool.txs,
		    tx_pool:dump(Block2),
		    OldTxs = tl(Block#block.txs),
		    Keep = lists:filter(fun(T) -> not(tx_pool_feeder:is_in(testnet_sign:data(T), OldTxs)) end, Txs),%This n**2 algorithm is slow. We can make it n*log(n) by sorting both lists first, and then comparing them.
		    tx_pool_feeder:absorb_async(Keep),
		    order_book:match(),
		    recent_blocks:add(BH, Header#header.accumulative_difficulty, Height),
		    %potential_block:new();
		    potential_block:save(),
		    sync:push_new_block(Block2);
		quick -> 
		    recent_blocks:add(BH, Header#header.accumulative_difficulty, Height),
		    tx_pool:dump(Block2),
		    potential_block:dump()
	    end,
            io:fwrite("absorb block "),
            io:fwrite(integer_to_list(Block2#block.height)),
            io:fwrite("\n"),
	    Header
    end.
do_save(BlockPlus) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(BlockPlus)),
    Hash = block:hash(BlockPlus),
    BlockFile = amoveo_utils:binary_to_file_path(blocks, Hash),
    ok = db:save(BlockFile, CompressedBlockPlus).
