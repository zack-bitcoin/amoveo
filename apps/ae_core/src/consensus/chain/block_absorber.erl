-module(block_absorber).
-behaviour(gen_server).
-include("../../records.hrl").
-export([prune/0, %% delete unneeded things from trees
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
handle_cast(prune, X) ->
    trees:prune(),
    {noreply, X};
handle_cast({doit, BP}, X) ->
    absorb_internal(BP),
    {noreply, X}.
handle_call({doit, BP}, _From, X) -> 
    absorb_internal(BP),
    {reply, ok, X}.
prune() -> gen_server:cast(?MODULE, prune).
enqueue([]) -> ok;
enqueue([B|T]) -> enqueue(B), enqueue(T);
enqueue(B) -> gen_server:cast(?MODULE, {doit, B}).
save([]) -> ok;
save([B|T]) -> save(B), save(T);
save(B) -> gen_server:call(?MODULE, {doit, B}).
absorb_internal(Block) ->
    BH = block:hash(Block),
    NextBlock = Block#block.prev_hash,
    Height = Block#block.height,
    BHC = block_hashes:check(BH),
    if
        Height == 0 -> ok;
        BHC -> ok; %we already have this block
	true ->
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    false = empty == block:get_by_hash(NextBlock), %check that previous block was valid
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
            TH = headers:read(BH),
            Header = case TH of
                         {ok, H} -> H;
                         error -> 
                             H = block:block_to_header(Block),
                             headers:absorb([H]),
                             H
                     end,
	    {true, Block2} = block:check(Block),
	    do_save(Block2),
	    BH = block:hash(Block2),
            {ok, PrunePeriod} = application:get_env(ae_core, prune_period),
            HeaderHeight = api:height(),
            recent_blocks:add(BH, Header#header.accumulative_difficulty, Height),
            if
                (((Height rem PrunePeriod) == 0) and (HeaderHeight > PrunePeriod)) -> trees:prune();
                true -> ok
            end,
            spawn(fun () ->
                          {_, _, Txs} = tx_pool:data(),
                          tx_pool:dump(),
                          tx_pool_feeder:absorb(Txs)
                  end),
            order_book:match(),
            io:fwrite("absorb block "),
            io:fwrite(integer_to_list(Block2#block.height)),
            io:fwrite("\n")
    end.
do_save(BlockPlus) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(BlockPlus)),
    Hash = block:hash(BlockPlus),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    ok = db:save(BlockFile, CompressedBlockPlus).
