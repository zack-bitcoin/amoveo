-module(block_absorber).
-behaviour(gen_server).

%% API
-export([
	 enqueue/1, %% async request
	 save/1,    %% returs after saving
	 do_save/1
]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init(ok) -> 
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    io:fwrite("block absorber died\n"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({doit, BP}, X) ->
    absorb_internal(BP),
    {noreply, X}.
handle_call({doit, BP}, _From, X) -> 
    absorb_internal(BP),
    {reply, ok, X}.
garbage() -> gen_server:cast(?MODULE, garbage).

%% API functions

enqueue(InputBlocks) when is_list(InputBlocks) ->
    [enqueue(InputBlock) || InputBlock <- InputBlocks];
enqueue(InputBlock) ->
    headers:absorb([block:block_to_header(InputBlock)]),
    gen_server:cast(?MODULE, {doit, InputBlock}).

save(InputBlocks) when is_list(InputBlocks) ->
    [save(InputBlock) || InputBlock <- InputBlocks];
save(InputBlock) ->
    gen_server:call(?MODULE, {doit, InputBlock}).

absorb_internal(Block) ->
    %io:fwrite("about to absorb a block \n"),
    %io:fwrite(packer:pack(Block)),
    %io:fwrite("\n"),
    BH = block:hash(Block),
    NextBlock = block:prev_hash(Block),
    Height = block:height(Block),
    BHC = block_hashes:check(BH),
    if
        Height == 0 -> ok;
        BHC ->
            %io:fwrite("we have seen this block before. so block absorber will ignore it.\n"),
	    ok;
	true ->
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    false = empty == block:get_by_hash(NextBlock), %check that previous block was valid
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    Header = block:block_to_header(Block),
	    headers:absorb([Header]),

	    %check that the proofs root matches the header.

	    {true, Block2} = block:check(Block),
	    do_save(Block2),
	    BH = block:hash(Block2),
            {ok, PrunePeriod} = application:get_env(ae_core, prune_period),
            HeaderHeight = api:height(),
            if
                (((Height rem PrunePeriod) == 0) and ((HeaderHeight - Height) > PrunePeriod)) ->
                    trees:prune([Block2]);
                (Height == HeaderHeight) ->
                    trees:prune();
                true -> ok
            end,
            spawn(fun () ->
                          {_, _, Txs} = tx_pool:data(),
                          tx_pool:dump(),
                          tx_pool_feeder:absorb(Txs)
                  end),
            order_book:match(),
            timer:sleep(20),
            io:fwrite("absorb block "),
            io:fwrite(integer_to_list(block:height(Block2))),
            io:fwrite("\n")
    end.
do_save(BlockPlus) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(CompressedBlockPlus)), % Sanity check, not important for long-term
    Hash = block:hash(BlockPlus),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    ok = db:save(BlockFile, CompressedBlockPlus).




