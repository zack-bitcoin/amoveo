-module(block_absorber).
-behaviour(gen_server).

%% API
-export([enqueue/1, %% async request
         save/1,    %% returns after saving
         do_save/1,
         garbage/0]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

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

do_save(BlockPlus) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(CompressedBlockPlus)), % Sanity check, not important for long-term
    Hash = block:hash(BlockPlus),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    ok = db:save(BlockFile, CompressedBlockPlus).

garbage() ->
    gen_server:cast(?MODULE, garbage).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    {ok, []}.

handle_call({doit, BP}, _From, State) ->
    ok = absorb_internal(BP),
    {reply, ok, State}.

handle_cast(garbage, State) ->
    trees:garbage(),
    {noreply, State};
handle_cast({doit, BP}, State) ->
    ok = absorb_internal(BP),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internals

absorb_internal(Block) ->
    BlockHash = block:hash(Block),
    NextBlock = block:prev_hash(Block),
    case block_hashes:check(BlockHash) of
        true ->
            lager:info("We have seen this block before, so block_absorber will ignore it");
        false ->
            true = block_hashes:check(NextBlock), %check that the previous block is known.
            false = empty == block:get_by_hash(NextBlock), %check that previous block was valid
            block_hashes:add(BlockHash),%Don't waste time checking invalid blocks more than once.
            Header = block:block_to_header(Block),
            headers:absorb([Header]),
            {true, Block2} = block:check(Block),
            do_save(Block2),
            BlockHash = block:hash(Block2),
            timer:sleep(100),
            {_, _, Txs} = tx_pool:data(),
            tx_pool:dump(),
            tx_pool_feeder:absorb(Txs)
    end.
