
-module(block_absorber).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 save_helper/1]).

%% API
-export([
    enqueue/1, %% async request
    save/1,    %% returs after saving
    garbage/0
]).

init(ok) -> 
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> lager:debug("terminating block_absorber gen_srv"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) -> 
    trees:garbage(),
    {noreply, X};
handle_cast({doit, BP}, X) ->
    absorb(BP),
    {noreply, X}.
handle_call({doit, BP}, _From, X) -> 
    absorb(BP),
    {reply, ok, X};
handle_call(_, _From, X) -> {reply, X, X}.
garbage() ->
    gen_server:cast(?MODULE, garbage).

enqueue(InputBlocks) when is_list(InputBlocks) ->
    [enqueue(InputBlock) || InputBlock <- InputBlocks];
enqueue(InputBlock) ->
    headers:absorb([block:block_to_header_new(InputBlock)]),
    gen_server:cast(?MODULE, {doit, InputBlock}).


save(InputBlocks) when is_list(InputBlocks) ->
    [save(InputBlock) || InputBlock <- InputBlocks];
save(InputBlock) ->
    gen_server:call(?MODULE, {doit, InputBlock}).

    
absorb(BP) ->
    %BH = block:hash(BP),
    Header = block_new:block_to_header(BP),
    headers:absorb([Header]),
    BH = block_new:hash(BP),


    {BH, NextBlock} = block:check1(BP),
    case block_hashes:check(BH) of
	true -> ok;%If we have seen this block before, then don't process it again.
	false ->
	    %{BH, _} = block:check1(BP),
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    BP2 = block:check2(BP),
	    %io:fwrite(packer:pack(BP)),
	    do_save(BP2)
    end.   
save_helper(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(BlockPlus),
    BF = block:binary_to_file(Hash),
    ok = db:save(BF, Z).
    
do_save(BlockPlus) ->
    save_helper(BlockPlus),
    top:add(BlockPlus),
    block:hash(BlockPlus).
