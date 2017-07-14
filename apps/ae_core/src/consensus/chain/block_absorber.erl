
-module(block_absorber).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 doit/1, garbage/0,
	 save_helper/1]).
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


doit([]) -> ok;
doit([H|T]) ->
    doit(H),
    doit(T);
doit(InputBlock) ->
    gen_server:cast(?MODULE, {doit, InputBlock}).

    
absorb(BP) ->
    %BH = block:hash(BP),
    BH = block:hash(BP),
    {BH, NextBlock} = block:check1(BP),
    case block_hashes:check(BH) of
	true -> ok;%If we have seen this block before, then don't process it again.
	false ->
	    %{BH, _} = block:check1(BP),
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    BP2 = block:check2(BP),
	    %io:fwrite(packer:pack(BP)),
	    save(BP2)
    end.   
save_helper(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(BlockPlus),
    BF = block:binary_to_file(Hash),
    ok = db:save(BF, Z).
    
save(BlockPlus) ->
    save_helper(BlockPlus),
    top:add(BlockPlus),
    block:hash(BlockPlus).
