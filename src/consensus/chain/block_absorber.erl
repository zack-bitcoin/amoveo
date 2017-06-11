-module(block_absorber).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 doit/1, garbage/0, save_helper/1]).
init(ok) -> 
    %save(block:genesis()),
    save(block:genesis_maker()),
    %block:make_files(),
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) -> 
    trees:garbage(),
    {noreply, X};
handle_cast({doit, BP}, X) -> 
    absorb(BP),
    %trees:garbage(),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
garbage() ->
    gen_server:cast(?MODULE, garbage).
    
doit(X) ->
    %absorb(X).
    gen_server:cast(?MODULE, {doit, X}).
    
absorb(BP) ->
    %BH = block:hash(BP),
    BH = block:hash(BP),
    {BH, _} = block:check1(BP),
    case block_hashes:check(BH) of
	true -> ok;%If we have seen this block before, then don't process it again.
	false ->
	    %{BH, _} = block:check1(BP),
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    BP2 = block:check2(BP),
	    io:fwrite("absorb block: "++
		      integer_to_list(block:height(BP)) ++
		      "\n"),
	    %io:fwrite(packer:pack(BP)),
	    save(BP2)
    end.   
save_helper(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(BlockPlus),
    BF = block:binary_to_file(Hash),
    db:save(BF, Z).
    
save(BlockPlus) ->
    save_helper(BlockPlus),
    top:add(BlockPlus),
    block:hash(BlockPlus).
