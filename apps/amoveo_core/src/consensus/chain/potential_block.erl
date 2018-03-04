-module(potential_block).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([new/0, read/0, save/0, dump/0, check/0, save/2]).
%-define(potential_block, "data/potential_blocks.db").
-define(refresh_period, 40).%in seconds
-include("../../records.hrl").
-record(pb, {block, time}).
init(ok) -> 
    process_flag(trap_exit, true),
    %X = new_internal(""),
    Z = #pb{block = "", time = now()},
    {ok, Z}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    %B = X#pb.block,
    %PH = B#block.prev_hash,
    %PB = block:get_by_hash(PH),
    %tree_data:garbage(B, PB),
    io:format("potential block died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(dump, _, _) -> 
    {reply, ok, #pb{block = "", time = now()}};
handle_call({save, Txs, Height}, _, _) -> 
    PB = block:get_by_height(Height),
    Top = block:block_to_header(PB),
    Block = block:make(Top, Txs, PB#block.trees, keys:pubkey()),
    {reply, ok, #pb{block = Block, time = now()}};
handle_call(save, _, _) -> 
    Block = new_internal(""),
    {reply, ok, #pb{block = Block, time = now()}};
handle_call(new, _, Old) -> 
    Block = new_internal(Old#pb.block),
    {reply, ok, #pb{block = Block, time = now()}};
handle_call(check, _From, X) -> 
    {reply, X#pb.block, X};
handle_call(read, _From, X) -> 
    D = delta(X#pb.time, now()),
    B = X#pb.block,
    BH = case B of
	     "" -> 0;
	     _ -> B#block.height
	 end,
    TP = tx_pool:get(),
    NH = TP#tx_pool.height,
    %api:sync(),
    %sync:start(),
    Y = if
	    B == "" ->
		#pb{block = new_internal2(TP), time = now()};
	    ((D > ?refresh_period) and (BH == NH)) ->
		#pb{block = new_internal(B, TP), time = now()};
	    (D > ?refresh_period) ->
		#pb{block = new_internal2(TP), time = now()};
	    true -> X
	end,
    {reply, Y#pb.block, Y};
handle_call(_, _From, X) -> {reply, X, X}.
delta({A, B, _}, {D, E, _}) ->%start, end
    C = (A*1000000) + B,
    F = (D*1000000) + E,
    F - C.
new() -> gen_server:call(?MODULE, new).
save() -> gen_server:call(?MODULE, save).
save(Txs, Height) -> gen_server:call(?MODULE, {save, Txs, Height}).
dump() -> gen_server:call(?MODULE, dump).
read() -> gen_server:call(?MODULE, read).
check() -> gen_server:call(?MODULE, check).
new_internal2(TP) ->
    Txs = TP#tx_pool.txs,
    T = TP#tx_pool.height,
    PB = block:get_by_height(T),
    Top = block:block_to_header(PB),%it would be way faster if we had a copy of the block's hash ready, and we just looked up the header by hash.
    block:make(Top, Txs, PB#block.trees, keys:pubkey()).
new_internal("") ->
    TP = tx_pool:get(),
    new_internal2(TP);
new_internal(Old) ->
    TP = tx_pool:get(),
    new_internal(Old, TP).
new_internal(Old, TP) ->
    PH = Old#block.prev_hash,
    PB = block:get_by_hash(PH),
    tree_data:garbage(Old, PB),
    new_internal2(TP).
    
    
