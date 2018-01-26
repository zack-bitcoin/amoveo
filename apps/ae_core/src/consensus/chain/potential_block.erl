-module(potential_block).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([new/0, read/0, save/0, check/0]).
%-define(potential_block, "data/potential_blocks.db").
-define(refresh_period, 40).%in seconds
-include("../../records.hrl").
-record(pb, {block, time}).
init(ok) -> 
    process_flag(trap_exit, true),
    %Old = db:read(?potential_block),
    X = new_internal(""),
    Z = #pb{block = X, time = now()},
    {ok, Z}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    B = X#pb.block,
    PH = B#block.prev_hash,
    PB = block:get_by_hash(PH),
    tree_data:prune(B, PB),
    io:format("potential block died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
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
    Y = if
	    D > ?refresh_period ->
		#pb{block = new_internal(B), time = now()};
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
read() -> gen_server:call(?MODULE, read).
check() -> gen_server:call(?MODULE, check).
new_internal("") ->
    TP = tx_pool:get(),
    Txs = TP#tx_pool.txs,
    T = TP#tx_pool.height,
    PB = block:get_by_height(T),
    Top = block:block_to_header(PB),%it would be way faster if we had a copy of the block's hash ready, and we just looked up the header by hash.
    Block = block:make(Top, Txs, PB#block.trees, keys:pubkey()),
    %db:save(?potential_block, Block),
    Block;
new_internal(Old) ->
    PH = Old#block.prev_hash,
    PB = block:get_by_hash(PH),
    tree_data:garbage(Old, PB),
    new_internal("").
    
    
