-module(potential_block).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([new/0, read/0, save/0]).
-define(recent_block, "data/recent_blocks.db").
-include("../../records.hrl").
init(ok) -> 
    {ok, db:read(?recent_block)}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(save, _) -> 
    Block = new_internal(""),
    {noreply, Block};
handle_cast(new, Old) -> 
    Block = new_internal(Old),
    {noreply, Block};
handle_cast(_, X) -> {noreply, X}.
handle_call(read, _From, X) -> 
    Y = case X of
	    "" -> new_internal("");
	    _ -> X
	end,
    {reply, Y, Y};
handle_call(_, _From, X) -> {reply, X, X}.
new() -> gen_server:cast(?MODULE, new).
save() -> gen_server:cast(?MODULE, save).
read() -> gen_server:call(?MODULE, read).
new_internal(Old) ->
    
    TP = tx_pool:get(),
    Txs = TP#tx_pool.txs,
    T = TP#tx_pool.height,
    PB = block:get_by_height(T),
    Hash = block:hash(PB),
    {ok, Top} = headers:read(Hash),
    %Top = block:block_to_header(PB),%it would be way faster if we had a copy of the block's hash ready, and we just looked up the header by hash.
    case Old of
	"" -> ok;
	_ -> trees:prune(Old, block:top())
    end,
    Block = block:make(Top, Txs, PB#block.trees, keys:pubkey()),
    db:save(?recent_block, Block),
    Block.
    
    
