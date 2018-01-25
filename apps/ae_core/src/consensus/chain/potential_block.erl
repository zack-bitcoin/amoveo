-module(potential_block).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([new/0, read/0, save/0]).
-define(potential_block, "data/potential_blocks.db").
-include("../../records.hrl").
init(ok) -> 
    {ok, db:read(?potential_block)}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(save, _, _) -> 
    Block = new_internal(""),
    {reply, ok, Block};
handle_call(new, _, Old) -> 
    Block = new_internal(Old),
    {reply, ok, Block};
handle_call(read, _From, X) -> 
    Y = case X of
	    "" -> new_internal("");
	    _ -> X
	end,
    {reply, Y, Y};
handle_call(_, _From, X) -> {reply, X, X}.
new() -> gen_server:call(?MODULE, new).
save() -> gen_server:call(?MODULE, save).
read() -> gen_server:call(?MODULE, read).
new_internal(Old) ->
    TP = tx_pool:get(),
    Txs = TP#tx_pool.txs,
    T = TP#tx_pool.height,
    PB = block:get_by_height(T),
    %Hash = block:hash(PB),
    %{ok, Top} = headers:read(Hash),
    Top = block:block_to_header(PB),%it would be way faster if we had a copy of the block's hash ready, and we just looked up the header by hash.
    io:fwrite("new internal old is "),
    io:fwrite(packer:pack(Old)),
    io:fwrite("\n"),
    case Old of
	"" -> ok;
	_ -> tree_data:prune(Old, block:top())
%trees:prune(Old, block:top())
    end,
    Block = block:make(Top, Txs, PB#block.trees, keys:pubkey()),
    db:save(?potential_block, Block),
    Block.
    
    
