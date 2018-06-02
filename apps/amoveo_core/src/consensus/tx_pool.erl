-module(tx_pool).
-behaviour(gen_server).
%% This module holds the txs ready for the next block, and it remembers the current consensus state, so it is ready to add a new tx at any time.
-export([data_new/0, get/0, dump/0, dump/1, absorb_tx/2, absorb/2]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("../records.hrl").
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
init(ok) ->
    block:initialize_chain(),
    State = current_state(),
    spawn(fun() ->
		  timer:sleep(2000),
		  sync:cron() end),
    io:fwrite("tx_pool: blockchain ready\n"),
    SyncBlockchain = application:get_env(amoveo_core, sync_blocks_boot, false),
    if
        SyncBlockchain ->
            io:fwrite("tx_pool: setting sync_mode normal\n"),
            sync_mode:normal();
        true -> ok
    end,
    {ok, State}.
handle_call({dump, TopBlock}, _From, _OldState) ->
    State = state2(TopBlock),
    {reply, 0, State};
handle_call(dump, _From, _OldState) ->
    State = current_state(),
    {reply, 0, State};
handle_call({absorb_tx, NewDict, Tx}, _From, F) ->
    NewTxs = [Tx | F#tx_pool.txs],
    NewChecksums = [checksum(Tx) | F#tx_pool.checksums],
    %io:fwrite(packer:pack([22, now()])),
    %io:fwrite("\n"),
    BlockSize = F#tx_pool.bytes + size(packer:pack(Tx)) + 1,
    %io:fwrite(packer:pack([23, now()])),
    %io:fwrite("\n"),
    %Governance = trees:governance(NewTrees),
    %MaxBlockSize = trees:dict_tree_get(governance, max_block_size),
    MaxBlockSize = trees:dict_tree_get(governance, max_block_size, F#tx_pool.dict, F#tx_pool.block_trees),
    %MaxBlockSize = governance:get_value(max_block_size, Governance),
    F2 = case BlockSize > (MaxBlockSize - 150) of
             true ->
                 io:fwrite("Cannot absorb tx - block is already full"),
                 F;
             false ->
                 F#tx_pool{txs = NewTxs, 
			   checksums = NewChecksums,
                           %trees = NewTrees, 
                           dict = NewDict,
			   bytes = BlockSize}
         end,
    {reply, 0, F2};
handle_call({absorb, NewTrees, Height}, _From, _) ->
    {reply, 0, #tx_pool{txs = [], checksums = [], block_trees = NewTrees, height = Height}};
handle_call(data_new, _From, F) -> 
    F2 = F#tx_pool{height = block:height()},
    {reply, F2, F}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:fwrite("tx_pool crashed \n this should never happen.\n"),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

data_new() -> tx_pool:get().
get() -> gen_server:call(?MODULE, data_new).
dump() -> 
    gen_server:call(?MODULE, dump).
dump(NewTop) -> 
    gen_server:call(?MODULE, {dump, NewTop}).
absorb_tx(NewDict, Tx) ->
    gen_server:call(?MODULE, {absorb_tx, NewDict, Tx}).
%absorb_tx(Trees, NewDict, Tx) ->
%    gen_server:call(?MODULE, {absorb_tx, Trees, NewDict, Tx}).
absorb(Trees, Height) ->
    gen_server:call(?MODULE, {absorb, Trees, Height}).
current_state() ->
    Block = block:top(),
    state2(Block).
state2(Block) ->
    %Header = block:block_to_header(Block),
    %case Block of
	%empty -> 
	%    {ok, PrevHeader} = headers:read(Header#header.prev_hash),
	%    state2(PrevHeader);
	%_ ->
            Trees = Block#block.trees,
	    #tx_pool{block_trees = Trees, 
                     height = Block#block.height}.
    %end.
checksum(Tx) ->
    <<X:32, _/binary>> = hash:doit(Tx),
    <<X:32>>.
    
