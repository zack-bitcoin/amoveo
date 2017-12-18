-module(tx_pool).
-behaviour(gen_server).
%% This module holds the txs ready for the next block, and it remembers the current consensus state, so it is ready to add a new tx at any time.
-export([data/0, data_new/0, dump/0, absorb_tx/3, absorb/2,
         txs/1, trees/1, new_trees/1, dict/1, facts/1, height/1]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record(f, {txs = [],
            trees,%this changes once per tx
            new_trees,%this changes once per block
            dict = dict:new(), facts = [], height = 0}).
txs(F) -> F#f.txs.
trees(F) -> F#f.trees.
new_trees(F) -> F#f.new_trees.
dict(F) -> F#f.dict.
facts(F) -> F#f.facts.
height(F) -> F#f.height.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
init(ok) ->
    State = initial_state(),
    io:fwrite("tx_pool- blockchain ready\n"),
    {ok, State}.
handle_call(dump, _From, _OldState) ->
    State = current_state(),
    {reply, 0, State};
handle_call({absorb_tx, NewTrees, NewDict, Tx}, _From, F) ->
    NewTxs = [Tx | F#f.txs],
    BlockSize = size(term_to_binary(NewTxs)),
    Governance = trees:governance(NewTrees),
    MaxBlockSize = governance:get_value(max_block_size, Governance),
    F2 = case BlockSize > MaxBlockSize of
             true ->
                 io:fwrite("Cannot absorb tx - block is already full"),
                 F;
             false ->
                 F#f{txs = NewTxs, 
                     trees = NewTrees, 
                     dict = NewDict}
         end,
    {reply, 0, F2};
handle_call({absorb, NewTrees, Height}, _From, _) ->
    {reply, 0, #f{txs = [], trees = NewTrees, new_trees = NewTrees, height = Height}};
handle_call(data_new, _From, F) ->
    {reply, F, F};
handle_call(data, _From, F) ->
    {ok, Header} = headers:read(block:hash(headers:top())),
    H = F#f.height,
    {reply, {F#f.trees, H, lists:reverse(F#f.txs)}, F}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:fwrite("tx_pool crashed \n this should never happen.\n"),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

data_new() -> gen_server:call(?MODULE, data_new).
data() -> gen_server:call(?MODULE, data).
dump() -> gen_server:call(?MODULE, dump).
absorb_tx(Trees, NewDict, Tx) ->
    gen_server:call(?MODULE, {absorb_tx, Trees, NewDict, Tx}).
absorb(Trees, Height) ->
    gen_server:call(?MODULE, {absorb, Trees, Height}).

initial_state() ->
    _Header = block:initialize_chain(),
    Block = block:top(),
    state2(Block).
current_state() ->
    Block = block:top(),
    state2(Block).
state2(Block) ->
    Header = block:block_to_header(Block),
    case Block of
	empty -> 
	    {ok, PrevHeader} = headers:read(headers:prev_hash(Header)),
	    state2(PrevHeader);
	_ ->
            Trees = block:trees(Block),
	    #f{trees = Trees,
               new_trees = Trees, 
	       height = block:height(Block)}
    end.
