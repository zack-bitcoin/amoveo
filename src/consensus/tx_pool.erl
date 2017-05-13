-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
absorb/3,absorb_tx/2,dump/0,data/0,test/0]).
-record(f, {txs = [], trees, height = 0}).%block:genesis only stores a single account, so the first time account was updated should be a 1.
init(ok) -> 
    io:fwrite("tx pool started\n"),
    %process_flag(trap_exit, true),
    F = state_now(),
    {ok, F}.
state_now() ->
    B = block:read(top:doit()),
    #f{trees = block:trees(B), height = block:height(B)}.
    
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("tx pool died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(dump, _From, _) -> {reply, 0, state_now()};
handle_call({absorb_tx, NewTrees, Tx}, _From, F) ->
    NewTxs = [Tx|F#f.txs],
    B = size(term_to_binary(NewTxs)),
    Governance = trees:governance(NewTrees),
    MBS = governance:get_value(max_block_size, Governance),
    %MBS = constants:max_block_size(),
    FinalTxs = if
	B > MBS ->
		       io:fwrite("block is already full\n"),
		       F#f.txs;
	true -> NewTxs
    end,
    {reply, 0, F#f{txs = FinalTxs, trees = NewTrees}};
handle_call({absorb, NewTrees, Txs, Height}, _From, _) ->
    {reply, 0, #f{txs = Txs, trees = NewTrees, height = Height}};
handle_call(data, _From, F) -> {reply, {F#f.trees, F#f.height, flip(F#f.txs)}, F}.
data() -> gen_server:call(?MODULE, data). %{accounts, channels, height, txs}
dump() -> gen_server:call(?MODULE, dump).
absorb_tx(Trees, Tx) ->
    gen_server:call(?MODULE, {absorb_tx, Trees, Tx}).
absorb(Trees, Txs, Height) ->
    gen_server:call(?MODULE, {absorb, Trees, Txs, Height}).
flip(X) -> flip(X, []).
flip([], A) -> A;
flip([H|T], A) -> flip(T, [H|A]).
    
test() ->
    success.
