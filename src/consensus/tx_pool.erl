-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
absorb/4,absorb_tx/3,dump/0,data/0,test/0]).
-record(f, {txs = [], accounts = 1, channels = 0, height = 0}).%block:genesis only stores a single account, so the first time account was updated should be a 1.
init(ok) -> 
    io:fwrite("tx pool started\n"),
    %process_flag(trap_exit, true),
    F = state_now(),
    {ok, F}.
state_now() ->
    B = block:read(top:doit()),
    #f{accounts = block:accounts(B), channels = block:channels(B), height = block:height(B)}.
    
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("tx pool died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(dump, _From, _) -> {reply, 0, #f{}};
handle_call({absorb_tx, NewChannels, NewAccounts, Tx}, _From, F) ->
    {reply, 0, F#f{txs = [Tx|F#f.txs], channels = NewChannels, accounts = NewAccounts}}; 
handle_call({absorb, NewChannels, NewAccounts, Txs, Height}, _From, _) ->
    {reply, 0, #f{txs = Txs, accounts = NewAccounts, channels = NewChannels, height = Height}};
handle_call(data, _From, F) -> {reply, {F#f.accounts, F#f.channels, F#f.height, flip(F#f.txs)}, F}.
data() -> gen_server:call(?MODULE, data). %{accounts, channels, height, txs}
dump() -> gen_server:call(?MODULE, dump).
absorb_tx(Channels, Accounts, Tx) -> gen_server:call(?MODULE, {absorb_tx, Channels, Accounts, Tx}).
absorb(Channels, Accounts, Txs, Height) ->
    gen_server:call(?MODULE, {absorb, Channels, Accounts, Txs, Height}).
flip(X) -> flip(X, []).
flip([], A) -> A;
flip([H|T], A) -> flip(T, [H|A]).
    
test() ->
    success.
