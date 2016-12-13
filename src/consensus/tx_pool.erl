-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
absorb/4,absorb_tx/3,dump/0,data/0,test/0]).
-record(f, {txs = [], accounts = 1, channels = 0, height = 0}).%block:genesis only stores a single account, so the first time account was updated should be a 1.
init(ok) -> 
    io:fwrite("tx pool started\n"),
    process_flag(trap_exit, true),
    {ok, #f{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(data, _From, F) -> {reply, {F#f.accounts, F#f.channels, F#f.height, F#f.txs}, F}.
handle_cast(dump, _) -> {noreply, #f{}};
handle_cast({absorb_tx, NewChannels, NewAccounts, Tx}, F) ->
    {noreply, F#f{txs = [Tx|F#f.txs], channels = NewChannels, accounts = NewAccounts}}; 
handle_cast({absorb, NewChannels, NewAccounts, Txs, Height}, _) ->
    {noreply, #f{txs = Txs, accounts = NewAccounts, channels = NewChannels, height = Height}}.
data() -> gen_server:call(?MODULE, data). %{accounts, channels, height, txs}
dump() -> gen_server:cast(?MODULE, dump).
absorb_tx(Channels, Accounts, Tx) -> gen_server:cast(?MODULE, {absorb_tx, Channels, Accounts, Tx}).
absorb(Channels, Accounts, Txs, Height) ->
    gen_server:cast(?MODULE, {absorb_tx, Channels, Accounts, Txs, Height}).
    
test() ->
    success.
