-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({absorb, SignedTx}, _From, X) ->
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    true = Fee > free_constants:minimum_tx_fee(),
    %make sure the fee is big enough.
    {Accounts, Channels, Height, _Txs} = tx_pool:data(),
    true = testnet_sign:verify(SignedTx, Accounts),
    {NewChannels, NewAccounts} = 
	txs:digest([SignedTx], Channels, Accounts, Height+1),
    tx_pool:absorb_tx(NewChannels, NewAccounts, SignedTx),
    {reply, 0, X}.
%handle_call(_, _From, X) -> {reply, X, X}.
    
absorb(SignedTx) -> 
    gen_server:call(?MODULE, {absorb, SignedTx}).

