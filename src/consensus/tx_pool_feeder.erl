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
    TotalCoins = tx_pool:total_coins(),
    Accounts = tx_pool:accounts(),
    Channels = tx_pool:channels(),
    Secrets = tx_pool:secrets(),
    true = testnet_sign:verify(SignedTx, Accounts),
    Txs = tx_pool:txs(),
    Tx = testnet_sign:data(SignedTx),
    %R = testnet_sign:revealed(SignedTx),
    B = to_channel_tx:is_tc(Tx),
    ID = if 
	     B -> to_channel_tx:id(Tx);
	     true -> 0
	 end,
    NewTx = if 
		B and (ID == -1) ->
	    %select the location for the new channel in the database at the very last possible moment. 
		    Revealed = to_channel_tx:next_top(block_tree:read(top), Channels),
		    testnet_sign:set_revealed(SignedTx, Revealed);
		true -> SignedTx
    end,
    H = block_tree:height(),
    if
	element(1, Tx) == sign_tx ->
	    false = sign_tx:repeat(element(2, Tx), Txs);
	true -> 0 = 0
    end,
    {NewChannels, NewAccounts, NewTotalCoins, NewSecrets} = txs:digest([NewTx], block_tree:read(top), Channels, Accounts, TotalCoins, Secrets, H+1),%Usually blocks are one after the other. Some txs may have to get removed if height increases by more than 1 between adjacent blocks.
    tx_pool:absorb(NewChannels, NewAccounts, NewTotalCoins, NewSecrets, [NewTx|flip(Txs)]),
    {reply, 0, X};
handle_call(_, _From, X) -> {reply, X, X}.
flip(X) -> flip(X, []).
flip([], X) -> X;
flip([H|T], L) -> flip(T, [H|L]).
    
absorb(SignedTx) -> 
    gen_server:call(?MODULE, {absorb, SignedTx}).

