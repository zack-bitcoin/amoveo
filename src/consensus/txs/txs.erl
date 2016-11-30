%each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.

-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,txs/0,digest/7,test/0]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("txs died!"), ok.
handle_info(_, X) -> {noreply, X}.

handle_call(txs, _From, X) -> {reply, X, X}.
handle_cast(dump, _) -> {noreply, []};
handle_cast({add_tx, Tx}, X) -> {noreply, [Tx|X]}.
dump() -> gen_server:cast(?MODULE, dump).
txs() -> gen_server:call(?MODULE, txs).
digest([], _, Channels, Accounts, TotalCoins, SecretHashes, _) -> {Channels, Accounts, TotalCoins, SecretHashes};
digest([SignedTx|Txs], ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight) ->
    true = testnet_sign:verify(SignedTx, Accounts),
    Tx = testnet_sign:data(SignedTx),
    {NewChannels, NewAccounts, NewTotalCoins, NewSecretHashes} = 
	case element(1, Tx) of
	    channel_funds_limit -> channel_funds_limit_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
	    repo -> repo_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            ca -> create_account_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            spend -> spend_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            da -> delete_account_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            tc -> to_channel_tx:doit(SignedTx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            signed_cb -> channel_block_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            timeout -> channel_timeout_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            channel_slash -> channel_slash_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            channel_close -> channel_close_tx:doit(Tx, ParentKey, Channels, Accounts, TotalCoins, SecretHashes, NewHeight);
            X -> 
		io:fwrite(packer:pack(Tx)),
		X=2
        end,
    digest(Txs, ParentKey, NewChannels, NewAccounts, NewTotalCoins, NewSecretHashes, NewHeight).

test() -> 0.
