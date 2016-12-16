-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,txs/0,digest/4,test/0]).
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
digest([], Channels, Accounts, _) -> {Channels, Accounts};
digest([SignedTx|Txs], Channels, Accounts, NewHeight) ->
    true = testnet_sign:verify(SignedTx, Accounts),
    Tx = testnet_sign:data(SignedTx),
    {NewChannels, NewAccounts} = digest2(Tx, Channels, Accounts, NewHeight),
    digest(Txs, NewChannels, NewAccounts, NewHeight).
digest2(A, B, C, D) ->
    case element(1, A) of
	ca -> create_account_tx:doit(A, B, C, D);
	spend -> spend_tx:doit(A, B, C, D);
	da -> delete_account_tx:doit(A, B, C, D);
	repo -> repo_tx:doit(A, B, C, D);
	%tc -> to_channel_tx:doit(A, B, C, D);
	%signed_cb -> channel_block_tx:doit(A, B, C, D);
	%timeout -> channel_timeout_tx:doit(A, B, C, D);
	%channel_slash -> channel_slash_tx:doit(A,B, C, D);
	%channel_close -> channel_close_tx:doit(A,B, C, D);
	%channel_funds_limit -> channel_funds_limit_tx:doit(A, B, C, D);
	%channel_repo -> channel_repo_tx:doit(A,B,C,D);
	X -> X=2
    end.
 
	    
test() ->
    S = success,
    S = test1(),
    S = test2(),
    S.
test1() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 100000000, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    tx_pool_feeder:absorb(Stx2),
    {Accounts3, _, _, _} = tx_pool:data(),

    {Ctx3, _} = delete_account_tx:make(1, 2, Fee, Accounts3),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, 2, Accounts3),
    tx_pool_feeder:absorb(Stx3),
    {_Accounts4, _, _, Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
    
test2() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,_NewPub,_NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 0, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = repo_tx:make(2, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    tx_pool_feeder:absorb(Stx2),
    {_Accounts3, _, _, Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
    
