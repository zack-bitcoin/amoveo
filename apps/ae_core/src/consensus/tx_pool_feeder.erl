-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1, absorb_unsafe/1]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({absorb, SignedTx}, X) ->
    {Trees, Height, Txs} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    %io:fwrite("tx pool feeder tx "),
    %io:fwrite(packer:pack(Tx)),
    %io:fwrite("\n"),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    {ok, MinimumTxFee} = application:get_env(ae_core, minimum_tx_fee),
    true = Fee > (MinimumTxFee + Cost),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(SignedTx, Accounts),
    B = is_in(SignedTx, Txs), %this is very ugly. once we have a proper CLI we can get rid of this crutch.
    if
	B ->  io:fwrite("already have this tx"),
	    ok;
	true ->
	    absorb_unsafe(SignedTx, Trees, Height)
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
absorb_unsafe(SignedTx) -> 
    {Trees, Height, _} = tx_pool:data(),
    absorb_unsafe(SignedTx, Trees, Height).
absorb_unsafe(SignedTx, Trees, Height) -> 
    NewTrees = 
	txs:digest([SignedTx], Trees, Height+1),
    tx_pool:absorb_tx(NewTrees, SignedTx).
    
absorb(Txs) when is_list(Txs) ->
    [absorb(Tx) || Tx <- Txs];

absorb(SignedTx) -> 
    {_, _, Txs} = tx_pool:data(),
    B = is_in(SignedTx, Txs),
    if
        B -> ok;
        true ->
            gen_server:cast(?MODULE, {absorb, SignedTx})
    end.

is_in(_, []) -> false;
is_in(STx, [STx2|T]) ->
    Tx = testnet_sign:data(STx),
    Tx2 = testnet_sign:data(STx2),
    if
	Tx == Tx2 -> true;
	true -> is_in(STx, T)
    end.

