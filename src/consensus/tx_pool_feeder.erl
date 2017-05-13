-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({absorb, SignedTx}, X) ->
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    io:fwrite("tx pool feeder tx "),
    io:fwrite(packer:pack(Tx)),
    io:fwrite("\n"),
    true = Fee > free_constants:minimum_tx_fee(),
    {Trees, Height, Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(SignedTx, Accounts),
    B = is_in(SignedTx, Txs), %this is very ugly. once we have a proper CLI we can get rid of this crutch.
    if
	B ->  io:fwrite("already have this tx"),
	    ok;
	true ->
	    NewTrees =
		txs:digest([SignedTx], Trees, Height+1),
	    tx_pool:absorb_tx(NewTrees, SignedTx)
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
    
absorb(SignedTx) -> 
    gen_server:cast(?MODULE, {absorb, SignedTx}).

is_in(A, [A|_]) -> true;
is_in(_, []) -> false;
is_in(A, [_|T]) -> is_in(A, T).
