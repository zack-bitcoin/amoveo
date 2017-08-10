-module(txs).
-behaviour(gen_server).

%% API
-export([dump/0,
	     txs/0,
	     digest/3,
	     fees/1]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API functions

dump() ->
	gen_server:cast(?MODULE, dump).

txs() ->
	gen_server:call(?MODULE, txs).

digest([], Trees, _) ->
	Trees;
digest([SignedTx | Txs], Trees, Height) ->
    true = testnet_sign:verify(SignedTx),
    Tx = testnet_sign:data(SignedTx),
    NewTrees = digest2(Tx, Trees, Height),
    digest(Txs, NewTrees, Height).
digest2(Tx, Trees, H) ->
    case element(1, Tx) of
        create_acc_tx -> create_account_tx:doit(Tx, Trees, H);
        spend -> spend_tx:doit(Tx, Trees, H);
        delete_acc_tx -> delete_account_tx:doit(Tx, Trees, H);
        %repo -> repo_tx:doit(Tx, Trees, H);
        nc -> new_channel_tx:doit(Tx, Trees, H);
        gc -> grow_channel_tx:doit(Tx, Trees, H);
        ctc -> channel_team_close_tx:doit(Tx, Trees, H);
        %cr -> channel_repo_tx:doit(Tx, Trees, H);
        csc -> channel_solo_close:doit(Tx, Trees, H);
        timeout -> channel_timeout_tx:doit(Tx, Trees, H);
        cs -> channel_slash_tx:doit(Tx, Trees, H);
        ex -> existence_tx:doit(Tx, Trees, H);
        oracle_new -> oracle_new_tx:doit(Tx, Trees, H);
        oracle_bet -> oracle_bet_tx:doit(Tx, Trees, H);
        oracle_close -> oracle_close_tx:doit(Tx, Trees, H);
        unmatched -> oracle_unmatched_tx:doit(Tx, Trees,H);
        oracle_shares -> oracle_shares_tx:doit(Tx,Trees,H);
	coinbase -> coinbase_tx:doit(Tx, Trees, H);
        X -> X = 2
    end.

fees([]) -> 0;
fees([H | T]) ->
    element(4, element(2, H)) + fees(T).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
	{ok, []}.

handle_call(txs, _From, Txs) ->
	{reply, Txs, Txs}.

handle_cast(dump, _) ->
	{noreply, []};
handle_cast({add_tx, Tx}, Txs) ->
	{noreply, [Tx | Txs]}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
