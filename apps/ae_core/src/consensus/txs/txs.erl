-module(txs).
-behaviour(gen_server).

%% API
-export([dump/0,
         txs/0,
         digest_from_dict/3,
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

digest_from_dict([C|T], Dict, H) ->
    case element(1, C) of
        coinbase ->
            NewDict = coinbase_tx:go(C, Dict, H),
            digest_from_dict3(T, NewDict, H);
        signed ->
            digest_from_dict3([C|T], Dict, H)
    end.
digest_from_dict3([], Dict, _) ->
    Dict;
digest_from_dict3([STx|T], Dict, Height) ->
    true = testnet_sign:verify(STx),
    Tx = testnet_sign:data(STx),
    NewDict = digest_from_dict2(Tx, Dict, Height),
    digest_from_dict3(T, NewDict, Height).
digest_from_dict2(Tx, Dict, H) ->
    case element(1, Tx) of
        create_acc_tx -> create_account_tx:go(Tx, Dict, H);
        spend -> spend_tx:go(Tx, Dict, H);
        delete_acc_tx -> delete_account_tx:go(Tx, Dict, H);
        nc -> new_channel_tx:go(Tx, Dict, H);
        gc -> grow_channel_tx:go(Tx, Dict, H);
        ctc -> channel_team_close_tx:go(Tx, Dict, H);
        csc -> channel_solo_close:go(Tx, Dict, H);
        timeout -> channel_timeout_tx:go(Tx, Dict, H);
        cs -> channel_slash_tx:go(Tx, Dict, H);
        ex -> existence_tx:go(Tx, Dict, H);
        oracle_new -> oracle_new_tx:go(Tx, Dict, H);
        oracle_bet -> oracle_bet_tx:go(Tx, Dict, H);
        oracle_close -> oracle_close_tx:go(Tx, Dict, H);
        unmatched -> oracle_unmatched_tx:go(Tx, Dict,H);
        oracle_shares -> oracle_shares_tx:go(Tx,Dict,H);
	coinbase_old -> coinbase_tx:go(Tx, Dict, H);
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
    io:fwrite("txs died\n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
