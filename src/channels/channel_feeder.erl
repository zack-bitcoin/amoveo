-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 spend/1,close/2,
	 lock_spend/1,bet/1]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

spend(SPK) ->%giving us money in the channel.
    ok.
close(Cid, SS) ->%closing the channel at it's current state
    ok.
lock_spend(SPK) ->%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    ok.
bet(SPK) -> %doing one of the bets that we offer.
    ok.
