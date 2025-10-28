-module(peers_heights).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([doit/0, refresh/0]).
-record(d, {time = {0,0,0}, peers_heights}).
-define(period, 120).
init(ok) -> {ok, #d{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(refresh, X) -> 
    N = erlang:now(),
    {noreply, X#d{time = N, peers_heights = peers_heights()}};
handle_cast(_, X) -> {noreply, X}.
handle_call(process_id, _, S) -> {reply, self(), S};
handle_call(doit, _From, X) -> 
    N = erlang:now(),
    T = timer:now_diff(N, X#d.time) div 1000000,%in seconds
    X2 = if
             (T > ?period) -> X#d{time = N, peers_heights = peers_heights()};
             true -> X
         end,
    {reply, X2#d.peers_heights, X2};
handle_call(process_id, _, S) -> {reply, self(), S};
handle_call(_, _From, X) -> {reply, X, X}.

refresh() -> gen_server:cast(?MODULE, refresh).
doit() ->
    gen_server:call(?MODULE, doit).

peers_heights() ->
    lists:map(fun(P) -> {P, talker:talk({height}, P)} end, peers:all()).
