-module(mining_pool_refresher).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
stop/0,start/0,set_period/1,status/0,cron/0
]).
-record(d, {on = false, period = 1}).

init(ok) -> {ok, #d{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(stop, X) -> 
    X2 = X#d{on = false},
    {noreply, X2};
handle_cast(start, X) -> 
    X2 = X#d{on = true},
    {noreply, X2};
handle_cast({set_period, P}, X) when ((P > 0) and ((is_float(P)) or (is_integer(P))))-> 
    P2 = min(P, 10),
    X2 = X#d{period = P2},
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> 
    {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

start() ->
    gen_server:cast(?MODULE, start).
stop() ->
    gen_server:cast(?MODULE, stop).
set_period(X) ->
    gen_server:cast(?MODULE, {set_period, X}).
status() ->
    gen_server:call(?MODULE, status).


cron() ->
    X = application:get_env(amoveo_core, mining_pool_refresher),
    case X of
        undefined -> ok;
        {ok, 0} -> ok;
        {ok, N} ->
            set_period(N),
            start()
    end,
    spawn(fun() ->
                  cron2()
          end).
cron2() ->
    X = status(),
    timer:sleep(round(1000 * X#d.period)),
    if
        X#d.on ->
            spawn(fun() ->
                          potential_block:read()
                  end);
        true -> ok
    end,
    cron2().
