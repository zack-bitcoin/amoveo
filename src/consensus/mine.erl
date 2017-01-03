-module(mine).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,
	 handle_info/2,init/1,terminate/2,
	 start/0,stop/0,status/0]).
init(ok) -> {ok, stop}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(mine, go) -> 
    spawn(fun() -> 
		  block:mine_blocks(10, 50000),
		  mine() end),
    spawn(fun() -> easy:sync() end),
    {noreply, go};
handle_cast(start, _) -> 
    io:fwrite("start start"),
    {noreply, go};
handle_cast(stop, _) ->
    {noreply, stop};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

start() -> 
    io:fwrite("mine start\n"),
    %easy:sync(),
    gen_server:cast(?MODULE, start),
    mine().
mine() ->
    gen_server:cast(?MODULE, mine).
	   
stop() -> gen_server:cast(?MODULE, stop).
status() ->
    gen_server:call(?MODULE, status).
