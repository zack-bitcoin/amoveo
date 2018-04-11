-module(confirmed_root).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	read/0]).
-define(Confirmations, 10).
-record(cr, {height = 0, root}).
init(ok) -> {ok, #cr{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(read, _From, X) -> 
    A = internal(X),
    {reply, A#cr.root, A}.
%handle_call(_, _From, X) -> {reply, X, X}.

internal(C) ->
    H = C#cr.height,
    Now = api:height(),
    if
	(Now - H) > ?Confirmations ->
	    NH = max(1, Now - ?Confirmations),
	    Root = block:hash(block:get_by_height(NH)),
	    C#cr{height = NH, root = Root};
	true -> C
    end.
	    

read() -> gen_server:call(?MODULE, read).
