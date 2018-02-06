-module(sync_mode).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	quick/0, normal/0, check/0]).
init(ok) -> 
    X = case application:get_env(ae_core, kind) of
	    {ok, "production"} -> quick;
	    _ -> normal
	end,
    {ok, X}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(quick, _From, _) -> 
    {reply, quick, quick};
handle_call(normal, _From, _) -> 
    {reply, normal, normal};
handle_call(_, _From, X) -> {reply, X, X}.

quick() ->
    gen_server:call(?MODULE, quick).
normal() ->
    gen_server:call(?MODULE, normal).
check() ->
    gen_server:call(?MODULE, check).
