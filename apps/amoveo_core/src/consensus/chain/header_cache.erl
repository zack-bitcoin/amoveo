-module(header_cache).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         store/2, read/1]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, N, Headers}, X) -> 
    X2 = dict:store(N, Headers, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, N}, _From, X) -> 
    {reply, dict:find(N, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

read(N) ->
    0 = N rem 5000,
    gen_server:call(?MODULE, {read, N}).
store(N, Headers) ->
    0 = N rem 5000,
    gen_server:cast(?MODULE, {store, N, Headers}).
