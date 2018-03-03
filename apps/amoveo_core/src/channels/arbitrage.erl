-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	write/2, check/1, remove/2, remove_row/1]).
-define(LOC, constants:arbitrage()).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> dict:new();
	     true -> X
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, K),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, SecretHash, L}, X) -> 
    Y = case dict:find(SecretHash, X) of
	    error -> dict:store(SecretHash, L, X);
	    {ok, Val} ->
		Z = L++Val,%plus(Val, L),
		dict:store(SecretHash, Z, X)
	end,
    db:save(?LOC, Y),
    {noreply, Y};
handle_cast({remove, SecretHash, L}, X) -> 
    Y = case dict:find(SecretHash, X) of
	    error -> X;
	    {ok, Val} ->
		Z = minus(Val, L),
		dict:store(SecretHash, Z, X)
	end,
    db:save(?LOC, Y),
    {noreply, Y};
handle_cast({remove_row, SecretHash}, X) -> 
    {noreply, dict:erase(SecretHash, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, SH}, _From, X) -> 
    {reply, dict:find(SH, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

minus([], _) -> [];
minus([A|B], C) ->
    D = is_in(A, C),
    if
	D -> minus(B, C);
	true -> [A|minus(B, C)]
    end.
is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T).
check(Code) ->
    SH = hash:doit(Code),
    gen_server:call(?MODULE, {check, SH}).
write(Code, L) ->
    SH = hash:doit(Code),
    gen_server:cast(?MODULE, {write, SH, L}).
remove(Code, L) ->
    SH = hash:doit(Code),
    gen_server:cast(?MODULE, {remove, SH, L}).
remove_row(Code) ->
    SH = hash:doit(Code),
    gen_server:cast(?MODULE, {remove_row, SH}).
