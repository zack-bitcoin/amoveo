%If you bet on the same thing twice, it is important to keep a connection between them. So if you learn a new way of closing one of the bets, you can use the knowledge on every other time you bet the same way.
%Don't remove state from arbitrage until the highest nonced channel state we recieved from our partner doesn't have the bet.
-module(arbitrage).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	write/2, check/1, remove/2, remove_row/1]).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, SecretHash, L}, X) ->
    Y = case dict:find(SecretHash, X) of
	    empty -> dict:store(SecretHash, L, X);
	    {ok, Val} -> 
		Z = plus(Val, L),
		dict:store(SecretHash, Z, X)
	end,
    {noreply, Y};
handle_cast({remove, SecretHash, L}, X) ->
    Y = case dict:find(SecretHash, X) of
	    empty -> X;
	    {ok, Val} -> 
		Z = minus(Val, L),
		dict:store(SecretHash, Z, X)
	end,
    {noreply, Y};
handle_cast({remove_row, SecretHash}, X) ->
    {noreply, dict:erase(SecretHash, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, SH}, _From, X) ->
    {reply, dict:find(SH, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

minus([], B) -> [];
minus([A|B], C) ->
    D = is_in(A, C),
    if
	D -> minus(B, C);
	true -> [A|minus(B, C)]
    end.
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [A|T]) -> is_in(X, T).
plus(B, []) -> B;
plus(B, [H|T]) ->
    C = is_in(H, B),
    if
	C -> plus(B, T);
	true -> [H|plus(B, T)]
    end.
check(SH) ->
    gen_server:call(?MODULE, {check, SH}).
write(SH, L) ->
    gen_server:cast(?MODULE, {write, SH, L}).
remove(SH, L) ->
    gen_server:cast(?MODULE, {remove, SH, L}).
remove_row(SH) ->
    gen_server:cast(?MODULE, {remove_row, SH}).
