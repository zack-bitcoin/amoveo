-module(blacklist_peer).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 add/1, check/1, remove/1]).
-define(Limit, 300).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, Peer}, X) -> 
    X2 = case dict:find(Peer, X) of
	     error -> X;
	     {ok, _} -> dict:erase(Peer, X)
	 end,
    {noreply, X2};
handle_cast({add, Peer, Now}, X) -> 
    X2 = dict:store(Peer, Now, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, Peer}, _From, X) -> 
    A = case dict:find(Peer, X) of
	    error -> 
		false;
	    {ok, N} -> 
		N2 = now(),
		D = time_diff(N2, N),
		B = D < ?Limit,
		B
	end,
    {reply, A, X};
handle_call(_, _From, X) -> {reply, X, X}.

remove(Peer) ->
    {{_,_,_,_},_} = Peer,
    gen_server:cast(?MODULE, {remove, Peer}).
add(Peer) ->
    {{_,_,_,_},_} = Peer,
    gen_server:cast(?MODULE, {add, Peer, now()}).
check(Peer) ->
    gen_server:call(?MODULE, {check, Peer}).
time_diff({N1, N2, _N3}, {O1, O2, _O3}) ->
    A1 = N1 - O1,
    A2 = N2 - O2,
    (A1 * 1000000) + A2.
    
