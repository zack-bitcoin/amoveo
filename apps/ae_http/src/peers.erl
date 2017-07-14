
-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 update/4,all/0, add/0, add/2,add/1,read/2,remove/2,
	update_score/3, initial_score/0,
	cid/1]).

-define(DEFAULT_IP, {46,101,103,165}).
-define(DEFAULT_PORT, 8080).

-record(r, {height =0, hash=0, cid, score=100000}).%lower score is better.
cid(X) -> X#r.cid.
init(ok) ->
    erlang:send_after(1000, self(), set_initial_peers),
    {ok, default_peers()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.

handle_info(set_initial_peers, State) ->
    {ok, Peers} = application:get_env(ae_core, peers),
    add(Peers),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast({remove, IP, Port}, X) ->
    K = key(IP, Port),
    NewX = dict:erase(K, X),
    {noreply, NewX};
handle_cast({add, IP, Port}, X) -> 
    NewX = load_peers([{IP, Port}], X),
    {noreply, NewX};
handle_cast({score, IP, Port, N}, X) ->
    K = key(IP, Port),
    {ok, Old} = dict:find(K, X),
    %A = Old#r.score,
    %B = (A*99+N) div 100,
    NewR = Old#r{score = N},
    NewX = dict:store(K, NewR, X),
    {noreply, NewX};
%handle_cast({set_cid, IP, Port, CID}, X) ->
%    K = key(IP, Port),
%    {ok, Old} = dict:find(K, X),
%    New = Old#r{cid = CID},
%    NewX = dict:store(K, New, X),
%    {noreply, NewX};
handle_cast({update, IP, Port, Height, Hash}, X) ->
    K = key(IP, Port),
    {ok, Old} = dict:find(K, X),
    NewX = if
	       Height > Old#r.height ->
		   A = Old#r{height = Height, hash = Hash},
		   dict:store(K, A, X);
	       true ->
		   X
	   end,
    {noreply, NewX}.
handle_call(all, _From, X) ->
    {reply, dict:fetch_keys(X), X};
handle_call({read, IP, Port}, _From, X) -> 
    K = key(IP, Port),
    O = case dict:find(K, X) of
        error -> <<"none">>;
        {ok, Val} -> Val
    end,
    {reply, O, X}.
%set_cid(IP, Port, CID) ->
%    gen_server:cast(?MODULE, {set_cid, IP, Port, CID}).
key(IP, Port) -> {IP, Port}.
all() -> gen_server:call(?MODULE, all).

add() ->
    Ip = application:get_env(ae_core, peers_ip, ?DEFAULT_IP),
    Port = application:get_env(ae_core, peers_port, ?DEFAULT_PORT),
    peers:add(Ip, Port).

add([]) -> ok;
add([[IP, Port]|T]) when ((size(IP) == 4) or (size(IP) == 16)) ->
    add(IP, Port),
    add(T);
add([{IP, Port}|T]) -> 
    add([[IP, Port]|T]);
add([_|T]) -> 
    add(T).
add(IP, Port) -> 
    NIP = if
	      is_tuple(IP) -> IP;
	      is_list(IP) -> list_to_tuple(IP)
	  end,
    gen_server:cast(?MODULE, {add, NIP, Port}).
update_score(IP, Port, N) ->
    gen_server:cast(?MODULE, {score, IP, Port, N}).

update(IP, Port, Height, Hash) ->
    gen_server:cast(?MODULE, {update, IP, Port, Height, Hash}).
remove(IP, Port) -> gen_server:cast(?MODULE, {remove, IP, Port}).
read(IP, Port) -> gen_server:call(?MODULE, {read, IP, Port}).

default_peers() -> 
    D = constants:peers(),
    load_peers(D, dict:new()).
load_peers([], D) -> D;
load_peers([{IP, Port}|T], Dict) ->
    K = key(IP, Port),
    %don't re-write the same peer twice.
    D2 = case dict:find(K, Dict) of
	     error ->
		 dict:store(K, #r{}, Dict);
	     _ -> Dict
	 end,
    load_peers(T, D2).

initial_score() -> 100000.
