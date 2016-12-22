-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 update/4,all/0,add/2,read/2,remove/2]).    
init(ok) -> {ok, default_peers()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, IP, Port}, X) -> 
    K = key(IP, Port),
    NewX = dict:remove(K, X),
    {noreply, NewX};
handle_cast({add, IP, Port}, X) -> 
    NewX = load_peers([{IP, Port}], X),
    {noreply, NewX};
handle_cast({update, IP, Port, Height, Hash}, X) ->
    K = key(IP, Port),
    NewX = dict:store(K, {Height, Hash}, X),
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
key(IP, Port) -> {IP, Port}.
all() -> gen_server:call(?MODULE, all).
add(IP, Port) -> 
    gen_server:cast(?MODULE, {add, IP, Port}).
update(IP, Port, Height, Hash) ->
    %only store data that increases peer.height
    {OldHeight, _} = read(IP, Port),
    true = Height > OldHeight,
    gen_server:cast(?MODULE, {update, IP, Port, Height, Hash}).
remove(IP, Port) -> gen_server:cast(?MODULE, {remove, IP, Port}).
read(IP, Port) -> gen_server:call(?MODULE, {read, IP, Port}).

default_peers() -> 
    D = constants:peers(),
    load_peers(D, dict:new()).
load_peers([], D) -> D;
load_peers([{IP, Port}|T], Dict) ->
    %P = #peer{ip = IP, port = Port },
    K = key(IP, Port),
    D2 = dict:store(K, {0, 0}, Dict),
    load_peers(T, D2).
