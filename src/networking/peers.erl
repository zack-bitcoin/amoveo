-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/3,read/2,remove/2]).
default_peers() -> dict:new().
init(ok) -> {ok, default_peers()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({remove, IP, Port}, X) -> 
    K = key(IP, Port),
    NewX = dict:remove(K, X),
    {noreply, NewX};
handle_cast({add, IP, Port, Data}, X) -> 
    K = key(IP, Port),
    NewX = dict:store(K, Data, X),
    {noreply, NewX}.
handle_call({read, IP, Port}, _From, X) -> 
    K = key(IP, Port),
    O = case dict:find(K, X) of
        error -> <<"none">>;
        {ok, Val} -> Val
    end,
    {reply, O, X}.
key(IP, Port) -> {IP, Port}.
add(IP, Port, Data) -> 
    gen_server:cast(?MODULE, {add, IP, Port, Data}).
remove(IP, Port) -> gen_server:cast(?MODULE, {remove, IP, Port}).
read(IP, Port) -> gen_server:call(?MODULE, {read, IP, Port}).
