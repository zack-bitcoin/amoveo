
-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
         handle_cast/2,handle_info/2,init/1,terminate/2]).

%% API
-export([
         add/1, %Add a Peer 
         remove/1, %Remove a Peer
         all/0, %Get list of all Peers
         read/1, %Get properties of Peer
	 update/2 %Update properties of Peer
]).

-record(r, {}). %that's empty for now, in future we will implement ranking mechanics and store bunch of properties here
init(ok) ->
    erlang:send_after(1000, self(), set_initial_peers),
    {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.

handle_info(set_initial_peers, State) ->
    {ok, Peers} = application:get_env(ae_core, peers),
    add(Peers),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast({remove, {_,_}=K}, X) ->
    NewX = dict:erase(K, X),
    {noreply, NewX};
handle_cast({add, {_,_}=K}, X) -> 
    NewX = load_peers([K], X),
    {noreply, NewX};
handle_cast({update, {_,_}=K, Properties}, X) ->
    NewX = dict:store(K, Properties, X),
    {noreply, NewX}.

handle_call(all, _From, X) ->
    {reply, dict:fetch_keys(X), X};
handle_call({read, {_,_}=K}, _From, X) ->
    O = case dict:find(K, X) of
        error -> <<"none">>;
        {ok, Val} -> Val
    end,
    {reply, O, X}.

all() -> gen_server:call(?MODULE, all).

add([]) -> ok;
add([[IP, Port]|T]) when ((size(IP) == 4) or (size(IP) == 16)) ->
    add({IP, Port}),
    add(T);
add([{IP, Port}|T]) -> 
    add([[IP, Port]|T]);
add([MalformedPeer|T]) ->
    lager:warning("Tried to add malformed peer:~p. Skipping...", [MalformedPeer]),
    add(T);
add({IP, Port}) -> 
    NIP = if
	      is_tuple(IP) -> IP;
	      is_list(IP) -> list_to_tuple(IP)
	  end,
    gen_server:cast(?MODULE, {add, {NIP, Port}}).

update(Peer, Properties) ->
    gen_server:cast(?MODULE, {update, Peer, Properties}).

remove(Peer) -> gen_server:cast(?MODULE, {remove, Peer}).

read(Peer) -> gen_server:call(?MODULE, {read, Peer}).

load_peers([], D) -> D;
load_peers([{_,_}=K|T], Dict) ->
    D2 = case dict:find(K, Dict) of
	     error ->
		 dict:store(K, #r{}, Dict);
	     _ -> Dict
	 end,
    load_peers(T, D2).
