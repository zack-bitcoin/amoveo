
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
-record(state, {peers}). %state record

init(ok) ->
    erlang:send_after(1000, self(), set_initial_peers),
    {ok, #state{peers = dict:new()}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, S, _Extra) -> {ok, S}.
terminate(_, _) -> io:format("died!"), ok.

handle_info(set_initial_peers, State) ->
    {ok, Peers} = application:get_env(ae_core, peers),
    add(Peers),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast({remove, {_,_}=Peer}, State) ->
    NewPeers = dict:erase(Peer, State#state.peers),
    {noreply, State#state{peers = NewPeers}};
handle_cast({add, {_,_}=Peer}, State) -> 
    NewPeers = load_peers([Peer], State#state.peers),
    {noreply, State#state{peers = NewPeers}};
handle_cast({update, {_,_}=Peer, NewProperties}, State) ->
    NewPeers = dict:store(Peer, NewProperties, State#state.peers),
    {noreply, State#state{peers = NewPeers}}.

handle_call(all, _From, State) ->
    {reply, dict:fetch_keys(State#state.peers), State};
handle_call({read, {_,_}=Peer}, _From, State) ->
    Properties = case dict:find(Peer, State) of
        error -> <<"none">>;
        {ok, Val} -> Val
    end,
    {reply, Properties, State}.

all() -> gen_server:call(?MODULE, all).

add([]) -> ok;
add([[IP, Port]|T]) when (is_list(IP)) ->
    add([[list_to_tuple(IP), Port]|T]);
add([[IP, Port]|T]) when ((size(IP) == 4) or (size(IP) == 16)) ->
    add({IP, Port}),
    add(T);
add([{IP, Port}|T]) -> 
    add([[IP, Port]|T]);
add([MalformedPeer|T]) ->
    io:fwrite("tried to add malformed peer, skipping."),
    io:fwrite(packer:pack(MalformedPeer)),
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

load_peers([], Dict) -> Dict;
load_peers([{_,_}=Peer|T], Dict) ->
    NewDict = case dict:find(Peer, Dict) of
             error ->
                 dict:store(Peer, #r{}, Dict);
             _ -> Dict
         end,
    load_peers(T, NewDict).
