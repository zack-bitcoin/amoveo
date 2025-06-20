
-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
         handle_cast/2,handle_info/2,init/1,terminate/2,
         peers_check/0,
         remove_all/0]).

%% API
-export([
	 my_ip/0,%my_ip(all())
	 %my_ip/1,%tells your ip address
         add/1, %Add a Peer 
         remove/1, %Remove a Peer
         all/0, %Get list of all Peers
         read/1, %Get properties of Peer
         update/2 %Update properties of Peer
]).

-record(r, {}). %that's empty for now, in future we will implement ranking mechanics and store bunch of properties here
-record(state, {peers}). %state record

init(ok) ->
    %erlang:send_after(1000, self(), set_initial_peers),
    spawn(fun() ->
		  timer:sleep(1000),
		  %{ok, Peers} = application:get_env(amoveo_core, peers),
                  Peers = case application:get_env(amoveo_core, peers) of
                              {ok, P} -> P;
                              undefined -> []
                          end,
		  {ok, Port} = application:get_env(amoveo_core, port),
		  add(Peers),
		  IP = my_ip(),
		  if
		      IP == empty -> ok;
		      true -> add({IP, Port})
		  end
		  
	  end),
    {ok, #state{peers = dict:new()}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, S, _Extra) -> {ok, S}.
terminate(_, _) -> io:format("died!"), ok.
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
add([error]) -> ok;
add(error) -> ok;
add([[IP, Port]|T]) when (is_list(IP)) ->
    add([[list_to_tuple(IP), Port]|T]);
add([[IP, Port]|T]) ->
    %add({IP, Port}),
    add([{IP, Port}|T]);
add([{IP, Port}|T]) when ((size(IP) == 4) or (size(IP) == 16)) ->
    spawn(fun() ->
		  add({IP, Port})
	  end),
    %timer:sleep(100),
    add(T);
add([MalformedPeer|T]) ->
    io:fwrite("tried to add malformed peer, skipping."),
    io:fwrite(packer:pack(MalformedPeer)),
    add(T);
%add({{10, _, _, _}, _Port}) -> ok;%these formats are only for private networks, not the public internet.
add({{0, 0, 0, 0}, _Port}) -> ok;
%add({{192, 168, _, _}, _Port}) -> ok;
%add({{172, X, _, _}, Port}) when ((X < 32) and (X > 15))-> ok;
add({IP, Port}) -> 
    NIP = if
              is_tuple(IP) -> IP;
              is_list(IP) -> list_to_tuple(IP)
          end,
    B = blacklist_peer:check({NIP, Port}),
    if
	B -> 
            %io:fwrite("blacklisted"),
            ok;
	true ->
	    case talker:talk({height}, {NIP, Port}) of
		bad_peer -> 
                    %io:fwrite("bad peer\n"),
                    blacklist_peer:add({NIP, Port});
		error -> 
                    io:fwrite("error peer\n"),
                    blacklist_peer:add({NIP, Port});
		{ok, H} -> 
                    V = version:doit(H),
                    case talker:talk({test, -1}, {NIP, Port}) of %eventually change to {version}, once most of the fullnodes update their apis.
                        {ok, V} ->
                            gen_server:cast(?MODULE, {add, {NIP, Port}});
                        _ ->
                            %io:fwrite("peer on different blockchain\n"),
                            blacklist_peer:add({NIP, Port})
                    end;
                _ ->
                    %io:fwrite("unknown peer error\n"),
                    %blacklist_peer:add({NIP, Port})
                    ok
	    end
    end.

update(Peer, Properties) ->
    gen_server:cast(?MODULE, {update, Peer, Properties}).

is_in(A, [A|_]) -> true;
is_in(A, [_|T]) -> is_in(A, T);
is_in(A, []) -> false.
peers_check() ->
    case application:get_env(amoveo_core, peers) of
        {ok, P} -> P;
        undefined -> []
    end.
remove(Peer) -> 
    %io:fwrite("removing peer "),
    %io:fwrite(packer:pack(Peer)),
    %io:fwrite("\n"),
    Peers = peers_check(),
    IsIn = is_in(Peer, Peers),
    if
        IsIn -> ok;
        true ->
            blacklist_peer:add(Peer),
            gen_server:cast(?MODULE, {remove, Peer})
    end.

read(Peer) -> gen_server:call(?MODULE, {read, Peer}).

load_peers([], Dict) -> Dict;
load_peers([{_,_}=Peer|T], Dict) ->
    NewDict = case dict:find(Peer, Dict) of
             error ->
                 dict:store(Peer, #r{}, Dict);
             _ -> Dict
         end,
    load_peers(T, NewDict).

my_ip() ->
    {ok, Addrs} = inet:getifaddrs(),
    X = [
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
                size(Addr) == 4, Addr =/= {127,0,0,1}
        ],
    case X of
        [] -> empty;
        _ -> hd(X)
    end.
            

remove_all() ->
    lists:map(fun(P) -> remove(P) end, all()).

    
