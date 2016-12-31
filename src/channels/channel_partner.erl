%this module needs to keep track of the highest-nonced transaction that the peer could possibly be aware of.

%We need the ability to spend and receive money, and to spend and receive hashlocked money.

-module(channel_partner).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,delete/1,keys/0,read_channel/1,store/2,new_channel/3]).
-define(LOC, constants:channel_partner()).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> 
		 K = dict:new(),
		 db:save(?LOC,K),
		 K;
	     true -> X
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC,K),
    io:format("channel_manager died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, N, Ch}, X) -> 
    %It would be nice to add a rule that we can only increase the nonce.
    {noreply, dict:store(N, Ch, X)};
handle_cast({delete, N}, X) -> 
    {noreply, dict:erase(N, X)}.
handle_call({read, N}, _From, X) -> 
    Out = case dict:find(N, X) of
	      error -> <<"does not exist">>;
	      Z -> Z
	  end,
    {reply, Out, X};
handle_call(keys, _From, X) -> 
    {reply, dict:fetch_keys(X), X}.
keys() -> gen_server:call(?MODULE, keys).
read_channel(Key) ->
    F = read(Key),
    testnet_sign:data(channel_manager_feeder:channel(F)).
store(ChId, Ch) -> 
    gen_server:cast(?MODULE, {store, ChId, Ch}).
is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T).
read(ChId) -> 
    K = keys(),
    case is_in(ChId, K) of
	true ->
	    {ok, Out} = gen_server:call(?MODULE, {read, ChId}),
	    Out;
	false -> 
	    empty
    end.
delete(ChId) -> gen_server:call(?MODULE, {delete, ChId}).
new_channel(ChId, Channel, Accounts) ->
    Ch = keys:sign(channel_block_tx:channel_block_from_channel(ChId, Channel, 0, 1, constants:max_reveal()-1, 0, []), Accounts),
    store(ChId, Ch).

    
