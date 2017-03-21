-module(top).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/1,doit/0,test/0]).
-define(LOC, constants:top()).
init(ok) -> 
    io:fwrite("start top"),
    X = db:read(?LOC),
    Ka = if
	     X == "" ->
		 G = block:genesis(),
		 block_absorber:save_helper(G),
		 add_internal(G),
		 I = keys:pubkey(),
		 M = constants:master_pub(),
		 if
		     I == M -> keys:update_id(1);
		     true -> ok
		 end,
		 block:hash(G);
	     true ->
		 X
	 end,
    {ok, Ka}.
    %{ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({add, Block}, _From, X) ->
    %check which block is higher and store it's hash as the top.
    %for tiebreakers, prefer the older block.
    OldBlock = block:read(X),
    NH = block:height(Block),
    OH = block:height(OldBlock),
    New = if
	NH > OH -> 
		  Channels = block:channels(Block),
		  Accounts = block:accounts(Block),
		  NH = block:height(Block),
		  tx_pool:absorb(Channels, Accounts, [], NH),
		  add_internal(Block);
	true -> X
    end,
    {reply, 0, New};
handle_call(_, _From, X) -> {reply, X, X}.

add(Block) -> gen_server:call(?MODULE, {add, Block}).
doit() -> gen_server:call(?MODULE, top).
add_internal(Block) ->
    Y = block:hash(Block),
    db:save(?LOC, Y),
    Y.

test() ->
    ok.
