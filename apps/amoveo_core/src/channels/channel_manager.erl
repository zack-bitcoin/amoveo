-module(channel_manager).
-include("../records.hrl").
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,
	 keys/0, read/1, delete/1, write/2]).
-define(LOC, constants:channel_manager()).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> dict:new();
	     true -> X
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, K),
    io:format("channel_manager died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({dump}, _) -> {noreply, dict:new()};
handle_cast({write, CID, Data}, X) -> 
    NewX = dict:store(CID, Data, X),
    db:save(?LOC, NewX),
    {noreply, NewX};
handle_cast({delete, CID}, X) -> 
    NewX = dict:erase(CID, X),
    db:save(?LOC, NewX),
    %this db:save is only for unexpected failures. Without it, you could lose channel data on power failure. This line can be removed to make the node update channels faster.
%{write, CID, Data} is the same way
    {noreply, NewX};
handle_cast(_, X) -> {noreply, X}.
handle_call(keys, _From, X) ->
    {reply, dict:fetch_keys(X), X};
handle_call({read, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.
dump() -> gen_server:cast(?MODULE, {dump}).
read(CID) -> gen_server:call(?MODULE, {read, CID}).
keys() -> gen_server:call(?MODULE, keys).
delete(CID) -> gen_server:cast(?MODULE, {delete, CID}).
write(CID, Data) -> 
    A = is_list(Data#cd.ssthem),
    B = is_list(Data#cd.ssme),
    C = length(Data#cd.ssme) == 
        length((Data#cd.me)#spk.bets),
    D = length(Data#cd.ssthem) == 
        length((testnet_sign:data(Data#cd.them))#spk.bets),
    if
        A and B and C and D ->
            gen_server:cast(?MODULE, {write, CID, Data});
        true -> ok
    end.
