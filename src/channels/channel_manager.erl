
-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,delete/1,store/2]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, CID, Data}, X) -> 
    {noreply, dict:store(CID, Data, X)};
handle_cast({delete, CID}, X) -> 
    {noreply, dict:delete(CID, X)};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, CID}, _From, X) -> 
    {reply, dict:read(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

read(CID) -> gen_server:call(?MODULE, {read, CID}).
delete(CID) -> gen_server:cast(?MODULE, {delete, CID}).
store(CID, Data) -> 
    gen_server:cast(?MODULE, {store, CID, Data}).

