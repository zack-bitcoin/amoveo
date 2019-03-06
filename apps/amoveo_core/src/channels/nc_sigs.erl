%with a open offer, one party only knows one of the signatures.
%* new merkel tree to store signature from attemps to close the channel. -- or maybe it is better to use the block-explorer principle. since we can verify the signature to know if it is valid.
%*we need a new service for following amoveo and keeping a copy of the most recent channel state signatures from each channel, and serves this data.

%every time we load a block that has a nc_accept tx, we should store the sig so you can look it up using the cid of the channel.
-module(nc_sigs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        get/1, store/2]).
-define(LOC, constants:nc_sigs()).
init(ok) -> 
    %read from a database file.
    io:fwrite("start nc_sigs\n"),
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> dict:new();
	     true -> X
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, CID, Sig}, X) -> 
    X2 = dict:store(CID, Sig, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({get, CID}, _From, X) -> 
    R = case dict:find(CID, X) of
            error -> "";
            {ok, V} -> V
        end,
    {reply, R, X};
handle_call(_, _From, X) -> {reply, X, X}.

store(CID, Sig) -> gen_server:cast(?MODULE, {store, CID, Sig}).
get(CID) -> gen_server:call(?MODULE, {get, CID}).
    
