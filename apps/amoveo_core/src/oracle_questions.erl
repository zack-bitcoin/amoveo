-module(oracle_questions).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        store/2, get/1, dump/0, all/0]).
-define(LOC, constants:oracle_questions_file()).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
	    X == "" -> 
                dict:new();
	    true -> X
	end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("oracle_questions died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(dump, _) -> 
    {noreply, dict:new()};
handle_cast(_, X) -> {noreply, X}.
handle_call(all, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call({get, Key}, _From, X) -> 
    {reply, dict:find(Key, X), X};
handle_call({store, Key, Value}, _From, X) -> 
    NewX = dict:store(Key, Value, X),
    {reply, ok, NewX};
handle_call(_, _From, X) -> {reply, X, X}.

all() -> gen_server:call(?MODULE, all).
store(Key, Value) ->
    gen_server:call(?MODULE, {store, Key, Value}).
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).
dump() ->
    gen_server:call(?MODULE, dump).
