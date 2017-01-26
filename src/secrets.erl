-module(secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	new/0,get/1]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({get, Commit}, _From, X) ->
    Secret = dict:fetch(Commit, X),
    {reply, Secret, X};
handle_call(new, _From, X) ->
    Secret = crypto:strong_rand_bytes(12),
    Commit = hash:doit(Secret),
    NewX = dict:store(Commit, Secret, X),
    {reply, {Commit, Secret}, NewX};
handle_call(_, _From, X) -> {reply, X, X}.

new() ->
    gen_server:call(?MODULE, new).
get(Commit) ->
    gen_server:call(?MODULE, {get, Commit}).
