-module(recent_blocks).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/0, add/2]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("recent blocks died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Hash, Height}, X) -> 
    {ok, FT} = application:get_env(ae_core, fork_tolerance),
    Y = remove_before(X, Height - FT),
    {noreply, [{Hash, Height}|Y]};
handle_cast(_, X) -> {noreply, X}.
handle_call(read, _From, X) -> 
    Y = get_hashes(X),
    GH = block:hash(block:get_by_height(0)),
    {reply, [GH|Y], X};
handle_call(_, _From, X) -> {reply, X, X}.

get_hashes([]) -> [];
get_hashes([{Hash, Height}|T]) -> 
    [Hash|get_hashes(T)].

remove_before([], _) -> [];
remove_before([{Hash, Height}|T], X) when Height < X ->
    remove_before(T, X);
remove_before([H|T], X) -> [H|remove_before(T, X)].

add(Hash, Height) ->
    gen_server:cast(?MODULE, {add, Hash, Height}).
read() ->
    gen_server:call(?MODULE, read).
