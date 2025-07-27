%this module seems unused.

-module(recent_blocks).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/0, add/2]).
-include("../../records.hrl").
-define(LOC, constants:recent_blocks()).
-record(r, {blocks = [], height = 0}).%storing all the valid blocks with a height bigger than or equal to r.height. Keep track of orphans too.

init(ok) -> 
    %io:fwrite("start recent_blocks"),
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = case X of
             "" -> #r{};
             #r{} -> X;
             _ -> #r{}
         end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, K),
    io:fwrite("recent blocks died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({add, Hash, Height}, _, X) ->
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    if
        ((Height - FT) > X#r.height) -> 
            NewBottom = Height - FT,
            X2 = X#r{height = NewBottom, blocks = remove_before(X#r.blocks ++ [{Height, Hash}], NewBottom)},
            {reply, ok, X2};
        true ->
            %when we are syncing in reverse
            {reply, ok, X}
    end;
handle_call(read, _From, X) -> 
    #r{blocks = Blocks, height = Height} = X,
    {reply, lists:map(fun({_, H}) -> H end, Blocks), X};
handle_call(_, _From, X) -> {reply, X, X}.

get_hashes([]) -> [];
get_hashes([{Hash, _}|T]) -> 
    [Hash|get_hashes(T)].
add(Hash, Height) ->
    gen_server:call(?MODULE, {add, Hash, Height}).
read() -> gen_server:call(?MODULE, read).

%assumes that the list is low to high.
remove_before([], _) -> [];
remove_before([{Height, Hash}|T], Bottom) when Bottom > Height ->
    remove_before(T, Bottom);
remove_before(X, _) -> X.


    
