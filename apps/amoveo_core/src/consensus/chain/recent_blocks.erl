%this module seems unused.

-module(recent_blocks).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/0, add/3, change_pointer/3, pointer/1, dump/0]).
-include("../../records.hrl").
-define(LOC, constants:recent_blocks()).
-record(r, {blocks = [], height = -10000}).%storing all the valid blocks with a height bigger than or equal to r.height. Keep track of orphans too.

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
handle_cast({change_pointer, Hash, Height, Pointer}, X) ->
    Blocks = X#r.blocks,
    Blocks2 = change_pointer_internal(Hash, Height, Pointer, X#r.blocks),
    X2 = X#r{blocks = Blocks2},
    {noreply, X2};
handle_cast(dump, X) -> 
    {noreply, #r{}};
handle_cast(_, X) -> {noreply, X}.
handle_call({add, Hash, Height, Pointer}, _, X) ->
    %io:fwrite("recent blocks adding a block\n"),
    %io:fwrite(packer:pack(Hash)),
    %io:fwrite("\n"),
    %{ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    if
        (Height > X#r.height) -> 
            NewBottom = Height - FT,
            X2 = X#r{height = NewBottom, blocks = remove_before(X#r.blocks ++ [{Height, Hash, Pointer}], NewBottom)},
            {reply, ok, X2};
        true ->
            %when we are syncing in reverse
            {reply, ok, X}
    end;
handle_call(process_id, _, S) -> {reply, self(), S};
handle_call({pointer, Hash}, _From, X) -> 
    %io:fwrite("recent blocks read pointer\n"),
    %io:fwrite(packer:pack(Hash)),
    %io:fwrite("\n"),
    P = internal_read_pointer(Hash, X#r.blocks),
    {reply, P, X};
handle_call(read, _From, X) -> 
    #r{blocks = Blocks, height = Height} = X,
    {reply, lists:map(fun({_, H, _}) -> H end, Blocks), X};
handle_call(_, _From, X) -> {reply, X, X}.

get_hashes([]) -> [];
get_hashes([{Hash, _}|T]) -> 
    [Hash|get_hashes(T)].
add(Hash, Height, Pointer) ->
    if
	is_integer(Pointer) ->
	    gen_server:call(?MODULE, {add, Hash, Height, Pointer});
	true -> ok
    end.
change_pointer(Hash, Height, Pointer) ->
    gen_server:cast(?MODULE, {change_pointer, Hash, Height, Pointer}).
dump() ->
    gen_server:cast(?MODULE, dump).
read() -> gen_server:call(?MODULE, read).
pointer(Hash) ->
    gen_server:call(?MODULE, {pointer, Hash}).

%assumes that the list is low to high.
remove_before([], _) -> [];
remove_before([{Height, _, _}|T], Bottom) when Bottom > Height ->
    remove_before(T, Bottom);
remove_before(X, _) -> X.

%change_pointer_internal(Hash, Height, Pointer, []) -> [{Height, Hash, Pointer}];
change_pointer_internal(_, _, _, []) -> [];
change_pointer_internal(Hash, Height, Pointer, [{Height, Hash, _}|T]) ->
    [{Height, Hash, Pointer}|T];
change_pointer_internal(Hash, Height, Pointer, [H|T]) ->
    [H|change_pointer_internal(Hash, Height, Pointer, T)].

internal_read_pointer(Hash, []) ->    
    fail;
internal_read_pointer(Hash, [{_, Hash, P}|_]) -> 
    P;
internal_read_pointer(Hash, [{_, Hash2, _}|T]) -> 
    %io:fwrite("not same " ++ packer:pack(Hash) ++ " " ++ packer:pack(Hash2) ++ "\n"),
    internal_read_pointer(Hash, T).
