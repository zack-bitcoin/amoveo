-module(block_organizer).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	add/1, check/0, view/0]).
-include("../../records.hrl").
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(check, BS) -> 
    BS2 = helper(BS),
    {noreply, BS2};
handle_cast({add, Block}, BS) -> 
    BS2 = merge(Block, BS),
    BS3 = helper(BS2),
    {noreply, BS3}.
handle_call(view, _, BS) -> 
    {reply, BS, BS};
handle_call(_, _From, X) -> {reply, X, X}.

view() ->
    gen_server:call(?MODULE, view).
merge(Block, []) -> 
    [Block];
merge(Block, [B2]) -> 
    H1 = Block#block.height,
    H2 = B2#block.height,
    if
	H1 =< H2 -> [Block, B2];
	H1 > H2 -> [B2, Block]
    end;
merge(Block, BS) ->
    %io:fwrite("organizer merge\n"),
    S = length(BS),
    {L1, L2} = lists:split(S div 2, BS),
    B2 = hd(L2),
    H1 = Block#block.height,
    H2 = B2#block.height,
    if
	H1 == (H2 - 1) -> L1 ++ [Block] ++ L2;
	H1 == H2 -> L1 ++ [Block] ++ L2;
	H1 < H2 -> merge(Block, L1) ++ L2;
	H1 > H2 -> L1 ++ merge(Block, L2)
    end.
helper([]) -> [];
helper([H|T]) ->
    %io:fwrite("organizer helper\n"),
    MyHeight = block:height(),
    H2 = H#block.height,
    if
	H2 =< MyHeight + 1 ->
	    block_absorber:save(H),
	    helper(T);
	true -> [H|T]
    end.
	    
check() -> gen_server:cast(?MODULE, check).
add([Block]) -> add(Block);
add([H|T]) -> add(H), add(T);
add(Block) ->
    true = is_record(Block, block),
    BH = block:hash(Block),
    BHC = block_hashes:check(BH),
    Height = Block#block.height,
    %MyHeight = block:height(), 
    %NextBlock = Block#block.prev_hash,
    if 
	Height == 0 -> 
	    %{ok, Header00} = headers:read(BH),
	    %Header00;
	    0;
	BHC -> 3;%we have already seen this block
	true ->
	    gen_server:cast(?MODULE, {add, Block}),
	    0
    end.
