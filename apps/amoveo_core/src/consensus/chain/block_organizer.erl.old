-module(block_organizer).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         top/0, add/1, check/0, view/0, pid/0]).
-include("../../records.hrl").
-record(d, {blocks = [], top = 0}).
init(ok) -> {ok, #d{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(check, BS) -> 
    B2 = helper(BS#d.blocks),
    BS2 = BS#d{blocks = B2},
    {noreply, BS2}.
handle_call({add, Blocks}, _From, BS) -> 
    BS2 = merge(Blocks, BS#d.blocks, [], BS#d.top),
    B2 = helper(BS2#d.blocks), 
    BS3 = BS2#d{blocks = B2},
    {reply, ok, BS3};
handle_call(top, _From, BS) -> 
    {reply, BS#d.top, BS};
handle_call(pid, _From, X) -> 
    {reply, self(), X};
handle_call(view, _, BS) -> 
    {reply, BS, BS};
handle_call(_, _From, X) -> {reply, X, X}.

pid() -> gen_server:call(?MODULE, pid).
view() ->
    gen_server:call(?MODULE, view).
merge(New, [], L, T) -> #d{blocks = L ++ [New], 
                        top = T};%merge sort
merge([], Old, L, T) -> #d{blocks = L ++ Old,
                        top = T};
merge([N|NT], [O|OT], L, Top) ->
    %HN = hd(N),
    HO = hd(O),
    H1 = N#block.height,
    H2 = HO#block.height,
    NewTop = max(H1, max(Top, H2)),
    if
	H2 < H1 -> 
            merge([N|NT], OT, L ++ [O], NewTop);
	true -> #d{blocks = L ++ [[N|NT]|[O|OT]],
                   top = NewTop}
    end.
helper([]) -> [];
helper([[]]) -> [];
helper([H|T]) ->
    %we should run this in the background, and if H has an error, don't drop the rest of the list.

    MyHeight = block:height(),
    HH = hd(H),
    H2 = HH#block.height,
    if
	H2 =< MyHeight + 1 ->
            %spawn(fun() ->
            block_absorber:save(H),%maybe this should be a cast.
                          %check(),
            %              ok end),
            %T;
	    helper(T);
	true -> [H|T]
    end.
	    
check() -> gen_server:cast(?MODULE, check).
add([]) -> 0;
add(Blocks) when not is_list(Blocks) -> 0;
add(Blocks) ->
    %io:fwrite("block organizer add\n"),
    true = is_list(Blocks),
    {Blocks2, AddReturn} = add1(Blocks, []),
    add3(lists:reverse(Blocks2)),
    AddReturn.
add3([]) -> ok;
add3(Blocks)->
    %{A, B} = lists:split(10, Blocks),
    {A, B} = lists:split(min(100, length(Blocks)), Blocks),
    add4(A),
    %timer:sleep(5),
    add3(B).
%add3(Blocks) -> add4(Blocks).
add4(Blocks) ->
    spawn(fun() ->
                  S = "block organizer add4, height: " ++ (integer_to_list(element(2, hd(Blocks)))) ++ ("\n"),
                  %io:fwrite(S),
                  %io:fwrite(packer:pack(hd(Blocks))),
                  %io:fwrite("\n"),
                  %io:fwrite(integer_to_list(element(2, hd(Blocks)))),
                  %io:fwrite(" "),
                  %io:fwrite(integer_to_list(element(2, hd(lists:reverse(Blocks))))),
                  %io:fwrite("\n"),
                  %io:fwrite("block organizer add4, many: "),
                  %io:fwrite(integer_to_list(length(Blocks))),
		  Blocks2 = add5(Blocks),
                  %io:fwrite("block organizer add4 2\n"),
		  %gen_server:call(?MODULE, {add, lists:reverse(Blocks2)})
		  gen_server:call(?MODULE, {add, Blocks2})
	  end).
top() -> gen_server:call(?MODULE, top).
add5([]) -> [];
add5([Block|T]) ->
    {Dict, NewDict, BlockHash} = block:check0(Block),
    Block2 = Block#block{trees = {Dict, NewDict, BlockHash}},
    [Block2|add5(T)].
add1([], []) -> {[], 0};
add1([X], L) -> 
    {L2, A} = add2(X, L),
    {L++L2, A};
add1([H|T], L) ->
    {L2, _} = add2(H, L),
    add1(T, L2).
add2(Block, Out) ->
    if
	not(is_record(Block, block)) -> {Out, 0};
	true ->
	    Height = Block#block.height,
	    BH = block:hash(Block),
	    BHC = block_hashes:check(BH),
	    if
		Height == 0 -> {Out, 0};
		BHC -> {Out, 3}; %we have seen this block already
		true -> {[Block|Out], 0}
	    end
    end.
