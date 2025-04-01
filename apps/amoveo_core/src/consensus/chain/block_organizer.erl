%stores valid blocks in order, waiting for their turn to be used to update the consensus state. Once we know all the ancestors of a block, that block can be passed along to the block absorber to be written to the hard drive.
%This module verifies that the block is valid and that we don't already know about them, and it puts these blocks in ram in order, so that they can be written to the hard drive by block_db. The validity checks run in parallel.
%The idea is that if different peers send us the same block, we don't have to store it twice waiting for it's turn to be verified. We can quickly prune out invalid blocks, or blocks we already know about.  This can be important for when the hard drive or verkle tree are the bottle neck. In that situation, peers can send us blocks much faster than we can process them. Since nodes push blocks to their peers, this situation happens frequently.

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
handle_call({add, Height, Many, Blocks}, _From, BS) ->
    BS2 = merge(Height, Many, Blocks, BS#d.blocks, [], BS#d.top),
    %io:fwrite(BS),
    B2 = helper(BS2#d.blocks), %crashes here.
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
merge(Height, Many, New, [], L, T) -> #d{blocks = L ++ [{Height, Many, New}], 
                        top = T};%merge sort
merge(_, _, [], Old, L, T) -> #d{blocks = L ++ Old,
                        top = T};
merge(Height, Many, N, [{Height2, Many2, O}|OT], L, Top) ->
    %HO = hd(O),
    P = {Height2, Many2, O},
    H1 = Height,
    %H2 = HO#block.height,
    H2 = Height2,
    NewTop = max(H1, max(Top, H2)),
    if
	H2 < H1 -> merge(Height, Many, N, OT, L ++ [P], NewTop);
        true -> #d{blocks = L ++ [{Height, Many, N}|[P|OT]],
                   top = NewTop}
    end.
    
old_merge([N|NT], [O|OT], L, Top) ->
    %HN = hd(N),
    HO = hd(O),
    H1 = N#block.height,
    H2 = HO#block.height,
    NewTop = max(H1, max(Top, H2)),
    if
	H2 < H1 -> 
            old_merge([N|NT], OT, L ++ [O], NewTop);
	true -> #d{blocks = L ++ [[N|NT]|[O|OT]],
                   top = NewTop}
    end.
helper([]) -> [];
helper([[]]) -> [];
helper([{Height, Many, CB}|T]) ->
    %we should run this in the background, and if H has an error, don't drop the rest of the list.

    MyHeight = block:height(),
    %HH = hd(H),
    %H2 = HH#block.height,
    H2 = Height,
    if
	H2 =< MyHeight + 1 ->
            %spawn(fun() ->
            %io:fwrite("block absorber save start"),
            %spawn(fun() ->
            UCB = block_db:uncompress(CB),
            block_absorber:save(UCB),%crashes here.
           %               ok
             %     end),
            %io:fwrite("block absorber save finish"),
                          %check(),
            %              ok end),
            %T;
	    helper(T);
	true -> [{Height, Many, CB}|T]
    end.
	    
check() -> gen_server:cast(?MODULE, check).
add([]) -> 0;
add(Blocks) when not is_list(Blocks) -> 0;
add(Blocks) ->
    true = is_list(Blocks),
    %io:fwrite("block organizer add\n"),
    %io:fwrite(packer:pack(Blocks)),
    {Blocks2, AddReturn} = add1(Blocks, []),
    %io:fwrite("block organizer add1 finished\n"),
    {ok, Cores} = application:get_env(amoveo_core, block_threads),
    %Cores = 4,
    start_validation_threads(lists:reverse(Blocks2), (length(Blocks2) div Cores) + 1),
    AddReturn. %if we have already seen this block, we return a 3. otherwise, it returns a 0. 
start_validation_threads([], _) -> ok;
start_validation_threads(Blocks, Many)->
    {A, B} = lists:split(min(Many, length(Blocks)), Blocks),
    merge_if_valid(A),
    timer:sleep(5),
    start_validation_threads(B, Many).
%start_validation_threads(Blocks) -> merge_if_valid(Blocks).
merge_if_valid(Blocks) ->
    %io:fwrite("block organizer add 4\n"),
    spawn(fun() ->
                  Height = element(2, hd(Blocks)),
                  %Height2 = element(2, hd(lists:reverse(Blocks))),
                  %S = "block organizer add4, height: " ++ (integer_to_list(Height)) ++ " " ++ integer_to_list(Height2) ++ (" many: ") ++ (integer_to_list(length(Blocks)))++ ("\n"),
                  %io:fwrite("block organizer add4, many: "),
                  %io:fwrite(integer_to_list(length(Blocks))),
                  %io:fwrite("block organizer add5 start\n"),
                  Blocks2 = block_validity_checks(Blocks),

                 gen_server:call(?MODULE, {add, Height, length(Blocks2), block_db:compress(Blocks2)})
	  end).
top() -> gen_server:call(?MODULE, top).
block_validity_checks([]) -> 
    %io:fwrite("block organizer add5 done\n"),
    [];
block_validity_checks([Block|T]) ->
    X = block:check0(Block),
    Block2 = Block#block{trees = X},
    [Block2|block_validity_checks(T)].
add1([], []) -> {[], 0};
add1([X], L) -> 
    add2(X, L);
%{L2, A} = add2(X, L),
%    {L2, A};
add1([H|T], L) ->
    {L2, _} = add2(H, L),
    add1(T, L2).
add2(Block, Out) ->
    if
	not(is_record(Block, block)) -> 
            io:fwrite("block organizer tried adding something that was not a block\n"),
            {Out, 0};
	true ->
	    Height = Block#block.height,
	    BH = block:hash(Block),
	    BHC = block_hashes:check(BH),
	    if
		Height == 0 -> 
                    io:fwrite("block organizer add2 height 0 \n"),
                    {Out, 0};
		BHC -> 
                    %io:fwrite("block organizer add2 block hash check activated \n"),
                    %io:fwrite(integer_to_list(Height)),
                    %io:fwrite("\n"),
                    {Out, 3}; %we have seen this block already
		true -> 

                    {[Block|Out], 0}
	    end
    end.
