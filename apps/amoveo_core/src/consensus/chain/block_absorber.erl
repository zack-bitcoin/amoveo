-module(block_absorber).
-behaviour(gen_server).
-include("../../records.hrl").
-export([%prune/0, %% delete unneeded things from trees asynchronously
	 recover/0,
	 check/0,
	 %synch_prune/1,
         %enqueue/1, %% async request
	 save/1,    %% returns after saving
	 do_save/1]). %% run without gen_server
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
init(ok) -> {ok, now()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> 
    io:fwrite("block absorber died! \n"),
    ok.
handle_info(_, X) -> {noreply, X}.
%handle_cast(prune, X) ->
%    trees:prune(),
%    {noreply, X};
handle_cast({doit, BP}, X) ->
    absorb_internal(BP),
    {noreply, now()}.
%handle_call({prune, Blocks}, _, X) ->
%    B = Blocks ++ trees:hash2blocks(recent_blocks:read()),
%    trees:prune(B),
%    {reply, ok, X};
handle_call(pid, _From, X) -> 
    {reply, self(), X};
handle_call(check, _From, X) -> 
    {reply, X, X};
handle_call({doit, BP}, _From, X) -> 
    Y = absorb_internal(BP),
    {reply, Y, now()}.
%prune() -> gen_server:cast(?MODULE, prune).
%synch_prune(Blocks) -> gen_server:call(?MODULE, {prune, Blocks}, 10000).
%enqueue([]) -> ok;
%enqueue([B|T]) -> enqueue(B, now()), enqueue(T);
%enqueue(X) -> enqueue(X, now()).
%enqueue(B, Time) -> gen_server:cast(?MODULE, {doit, B, Time}).
check() -> gen_server:call(?MODULE, check).
save([]) -> ok;
save([T]) -> save(T);
save([B|T]) -> save(B), save(T);
save(X) -> 
    %io:fwrite("block absorber \n"),
    case sync:status() of
	go ->
	    gen_server:call(?MODULE, {doit, X}, 10000);
	_ -> ok
    end.
absorb_internal(error) -> error;
absorb_internal(Block) ->
    Height = Block#block.height,
    MyHeight = block:height(), 
    if
	Height > (MyHeight + 1) ->
	    throw("too high");
	true ->
	    BH = block:hash(Block),
	    BHC = block_hashes:check(BH),
	    NextBlock = Block#block.prev_hash,
	    Bool = block_hashes:check(NextBlock),
	    if
		Height == 0 -> 0;
		%{ok, Header00} = headers:read(BH),
	%	    Header00;
		BHC -> 3; %we already have this block
		not(Bool) -> 0;%we dont' know the previous block
		true ->
		    false = empty == block:get_by_hash(NextBlock), %check that previous block was valid
		    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
		    TH = headers:read(BH),
		    Header = case TH of
				 {ok, H} -> 
				     headers:absorb_with_block([H]),
				     H;
				 error -> 
				     H = block:block_to_header(Block),
				     headers:absorb([H]),
				     headers:absorb_with_block([H]),
				     H
			     end,
		    {true, Block2} = block:check(Block),
		    BH = block:hash(Block2),
		    do_save(Block2),
	    %if 
		%(Height == 1) ->
		%    {ok, RD} = application:get_env(amoveo_core, revert_depth),
	    %HH = (headers:top())#header.height,
	    %if
		%HH == Height -> sync_mode:normal();
		%true -> ok
	    %end,
	    %if
		%	HH - RD < Height -> sync_mode:normal();
		%	true -> ok
		%    end;
		%true -> ok
	    %end,
		    case sync_mode:check() of
			normal -> 
			    Txs = (tx_pool:get())#tx_pool.txs,
			    tx_pool:dump(Block2),
			    OldTxs = tl(Block#block.txs),
			    Keep = lists:filter(fun(T) -> not(tx_pool_feeder:is_in(testnet_sign:data(T), OldTxs)) end, Txs),%This n**2 algorithm is slow. We can make it n*log(n) by sorting both lists first, and then comparing them.
			    tx_pool_feeder:absorb_async(Keep),
			    order_book:match(),
			    recent_blocks:add(BH, Header#header.accumulative_difficulty, Height),
						%potential_block:new();
			    potential_block:save(),
			    sync:push_new_block(Block2);
			quick -> 
			    recent_blocks:add(BH, Header#header.accumulative_difficulty, Height),
			    tx_pool:dump(Block2),
			    potential_block:dump()
		    end,
		    if
			(Block2#block.height rem 10) == 0 ->
			%1 == 1 ->
			    io:fwrite("absorb block "),
			    io:fwrite(integer_to_list(Block2#block.height)),
			    io:fwrite("\n");
			true -> ok
		    end,
		    Header
	    end
    end.
do_save(BlockPlus) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(BlockPlus)),
    Hash = block:hash(BlockPlus),
    BlockFile = amoveo_utils:binary_to_file_path(blocks, Hash),
    %io:fwrite("deleting blockfile "),
    %io:fwrite(BlockFile),
    %io:fwrite("\n"),
    %file:delete(BlockFile),
    %timer:sleep(100),
    ok = db:save(BlockFile, CompressedBlockPlus).
highest_block(B, []) -> B;
highest_block(A, [B|C]) ->
    D = if
	    A#block.height > B#block.height -> A;
	    true -> B
	end,
    highest_block(D, C).
first(0, _) -> [];
first(_, []) -> [];
first(N, [H|T]) -> [H|first(N-1, T)].
recover() ->
    %sync:stop(),
    %timer:sleep(20000),
    sync_kill:start(),
    sync_mode:quick(),
    %timer:sleep(100),
    {ok, BlockFiles} = file:list_dir("blocks/"),
    TBFs = first(10, BlockFiles),
    Blocks = lists:map(fun(BF) -> 
			       binary_to_term(zlib:uncompress(db:read("blocks/"++BF)))
		       end,
		       TBFs),
    io:fwrite("recover 1\n"),
    Block = highest_block(hd(Blocks), tl(Blocks)),
    io:fwrite("heighest block \n"),
    io:fwrite("recover 2\n"),
    io:fwrite(integer_to_list(Block#block.height)),
    io:fwrite("is block height \n"),
    Hashes = [block:hash(Block)|hashes_to_root(Block)],
    io:fwrite("recover 3\n"),
    io:fwrite(integer_to_list(length(Hashes))),
    io:fwrite(" is many hashes\n"),
    io:fwrite(packer:pack(hd(lists:reverse(Hashes)))),
    io:fwrite("\n"),
    Pid = block_organizer:pid(),
    read_absorb(lists:reverse(Hashes), Pid, []).
hashes_to_root(Block) ->
    H = Block#block.height,
    if
	H == 1 -> [];
	true ->
	    PH = Block#block.prev_hash,
	    PB = block:get_by_hash(PH),
	    [PH|hashes_to_root(PB)]
    end.
read_absorb([], _, X) -> block_organizer:add(lists:reverse(X));
read_absorb(A, Pid, B) when length(B) > 50 ->
    block_organizer:add(lists:reverse(B)),
    read_absorb(A, Pid, []);
read_absorb([H|T], Pid, L) ->
    {message_queue_len, Size} = erlang:process_info(Pid, message_queue_len),
    if
	Size < 4 ->
	    B = block:get_by_hash(H),
	    read_absorb(T, Pid, [B|L]);
	true ->
	    timer:sleep(50),
	    read_absorb([H|T], Pid, L)
    end.

	    
