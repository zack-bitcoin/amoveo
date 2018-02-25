-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	 start/1, start/0, stop/0, status/0, 
	 give_blocks/3, push_new_block/1, remote_peer/2]).
-include("../records.hrl").
-define(tries, 200).%20 tries per second. 
%so if this is 400, that means we have 20 seconds to download download_block_batch * download_block_many blocks
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("sync died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
%handle_cast(start, _) -> {noreply, go};
%handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, _) -> 
    case Peer of 
	error -> ok;
	_ ->
	    case status() of
		go ->
		    io:fwrite("syncing with this peer now "),
		    io:fwrite(packer:pack(Peer)),
		    io:fwrite("\n"),
		    sync_peer(Peer);
		_ -> 
		    io:fwrite("not syncing with this peer now "),
		    io:fwrite(packer:pack(Peer)),
		    io:fwrite("\n"),
		    ok
	    end
    end,
    {noreply, []};
handle_cast(_, X) -> {noreply, X}.
%handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.
status() -> sync_kill:status().
stop() -> sync_kill:stop().
start() -> start(peers:all()).
start(P) ->
    sync_kill:start(),
    %gen_server:cast(?MODULE, start),
    doit2(P).
randoms(N0, Input) ->
    S = length(Input),
    N = min(N0, S),
    IDS = randoms(N, S, []),
    T = list_to_tuple(Input),
    randoms2(IDS, T).
randoms2([], _) -> [];
randoms2([H|T], Tup) ->
    [element(H, Tup)|randoms2(T, Tup)].
randoms(0, InputSize, Output) -> Output;
randoms(N, InputSize, Output) ->
    %true = N < InputSize,
    M = random:uniform(InputSize),
    B = lists:member(M, Output),
    if 
	B -> randoms(N, InputSize, Output);
	true -> randoms(N-1, InputSize, [M|Output])
    end.
doit3([]) -> ok;
doit3([H|T]) ->
    gen_server:cast(?MODULE, {main, H}),
    doit3(T).
doit2([]) -> ok;
%doit2([Peer|T]) ->
doit2(L0) ->
    L = remove_self(L0),
    if
	length(L) == 0 ->
	    io:fwrite("no one to sync with\n"),
	    ok;
	true ->
	    N = min(length(L), 3),
	    Peers = randoms(N, L),
	    io:fwrite("eventually will sync with these peers "),
	    io:fwrite(packer:pack(Peers)),
	    io:fwrite("\n"),
	    doit3(Peers)
    end.
    %timer:sleep(500),
    %doit2(T).
blocks(CommonHash, Block) ->
    BH = block:hash(Block),
    if 
        BH == CommonHash -> [];
        true ->
            PrevBlock = block:get_by_hash(Block#block.prev_hash),
            [Block|blocks(CommonHash, PrevBlock)]
    end.
give_blocks(Peer, CommonHash, TheirBlockHeight) -> 
    %io:fwrite("give blocks\n"),
    go = sync_kill:status(),
    {ok, DBB} = application:get_env(amoveo_core, push_blocks_batch),
    H = min(block:height(), max(0, TheirBlockHeight + DBB - 1)),
    Blocks = lists:reverse(blocks(CommonHash, block:get_by_height(H))),
    if 
        length(Blocks) > 0 ->
            remote_peer({give_block, Blocks}, Peer),
	    timer:sleep(1000),
	    {ok, _, TheirBlockHeight2} = remote_peer({top}, Peer),
	    if
		TheirBlockHeight2 > TheirBlockHeight ->
	    
		    NewCommonHash = block:hash(hd(lists:reverse(Blocks))),
		    give_blocks(Peer, NewCommonHash, TheirBlockHeight2);
		true -> 
		    %we should remove them from the list of peers.
		    io:fwrite("they are not accepting our blocks."),
		    ok
	    end;
        true -> 
            io:fwrite("finished sending blocks"),
            false
    end.

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
	bad_peer -> %remove from peers, add to a black list for next N minutes.
	    {{_,_,_,_},_} = Peer,
	    io:fwrite("removing peer "),
	    io:fwrite(packer:pack(Peer)),
	    io:fwrite("\n"),
	    io:fwrite("command was "),
	    io:fwrite(element(1, Transaction)),
	    io:fwrite("\n"),
	    blacklist_peer:add(Peer),
	    peers:remove(Peer),
	    error;
        Return1 -> Return1
    end.
trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = amoveo_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).
-define(HeadersBatch, application:get_env(amoveo_core, headers_batch)).
get_headers(Peer) -> 
    N = (headers:top())#header.height,
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    Start = max(0, N - FT), 
    get_headers2(Peer, Start).
get_headers2(Peer, N) ->%get_headers2 only gets called more than once if fork_tolerance is bigger than HeadersBatch.
    {ok, HB} = ?HeadersBatch,
    Headers = remote_peer({headers, HB, N}, Peer),
    case Headers of
	error -> error;
	bad_peer -> error;
	_ ->
	    CommonHash = headers:absorb(Headers),
	    L = length(Headers),
	    case CommonHash of
		<<>> -> 
		    if 
			(L+5) > HB -> get_headers2(Peer, N+HB-1);
			true -> ok
		    end;
		_ -> spawn(fun() -> get_headers3(Peer, N+HB-1) end),
						%Once we know the CommonHash, then we are ready to start downloading blocks. We can download the rest of the headers concurrently while blocks are downloading.
		     CommonHash
	    end
    end.
get_headers3(Peer, N) ->
    {ok, HB} = ?HeadersBatch,
    Headers = remote_peer({headers, HB, N}, Peer),
    headers:absorb(Headers),
    if
        length(Headers) > (HB div 2) -> 
            get_headers3(Peer, N+HB-1);
        true -> ok
    end.
common_block_height(CommonHash) ->
    case block:get_by_hash(CommonHash) of
        empty -> 
            {ok, Header} = headers:read(CommonHash),
            PrevCommonHash = Header#header.prev_hash,
            common_block_height(PrevCommonHash);
        B -> B#block.height
    end.
get_blocks(Peer, N, 0) ->
    io:fwrite("could not get block "),
    io:fwrite(integer_to_list(N)),
    io:fwrite(" from peer "),
    io:fwrite(packer:pack(Peer));
get_blocks(Peer, N, Tries) ->
    %io:fwrite("syncing. use `sync:stop().` if you want to stop syncing.\n"),
    %io:fwrite("get blocks\n"),
    {ok, BB} = application:get_env(amoveo_core, download_blocks_batch),
    {ok, BM} = application:get_env(amoveo_core, download_blocks_many),
    timer:sleep(50),
    go = sync_kill:status(),
    Height = block:height(),
    AHeight = api:height(),
    if
	Height == AHeight -> ok;%done syncing
	N > Height + (BM * BB) ->%This uses up 10 * BB * block_size amount of ram.
	    
	    %trapped here because blocks aren't syncing.
	    %This is bad, we shouldn't let our partner trap us this way.
	    %timer:sleep(500),
	    get_blocks(Peer, N, Tries-1);
	true ->
	    %io:fwrite("another get_blocks thread\n"),
	    timer:sleep(100),
	    spawn(fun() ->
			  get_blocks2(BB, N, Peer, 5)
		  end),
	    get_blocks(Peer, N+BB, ?tries)
    end.
get_blocks2(_BB, _N, _Peer, 0) ->
    io:fwrite("get_blocks2 failed\n"),
    ok;
get_blocks2(BB, N, Peer, Tries) ->
    %io:fwrite("get blocks 2\n"),
    go = sync_kill:status(),
    %timer:sleep(500),
    Blocks = talker:talk({blocks, BB, N}, Peer),
    go = sync_kill:status(),
    Sleep = 600,
    case Blocks of
	{error, _} -> 
	    io:fwrite("get blocks 2 failed connect error\n"),
	    io:fwrite(packer:pack([BB, N, Peer, Tries])),
	    io:fwrite("\n"),
	    timer:sleep(Sleep),
	    get_blocks2(BB, N, Peer, Tries - 1);
	bad_peer -> 
	    io:fwrite("get blocks 2 failed connect bad peer\n"),
	    io:fwrite(packer:pack([BB, N, Peer, Tries])),
	    io:fwrite("\n"),
	    timer:sleep(Sleep),
	    get_blocks2(BB, N, Peer, Tries - 1);
	{ok, Bs} -> %block_absorber:enqueue(Bs);
	    block_organizer:add(Bs);
	_ -> %block_absorber:enqueue(Blocks)
	    block_organizer:add(Blocks)
    end.
remove_self(L) ->%assumes that you only appear once or zero times in the list.
    MyIP = my_ip:get(),
    {ok, MyPort} = application:get_env(amoveo_core, port),
    Me = {MyIP, MyPort},
    remove_self2(L, Me).
remove_self2([], _) -> [];
remove_self2([H|T], Me) ->
    if
	H == Me -> T;
	true -> [H|remove_self2(T, Me)]
    end.
shuffle([]) -> [];
shuffle([X]) -> [X];
shuffle(L) -> shuffle(L, length(L), []).
shuffle([], 0, Result) -> Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(random:uniform(Len), List, []),
    shuffle(Rest, Len - 1, [Elem|Result]).
nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).
push_new_block(Block) ->
    %keep giving this block to random peers until 1/2 the people you have contacted already know about it. Don't talk to the same peer multiple times.
    Peers0 = peers:all(),
    Peers = remove_self(Peers0),
    spawn(fun() -> push_new_block_helper(0, 0, shuffle(Peers), Block) end).
push_new_block_helper(_, _, [], _) -> ok;%no one else to give the block to.
push_new_block_helper(N, M, _, _) when ((M > 0) and ((N*3) >= (M*2))) -> ok;%the majority of peers probably already know.
push_new_block_helper(N, M, [P|T], Block) ->
    %X = talker:talk({give_block, Block}, P),
    X = remote_peer({give_block, Block}, P),
    %io:fwrite(packer:pack(X)),
    Z = case X of
	    3 -> 1;
	    _ -> 
		spawn(fun() ->
			      {ok, _, TheirBlockHeight} = remote_peer({top}, P),
			      MyHeight = block:height(),
			      if 
				  TheirBlockHeight < MyHeight ->
				      CommonBlocksHash = block:hash(block:get_by_height(TheirBlockHeight)),
				      give_blocks(P, CommonBlocksHash, TheirBlockHeight);
				  true -> ok
			      end
		      end),
		0
	end,
    push_new_block_helper(N+Z, M+1, T, Block).
trade_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb_async(Txs),
    Mine = (tx_pool:get())#tx_pool.txs,
    remote_peer({txs, lists:reverse(Mine)}, Peer).

sync_peer(Peer) ->
    io:fwrite("trade peers\n"),
    trade_peers(Peer),
    MyTop = headers:top(),
    io:fwrite("get their top header\n"),
    TheirTop = remote_peer({header}, Peer), 
    MyBlockHeight = block:height(),
    TheirTopHeight = TheirTop#header.height,
    if
        not(MyTop == TheirTop) ->
	    io:fwrite("get headers\n"),
            CommonHash = get_headers(Peer),
            {ok, TBH} = headers:read(block:hash(block:top())),
            MD = TBH#header.accumulative_difficulty,
            TD = TheirTop#header.accumulative_difficulty,
            if
		CommonHash == error -> error;
                TD < MD -> 
                    {ok, _, TheirBlockHeight} = remote_peer({top}, Peer),
                    CommonBlocksHash = block:hash(block:get_by_height(TheirBlockHeight)),
		    spawn(fun() ->
				  give_blocks(Peer, CommonBlocksHash, TheirBlockHeight)
			  end);
                true ->
                    CommonBlockHeight = common_block_height(CommonHash),
		    {ok, ForkTolerance} = application:get_env(amoveo_core, fork_tolerance),
		    CBH = max(CommonBlockHeight, MyBlockHeight - ForkTolerance),
                    get_blocks(Peer, CommonBlockHeight, ?tries)
            end;
        MyBlockHeight < TheirTopHeight ->
	    io:fwrite("my height is less\n"),
            {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
            get_blocks(Peer, max(0, MyBlockHeight - FT), 80);
        true -> 
	    io:fwrite("already synced with this peer \n"),
	    ok
    end,
    spawn(fun() ->
		  trade_txs(Peer)
	  end).


