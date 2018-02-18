-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, start/0, stop/0, status/0, give_blocks/3, push_new_block/1]).
-include("../records.hrl").
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("sync died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, go) -> 
    sync_peer(Peer),
    {noreply, go};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.
status() -> gen_server:call(?MODULE, status).
stop() -> gen_server:cast(?MODULE, stop).
start() -> start(peers:all()).
start(P) ->
    gen_server:cast(?MODULE, start),
    doit2(P).
doit2([]) -> ok;
%doit2([Peer|T]) ->
doit2(L0) ->
    L = remove_self(L0),
    T = list_to_tuple(L),
    <<X:24>> = crypto:strong_rand_bytes(3),
    M = X rem size(T),
    Peer = element(M+1, T),
    if
	length(L) == 0 ->
	    io:fwrite("no one to sync with\n"),
	    ok;
	true ->
	    io:fwrite("syncing with peer "),
	    io:fwrite(packer:pack(Peer)),
	    io:fwrite("\n"),
	    %gen_server:cast(?MODULE, {main, Peer}),
	    spawn(fun() -> sync_peer(Peer) end),
	    ok
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
    io:fwrite("give blocks\n"),
    {ok, DBB} = application:get_env(ae_core, push_blocks_batch),
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
	    peers:remove(Peer),
	    blacklist_peer:add(Peer),
	    error;
        Return1 -> Return1
    end.
trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).
-define(HeadersBatch, application:get_env(ae_core, headers_batch)).
get_headers(Peer) -> 
    N = (headers:top())#header.height,
    {ok, FT} = application:get_env(ae_core, fork_tolerance),
    Start = max(0, N - FT), 
    get_headers2(Peer, Start).
get_headers2(Peer, N) ->%get_headers2 only gets called more than once if fork_tolerance is bigger than HeadersBatch.
    {ok, HB} = ?HeadersBatch,
    Headers = remote_peer({headers, HB, N}, Peer),
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
get_blocks(Peer, N) ->
    Status = status(),
    case status() of
	go ->
	    {ok, BB} = application:get_env(ae_core, download_blocks_batch),
	    Blocks = remote_peer({blocks, BB, N}, Peer),
	    case Blocks of
		{error, _} -> 
		    io:fwrite("sync:get_blocks/2 error\n"),
		    get_blocks(Peer, N);
		_ ->
		    block_absorber:save(Blocks),
		    if
			length(Blocks) > (BB div 2) ->
			    get_blocks(Peer, N+BB);
			true -> ok
		    end
	    end;
	_ -> ok
    end.
remove_self(L) ->%assumes that you only appear once or zero times in the list.
    MyIP = peers:my_ip(),
    {ok, MyPort} = application:get_env(ae_core, port),
    Me = {MyIP, MyPort},
    remove_self2(L, Me).
remove_self2([], _) -> [];
remove_self2([H|T], Me) ->
    if
	H == Me -> T;
	true -> [H|remove_self2(T, Me)]
    end.
	    
push_new_block(Block) ->
    Peers0 = peers:all(),
    Peers = remove_self(Peers0),
    push_new_block_helper(0, 0, Peers, Block).
push_new_block_helper(_, _, [], _) ->
    %no one else to give the block to.
    ok;
push_new_block_helper(N, M, _, _) when ((N*2) >= M) ->
    %the majority of peers already know.
    ok;
push_new_block_helper(N, M, [P|T], Block) ->
    ok.
    %keep giving this block to random peers until 1/2 the people you have contacted already know about it. Don't talk to the same peer multiple times.
trade_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
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
                TD < MD -> 
                    {ok, _, TheirBlockHeight} = remote_peer({top}, Peer),
                    CommonBlocksHash = block:hash(block:get_by_height(TheirBlockHeight)),
                    give_blocks(Peer, CommonBlocksHash, TheirBlockHeight);
                true ->
                    CommonBlockHeight = common_block_height(CommonHash),
                    get_blocks(Peer, CommonBlockHeight)
            end;
        MyBlockHeight < TheirTopHeight ->
	    io:fwrite("my height is less\n"),
            {ok, FT} = application:get_env(ae_core, fork_tolerance),
            get_blocks(Peer, max(0, MyBlockHeight - FT));
        true -> 
	    io:fwrite("already synced with this peer \n"),
	    ok
    end,
    trade_txs(Peer).


