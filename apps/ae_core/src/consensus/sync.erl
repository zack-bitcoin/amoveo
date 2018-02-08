-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, start/0, stop/0, status/0, give_blocks/2]).
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
doit2(L) ->
    T = list_to_tuple(L),
    <<X:24>> = crypto:strong_rand_bytes(3),
    M = X rem size(T),
    Peer = element(M+1, T),
    %gen_server:cast(?MODULE, {main, Peer}),
    spawn(fun() -> sync_peer(Peer) end),
    ok.
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
give_blocks(Peer, CommonHash) -> 
    io:fwrite("give blocks\n"),
    {ok, DBB} = application:get_env(ae_core, push_blocks_batch),
    Blocks0 = lists:reverse(blocks(CommonHash, block:top())),
    Blocks = if
                 length(Blocks0) < DBB -> Blocks0;
                 true ->
                     {X, _} = lists:split(DBB, Blocks0),
                     X
             end,
    if 
        length(Blocks) > 0 ->
            remote_peer({give_block, Blocks}, Peer),
            NewCommonHash = block:hash(hd(lists:reverse(Blocks))),
            give_blocks(Peer, NewCommonHash);
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
    case CommonHash of
        <<>> -> get_headers2(Peer, N+HB-1);
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
                    give_blocks(Peer, CommonBlocksHash);
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


