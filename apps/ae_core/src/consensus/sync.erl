-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, start/0, stop/0, status/0]).
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("sync died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, go) -> 
    trade_peers(Peer),
    MyTop = headers:top(),
    TheirTop = remote_peer({header}, Peer), 
    if
        not(MyTop == TheirTop) ->
            CommonHash = get_headers(Peer),
            io:fwrite("main commonHash is "),
            io:fwrite(packer:pack(CommonHash)),
            io:fwrite("\n"),
            {ok, TBH} = headers:read(block:hash(block:top())),
            MD = headers:accumulative_difficulty(TBH),
            TD = headers:accumulative_difficulty(TheirTop),
            if
                TD < MD -> 
                    io:fwrite("give blocks\n"),
                    give_blocks(Peer, CommonHash);
                true ->
                    io:fwrite("get blocks\n"),
                    CommonBlockHeight = common_block_height(CommonHash),
                    B = get_blocks(Peer, CommonBlockHeight)
            end;
        true -> ok
    end,
    trade_txs(Peer),
    {noreply, go};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.
status() -> gen_server:call(?MODULE, status).
stop() -> gen_server:cast(?MODULE, stop).
%doit() ->
%    P = peers:all(),
%    start(P).
%start(P) ->
%    download_blocks:sync_all(P, block:height()).
start() ->
    P = peers:all(),
    start(P).
start(P) ->
    io:fwrite("sync with peer "),
    io:fwrite(packer:pack(P)),
    io:fwrite("\n"),
    gen_server:cast(?MODULE, start),
    doit2(P).
doit2([]) ->
    ok;
doit2([Peer|T]) ->
    %check if our version is the same.
    gen_server:cast(?MODULE, {main, Peer}),
    doit2(T).
blocks(CommonHash, Block) ->
    io:fwrite("blocks function"),
    BH = block:hash(Block),
    if 
        BH == CommonHash -> [];
        true ->
            PrevBlock = block:get_by_hash(block:prev_hash(Block)),
            [Block|blocks(CommonHash, PrevBlock)]
    end.
give_blocks(Peer, CommonHash) -> 
    io:fwrite("give blocks function \n"),
    Blocks = lists:reverse(blocks(CommonHash, block:top())),
    io:fwrite("give this many blocks"),
    io:fwrite(integer_to_list(length(Blocks))),
    if 
        length(Blocks) > 0 ->
            %spawn(fun() -> do_send_blocks(Peer, Blocks) end);
            do_send_blocks(Peer, Blocks);
        true -> false
    end.
do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
    io:fwrite("do send blocks function \n"),
    io:fwrite("give block "),
    io:fwrite(integer_to_list(block:height(Block))),
    io:fwrite("\n"),
    remote_peer({give_block, Block}, Peer),
    timer:sleep(20),
    do_send_blocks(Peer, T).
remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).
-define(HeadersBatch, 101).
-define(BlocksBatch, 10).
-define(ForkTolerance, application:get_env(ae_core, fork_tolerance)).
get_headers(Peer) -> 
    N = headers:height(headers:top()),
    {ok, FT} = ?ForkTolerance,
    Start = max(0, N - FT), %start earlier in case they are on a fork.
    _CommonHash = get_headers2(Peer, Start, []).
get_headers2(Peer, N, CHT) ->
    Headers = remote_peer({headers, ?HeadersBatch, N}, Peer),
    CommonHash = headers:absorb(Headers),%we only actually care about the CommonHash from the first iteration of get_headers2. Not from any recursive call.
    spawn(fun() -> get_headers3(Peer, N+?HeadersBatch-1) end),
    CommonHash.
get_headers3(Peer, N) ->
    Headers = remote_peer({headers, ?HeadersBatch, N}, Peer),
    if
        length(Headers) > (?HeadersBatch div 2) -> 
            get_headers3(Peer, N+?HeadersBatch-1);
        true -> ok
    end.
common_block_height(CommonHash) ->
    case block:get_by_hash(CommonHash) of
        empty -> 
            Header = headers:read(CommonHash),
            PrevCommonHash = headers:prev_hash(Header),
            common_block_height(PrevCommonHash);
        B -> block:height(B)
    end.
get_blocks(Peer, N) ->
    Blocks = remote_peer({blocks, ?BlocksBatch, N}, Peer),
    block_absorber:save(Blocks),
    if
        length(Blocks) > (?BlocksBatch div 2) ->
            get_blocks(Peer, N+?BlocksBatch);
        true -> ok
    end.
trade_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).
            
