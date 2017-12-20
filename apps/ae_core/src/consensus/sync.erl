-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	doit/0, start/1, start/0, stop/0, status/0]).
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, go) -> 
    trade_peers(Peer),
    MyTop = headers:top(),
    TheirTop = remote_peer({header}, Peer), 
    io:fwrite(packer:pack({tops, MyTop, TheirTop})),
    1=2,
    %if their top header is different than ours:
    CommonHash = get_headers(Peer),
    %if their top header has less work than ours:
    %give_blocks(Peer, CommonHash),
    %else if their top header has more work than ours:
    CommonBlockHeight = common_block_height(CommonHash),
    get_blocks(Peer, CommonBlockHeight),
    %endif
    %endif
    trade_txs(Peer),
    {noreply, go};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.
status() -> gen_server:call(?MODULE, status).
stop() -> gen_server:cast(?MODULE, stop).
start() ->
    P = peers:all(),
    start(P).
start(P) ->
    download_blocks:sync_all(P, block:height()).
doit() ->
    P = peers:all(),
    start_new(P).
start_new(P) ->
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
give_headers(_, _) -> ok.
give_blocks(_, _) -> ok.
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
    L = get_headers2(Peer, Start, []),
    hd(lists:reverse(L)).%returns the hash of the header that we have in common which has the most difficulty.
get_headers2(Peer, N, CHT) ->
    Headers = remote_peer({headers, ?HeadersBatch, N}, Peer),
    CommonHash = headers:absorb(Headers),
    if
        length(Headers) > (?HeadersBatch div 2) -> 
            get_headers2(Peer, N+?HeadersBatch, [CommonHash|CHT]);
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
            
