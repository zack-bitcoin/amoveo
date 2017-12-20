-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, start/0, stop/0, status/0]).
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
handle_cast(stop, _) -> {noreply, stop};
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
start_new(P) ->
    io:fwrite("sync with peer "),
    io:fwrite(packer:pack(P)),
    io:fwrite("\n"),
    gen_server:cast(?MODULE, start),
    BH = block:height(),
    HH = headers:height(headers:top()),
    doit2(P, BH, HH).
doit2([], _, _) ->
    ok;
doit2([Peer|T], BH, HH) ->
    S = status(),
    %check if our version is the same.
    case S of
        go ->
            trade_peers(Peer),
            give_headers(Peer, HH),
            get_headers(Peer, HH),
            give_blocks(Peer, BH),
            get_blocks(Peer, BH),
            trade_txs(Peer);
        stop ->
            ok
    end.
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
get_headers(Peer, N) ->
    Headers = remote_peer({headers, ?HeadersBatch, N}, Peer),
    headers:absorb(Headers),
    if
        length(Headers) > (?HeadersBatch div 2) -> 
            get_headers(Peer, headers:height(headers:top()));
        true -> ok
    end.
get_blocks(Peer, N) ->
    Blocks = remote_peer({blocks, ?BlocksBatch, N}, Peer),
    block_absorber:save(Blocks),
    if
        length(Blocks) > (?BlocksBatch div 2) ->
            get_blocks(Peer, block:height());
        true -> ok
    end.
trade_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).
            
