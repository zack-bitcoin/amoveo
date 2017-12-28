-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, start/0, stop/0, status/0]).
-include("../spk.hrl").
init(ok) -> {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("sync died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, go) -> 
    trade_peers(Peer),
    MyTop = headers:top(),
    TheirTop = remote_peer({header}, Peer), 
    MyBlockHeight = block:height(),
    TheirTopHeight = TheirTop#header.height,
    if
        not(MyTop == TheirTop) ->
            CommonHash = get_headers(Peer),
            {ok, TBH} = headers:read(block:hash(block:top())),
            MD = TBH#header.accumulative_difficulty,
            TD = TheirTop#header.accumulative_difficulty,
            if
                TD < MD -> give_blocks(Peer, CommonHash);
                true ->
                    CommonBlockHeight = common_block_height(CommonHash),
                    get_blocks(Peer, CommonBlockHeight)
            end;
        MyBlockHeight < TheirTopHeight ->
            {ok, FT} = application:get_env(ae_core, fork_tolerance),
            get_blocks(Peer, max(0, MyBlockHeight - FT));
        true -> ok
    end,
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
    gen_server:cast(?MODULE, start),
    doit2(P).
doit2([]) ->
    ok;
doit2([Peer|T]) ->
    %check if our version is the same.
    gen_server:cast(?MODULE, {main, Peer}),
    doit2(T).
blocks(CommonHash, Block) ->
    BH = block:hash(Block),
    if 
        BH == CommonHash -> [];
        true ->
            PrevBlock = block:get_by_hash(block:prev_hash(Block)),
            if
                Block == empty -> 
                    blocks(CommonHash, PrevBlock);
                true ->
                    [Block|blocks(CommonHash, PrevBlock)]
            end
    end.
give_blocks(Peer, CommonHash) -> 
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
            %spawn(fun() -> do_send_blocks(Peer, Blocks) end);
            do_send_blocks(Peer, Blocks),
            NewCommonHash = block:hash(hd(lists:reverse(Blocks))),
            give_blocks(Peer, NewCommonHash);
        true -> 
            io:fwrite("finished sending blocks"),
            false
    end.
do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
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
            Header = headers:read(CommonHash),
            PrevCommonHash = Header#header.prev_hash,
            common_block_height(PrevCommonHash);
        B -> block:height(B)
    end.
get_blocks(Peer, N) ->
    {ok, BB} = application:get_env(ae_core, download_blocks_batch),
    Blocks = remote_peer({blocks, BB, N}, Peer),
    case Blocks of
        {error, _} -> get_blocks(Peer, N);
        _ ->
            block_absorber:save(Blocks),
            if
                length(Blocks) > (BB div 2) ->
                    get_blocks(Peer, N+BB);
                true -> ok
            end
    end.
trade_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).
            
