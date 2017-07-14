-module(download_blocks).
-export([sync_all/2, sync/2, do_send_blocks/2]).

%% TODO: introduce ranking mechanism. It may be based on initial top request.
%%       The quickest and the longest chains are promoted
%%       There is no point of quering thousands of peers and updating our chain at once from all of them

sync_all([], _) -> ok;
sync_all([Peer|T], Height) ->
    spawn(fun() ->
		  sync(Peer, Height)
	  end),
    sync_all(T, Height).

sync(Peer, MyHeight) ->
    RemoteTop = remote_peer({top}, Peer),
	do_sync(RemoteTop, MyHeight, Peer).

do_sync(error, _, _) ->
    ok;
do_sync({ok, TopBlock, Height} = _RemoteTopResult, MyHeight, Peer) ->
    {ok, DBB} = application:get_env(ae_core, download_blocks_batch),
    JumpHeight = MyHeight + DBB,
    if
        JumpHeight < Height ->
            io:fwrite("JumpHeight < Height\n"),
            BlockAtJumpHeight = remote_peer({block, JumpHeight}, Peer),
            trade_blocks(Peer, [BlockAtJumpHeight], JumpHeight);
        true ->
            trade_blocks(Peer, [TopBlock], Height)
    end,
    get_txs(Peer),
    trade_peers(Peer).

trade_blocks(_, L, 1) ->
	block_absorber:enqueue(L);
trade_blocks(Peer, [PrevBlock|PBT] = CurrentBlocks, Height) ->
    PrevHash = block:hash(PrevBlock),
    {PrevHash, NextHash} = block:check1(PrevBlock),
	OurChainAtPrevHash = block:read(PrevHash),
	case OurChainAtPrevHash of
        empty ->
            RemoteBlockThatWeMiss = remote_peer({block, Height-1}, Peer),
            NextHash = block:hash(RemoteBlockThatWeMiss),
            trade_blocks(Peer, [RemoteBlockThatWeMiss|CurrentBlocks], Height-1);
        _ ->
            block_absorber:enqueue(PBT),
            send_blocks(Peer, top:doit(), PrevHash, [], 0)
    end.

send_blocks(Peer, Hash, Hash, Blocks, _N) ->
    send_blocks_external(Peer, Blocks);
send_blocks(Peer, OurTopHash, CommonHash, Blocks, N) ->
    if
        OurTopHash == 0 -> send_blocks_external(Peer, Blocks);
        true ->
            BlockPlus = block:read(OurTopHash),
            PrevHash = block:prev_hash(BlockPlus),
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|Blocks], N+1)
    end.

send_blocks_external(Peer, Blocks) ->
    spawn(?MODULE, do_send_blocks, [Peer, Blocks]).

do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
    remote_peer({give_block, Block}, Peer),
    timer:sleep(20),
	do_send_blocks(Peer, T).

get_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).

trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
