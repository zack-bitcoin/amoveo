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
            lager:debug("JumpHeight < Height"),
	    true = JumpHeight > 0,
            BlockAtJumpHeight = remote_peer({block, JumpHeight}, Peer),
            trade_blocks(Peer, [BlockAtJumpHeight], JumpHeight);
        true ->
            trade_blocks(Peer, [TopBlock], Height)
    end,
    get_txs(Peer),
    trade_peers(Peer).

trade_blocks(Peer, L, 0) ->
    lager:debug("downloader blocks trade blocks 0 absorbing blocks"),
    %block_absorber:enqueue(L),
    block_absorber:save(L),
    Genesis = block:read_int(0),
    GH = block:hash(Genesis),
    send_blocks(Peer, block:hash(block:top()), GH, [], 0);
trade_blocks(Peer, [PrevBlock|PBT] = CurrentBlocks, Height) ->
    lager:debug("trade_blocks: ~p", [packer:pack({prev_block, PrevBlock, PBT})]),
    PrevHash = block:hash(PrevBlock),
    NextHash = block:prev_hash(PrevBlock),
    OurChainAtPrevHash = block:read(NextHash),
    Height = block:height(PrevBlock),
    case OurChainAtPrevHash of
        empty ->
    	    lager:debug("we don't have a parent for this block ~p", [OurChainAtPrevHash]),
	    true = Height > 1,
            RemoteBlockThatWeMiss = remote_peer({block, Height-1}, Peer),
    	    lager:debug("trade_blocks: height > 1 ~p", [packer:pack({got_block, RemoteBlockThatWeMiss})]),
            trade_blocks(Peer, [RemoteBlockThatWeMiss|CurrentBlocks], Height-1);
        _ ->
    	    lager:debug("we have a parent for this block ~p", [OurChainAtPrevHash]),
            block_absorber:save(CurrentBlocks),
    	    lager:debug("about to send blocks"),
    	    H = headers:top(),
    	    case headers:height(H) of
        		0 -> ok;
        		_ ->
        		    send_blocks(Peer, block:hash(block:top()), PrevHash, [], 0)
    	    end
    end.

send_blocks(Peer, Hash, Hash, Blocks, _N) ->
    lager:debug("send blocks 1 (OurTopHash = CommonHash)"),
    send_blocks_external(Peer, Blocks);
send_blocks(Peer, OurTopHash, CommonHash, Blocks, N) ->
    lager:debug("send blocks 2 ~p", [integer_to_list(N)]),
    GH = block:hash(block:read_int(0)),
    if
        OurTopHash == GH -> send_blocks_external(Peer, Blocks);
        true ->
            BlockPlus = block:read(OurTopHash),
            PrevHash = block:prev_hash(BlockPlus),
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|Blocks], N+1)
    end.

send_blocks_external(Peer, Blocks) ->
    lager:debug("send_blocks_external: ~p" ,[packer:pack({sending_blocks, Blocks})]),
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
