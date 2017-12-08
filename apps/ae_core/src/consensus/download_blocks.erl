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
    %io:fwrite("download blocks sync\n"),
    RemoteTop = remote_peer({top}, Peer),
	do_sync(RemoteTop, MyHeight, Peer).

do_sync(error, _, _) ->
    ok;
do_sync({ok, TopBlock, Height} = _RemoteTopResult, MyHeight, Peer) ->
    {ok, DBB} = application:get_env(ae_core, download_blocks_batch),
    JumpHeight = MyHeight + DBB,
    if
        JumpHeight < Height ->
	    true = JumpHeight > 0,
            BlockAtJumpHeight = remote_peer({block, JumpHeight}, Peer),
            trade_blocks(Peer, [BlockAtJumpHeight], JumpHeight);
        Height < MyHeight ->
            {ok, PBB} = application:get_env(ae_core, push_blocks_batch),
            B = block:get_by_height(max(0, Height - PBB)),
            CommonHash = block:hash(B),
            send_blocks(Peer, block:hash(block:top()), CommonHash, [], 0);
        true ->
            trade_blocks(Peer, [TopBlock], Height)
    end,
    get_txs(Peer),
    trade_peers(Peer).
case_sync(L, Peer) ->
    B = length(L) > 1,
    if
        B -> sync(Peer, block:height(block:top()));
            %ok;
        true -> ok
    end.
trade_blocks(Peer, L, 0) ->
    io:fwrite("downloader blocks trade blocks 0 absorbing blocks"),
    Genesis = block:get_by_height(0),
    GH = block:hash(Genesis),
    %send_blocks(Peer, block:hash(block:top()), GH, [], 0),
    block_absorber:enqueue(L),
    case_sync(L, Peer);
trade_blocks(Peer, [PrevBlock|PBT] = CurrentBlocks, Height) ->
    %io:fwrite("trade_blocks: \n"),
    %io:fwrite(packer:pack({prev_block, PrevBlock, PBT, block:block_to_header(PrevBlock)})),
    %io:fwrite("\n"),
    PrevHash = block:hash(PrevBlock),
    NextHash = block:prev_hash(PrevBlock),
    OurChainAtPrevHash = block:get_by_hash(NextHash),
    Height = block:height(PrevBlock),
    case OurChainAtPrevHash of
        empty ->
    	    %io:fwrite("we don't have a parent for this block ~p", [NextHash]),
	    true = Height > 1,
            RemoteBlockThatWeMiss = remote_peer({block, Height-1}, Peer),
            trade_blocks(Peer, [RemoteBlockThatWeMiss|CurrentBlocks], Height-1);
        _ ->
            block_absorber:save(CurrentBlocks),
            case_sync(CurrentBlocks, Peer),
    	    %io:fwrite("about to send blocks"),
    	    H = headers:top(),
    	    case headers:height(H) of
                0 -> ok;
                _ ->
        		    %send_blocks(Peer, block:hash(block:top()), PrevHash, [], 0)
                    ok
    	    end
    end.

send_blocks(Peer, Hash, Hash, Blocks, _N) ->
    io:fwrite("send blocks 1 (OurTopHash = CommonHash)"),
    send_blocks_external(Peer, Blocks);
send_blocks(Peer, OurTopHash, CommonHash, Blocks, N) ->
    io:fwrite("send blocks 2 ~p", [integer_to_list(N)]),
    GH = block:hash(block:get_by_height(0)),
    if
        OurTopHash == GH -> send_blocks_external(Peer, Blocks);
        true ->
            BlockPlus = block:get_by_hash(OurTopHash),
            PrevHash = block:prev_hash(BlockPlus),
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|Blocks], N+1)
    end.

send_blocks_external(Peer, Blocks) ->
    io:fwrite("send_blocks_external: ~p" ,[packer:pack({sending_blocks, Blocks})]),
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
    %io:fwrite("transaction is "),
    %io:fwrite(packer:pack(Transaction)),
    %io:fwrite("\n"),
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
