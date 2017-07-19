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

    
%new_sync(Peer, MyHeight) ->
%    if
	%MyHeight + 100 < Height -> get_all_headers(Peer);
%	MyHeight < Height -> get_headers(Peer, Height, headers:top());
%	true -> give_headers(Peer, headers:top(), Height - free_constants:fork_tolerance())
%    end,
%    download_blocks(Peer, headers:top()).
%get_headers(Peer, PeerHeight, TopHeader) ->
    %first walk backward fork_tolerance() headers, then download all the headers from there until peerHeight

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
	    true = JumpHeight > 0,
            BlockAtJumpHeight = remote_peer({block, JumpHeight}, Peer),
            trade_blocks(Peer, [BlockAtJumpHeight], JumpHeight);
        true ->
            trade_blocks(Peer, [TopBlock], Height)
    end,
    get_txs(Peer),
    trade_peers(Peer).

trade_blocks(Peer, L, 0) ->
    io:fwrite("downloader blocks trade blocks 0 absorbing blocks\n"),
    block_absorber:enqueue(L),
    Genesis = block:read_int(0),
    GH = block:hash(Genesis),
    send_blocks(Peer, block:hash(block:top()), GH, [], 0);
trade_blocks(Peer, [PrevBlock|PBT] = CurrentBlocks, Height) ->
    io:fwrite(packer:pack({prev_block, PrevBlock, PBT})),
    io:fwrite("\n"),
    PrevHash = block:hash(PrevBlock),
    NextHash = block:prev_hash(PrevBlock),
    %{PrevHash, NextHash} = block:check1(PrevBlock),
    %OurChainAtPrevHash = block:read(PrevHash),
    OurChainAtPrevHash = block:read(NextHash),
    Height = block:height(PrevBlock),
    case OurChainAtPrevHash of
        empty ->
	    io:fwrite("we don't have a parent for this block\n"),
	    true = Height > 1,
            RemoteBlockThatWeMiss = remote_peer({block, Height-1}, Peer),
	    io:fwrite(packer:pack({got_block, RemoteBlockThatWeMiss})),
            %NextHash = block:hash(RemoteBlockThatWeMiss),
            trade_blocks(Peer, [RemoteBlockThatWeMiss|CurrentBlocks], Height-1);
        _ ->
	    io:fwrite("we have a parent for this block\n"),
            block_absorber:save(CurrentBlocks),
	    io:fwrite("about to send blocks\n"),
	    H = headers:top(),
	    case headers:height(H) of
		0 -> ok;
		_ ->
		    send_blocks(Peer, block:hash(block:top()), PrevHash, [], 0)
	    end
    end.

send_blocks(Peer, Hash, Hash, Blocks, _N) ->
    io:fwrite("send blocks 1\n"),
    send_blocks_external(Peer, Blocks);
send_blocks(Peer, OurTopHash, CommonHash, Blocks, N) ->
    io:fwrite("send blocks 2 " ++ integer_to_list(N) ++ " \n"),
    GH = block:hash(block:read_int(0)),
    if
        OurTopHash == GH -> send_blocks_external(Peer, Blocks);
        true ->
            BlockPlus = block:read(OurTopHash),
            PrevHash = block:prev_hash(BlockPlus),
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|Blocks], N+1)
    end.

send_blocks_external(Peer, Blocks) ->
    io:fwrite(packer:pack({sending_blocks, Blocks})),
    io:fwrite("\n"),
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
