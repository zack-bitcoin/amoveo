-module(download_blocks).
-export([sync_cron/0, sync_cron/1, sync_all/2, sync/3, absorb_txs/1]).

sync_cron() -> sync_cron(30000).
sync_cron(N) -> %30000 is 30 second.
    timer:sleep(N),
    Height = block:height(block:read(top:doit())),
    P = peers:all(),
    P2 = rank_filter(P),
    sync_all(P2, Height),
    sync_cron(N).

rank_filter(P) ->
    %probabilistically remove the higher-ranked peers,
    P.
    
    
sync_all([], _) -> success;
sync_all([{IP, Port}|T], Height) ->
    spawn(download_blocks, sync, [IP, Port, Height]),
    sync_all(T, Height).
sync(IP, Port, MyHeight) ->
    %lower their ranking
    peers:update_score(IP, Port, peers:initial_score()),
    S = erlang:timestamp(),
    {ok, TopBlock, Height} = talker:talk({top}, IP, Port),
    trade_blocks(IP, Port, [TopBlock], Height),
    get_txs(IP, Port),
    trade_peers(IP, Port),
    Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
    Score = abs(Time)*(1+abs(Height - MyHeight)),
    peers:update_score(IP, Port, Score).
    %raise their ranking.
trade_blocks(IP, Port, [PrevBlock|L], Height) ->

    PrevHash = block:hash(PrevBlock),
    io:fwrite("in trade blocks "),
    io:fwrite(integer_to_list(Height)),
    io:fwrite("\n"),
    {ok, PowBlock} = talker:talk({block, Height}, IP, Port),
    {Hash, PrevHash} = block:check1(PowBlock),
    M = block:read(Hash),
    case M of
	empty -> trade_blocks(IP, Port, [PowBlock|[PrevBlock|L]], Height - 1);
	_ -> 
	    sync3([PrevBlock|L]),
	    send_blocks(IP, Port, top:doit(), block:hash(PrevBlock))
    end.
send_blocks(_, _, T, T) -> ok;
send_blocks(IP, Port, TopHash, CommonHash) ->
    BlockPlus = block:read(TopHash),
    Block = block:block(BlockPlus),
    talker:talk({give_block, Block}, IP, Port),
    PrevHash = block:prev_hash(BlockPlus),
    send_blocks(IP, Port, PrevHash, CommonHash).
    
sync3([]) -> ok;
sync3([B|T]) -> 
    block:absorb(B),
    sync3(T).
absorb_txs([]) -> ok;
absorb_txs([H|T]) -> 
    tx_pool_feeder:absorb(H),
    absorb_txs(T).
get_txs(IP, Port) ->
    Them = talker:talk({txs}, IP, Port),
    absorb_txs(Them),
    {_,_,_,Mine} = tx_pool:data(),
    talker:talk({txs, Mine}, IP, Port).
trade_peers(IP, Port) ->
    {ok, Peers} = talker:talk({peers}, IP, Port),
    MyPeers = peers:all(),
    talker:talk({peers, MyPeers}, IP, Port),
    peers:add(Peers).
    
    
    


