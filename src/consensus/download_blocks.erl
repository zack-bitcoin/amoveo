-module(download_blocks).
-export([sync_cron/0, sync_cron/1, sync_all/2, 
	 sync/3, absorb_txs/1, tuples2lists/1]).

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
    %peers:update_score(IP, Port, peers:initial_score()),
    S = erlang:timestamp(),
    {ok, TopBlock, Height} = talker:talk({top}, IP, Port),
    trade_blocks(IP, Port, [TopBlock], Height),
    get_txs(IP, Port),
    trade_peers(IP, Port),
    Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
    Score = abs(Time)*(1+abs(Height - MyHeight)).
    %peers:update_score(IP, Port, Score).
    %raise their ranking.
trade_blocks(IP, Port, [PrevBlock|L], Height) ->
    %"nextBlock" is from earlier in the chain than prevblock. we are walking backwards
    PrevHash = block:hash(PrevBlock),
    %{ok, PowBlock} = talker:talk({block, Height}, IP, Port),
    {PrevHash, NextHash} = block:check1(PrevBlock),
    M = block:read(PrevHash),%check if it is in our memory already.
    io:fwrite("trade blocks\n"),
    case M of
	empty -> 
	    io:fwrite("was empty\n"),
	    {ok, NextBlock} = talker:talk({block, Height-1}, IP, Port),
	    NextHash = block:hash(NextBlock),
	    trade_blocks(IP, Port, [NextBlock|[PrevBlock|L]], Height - 1);
	_ -> 
	    sync3(L),
	    send_blocks(IP, Port, top:doit(), PrevHash, [])
    end.
send_blocks(IP, Port, T, T, L) -> 
    send_blocks2(IP, Port, L);
send_blocks(IP, Port, TopHash, CommonHash, L) ->
    BlockPlus = block:read(TopHash),
    PrevHash = block:prev_hash(BlockPlus),
    send_blocks(IP, Port, PrevHash, CommonHash, [BlockPlus|L]).
send_blocks2(_, _, []) -> ok;
send_blocks2(IP, Port, [Block|T]) -> 
    talker:talk({give_block, Block}, IP, Port),
    send_blocks2(IP, Port, T).
    
sync3([]) -> ok;
sync3([B|T]) -> 
    io:fwrite("sync 3\n"),
    block:absorb(B),
    sync3(T).
absorb_txs([]) -> ok;
absorb_txs([H|T]) -> 
    tx_pool_feeder:absorb(H),
    absorb_txs(T).
get_txs(IP, Port) ->
    {ok, Them} = talker:talk({txs}, IP, Port),
    absorb_txs(Them),
    {_,_,_,Mine} = tx_pool:data(),
    talker:talk({txs, Mine}, IP, Port).
trade_peers(IP, Port) ->
    {ok, Peers} = talker:talk({peers}, IP, Port),
    MyPeers = tuples2lists(peers:all()),
    talker:talk({peers, MyPeers}, IP, Port),
    peers:add(Peers).
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) -> 
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.
    
    
    


