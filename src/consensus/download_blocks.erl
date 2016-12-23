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
    {ok, TopHash, Height} = talker:talk(IP, Port, top),
    trade_blocks(IP, Port, [TopHash], Height),
    get_txs(IP, Port),
    trade_peers(IP, Port),
    Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
    Score = abs(Time)*(1+abs(Height - MyHeight)),
    peers:update_score(IP, Port, Score).
    %raise their ranking.
trade_blocks(IP, Port, [PrevBlock|L], Height) ->
    
    PrevHash = block:hash(pow:data(PrevBlock)),
    {ok, PowBlock} = talker:talk(IP, Port, {block, Height}),
    Block = pow:data(PowBlock),
    {Hash, PrevHash} = block:check1(Block),
    M = block:read(Hash),
    case M of
	empty -> trade_blocks(IP, Port, [PowBlock|[PrevBlock|L]], Height - 1);
	_ -> sync3([PrevBlock|L])
    end.
sync3([]) -> ok;
sync3([B|T]) -> 
    block:absorb(B),
    sync3(T).
absorb_txs([]) -> ok;
absorb_txs([H|T]) -> 
    tx_pool_feeder:absorb(H),
    absorb_txs(T).
get_txs(IP, Port) ->
    Them = talker:talk(IP, Port, {txs}),
    absorb_txs(Them),
    {_,_,_,Mine} = tx_pool:data(),
    talker:talk(IP, Port, {txs, Mine}).
trade_peers(IP, Port) ->
    {ok, Peers} = talker:talk(IP, Port, {peers}),
    MyPeers = peers:all(),
    talker:talk(IP, Port, {peers, MyPeers}),
    peers:add(Peers).
    
    
    


