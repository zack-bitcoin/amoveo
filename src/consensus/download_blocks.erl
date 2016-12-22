-module(download_blocks).
-export([sync_cron/0, sync_cron/1, sync_all/0, sync/2, absorb_txs/1]).

sync_cron() -> sync_cron(30000).
sync_cron(N) -> %30000 is 30 second.
    timer:sleep(N),
    spawn(download_blocks, sync_all, []),
    sync_cron(N).
    
sync_all() ->
    Peers = peers:all(),
    sync_all(Peers).
sync_all([]) -> success;
sync_all([{IP, Port}|T]) ->
    spawn(download_blocks, sync, [IP, Port]),
    sync_all(T).
sync(IP, Port) ->
    {ok, TopHash, Height} = talker:talk(IP, Port, top),
    sync2(IP, Port, [TopHash], Height),
    get_txs(IP, Port).
sync2(IP, Port, [PrevBlock|L], Height) ->
    
    PrevHash = block:hash(pow:data(PrevBlock)),
    {ok, PowBlock} = talker:talk(IP, Port, {block, Height}),
    Block = pow:data(PowBlock),
    {Hash, PrevHash} = block:check1(Block),
    M = block:read(Hash),
    case M of
	empty -> sync2(IP, Port, [PowBlock|[PrevBlock|L]], Height - 1);
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


