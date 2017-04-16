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
    %probabilistically select a peer. prefer lower ranked pers.
    P.
    
    
sync_all([], _) -> success;
sync_all([{IP, Port}|T], Height) ->
    spawn(fun() ->
		  sync(IP, Port, Height)
	  end),
    %spawn(download_blocks, sync, [IP, Port, Height]),
    %timer:sleep(3000),
    sync_all(T, Height).
sync(IP, Port, MyHeight) ->
    io:fwrite("syncing with peer"),
    %lower their ranking
    %peers:update_score(IP, Port, peers:initial_score()),
    S = erlang:timestamp(),
    io:fwrite("check top"),
    talk({top}, IP, Port, 
	 fun(X) ->
		 case X of
		     {error, failed_connect} -> 
			 io:fwrite("failed connect"),
			 ok;
		     {ok, TopBlock, Height}  ->
			 HH = MyHeight + 100,
			 if
			     HH < Height ->
				 {ok, Block} = talker:talk({block, HH}, IP, Port),
				 trade_blocks(IP, Port, [Block], HH);
			     true ->
				 trade_blocks(IP, Port, [TopBlock], Height),
				 get_txs(IP, Port)
				     
			 end,
			 trade_peers(IP, Port),
			 Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
			 Score = abs(Time)*(1+abs(Height - MyHeight));
		     X -> io:fwrite(X)
		 end
	 end).


    %peers:update_score(IP, Port, Score).
    %raise their ranking.
get_blocks(_, 0, _, _, L) -> L;
get_blocks(H, _, _, _, L) when H < 1 -> L;
get_blocks(Height, N, IP, Port, L) -> 
    talk({block, Height}, IP, Port,
	 fun(X) -> get_blocks(Height-1, N-1, IP, Port, [X|L])
	 end).
    
trade_blocks(_IP, _Port, L, 1) ->
    sync3(L);
    %sync3(get_blocks(1, 100, IP, Port, [])++L);
trade_blocks(IP, Port, [PrevBlock|L], Height) ->
    %"nextBlock" is from earlier in the chain than prevblock. we are walking backwards
    PrevHash = block:hash(PrevBlock),
    %{ok, PowBlock} = talker:talk({block, Height}, IP, Port),
    {PrevHash, NextHash} = block:check1(PrevBlock),
    M = block:read(PrevHash),%check if it is in our memory already.
    case M of
	empty -> 
	    {ok, NextBlock} = talker:talk({block, Height-1}, IP, Port),
	    NextHash = block:hash(NextBlock),
	    trade_blocks(IP, Port, [NextBlock|[PrevBlock|L]], Height - 1);
	_ -> 
						%download 100 blocks earlier, to handle forks.
	    L2 = get_blocks(Height-1, free_constants:fork_tolerance(), IP, Port, []),
	    case L2 of
		error -> error;
		_ ->
		    sync3(L2++L),
		    send_blocks(IP, Port, top:doit(), PrevHash, [], 0)
	    end
    end.
send_blocks(IP, Port, T, T, L, _N) -> 
    %io:fwrite("finished sending blocks"),
    send_blocks2(IP, Port, L);
send_blocks(IP, Port, TopHash, CommonHash, L, N) ->
    if
	TopHash == 0 -> send_blocks2(IP, Port, L);
	N>4000 -> send_blocks2(IP, Port, L);
	true -> 
	    BlockPlus = block:read(TopHash),
	    PrevHash = block:prev_hash(BlockPlus),
	    send_blocks(IP, Port, PrevHash, CommonHash, [BlockPlus|L], N+1)
    end.
send_blocks2(_, _, []) -> ok;
send_blocks2(IP, Port, [Block|T]) -> 
    %io:fwrite("give block !!!!!!!"),
    talker:talk({give_block, block:pow_block(Block)}, IP, Port),
    send_blocks2(IP, Port, T).
    
sync3([]) -> ok;
sync3([B|T]) -> 
    %io:fwrite("sync 3\n"),
    block_absorber:doit(B),
    sync3(T).
absorb_txs([]) -> ok;
absorb_txs([H|T]) -> 
    tx_pool_feeder:absorb(H),
    absorb_txs(T).
talk(CMD, IP, Port, F) ->
    talk(CMD, IP, Port, F, 100).
talk(_, _, _, _, 0) -> error;
talk(CMD, IP, Port, F, N) ->
    case talker:talk(CMD, IP, Port) of
	{error, failed_connect} -> talk(CMD, IP, Port, F, N-1);
	{ok, X} -> F(X);
	X -> F(X)
		       
    end.
	   
get_txs(IP, Port) ->
    %io:fwrite("download blocks get txs\n"),
    talk({txs}, IP, Port, 
	 fun(X) ->
		 absorb_txs(X),
		 {_,_,_,Mine} = tx_pool:data(),
		 talker:talk({txs, Mine}, IP, Port)
	 end).
trade_peers(IP, Port) ->
    talk({peers}, IP, Port,
	 fun(X) ->
		 MyPeers = tuples2lists(peers:all()),
		 talker:talk({peers, MyPeers}, IP, Port),
		 peers:add(X)
	 end).
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) -> 
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.
    
    
    


