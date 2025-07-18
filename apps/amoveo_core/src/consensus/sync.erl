-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	 start/1, start/0, stop/0, status/0, cron/0,
	 give_blocks/3, push_new_block/1, remote_peer/2,
	 get_headers/1, trade_txs/1, force_push_blocks/1,
         get_headers/0, remove_self/1,
	 trade_peers/1, cron/0, shuffle/1,
         low_to_high/1, dict_to_blocks/2]).
-include("../records.hrl").
-define(HeadersBatch, application:get_env(amoveo_core, headers_batch)).
-define(tries, 300).%20 tries per second. 
-define(Many, 1).%how many to sync with per calling `sync:start()`
%so if this is 400, that means we have 20 seconds to download download_block_batch * download_block_many blocks
-define(download_ahead, application:get_env(amoveo_core, get_block_buffer)).
init(ok) -> 
    {ok, start}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("sync died!\n"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(start, _) -> {noreply, go};
%handle_cast(stop, _) -> {noreply, stop};
handle_cast({main, Peer}, _) -> 
    io:fwrite("sync main \n"),
    BL = case application:get_env(amoveo_core, kind) of
	     {ok, "production"} ->%don't blacklist peers in test mode.
		 blacklist_peer:check(Peer);
	     _ -> false
	 end,
    S = status(),
    if 
	BL -> 
            io:fwrite("peer is blacklisted\n"),
            ok;
	Peer == error -> 
            io:fwrite("peer is error\n"),
            ok;
	not(S == go) -> 
	    io:fwrite("not syncing with this peer now "),
	    io:fwrite(packer:pack(Peer)),
	    io:fwrite("\n"),
	    ok;
	true ->
	    io:fwrite("syncing with this peer now "),
	    io:fwrite(packer:pack(Peer)),
	    io:fwrite("\n"),
	    sync_peer(Peer),
	    case application:get_env(amoveo_core, kind) of
		{ok, "production"} ->
		    case sync_mode:check() of
			quick ->
			    timer:sleep(5000);
			normal -> ok
		    end;
		    %timer:sleep(1000);
		_ -> ok
	    end
    end,
    %spawn(fun() -> start() end),
    {noreply, []};
handle_cast(_, X) -> {noreply, X}.
%handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.
status() -> sync_kill:status().
stop() -> sync_kill:stop().
start() -> start(peers:all()).
start(P) when not(is_list(P)) -> start([P]);
start(P) ->
    %io:fwrite("sync start\n"),
    sync_kill:start(),
    H = api:height(),
    %{ok, Reverse} = application:get_env(amoveo_core, reverse_syncing),
    if
        (H == 0) ->
            spawn(fun() ->
                          %this is so ugly. it can't be the right way to do it.
                          timer:sleep(500),
                          start(P)
                  end);
        true ->
            spawn(fun() ->
                          doit2(P)
                  end)
    end.
doit2([]) -> ok;
doit2(L0) ->
    L = remove_self(L0),
    BH = block:height(),
    HH = api:height(),
    if
	length(L) == 0 ->
	    io:fwrite("no one to sync with\n"),
	    ok;
	BH < HH ->
	    gen_server:cast(?MODULE, {main, hd(shuffle(L))});
	true -> 
	    io:fwrite("nothing to sync\n"),
	    ok
    end.
blocks(CommonHash, Block) ->
    BH = block:hash(Block),
    if 
        BH == CommonHash -> [];
        true ->
            PrevBlock = block:get_by_hash(Block#block.prev_hash),
            [Block|blocks(CommonHash, PrevBlock)]
    end.
give_blocks(Peer, _, _) ->
    H = headers:top(),
    HH = block:hash(H),
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    M = min(H#header.height, FT),
    Headers = list_headers([H], M),
    push_new_block_helper(0,0,[Peer],HH,Headers).
    %remote_peer({give_block, [block:top()]}, Peer).
   
force_push_blocks(Peer) -> 
    TheirBlockHeight = remote_peer({height}, Peer),
    CH = block:hash(block:get_by_height(TheirBlockHeight)),
    give_blocks_old(Peer, CH, TheirBlockHeight).
    
give_blocks_old(Peer, CommonHash, TheirBlockHeight) -> 
    %Common hash defaults to genesis, which makes blocks/2 super slow. This all needs to be redone.
    %io:fwrite("give blocks\n"),
    go = sync_kill:status(),
    {ok, DBB} = application:get_env(amoveo_core, push_blocks_batch),
    H = min(block:height(), max(0, TheirBlockHeight + DBB - 1)),
    Blocks0 = blocks(CommonHash, block:get_by_height(H)),
    Blocks = lists:reverse(Blocks0),
    if 
        length(Blocks) > 0 ->
	    SendHeight = (hd(Blocks0))#block.height,
            remote_peer({give_block, Blocks}, Peer),
	    timer:sleep(2000),
	    TheirBlockHeight2 = remote_peer({height}, Peer),
	    if
		(TheirBlockHeight2 > TheirBlockHeight) or (TheirBlockHeight > SendHeight) ->
	    
		    NewCommonHash = block:hash(hd(Blocks0)),
		    give_blocks(Peer, NewCommonHash, TheirBlockHeight2);
		true -> 
		    %we should remove them from the list of peers.
		    peers:remove(Peer),
		    %io:fwrite("they are not accepting our blocks."),
		    ok
	    end;
        true -> 
            %io:fwrite("finished sending blocks"),
            false
    end.

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
	bad_peer -> %remove from peers, add to a black list for next N minutes.
	    {{_,_,_,_},_} = Peer,
	    %io:fwrite("removing peer "),
	    %io:fwrite(packer:pack(Peer)),
	    %io:fwrite("\n"),
	    %io:fwrite("command was "),
	    %io:fwrite(element(1, Transaction)),
	    %io:fwrite("\n"),
	    blacklist_peer:add(Peer),
	    peers:remove(Peer),
	    error;
        Return1 -> Return1
    end.
trade_peers(Peer) ->
    %io:fwrite("trade peers\n"),
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = amoveo_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).
get_headers() -> 
    Peers = shuffle(remove_self(peers:all())),
    spawn(fun() ->
                  get_headers(hd(Peers))
          end).
get_headers(Peer) -> 
    N = (headers:top())#header.height,
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    Start = max(0, N - FT), 
    %Start = max(0, N+1), 
    get_headers2(Peer, Start).
get_headers2(Peer, N) ->%get_headers2 only gets called more than once if fork_tolerance is bigger than HeadersBatch.
    {ok, HB} = ?HeadersBatch,
    io:fwrite("get headers 2 inputs " ++ integer_to_list(HB) ++ " " ++ integer_to_list(N) ++ "\n"),
    {{P1, P2, P3, P4}, _} = Peer,
    io:fwrite("peer is " ++ integer_to_list(P1) ++ "." ++ integer_to_list(P2) ++ "." ++ integer_to_list(P3) ++ "." ++ integer_to_list(P4) ++ " \n"),
    Headers = remote_peer({headers, HB, N}, Peer),
    case Headers of
	error -> 
            io:fwrite("get headers error 1\n"),
            error;
	bad_peer -> 
            io:fwrite("get headers error 2\n"),
            error;
	[_|_] ->
            io:fwrite("absorbing " ++ integer_to_list(length(Headers)) ++ " headers, starting at height " ++ integer_to_list((hd(Headers))#header.height)),
	    CommonHash = headers:absorb(Headers),
	    L = length(Headers),
            %io:fwrite("headers length"),
            %io:fwrite(integer_to_list(L)),
            %io:fwrite("\n"),
            %io:fwrite("headers requested height start "),
            %io:fwrite(integer_to_list(N)),
            %io:fwrite("\n"),
	    case CommonHash of
		<<>> -> 
		    if 
			(L+5) > HB -> get_headers2(Peer, N+HB-1);
			true -> error%fork is bigger than fork_tolerance
		    end;
		_ -> spawn(fun() -> get_headers3(Peer, N+HB-1) end),
						%Once we know the CommonHash, then we are ready to start downloading blocks. We can download the rest of the headers concurrently while blocks are downloading.
		     CommonHash
	    end;
        [] -> ok;
        _ ->
            io:fwrite("headers not a list"),
            io:fwrite(Headers)
    end.
get_headers3(Peer, N) ->
    AH = api:height(),
    {ok, HB} = ?HeadersBatch,
    %true = (N > AH - HB - 1),
    Headers = remote_peer({headers, HB, N}, Peer),
    AH2 = api:height(),
    true = (N > AH2 - HB - 1),
    headers:absorb(Headers),
    if
        length(Headers) > (HB div 2) -> 
            get_headers3(Peer, N+HB-1);
        true -> ok
    end.

ip_url_format({{A, B, C, D}, _}) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

stream_get_blocks(Peer, N, TheirBlockHeight) ->
    io:fwrite("stream get blocks\n"),
    true = N < TheirBlockHeight,
    %PM = packer:pack({N, TheirBlockHeight}),
    Url = "http://" ++ ip_url_format(Peer) ++ ":8080/blocks/" ++ integer_to_list(N) ++ "_" ++ integer_to_list(TheirBlockHeight),
    io:fwrite(Url),
    io:fwrite("\n"),
    httpc:request(
      get,
      {list_to_binary(Url), []},
      [],
      [{timeout, 2000}, 
       {stream, self},
       {sync, false}]),
    receive
        {http, {_Ref, stream_start, [{"date", _}, {_, "chunked"}, {"server", "Cowboy"}]}} ->
            blocks_process_stream(<<>>, block:top());
        {http, {_Ref, {{"HTTP/1.1",404,"Not Found"},[_,_,_],_}}} ->
            io:fwrite("stream returned 404 - Not Found"),
            spawn(fun() ->
                          new_get_blocks(Peer, N, TheirBlockHeight, ?tries)
                  end),
            ok;
%i{http,{#Ref<0.3209288097.2305294337.37580>,{{"HTTP/1.1",404,"Not Found"},[{"date","Tue, 03 Jun 2025 09:55:56 GMT"},{"server","Cowboy"},{"content-length","0"}],<<>>}}}
        X ->
            io:fwrite("unhandled stream header\n"),
            io:fwrite(X),
            ok
    after 1000 ->
            io:fwrite("failed to start receiving stream\n"),
            ok
    end.
blocks_process_stream(Data0, MyTopBlock) ->
    receive
        {http, {_Ref, stream, Data}} ->
            %io:fwrite("process stream, more data\n"),
            Data2 = <<Data0/binary, Data/binary>>,
            {Data3, NewTop} = try_process_block(Data2, MyTopBlock),
            blocks_process_stream(Data3, NewTop);
        {http, {_Ref, stream_end, _}} -> <<>>;
        X -> 
            io:fwrite("unhandled stream body"),
            io:fwrite(X)
    after 2000 -> 
            io:fwrite("cut off mid stream\n"),
            ok
    end.
   
try_process_block(FullData = <<Size:64, Data/binary>>, MyTopBlock) -> 
    go = sync_kill:status(),
    {ok, MTV} = application:get_env(
                  amoveo_core, minimum_to_verify),
    {ok, TestMode} = application:get_env(
                       amoveo_core, test_mode),
    Height = MyTopBlock#block.height,
    F52 = forks:get(52),
    if
        ((Height > F52) and ((Height rem 20) == 0)) or 
        ((Height rem 200) == 0) ->
            {_, T1, T2} = erlang:timestamp(),
            io:fwrite("absorb height " ++
                          integer_to_list(Height) ++
                          " time: " ++
                          integer_to_list(T1) ++
                          " " ++
                          integer_to_list(T2) ++
                          "\n");
        true -> ok
    end,
    S = size(Data),
    if
        (S >= Size) -> 
            %we got another block
            %io:fwrite("got a block in try_process_block\n"),
            <<Blockx:(Size*8), Rest/binary>> = Data,
            Block = block_db3:uncompress(<<Blockx:(Size*8)>>),
            Block2 = process_block_sequential(Block, MyTopBlock), 

%            BH = block:hash(Block),
%            true = (MyTopBlock#block.height + 1 == Block#block.height),
%            #block{
%               height = Height2
%              } = Block,
%            if
%                (Height2 == F52) -> 
                    %hardcode verkle update block hash, so we don't have to check blocks from before this point.
%                    true = BH == <<185,59,27,106,59,121,158,59,113,186,179,200,161,70,238, 229,35,162,169,31,168,11,112,101,135,49,179,32,111,90,87,192>>;
%                true -> ok
%            end,
%            Block2 = if
%                         (TestMode or ((Height2 > F52) and (Height2 > MTV))) ->
                   %after verkle update
%                             X = block:check0(Block),
%                             {true, Block3} = block:check2(MyTopBlock, Block#block{trees = X}),
%                             Block3;
%                         true ->
%                             Block
%                     end,
            %check every block matches it's header, even from before the verkle update height
%            {ok, Header} = headers:read(BH),
%            true = (Header#header.height == 
%                        Block#block.height),
%            headers:absorb_with_block([Header]),
%            block_db3:write(Block2),
            {Rest, Block2};
        true ->
            %io:fwrite("less than entire block " ++ integer_to_list(size(Data)) ++ " " ++ integer_to_list(Size) ++ "\n"),
            {FullData, MyTopBlock}
    end;
try_process_block(Small, Top) -> 
    {Small, Top}.
        

new_get_blocks(Peer, N, TheirBlockHeight, _) when (N > TheirBlockHeight) ->
    io:fwrite("done syncing");
new_get_blocks(Peer, N, TheirBlockHeight, 0) ->
    io:fwrite("gave up syncing");
new_get_blocks(Peer, N, TheirBlockHeight, Tries) ->
    io:fwrite("sync new get blocks\n"),
    go = sync_kill:status(),
    Height = block:height(),
    AHeight = api:height(),
    {ok, DA} = ?download_ahead,
    if
	Height == AHeight -> 
            io:fwrite("done syncing\n"),
            ok;%done syncing
        (N > (1 + Height + DA)) ->
            io:fwrite("wait to get more blocks\n"),
            timer:sleep(100),
            new_get_blocks(Peer, N, TheirBlockHeight, Tries - 1);
        true ->
            spawn(fun() ->
                          new_get_blocks2(TheirBlockHeight, N, Peer, 5)
                  end)
                %new_get_blocks(Peer, N, TheirBlockHeight, ?tries)
    end.
    
new_get_blocks2(_TheirBlockHeight, _N, _Peer, 0) ->
    ok;
new_get_blocks2(TheirBlockHeight, N, Peer, Tries) ->
    %io:fwrite("new get blocks 2 request blocks "),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite("\n"),
    BH0 = block:height(),
    true = N < TheirBlockHeight + 1,
    go = sync_kill:status(),
    %N2 = min(BH0, N),
    Blocks = talker:talk({blocks, 50, N}, Peer),
    BH2 = block:height(),
    go = sync_kill:status(),
    case Blocks of
	{error, _} -> 
	    io:fwrite("get blocks 2 failed connect error\n"),
	    %io:fwrite(packer:pack([N, Peer, Tries])),
	    io:fwrite("\n"),
	    timer:sleep(2000),
	    new_get_blocks2(TheirBlockHeight, N, Peer, Tries - 1);
	bad_peer -> 
	    io:fwrite("get blocks 2 failed connect bad peer\n"),
	    %io:fwrite(packer:pack([N, Peer, Tries])),
	    io:fwrite("\n"),
	    timer:sleep(600),
	    new_get_blocks2(TheirBlockHeight,  N, Peer, Tries - 1);
	{ok, Bs} -> 
            io:fwrite("got compressed blocks, asked for height: "),
            io:fwrite(integer_to_list(N)),
            io:fwrite("\n"),
            L = if
                    is_list(Bs) -> Bs;
                    true ->
                        Dict = block_db:uncompress(Bs),
                        L0 = low_to_high(dict_to_blocks(dict:fetch_keys(Dict), Dict)),
                        L0
                end,
            %io:fwrite("uncompressed the blocks\n"),
            S = length(L),
            io:fwrite("many blocks: "),
            io:fwrite(integer_to_list(S)),
            io:fwrite("\n"),
            io:fwrite("first height: "),
            io:fwrite(integer_to_list(element(2, hd(L)))),
            io:fwrite("\n"),
            io:fwrite("last height: "),
            io:fwrite(integer_to_list(element(2, hd(lists:reverse(L))))),
            io:fwrite("\n"),
            if
                S == 0 -> ok;
                true ->
                    GH = ((hd(lists:reverse(L)))#block.height),
                    AH = api:height(),
                    if
                        AH > GH ->
                            wait_do(fun() ->
                                            {ok, DA} = ?download_ahead,
                                            (N + S) < (block:height() + DA)
                                    end,
                                    fun() ->
                                            %new_get_blocks2(TheirBlockHeight, N + S, Peer, 5)
                                            new_get_blocks2(TheirBlockHeight, GH+1, Peer, 5)
                                    end,
                                    200);
                        true -> ok
                    end
            end,
            %io:fwrite("sync n s: "),
            %io:fwrite(packer:pack([N, S])),
            %io:fwrite("\n"),
            %{ok, Cores} = application:get_env(amoveo_core, block_threads),
            %Cores = 20,
            %S2 = S div Cores,
            %io:fwrite("sync:new_get_blocks2. add "),
            %io:fwrite(integer_to_list(length(L))),
            %io:fwrite(" blocks\n"),
            %io:fwrite("requested from "),
            %io:fwrite(integer_to_list(N2)),
            %io:fwrite("\n"),
            %io:fwrite(integer_to_list((hd(L))#block.height)),
            %io:fwrite("\n"),
            %io:fwrite(integer_to_list((hd(tl(L)))#block.height)),
            io:fwrite("adding blocks to block organizer\n"),
            skip_ahead_to_the_checkpoint(L)
    end.
skip_ahead_to_the_checkpoint([]) -> ok;
skip_ahead_to_the_checkpoint(L) ->
    Start = (hd(L))#block.height - 1,
    io:fwrite("skip ahaead checkpoint " ++ integer_to_list(Start) ++ "\n"),
    BH = block:height(),
    if
        (Start > (BH+1)) -> 
            io:fwrite("too far\n"),
            ok;
        true ->
            Top = block:get_by_height(Start),
            case Top of
                error -> skip_ahead_to_the_checkpoint(tl(L));
                _ -> 
                    sequential_loop([Top|L]),
                    sync:start()
            end
    end.
            
sequential_loop([X]) -> ok;
sequential_loop([Prev|[Block|Rest]]) -> 
    Block2 = process_block_sequential(Block, Prev),
    sequential_loop([Block2|Rest]).
    
process_block_sequential(Block, Prev) ->
    go = sync_kill:status(),
    BH = block:hash(Block),
    %io:fwrite(Prev),
    true = (Prev#block.height + 1 == Block#block.height),
    #block{
       height = Height2
      } = Block,
    F52 = forks:get(52),
    {ok, MTV} = application:get_env(
                  amoveo_core, minimum_to_verify),
    {ok, TestMode} = application:get_env(
                       amoveo_core, test_mode),
    MyHeight = block:height(),
    if
        (Height2 == F52) -> 
           %hardcode verkle update block hash, so we don't have to check blocks from before this point.
            true = BH == <<185,59,27,106,59,121,158,59,113,186,179,200,161,70,238, 229,35,162,169,31,168,11,112,101,135,49,179,32,111,90,87,192>>;
        true -> ok
    end,
    Block2 = if
                 (TestMode or ((Height2 > F52) and (Height2 > MTV))) ->
                   %after verkle update
                     io:fwrite("verifying block " ++ integer_to_list(Height2) ++ "\n"),
                     X = block:check0(Block),
                     {true, Block3} = block:check2(Prev, Block#block{trees = X}),
                     Block3;
                 true ->
                     io:fwrite("block left unverified\n"),
                     Block
             end,
            %check every block matches it's header, even from before the verkle update height
    {ok, Header} = headers:read(BH),
    true = (Header#header.height == 
                Block#block.height),
    headers:absorb_with_block([Header]),
    block_db3:write(Block2, BH),
    Block2.
wait_do(FB, F, T) ->
    spawn(fun() ->
                  go = sync_kill:status(),
                  B = FB(),
                  if
                      B -> 
                                                %io:fwrite("wait do done waiting \n"),
                          F();
                      true ->
                          timer:sleep(T),
                          wait_do(FB, F, T)
                  end
          end).
    
dict_to_blocks([], _) -> [];
dict_to_blocks([top_hash|T], D) ->
    dict_to_blocks(T, D);
dict_to_blocks([H|T], D) ->
    B = dict:fetch(H, D),
    [B|dict_to_blocks(T, D)].
low_to_high(L) ->
    L2 = listify(L),
    low2high2(L2).
listify([]) -> [];
listify([H|T]) -> [[H]|listify(T)].
low2high2([X]) -> X;
low2high2(T) ->
    low2high2(l2himprove(T)).
l2himprove([]) -> [];
l2himprove([H]) -> [H];
l2himprove([H1|[H2|T]]) ->
    [merge(H1, H2)|l2himprove(T)].
merge([], L) -> L;
merge(L, []) -> L;
merge([H1|T1], [H2|T2]) ->
    BH1 = element(2, H1),
    BH2 = element(2, H2),
    if
        BH1 > BH2 -> [H2|merge([H1|T1], T2)];
        true -> [H1|merge(T1, [H2|T2])]
    end.
            
remove_self(L) ->
%assumes that you only appear once or zero times in the list.
    MyIP = peers:my_ip(),
    {ok, MyPort} = application:get_env(amoveo_core, port),
    Me = {MyIP, MyPort},
    remove_self2(L, Me).
remove_self2([], _) -> [];
%remove_self2([{{192,168,0, _}, _}|T], Me) ->
%    remove_self2(T, Me);
remove_self2([{{127,0,0,1}, MyPort}|T], {MyIP, MyPort}) ->
    Me = {MyIP, MyPort},
    remove_self2(T, Me);
remove_self2([Me|T], Me) -> 
    remove_self2(T, Me);
remove_self2([H|T], Me) ->
    [H|remove_self2(T, Me)].
shuffle([]) -> [];
shuffle([X]) -> [X];
shuffle(L) -> shuffle(L, length(L), []).
shuffle([], 0, Result) -> Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(rand:uniform(Len), List, []),
    shuffle(Rest, Len - 1, [Elem|Result]).
nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).
list_headers(X, 0) -> X;
list_headers([H|T], N) ->
    %io:fwrite("list headers 0\n"),
    case headers:read(H#header.prev_hash) of
	error -> [H|T];
	{ok, H2}  -> %headers:read(H#header.prev_hash),
	    list_headers([H2|[H|T]], N-1)
    end.
push_new_block(Block) ->
    %keep giving this block to random peers until 1/2 the people you have cont
    %acted already know about it. Don't talk to the same peer multiple times.
    Peers0 = peers:all(),
    Peers = remove_self(Peers0),
    Hash = block:hash(Block),
    Header = block:block_to_header(Block),
    %Header = headers:top_with_block(),
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    M = min(Header#header.height, FT),
    Headers = list_headers([Header], M),
    Pools = case application:get_env(amoveo_core, pools) of
                {ok, P} -> P;
                undefined -> []
            end,
    spawn(fun() -> push_new_block_helper(0, 0, shuffle(Pools), Hash, Headers) end),
    spawn(fun() -> push_new_block_helper(0, 0, shuffle(Peers), Hash, Headers) end).
push_new_block_helper(_, _, [], _, _) -> ok;%no one else to give the block to.
push_new_block_helper(N, M, _, _, _) when ((M > 1) and (((N-2)*2) > (M*1))) -> ok;%the majority of peers probably already know.
push_new_block_helper(N, M, [P|T], Hash, Headers) ->
    X = remote_peer({header, Hash}, P),
    {Top, Bottom} = case X of
	    3 -> 
		{1, 1};
	    error -> {0, 0};
	    bad_peer -> {0, 0};
	    _ -> 
		spawn(fun() ->
			      remote_peer({headers, Headers}, P)
		      end),
		{0, 1}
	end,
    push_new_block_helper(N+Top, M+Bottom, T, Hash, Headers).
trade_txs(Peer) ->
    %io:fwrite("trade txs "),
    %io:fwrite(packer:pack(Peer)),
    %io:fwrite("\n"),
    spawn(fun() ->
                  TP = tx_pool:get(),
                  Checksums = remote_peer({txs, 2}, Peer),
                  MyChecksums = TP#tx_pool.checksums,
                  MyTxs = TP#tx_pool.txs,
                  Requests = checksum_minus(Checksums, MyChecksums),
                  Txs2 = remote_peer({txs, 2, Requests}, Peer),
                  tx_pool_feeder:absorb_async(Txs2),
                  SendChecksums = checksum_minus(MyChecksums, Checksums),
                  Give = ext_handler:send_txs(MyTxs, MyChecksums, SendChecksums, []),
                  remote_peer({txs, Give}, Peer)
                      
          end).
%    end.
   
sync_peer(Peer) ->
    io:fwrite("sync peer \n"),
    io:fwrite("\n"),
    io:fwrite("trade peers \n"),
    spawn(fun() -> trade_peers(Peer) end),
    MyTop = headers:top(),
    io:fwrite("trade headers \n"),
    spawn(fun() -> get_headers(Peer) end),
    {ok, HB} = ?HeadersBatch,
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    MyBlockHeight = block:height(),
    TheirTop = remote_peer({header}, Peer), 
    TheirBlockHeight = remote_peer({height}, Peer),
    TheirHeaders = remote_peer({headers, HB, max(0, MyBlockHeight - FT)}, Peer),
    TopCommonHeader = top_common_header(TheirHeaders),
    
    io:fwrite("start if statement\n"),
    if
        is_atom(TheirTop) -> error;
        is_atom(TheirBlockHeight) -> error;
        is_atom(TopCommonHeader) -> 
            io:fwrite("failed to calculate top common header\n"),
            io:fwrite(TopCommonHeader),
            error;
        true -> 
            io:fwrite("exited if statement\n"),
            sync_peer2(Peer, TopCommonHeader, TheirBlockHeight, MyBlockHeight, TheirTop)
    end.
sync_peer2(Peer, TopCommonHeader, TheirBlockHeight, MyBlockHeight, TheirTopHeader) ->
    io:fwrite("sync_peer2\n"),
    TTHH = TheirTopHeader#header.height,
    MTHH = (headers:top())#header.height,
    if
	TTHH < MTHH ->
	    io:fwrite("send them headers.\n"),
	    H = headers:top(),
	    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
	    GiveHeaders = list_headers([H], FT),
	    spawn(fun() -> remote_peer({headers, GiveHeaders}, Peer) end),
	    ok;
	true -> ok
    end,
    if
        TheirBlockHeight > MyBlockHeight ->
	    io:fwrite("get blocks from them.\n"),
	    CommonHeight = min(TopCommonHeader#header.height, block:height()),
            RS = reverse_syncing(),
            %BH = block_db:ram_height(),
            BH = block:height(),
            if
                (RS and (BH < 1)) -> 
                    io:fwrite("reverse sync prevents normal sync here.\n"),
                    %todo, download and sync from a checkpoint.
                    ok;
                true -> 
                    io:fwrite("new get blocks start, common height " ++ integer_to_list(CommonHeight) ++ " their height: " ++ integer_to_list(TheirBlockHeight) ++ "\n"),
                    %new_get_blocks(Peer, CommonHeight + 1, TheirBlockHeight, ?tries)
                    stream_get_blocks(Peer, CommonHeight + 1, TheirBlockHeight)
            end;
	%true ->
	(TheirBlockHeight == MyBlockHeight) ->
	    spawn(fun() ->
			  trade_txs(Peer)
		  end),
	    %io:fwrite("already synced with this peer \n"),
	    ok;
	true ->
            io:fwrite("we have more blocks than them, so we don't need to trade txs."),
            ok
    end.
top_common_header(L) when is_list(L) ->
    tch(lists:reverse(L));
top_common_header(_) -> error.
tch([]) -> error;
tch([H|T]) ->
    Tch = block:get_by_hash(block:hash(H)),
    %io:fwrite("tch is "),
    %io:fwrite(packer:pack(Tch)),
    %io:fwrite("\n"),
    %io:fwrite(packer:pack(block:hash(H))),
    %io:fwrite("\n"),
    case block:get_by_hash(block:hash(H)) of
	error -> tch(T);
	empty -> tch(T);
	_ -> H
    end.
reverse_syncing() ->
    case application:get_env(amoveo_core, reverse_syncing) of
        undefined -> false;
        {ok, R} -> R
    end.
            
    
    
	    
cron() ->
    %io:fwrite("sync cron 1\n"),
    spawn(fun() ->
		  timer:sleep(4000),
		  Peers = shuffle(peers:all()),
		  LP = length(Peers),
		  if
		      LP > 0 ->
                          get_headers(hd(Peers)),
                          trade_peers(hd(Peers)),
                          timer:sleep(3000);
		      true -> ok
		  end,
		  if
		      LP > 1 ->
                          get_headers(hd(tl(Peers))),
                          trade_peers(hd(tl(Peers))),
                          timer:sleep(3000);
                      true -> ok
		  end,
		  if
		      LP > 2 ->
                          get_headers(hd(tl(tl(Peers)))),
                          trade_peers(hd(tl(tl(Peers))));
                      true -> ok
		  end
          end),
    spawn(fun() ->
		  timer:sleep(4000),
		  cron2()
	  end).
cron2() ->   
    %io:fwrite("sync cron 2\n"),
    SS = sync:status(),
    SC = sync_mode:check(),
    AHeight = api:height(),
    BHeight = block:height(),
    B = AHeight > BHeight,
    %SameHeight = (AHeight == BHeight),
    if 
	((SS == go) and (SC == normal)) ->
	    spawn(fun() ->
			  if 
			      B -> sync:start();
			      true -> 
				  P2 = shuffle(remove_self(peers:all())),
				  LP = length(P2),
				  if
				      LP > 0 ->
                                          TheirBlockHeight = 
                                              remote_peer({height}, hd(P2)),
                                          if
                                              (BHeight == TheirBlockHeight) ->
                                                  trade_txs(hd(P2));
                                              true -> ok
                                          end;
				      true -> ok
				  end
			  end
		  end);
	true -> ok
    end,
    timer:sleep(5000),
    cron2().
checksum_minus([], _) -> [];
checksum_minus(A, []) -> A;
checksum_minus([A|AT], B) ->
    Bool = lists:member(A, B),
    if
	Bool -> checksum_minus(AT, B);
	true -> [A|checksum_minus(AT, B)]
    end.
	    
