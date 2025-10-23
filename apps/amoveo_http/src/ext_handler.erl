-module(ext_handler).
-include("../../amoveo_core/src/records.hrl").

-export([init/3, handle/2, terminate/3, doit/1,
	send_txs/4, init/2, many_headers/2,
        get_header_by_height/2, many_headers2/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '["test"]' http://localhost:3011
init(Req0, Opts) ->
    handle(Req0, Opts).	
handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    {IP, _} = cowboy_req:peer(Req2),
    D = case request_frequency:doit(IP) of
	    ok ->
		true = is_binary(Data),
		A = packer:unpack(Data),
		B = case A of
			{f} -> {ok, IP};
			{headers, H} ->
			    peers:add({IP, 8080}),
			    headers:absorb(H),
			    spawn(fun() ->
					  HH = api:height(),
					  BH = block:height(),
					  if 
					      HH > BH -> sync:start([{IP, 8080}]);
					      true -> ok
					  end
				  end),
			    {ok, 0};
                        {txs, []} -> {ok, ok};
                        {txs, 2} -> doit(A);
                        {txs, Txs = [_,[_|_]]} ->
                            io:fwrite("the tx spam handler is being activated\n"),
                            tx_spam_handler(Txs, IP);
			_ -> doit(A)
		    end,
		packer:pack(B);
	    _ -> 
                io:fwrite("spammer's ip: "),
                io:fwrite(packer:pack(IP)),
                io:fwrite("\n"),
		packer:pack({ok, <<"stop spamming the server">>})
	end,	    

    Headers = #{ <<"content-type">> => <<"application/octet-stream">>,
	       <<"Access-Control-Allow-Origin">> => <<"*">>},
    %Headers = [{<<"content-type">>, <<"application/octet-stream">>},
%	       {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Req4 = cowboy_req:reply(200, Headers, D, Req2),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
doit({f, 1}) ->
    {ok, {block:hashes_per_block(),
	  block:hashrate_estimate(),
	  block:period_estimate()}};
doit({account, Pubkey}) -> 
    {ok, api:account(Pubkey)};
doit({accounts, Pubkey}) -> 
    {ok, api:account(Pubkey)};
doit({markets, MID}) ->
    {ok, api:tree_common(markets, MID)};%trees:get(markets, MID)};
doit({contracts, CID}) ->
    {ok, api:tree_common(contracts, CID)};%trees:get(contracts, CID)};
doit({sub_accounts, IDs}) when is_list(IDs) ->
    true = (length(IDs) < 1000),
    Accs = lists:map(fun(X) -> api:tree_common(sub_accounts, X) end, IDs),
    {ok, Accs}; 
doit({sub_accounts, ID}) ->
    {ok, api:tree_common(sub_accounts, ID)};%trees:get(sub_accounts, ID)};
doit({oracles, ID}) ->
    {ok, api:tree_common(oracles, ID)};%trees:get(sub_accounts, ID)};
doit({trades, ID}) ->
    {ok, api:tree_common(trades, ID)};
doit({receipts, ID}) ->
    {ok, api:tree_common(receipts, ID)};
doit({jobs, ID}) ->
    {ok, api:tree_common(jobs, ID)};
doit({futarchy, FID}) ->
    {ok, api:tree_common(futarchy, FID)};
doit({futarchy_unmatched, TID}) ->
    {ok, api:tree_common(futarchy_unmatched, TID)};
doit({futarchy_matched, TID}) ->
    {ok, api:tree_common(futarchy_matched, TID)};
doit({pubkey}) -> {ok, keys:pubkey()};
doit({height}) -> {ok, block:height()};
doit({height, 1}) -> {ok, api:height()};
doit({height, 2}) -> {ok, api:height(2)};
doit({height, 3}) -> {ok, block:bottom()};
doit({version}) -> {ok, version:doit(block:height())};
doit({version, 1}) -> 
    {ok, Version} = application:get_env(amoveo_core, db_version),
    {ok, Version};
doit({version, 2, N}) ->
    F = forks:get(N),
    {ok, F};
doit({version, 3}) ->
    N = forks:top(),
    G = forks:get(N),
    {ok, N, G};
doit({give_block, Block}) -> %block can also be a list of blocks.
    %io:fwrite("ext_handler receiving blocks\n"),
    %Response = block_absorber:save(Block),
    case sync_mode:check() of
        quick -> {ok, 0};
        normal ->
            A = if
                    is_list(Block) -> Block;
                    true -> [Block]
                end,
            Response = block_organizer:add(A),
            R2 = if
                     is_atom(Response) -> 0;
                     true -> Response
                 end,
            {ok, R2}
    end;
doit({block, N}) when (is_integer(N) and (N > -1))->
    {ok, block:get_by_height(N)};
doit({block, 2, H}) ->
    {ok, block:get_by_hash(H)};
doit({blocks, Many, N}) -> 
    %X = block_db:read(Many, N),
    X = block_db3:read(N, Many+N),
    {ok, X};
doit({blocks, -1, Many, Highest}) ->
    %X = block_db:read_reverse(Many, Highest),
    X = block_db3:read(Highest - Many, Highest),
    {ok, X};
doit({header, N}) when is_integer(N) -> 
    {ok, block:block_to_header(block:get_by_height(N))};
doit({header, H}) ->
    case headers:read(H) of
	error -> {ok, 0};
	_ -> {ok, 3}
    end;
doit({headers, _H}) ->
    %headers:absorb(H),
    %spawn(fun() ->
	%	  HH = api:height(),
	%	  BH = block:height(),
%		  if 
%		      HH > BH -> sync:start();
%		      true -> ok
%		  end
%	  end),
    {ok, 0};
doit({headers, Many0, N}) -> 
    Many = min(Many0, 5000),
    {ok, many_headers(Many, N)};
doit({headers, _Many, N}) ->  % unused
    
    %todo. if we look up earlier than 260000, the output is 5000 blocks earlier than we had wanted.
    %todo. doesn't include top header in output.
    

    %X = many_headers(Many, N),
    T = api:height(),
    N2 = N - (N rem 5000),
    %N2 = N rem 5000,
    if
        %N2 < (T - 5000) ->
        N < (T - 5000) ->
            X = many_headers(5000, N2),
            {ok, X};
        true ->
            X = many_headers(T-N+1, N),
            {ok, X}
    end;
doit({header}) -> {ok, headers:top()};
doit({peers}) ->
    P = peers:all(),
    P2 = amoveo_utils:tuples2lists(P),
    {ok, P2};
doit({peers, 2}) ->
    {ok, peers_heights:doit()};
doit({peers, Peers}) ->
    peers:add(Peers),
    {ok, 0};
doit({txs}) -> {ok, lists:reverse((tx_pool:get())#tx_pool.txs)};
doit({txs, 2}) ->%request a list of your checksums
    TP = tx_pool:get(),
    X = TP#tx_pool.checksums,
    {ok, X};
doit({txs, 2, Checksums}) ->%request the txs for these checksums
    TP = tx_pool:get(),
    CS = TP#tx_pool.checksums,
    Txs = TP#tx_pool.txs,
    ST = send_txs(Txs, CS, Checksums, []),
    {ok, ST};
doit({txs, [Tx]}) ->
    tx_pool_feeder:absorb(Tx),
    timer:sleep(200),
    Txs = (tx_pool:get())#tx_pool.txs,
    B = is_in(Tx, Txs),
    Y = case B of
            true -> hash:doit(signing:data(Tx));
            false -> <<"error">>
        end,
    {ok, Y};
doit({txs, Txs}) ->
    ok = tx_pool_feeder:absorb(Txs),
    {ok, 0};
doit({txs, 3, N}) ->
    B = block:get_by_height(N),
    Txs = tl(B#block.txs),
    Txids = lists:map(
	      fun(Tx) -> hash:doit(signing:data(Tx)) end, 
	      Txs),
    X = [Txs, Txids],
    {ok, X};
doit({top}) -> 
    Top = block:top(),
    {ok, Top, Top#block.height};
doit({test, -1}) -> 
    {ok, version:doit(block:height())};
doit({test}) -> 
    {test};
doit({test, N}) ->
    M = 8 * N,
    {test, <<0:M>>};
doit({spend_tx, Amount, Fee, From, To}) ->
    Tx = spend_tx:make_dict(To, Amount, Fee, From),
    {ok, Tx};
doit({create_account_tx, Amount, Fee, From, To}) ->
    Tx = create_account_tx:make_dict(To, Amount, Fee, From),
    {ok, Tx};
doit({delete_acc_tx, To, From, Fee}) ->
    Tx = delete_account_tx:make_dict(To, From, Fee),
    {ok, Tx};
doit({new_channel_tx, Acc1, Acc2, B1, B2, Delay, Fee}) ->
    CID = api:find_id2(),
    Tx = new_channel_tx:make_dict(CID, Acc1, Acc2, B1, B2, Delay, Fee),
    {ok, Tx};
doit({time_value}) ->
    application:get_env(amoveo_core, time_value);
doit({new_channel, STx, SSPK, Expires}) ->
    {ok, true} = application:get_env(amoveo_core, channels),
    unlocked = keys:status(),
    LifeSpan = Expires - api:height(),
    {ok, MinimumChannelLifespan} = 
        application:get_env(amoveo_core, min_channel_lifespan),
    true = LifeSpan > MinimumChannelLifespan,
    Tx = signing:data(STx),
    SPK = signing:data(SSPK),
    TheirPub = channel_feeder:other(Tx),
    error = channel_manager:read(TheirPub),
    %undefined = channel_feeder:cid(Tx),
    Amount = SPK#spk.amount,
    Bal1 = new_channel_tx:bal1(Tx),
    Bal2 = new_channel_tx:bal2(Tx),
    true = Amount < Bal1,
    true = (- Amount) < Bal2,
    Delay = new_channel_tx:delay(Tx),
    {ok, MinimumChannelDelay} =
	application:get_env(amoveo_core, min_channel_delay),
    true = Delay >= MinimumChannelDelay,
    {ok, TV} = application:get_env(amoveo_core, time_value),
    CFee = TV * (Delay + LifeSpan) * (Bal1 + Bal2) div 100000000,
    true = CFee =< SPK#spk.amount,
    true = CFee < Bal1,%make sure they can afford the fee.
    true = channel_feeder:new_channel_check(Tx), %make sure we are not already storing a channel with this same partner.
    SSTx = keys:sign(STx),
    tx_pool_feeder:absorb(SSTx),
    S2SPK = keys:sign(SPK),
    channel_feeder:new_channel(Tx, SSPK, Expires),
    {ok, [SSTx, S2SPK]};
doit({spk, TheirPub})->
    {ok, CD} = channel_manager:read(TheirPub),
    ME = keys:sign(CD#cd.me),
    {ok, [CD, ME]};
doit({channel_payment, SSPK, Amount}) ->
    R = channel_feeder:spend(SSPK, Amount),
    {ok, R};
doit({channel_close, CID, PeerId, SS, STx}) ->
    channel_feeder:close(SS, STx),
    Tx = signing:data(STx),
    Fee = channel_team_close_tx:fee(Tx),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = CD#cd.me,
    Height = (headers:top())#header.height,
    Dict = (tx_pool:get())#tx_pool.dict,
    {Amount, _, _} = spk:dict_run(fast, SS, SPK, Height, 0, Dict),
    Channel = trees:get(channels, CID),
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    {ok, TV} = application:get_env(amoveo_core, time_value),
    Expires = CD#cd.expiration,
    LifeSpan= max(0, Expires - Height),
    CFee = TV * LifeSpan * (Bal1 + Bal2) div 100000000,
    Tx2 = channel_team_close_tx:make_dict(CID, Amount-CFee, Fee),
    io:fwrite("channel close compare txs \n"),
    io:fwrite(packer:pack(Tx)),
    io:fwrite(packer:pack(Tx2)),
    Tx = Tx2,
    SSTx = keys:sign(STx),
    tx_pool_feeder:absorb(SSTx),
    {ok, SSTx};
doit({locked_payment, SSPK, Amount, Fee, Code, Sender, Recipient, ESS}) ->
    true = size(ESS) < 200,
    _R = channel_feeder:lock_spend(SSPK, Amount, Fee, Code, Sender, Recipient, ESS),
    {ok, keys:sign(SSPK)};
doit({learn_secret, From, Secret, Code}) ->
    {ok, OldCD} = channel_manager:read(From),
    secrets:add(Code, Secret),
    io:fwrite("learn secret oldcd issue"),
    io:fwrite(packer:pack([0, OldCD, OldCD#cd.ssme])),
    SS = OldCD#cd.ssme,
    CFME = OldCD#cd.me,
    {NewSS, SPK, _Secrets, SSThem} = 
	spk:bet_unlock(CFME, SS),
    if
	NewSS == SS -> ok;
	true -> 
            NewCD = OldCD#cd{me = SPK, ssme = NewSS, ssthem = SSThem, emsg = []},
	    channel_manager:write(From, NewCD),
	    {ok, Current} = arbitrage:check(Code),
	    IDS = minus(Current, From),
	    channel_feeder:bets_unlock(IDS)
    end,
    {ok, 0};
doit({channel_sig, CID}) ->
    X = nc_sigs:get(CID),
    {ok, X};
doit({channel_sync, From, SSPK}) ->
    io:fwrite("ext_handler channel sync"),
    io:fwrite(packer:pack(SSPK)),
    io:fwrite("\n"),
    Return = channel_feeder:update_to_me(SSPK, From),
    io:fwrite("channel sync succeeded.\n"),
    io:fwrite(packer:pack(From)),
    io:fwrite("\n"),
    {ok, Return};
doit({bets}) ->
    free_variables:bets();
doit({proof, IDs, Hash}) ->
    %batch verkle proofs.
    B = block:get_by_hash(Hash),
    Height = B#block.height,
    Loc = B#block.trees,
    true = is_integer(Loc),
    
    %io:fwrite({IDs, Loc}),%{<<_:520>>, 13}
%    IDs2 = lists:map(fun([TreeName, ID]) ->
                             %trees2:key({TreeName, ID}) end,
%                             {TreeName, ID} end,
%                     IDs),
    {Proof, IDs3} = trees2:get_proof(IDs, Loc, fast, Height),
    %todo. put this proof in a format that javascript will understand.
    {ok, {Proof, IDs3}};
    
doit({proof, TreeName, ID, Hash}) ->
%here is an example of looking up the 5th governance variable. the word "governance" has to be encoded base64 to be a valid packer:pack encoding.
%curl -i -d '["proof", "Z292ZXJuYW5jZQ==", 5, Hash]' http://localhost:8080 
    %io:fwrite(base64:encode(Hash)),
    %io:fwrite("\n"),
    Trees = (block:get_by_hash(Hash))#block.trees,
    TN = trees:name(TreeName), 
    if
        is_integer(Trees) ->
            %io:fwrite("we needed a verkle proof, not a merkle proof\n"),
            doit({proof, [{TN, ID}], Hash});
        true ->
            %merkle case
            Root = trees:TN(Trees),
            {RootHash, Value, Proof} = TN:get(ID, Root),
            Proof2 = proof_packer(Proof),
            {ok, {return, trees:serialized_roots(Trees), RootHash, Value, Proof2}}
    end;
doit({list_oracles}) ->
    {ok, order_book:keys()};
doit({oracle, 2, QuestionHash}) ->
    {ok, Q} = oracle_questions:get(QuestionHash),
    {ok, Q};
doit({oracle, Y}) ->
    %X = base64:decode(Y),
    X = Y,
    Oracle = trees:get(oracles, X),
    case Oracle of
        empty -> {ok, 0};
        _ ->
            QH = element(4, Oracle),
            {ok, Question} = oracle_questions:get(QH),
            Z = case order_book:data(X) of
                    {ok, OB} -> OB;
                    error -> 0
                end,
 %{ok, OB} = order_book:data(X),
            {ok, {Z, Question}}
    end;
doit({oracle_bets, OID}) ->
    %This is a very poor choice of name. "oracle_bets" for something that doesn't touch the oracle_bets merkel tree, and only touches the orders merkel tree.
    B = block:top(),
    Trees = B#block.trees,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    orders:all(Oracle#oracle.orders);%This does multiple hard drive reads. It could be a security vulnerability. Maybe we should keep copies of this data in ram, for recent blocks.
doit({market_data, OID}) ->
    %{ok, OB} = order_book:data(base64:decode(OID)),
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    OBData = order_book:ob_type(OB),
    %OBdata is either {binary} or {scalar, LL, UL, ??}
    {ok, {Expires, keys:pubkey(), Period, OBData}};
doit({trade, Account, Price, Type, Amount, OID, SSPK, Fee}) ->
    %make sure they pay a fee in channel for having their trade listed. 
    _BetLocation = constants:oracle_bet(),
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    _Period = order_book:period(OB),
    {ok, CD} = channel_manager:read(Account),
    TPG = tx_pool:get(),
    Height = TPG#tx_pool.height,
    {ok, Confirmations} = application:get_env(amoveo_core, confirmations_needed),
    OldBlock = block:get_by_height(Height - Confirmations),
    OldTrees = OldBlock#block.trees,
    false = empty == trees:get(channels, CD#cd.cid, dict:new(), OldTrees),%channel existed confirmation blocks ago.

    true = Expires < CD#cd.expiration,
    %SC = market:market_smart_contract(BetLocation, OID, Type, Expires, Price, keys:pubkey(), Period, Amount, OID, api:height()),
    SSPK2 = channel_feeder:trade(Account, Price, Type, Amount, OID, SSPK, Fee),
    SPK = signing:data(SSPK),
    SPK = signing:data(SSPK2),
    Order = order_book:make_order(Account, Price, Type, Amount),
    order_book:add(Order, OID),
    {ok, SSPK2};
doit({cancel_trade, TheirPub, N, SSPK}) ->
    SSPK2 = channel_feeder:cancel_trade_server(N, TheirPub, SSPK),
    {ok, SSPK2};
doit({combine_cancel_assets, TheirPub, SSPK}) ->
    SSPK2 = channel_feeder:combine_cancel_assets_server(TheirPub, SSPK),
    {ok, SSPK2};
doit({mining_data}) ->
    B = block:top(),
    Diff = pow:sci2int(B#block.difficulty),
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    %Trees = 0,
    Reward = governance:get_value(block_reward, trees:governance(Trees)),
    Hashrate = Diff div 660,
    D = [B#block.height, Diff, Hashrate, Reward],
    {ok, D};
doit({checkpoint}) ->
    X = checkpoint:recent(),
    {ok, X};
doit({checkpoint, Hash, N}) ->
    CR = constants:custom_root(),
    Encoded = base58:binary_to_base58(Hash),
    case file:read_file(
           CR ++ "checkpoints/"++Encoded++
               "/" ++ checkpoint:chunk_name(N)) of
        {ok, D} -> {ok, D};
        {error, enoent} -> {error, "out of bounds"};
        {error, _} -> {error, "unhandled error"}
    end;
doit(X) ->
    io:fwrite("I can't handle this \n"),
    io:fwrite(packer:pack(X)), %unlock2
    {error}.
proof_packer(X) when is_tuple(X) ->
    proof_packer(tuple_to_list(X));
proof_packer([]) -> [];
proof_packer([H|T]) ->
    [proof_packer(H)|proof_packer(T)];
proof_packer(X) -> X.
%many_blocks(M, _) when M < 1 -> [];
%many_blocks(Many, N) ->
%    H = block:height(),
%    if N > H -> [];
%       true ->
%            B = block:get_by_height(N),
%            case B of
%                empty -> many_blocks(Many-1, N+1);
%                _ ->
%                    [B|many_blocks(Many-1, N+1)]
%            end
%    end.
get_header_by_height(N, H) ->
    HH = H#header.height,
    if
        (HH == N) -> H;
        (HH < N) -> H;
        true ->
            case headers:read(H#header.prev_hash) of
                {ok, Child} ->
                    get_header_by_height(N, Child);
                error -> io:fwrite({N, H})
            end
    end.
        
                         
%    case H#header.height of
        %0 -> H;
%	N -> H;
%	_ -> 
%            case headers:read(H#header.prev_hash) of
%                {ok, Child} ->
%                    get_header_by_height(N, Child);
%                error -> io:fwrite({N, H}),
%                         1=2
%            end
%    end.
	    
many_headers_cached_broken(Many, X) ->
    %io:fwrite("many headers "), 
    %io:fwrite(packer:pack([Many, X])), 
    %io:fwrite("\n"),
    Z = max(0, X + Many - 1),%number of highest header that we want.
    %H = headers:top(),
    APIHeight = api:height(),
    case APIHeight >= (X) of
	false -> [];
	true ->
	    {N, Many2} = 
		if 
		    Z < APIHeight ->
                        %we have all the headers we need to create this batch.
			{Z, Many};
		    true ->
                        %we don't have enough headers.
			{APIHeight, Many - (Z - APIHeight)}
		end,
						%N = min(H#header.height, X + Many - 1),
            N = min(Z, APIHeight),%end of range that we want.
            Many2 = Many - max(0, (Z - APIHeight)),
	    %Nth = get_header_by_height(N, H),
	    %Result = many_headers2(Many2, Nth, []),
            N2 = (N - (N rem 5000)),
            case Many2 of
                5000 ->
                    case header_cache:read(N2) of
                        error ->
                            %io:fwrite("slow\n"),
                            io:fwrite(integer_to_list(N2)),
                            %H = block:block_to_header(block:top()),
                            H = headers:top(),
                            Nth = get_header_by_height(N2, H),
                            Result = many_headers2(Many2, Nth, []),
                            header_cache:store(N2, Result),
                            Result;
                        {ok, Result2} ->
                            %io:fwrite("fast\n"),
                            Result2
                    end;
                _ ->
                    H = block:block_to_header(block:top()),
                    Nth = get_header_by_height(N, H),
                    many_headers2(Many2, Nth, [])
            end
    end.

broken_many_headers(Many, TheirHeight) -> %
    %return Many headers, starting at height X.
    Height = block:height(), %12
    {Start, End} = 
        if
            (TheirHeight+Many-1) > Height -> {TheirHeight, Height};
            true -> {TheirHeight, TheirHeight+Many-1}
        end,
    %io:fwrite("ext handler many headers " ++ integer_to_list(Start) ++ " " ++ integer_to_list(End) ++ "\n"),
    case block:get_by_height(End) of
        error -> 
            %io:fwrite("can't share headers if we don't have the blocks\n"),
            ok;
        Block ->
            Header = block:block_to_header(Block),
            %Header = block:block_to_header(block:get_by_height(End)),
            many_headers2(min(Many, End-Start+1), Header, [])
    end.
        

many_headers(Many, X) ->
%return Many headers, starting at height X?
    %io:fwrite("many headers "), 
    %io:fwrite(packer:pack([Many, X])), 
    %io:fwrite("\n"),
    Z = max(0, X + Many - 1),
    %H = headers:top(),
    H = block:block_to_header(block:top()),
    case (true and ((H#header.height) >= (X))) of
	false -> 
            %many headers height low 381720 386213
            io:fwrite("many headers height low " ++ integer_to_list(H#header.height) ++ " " ++ integer_to_list(X) ++ "\n"),%many headers height low 381718 386213
            [];
	true ->
	    {N, Many2} = 
		if 
		    Z < H#header.height ->
			{Z, Many};
		    true ->
			{H#header.height, Many - (Z - H#header.height)}
		end,
	    %N = min(H#header.height, X + Many - 1),
	    Nth = get_header_by_height(N, H),
	    %Many2 = max(0, N - X),
	    many_headers2(Many2, Nth, [])
    end.
many_headers2(0, _, Out) -> Out;
many_headers2(Many, H, Out) ->
    %{ok, H} = headers:read(Hash),
    true = (Many < 10000),
    case H#header.height of
	0 -> [H|Out];
	_ ->
	    {ok, H2} = headers:read(H#header.prev_hash),
	    many_headers2(Many-1, H2, [H|Out])
    end.

%many_headers(M, N) ->
%    B = many_blocks(M, N),
%    blocks2headers(B).
%blocks2headers([]) -> [];
%blocks2headers([B|T]) ->
%    [block:block_to_header(B)|
%    blocks2headers(T)].
%many_headers(M, _) when M < 1 -> [];
%many_headers(Many, N) ->    
%    H = api:height(),
%    if
%        N > H -> [];
%        true ->
%            [block:block_to_header(block:get_by_height(N))|
%             many_headers(Many-1, N+1)]
%    end.
minus([T|X], T) -> X;
minus([A|T], X) -> [A|minus(T, X)].
send_txs(_, _, [], X) -> X;
send_txs(Txs, MyCS, [R|T], X) ->
    X2 = send_txs2(R, MyCS, Txs),
    send_txs(Txs, MyCS, T, X2 ++ X).
send_txs2(_, [], []) -> [];
send_txs2(Checksum, [Checksum|_], [T|_]) -> [T];
send_txs2(Checksum, [_|CT], [_|T]) ->
    send_txs2(Checksum, CT, T).
    

is_in(X, [X|_]) -> true;
is_in(_, []) -> false;
is_in(X, [_|T]) -> 
    is_in(X, T).


tx_spam_handler([], _) ->
    io:fwrite("spam handler ended\n"),
    {ok, 0};
tx_spam_handler([Tx|T], IP) ->
    io:fwrite("tx spam handler\n"),
    case tx_pool_feeder:absorb(Tx) of
        timeout_error -> 
 %this means the tx failed in a way that makes it seem like it was included already.
            %we know that this tx is not in the tx pool.
 %if this tx is not in the most recent block or the tx pool, then we need to blacklist the sender.
            case request_frequency:doit(IP, 10) of
                ok -> tx_spam_handler(T, IP);
                _ -> 
                    io:fwrite("received expired tx\n"),
                    {ok, <<"stop spamming the server">>}
            end;
        error -> tx_spam_handler(T, IP);
        ok -> tx_spam_handler(T, IP);
        ERROR ->
            %unexpected error
            ERROR
    end;
tx_spam_handler(Tx, IP) ->
    io:fwrite({Tx, IP}).

