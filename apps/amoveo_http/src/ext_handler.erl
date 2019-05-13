-module(ext_handler).
-include("../../amoveo_core/src/records.hrl").

-export([init/3, handle/2, terminate/3, doit/1,
	send_txs/4]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '["test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    D = case request_frequency:doit(IP) of
	    ok ->
						%ok = request_frequency:doit(IP),
		%{ok, TimesPerSecond} = application:get_env(amoveo_core, request_frequency),
		%timer:sleep(round(1000/TimesPerSecond)),
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
			_ -> doit(A)
		    end,
		packer:pack(B);
	    _ -> 
		packer:pack({ok, <<"stop spamming the server">>})
	end,	    

    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
	       {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
doit({f, 1}) ->
    {ok, {block:hashes_per_block(),
	  block:hashrate_estimate(),
	  block:period_estimate()}};
doit({account, Pubkey}) -> 
    {ok, api:account(Pubkey)};
doit({pubkey}) -> {ok, keys:pubkey()};
doit({height}) -> {ok, block:height()};
doit({version}) -> {ok, version:doit(block:height())};
doit({version, 1}) -> 
    {ok, Version} = application:get_env(amoveo_core, db_version),
    {ok, Version};
doit({version, 2, N}) ->
    F = forks:get(N),
    {ok, F};
doit({give_block, Block}) -> %block can also be a list of blocks.
    io:fwrite("ext_handler receiving blocks\n"),
    %Response = block_absorber:save(Block),
    A = if
	    is_list(Block) -> Block;
	    true -> [Block]
	end,
    Response = block_organizer:add(A),
    R2 = if
	     is_atom(Response) -> 0;
	     true -> Response
	 end,
    {ok, R2};
doit({block, N}) when (is_integer(N) and (N > -1))->
    {ok, block:get_by_height(N)};
doit({blocks, Many, N}) -> 
    %true = Many < 60,
    %X = block_reader:doit(Many, N),
    X = block_db:read(Many, N),
    %X = many_blocks(Many, N),
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
doit({headers, Many, N}) -> 
    X = many_headers(Many, N),
    {ok, X};
doit({header}) -> {ok, headers:top()};
doit({peers}) ->
    P = peers:all(),
    P2 = amoveo_utils:tuples2lists(P),
    {ok, P2};
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
    X = tx_pool_feeder:absorb(Tx),
    Y = case X of
	    ok -> hash:doit(testnet_sign:data(Tx));
	    _ -> <<"error">>
		     end,
    {ok, Y};
doit({txs, Txs}) ->
    ok = tx_pool_feeder:absorb(Txs),
    {ok, 0};
doit({txs, 3, N}) ->
    B = block:get_by_height(N),
    Txs = tl(B#block.txs),
    Txids = lists:map(
	      fun(Tx) -> hash:doit(testnet_sign:data(Tx)) end, 
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
    Tx = testnet_sign:data(STx),
    SPK = testnet_sign:data(SSPK),
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
    Tx = testnet_sign:data(STx),
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
doit({proof, TreeName, ID, Hash}) ->
%here is an example of looking up the 5th governance variable. the word "governance" has to be encoded base64 to be a valid packer:pack encoding.
%curl -i -d '["proof", "Z292ZXJuYW5jZQ==", 5, Hash]' http://localhost:8080 
    Trees = (block:get_by_hash(Hash))#block.trees,%this line failed.b
    TN = trees:name(TreeName),
    Root = trees:TN(Trees),
    %io:fwrite(packer:pack([ext_handler_proof2, TreeName, ID, Root])),
    {RootHash, Value, Proof} = TN:get(ID, Root),
    Proof2 = proof_packer(Proof),
    {ok, {return, trees:serialized_roots(Trees), RootHash, Value, Proof2}};
doit({list_oracles}) ->
    {ok, order_book:keys()};
doit({oracle, Y}) ->
    %X = base64:decode(Y),
    X = Y,
    Oracle = trees:get(oracles, X),
    QH = element(4, Oracle),
    {ok, Question} = oracle_questions:get(QH),
    Z = case order_book:data(X) of
            {ok, OB} -> OB;
            error -> 0
        end,
    %{ok, OB} = order_book:data(X),
    {ok, {Z, Question}};
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
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    Order = order_book:make_order(Account, Price, Type, Amount),
    order_book:add(Order, OID),
    {ok, SSPK2};
doit({cancel_trade, TheirPub, N, SSPK}) ->
    SSPK2 = channel_feeder:cancel_trade_server(N, TheirPub, SSPK),
    {ok, SSPK2};
doit({combine_cancel_assets, TheirPub, SSPK}) ->
    SSPK2 = channel_feeder:combine_cancel_assets_server(TheirPub, SSPK),
    {ok, SSPK2};
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
    case H#header.height of
	N -> H;
	_ -> 
	    {ok, Child} = headers:read(H#header.prev_hash),
	    get_header_by_height(N, Child)
    end.
	    
many_headers(Many, X) ->
    %io:fwrite("many headers "), 
    %io:fwrite(packer:pack([Many, X])), 
    %io:fwrite("\n"),
    Z = max(0, X + Many - 1),
    H = headers:top(),
    case (H#header.height) >= (X) of
	false -> [];
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
    
