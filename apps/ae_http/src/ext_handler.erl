
-module(ext_handler).
-include("../../ae_core/src/spk.hrl").

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    request_frequency:doit(IP),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
doit({account, Pubkey}) -> 
    {ok, api:account(Pubkey)};
doit({pubkey}) -> {ok, keys:pubkey()};
doit({height}) -> {ok, block:height()};
doit({give_block, Block}) -> 
    block_absorber:save(Block),
    {ok, 0};
doit({block, N}) when (is_integer(N) and (N > -1))->
    {ok, block:get_by_height(N)};
doit({blocks, Many, N}) -> 
    X = many_blocks(Many, N),
    {ok, X};
doit({header, N}) -> 
    {ok, block:block_to_header(block:get_by_height(N))};
doit({headers, Many, N}) -> 
    X = many_headers(Many, N),
    {ok, X};
doit({header}) -> {ok, headers:top()};
doit({peers}) ->
    P = peers:all(),
    P2 = ae_utils:tuples2lists(P),
    {ok, P2};
doit({peers, Peers}) ->
    peers:add(Peers),
    {ok, 0};
doit({txs}) -> 
    {_,_,Txs} = tx_pool:data(),
    {ok, Txs};
doit({txs, Txs}) ->
    tx_pool_feeder:absorb(Txs),
    {ok, 0};
doit({top}) -> 
    Top = block:top(),
    Height = block:height(Top),
    {ok, Top, Height};
doit({test}) -> 
    {test_response};
doit({test, N}) ->
    M = 8 * N,
    {test_response, <<0:M>>};
doit({min_channel_ratio}) ->
    application:get_env(ae_core, min_channel_ratio);
doit({spend_tx, Amount, Fee, From, To}) ->
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = spend_tx:make(To, Amount, Fee, From, Trees),
    {ok, Tx};
doit({create_account_tx, Amount, Fee, From, To}) ->
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = create_account_tx:new(To, Amount, Fee, From, Trees),
    {ok, Tx};
doit({new_channel_tx, Acc1, Acc2, B1, B2, Delay, Fee}) ->
    {Trees, _, _} = tx_pool:data(),
    Channels = trees:channels(Trees),
    CID = api:find_id(channels, Channels),
    {Tx, _} = new_channel_tx:make(CID, Trees, Acc1, Acc2, B1, B2, Delay, Fee),
    {ok, Tx};
doit({time_value}) ->
    application:get_env(ae_core, time_value);
doit({new_channel, STx, SSPK, Expires}) ->
    unlocked = keys:status(),
    Tx = testnet_sign:data(STx),
    SPK = testnet_sign:data(SSPK),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    undefined = channel_feeder:cid(Tx),
    %true = new_channel_tx:good(Tx),%checks the min_channel_ratio.
    CFee = SPK#spk.amount,
    Bal1 = new_channel_tx:bal1(Tx),
    Bal2 = new_channel_tx:bal2(Tx),
    Delay = new_channel_tx:delay(Tx),
    LifeSpan = Expires - api:height(),
    {ok, TV} = application:get_env(ae_core, time_value),
    CFee = TV * (Delay + LifeSpan) * (Bal1 + Bal2) div 100000000,
    true = CFee < Bal1,%make sure they can afford the fee.
    true = channel_feeder:new_channel_check(Tx), %make sure we are not already storing a channel with this same partner.
    SSTx = keys:sign(STx),
    tx_pool_feeder:absorb(SSTx),
    S2SPK = keys:sign(SPK),
    channel_feeder:new_channel(Tx, SSPK, Expires),
    {ok, [SSTx, S2SPK]};
doit({grow_channel, Stx}) ->
    Tx = testnet_sign:data(Stx),
    true = grow_channel_tx:good(Tx),%checks the min_channel_ratio
    SStx = keys:sign(Stx),
    tx_pool_feeder:absorb(SStx),
    {ok, ok};
doit({spk, TheirPub})->
    {ok, CD} = channel_manager:read(TheirPub),
    ME = keys:sign(channel_feeder:me(CD)),
    {ok, [CD, ME]};
doit({channel_payment, SSPK, Amount}) ->
    R = channel_feeder:spend(SSPK, Amount),
    {ok, R};
doit({close_channel, CID, PeerId, SS, STx}) ->
    channel_feeder:close(SS, STx),
    Tx = testnet_sign:data(STx),
    Fee = channel_team_close_tx:fee(Tx),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = channel_feeder:me(CD),
    Height = (headers:top())#header.height,
    {Trees,_,_} = tx_pool:data(),
    {Amount, _, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
    {Tx, _} = channel_team_close_tx:make(CID, Trees, Amount, Fee),
    SSTx = keys:sign(STx),
    tx_pool_feeder:absorb(SSTx),
    {ok, SSTx};
doit({locked_payment, SSPK, Amount, Fee, Code, Sender, Recipient, ESS}) ->
    true = size(ESS) < 200,
    R = channel_feeder:lock_spend(SSPK, Amount, Fee, Code, Sender, Recipient, ESS),
    {ok, R};
doit({learn_secret, From, Secret, Code}) ->
    {ok, OldCD} = channel_manager:read(From),
    secrets:add(Code, Secret),
    SS = channel_feeder:script_sig_me(OldCD),
    CFME = channel_feeder:me(OldCD),
    {NewSS, SPK, _Secrets, SSThem} = 
	spk:bet_unlock(CFME, SS),
    if
	NewSS == SS -> ok;
	true -> 
	    NewCD = channel_feeder:new_cd(
		      SPK, channel_feeder:them(OldCD),
		      NewSS, SSThem,
		      channel_feeder:cid(OldCD),
                      channel_feeder:expiration(OldCD)),
	    channel_manager:write(From, NewCD),
	    {ok, Current} = arbitrage:check(Code),
	    IDS = minus(Current, From),
	    channel_feeder:bets_unlock(IDS)
    end,
    {ok, 0};
doit({channel_sync, From, SSPK}) ->
    Return = channel_feeder:update_to_me(SSPK, From),
    {ok, Return};
doit({bets}) ->
    free_variables:bets();
doit({proof, TreeName, ID, Hash}) ->
%here is an example of looking up the 5th governance variable. the word "governance" has to be encoded base64 to be a valid packer:pack encoding.
%curl -i -d '["proof", "Z292ZXJuYW5jZQ==", 5]' http://localhost:8040
    Trees = block:trees(block:get_by_hash(Hash)),
    TN = trees:name(TreeName),
    Root = trees:TN(Trees),
    {RootHash, Value, Proof} = TN:get(ID, Root),
    Proof2 = proof_packer(Proof),
    {ok, {return, trees:serialized_roots(Trees), RootHash, Value, Proof2}};
doit({list_oracles}) ->
    {ok, order_book:keys()};
doit({oracle, X}) ->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(X, Oracles),
    {ok, Question} = oracle_questions:get(oracles:question(Oracle)),
    {ok, OB} = order_book:data(X),
    {ok, {OB, Question}};
doit({market_data, OID}) ->
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    {ok, {Expires, keys:pubkey(), Period}};
doit({trade, Account, Price, Type, Amount, OID, SSPK, Fee}) ->
    %make sure they pay a fee in channel for having their trade listed. 
    BetLocation = constants:oracle_bet(),
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    {ok, CD} = channel_manager:read(Account),
    true = Expires < channel_feeder:expiration(CD),
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
many_blocks(M, _) when M < 1 -> [];
many_blocks(Many, N) ->
    H = block:height(),
    if N > H -> [];
       true ->
            B = block:get_by_height(N),
            case B of
                empty -> many_blocks(Many-1, N+1);
                _ ->
                    [B|many_blocks(Many-1, N+1)]
            end
    end.
many_headers(M, N) ->
    B = many_blocks(M, N),
    blocks2headers(B).
blocks2headers([]) -> [];
blocks2headers([B|T]) ->
    [block:block_to_header(B)|
    blocks2headers(T)].
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
