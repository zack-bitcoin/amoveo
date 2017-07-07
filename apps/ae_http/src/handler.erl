-module(handler).

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    %{Length, Req2} = cowboy_req:body_length(Req),
    %{ok, X, Req1}
	%= cowboy_req:parse_header(<<"te">>, Req),
    {ok, Data, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    request_frequency:doit(IP),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    %io:fwrite("response is "),
    %io:fwrite(D),
    %io:fwrite("\n"),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
%-define(WORD, 10000000).%10 megabytes.
doit({pubkey}) -> {ok, keys:pubkey()};
%doit({height}) -> {ok, block_tree:height()};
%doit({total_coins}) -> {ok, block_tree:total_coins()};
doit({give_block, SignedBlock}) -> 
    %true = block:height(SignedBlock) < easy:height() + 2,
    block_absorber:doit(SignedBlock),
    {ok, 0};
doit({block, N, Many}) -> 
    {ok, block:read_many(N, Many)};
doit({block, N}) -> 
    {ok, block:read_int(N)};
doit({header, N}) -> 
    {ok, block:block_to_header(block:read_int(N))};
doit({headers, Many, N}) -> 
    X = many_headers(Many, N),
    {ok, X};
    %{ok, block_tree:read_int(N)};
doit({tophash}) -> {ok, top:doit()};
%doit({recent_hash, H}) -> {ok, block_tree:is_key(H)};
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
doit({id}) -> {ok, keys:id()};
doit({top}) -> 
    Top = block:read(top:doit()),
    Height = block:height(Top),
    {ok, Top, Height};
doit({test}) -> 
    {test_response};
doit({test, N}) ->
    M = 8 * N,
    {test_response, <<0:M>>};
doit({min_channel_ratio}) ->
    application:get_env(ae_core, min_channel_ratio);
doit({new_channel, STx, SSPK}) ->
    unlocked = keys:status(),
    %io:fwrite(STx),
    Tx = testnet_sign:data(STx),
    SPK = testnet_sign:data(SSPK),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    undefined = channel_feeder:cid(Tx),
    true = new_channel_tx:good(Tx),%checks the min_channel_ratio.
    true = channel_feeder:new_channel_check(Tx), %make sure we aren't already storing a channel with this same CID/partner combo. Also makes sure that we aren't reusing entropy.
    SSTx = keys:sign(STx, Accounts),
    tx_pool_feeder:absorb(SSTx),
    S2SPK = keys:sign(SPK, Accounts),
    channel_feeder:new_channel(Tx, SSPK, Accounts),
    %easy:sync(),
    {ok, SSTx, S2SPK};
doit({grow_channel, Stx}) ->
    Tx = testnet_sign:data(Stx),
    true = grow_channel_tx:good(Tx),%checks the min_channel_ratio
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    SStx = keys:sign(Stx, Accounts),
    tx_pool_feeder:absorb(SStx),
    {ok, ok};
doit({spk, CID})->
    {ok, CD} = channel_manager:read(CID),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    ME = keys:sign(channel_feeder:me(CD), Accounts),
    Out = {ok, CD, ME},
    Out = packer:unpack(packer:pack(Out)),
    Out;
doit({channel_payment, SSPK, Amount}) ->
    R = channel_feeder:spend(SSPK, Amount),
    {ok, R};
doit({close_channel, CID, PeerId, SS, STx}) ->
    channel_feeder:close(SS, STx),
    Tx = testnet_sign:data(STx),
    Fee = channel_team_close_tx:fee(Tx),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = channel_feeder:me(CD),
    Height = block:height(block:read(top:doit())),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Amount, _, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
    Shares = [],
    {Tx, _} = channel_team_close_tx:make(CID, Trees, Amount, Shares, Fee),
    SSTx = keys:sign(STx, Accounts),
    tx_pool_feeder:absorb(SSTx),
    {ok, SSTx};
doit({locked_payment, SSPK, Amount, Fee, Code, Sender, Recipient, ESS}) ->
    true = size(ESS) < 200,
    R = channel_feeder:lock_spend(SSPK, Amount, Fee, Code, Sender, Recipient, ESS),
    {ok, R};
doit({learn_secret, From, Secret, Code}) ->
    {ok, OldCD} = channel_manager:read(From),
    %check that code is actually used in the channel state.
    secrets:add(Code, Secret),
    
    SS = channel_feeder:script_sig_me(OldCD),
    CFME = channel_feeder:me(OldCD),
    {NewSS, SPK, _Secrets, SSThem} = 
	spk:bet_unlock(CFME, SS),
    io:fwrite("learn secret new ss is "),
    io:fwrite(packer:pack(NewSS)),
    if
	NewSS == SS -> ok;
	true -> 
	    NewCD = channel_feeder:new_cd(
		      SPK, channel_feeder:them(OldCD),
		      NewSS, SSThem,
		      channel_feeder:entropy(OldCD),
		      channel_feeder:cid(OldCD)),
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
doit({proof, TreeName, ID}) ->
%here is an example of looking up the 5th governance variable. the word "governance" has to be encoded base64 to be a valid packer:pack encoding.
%curl -i -d '["proof", "Z292ZXJuYW5jZQ==", 5]' http://localhost:8040
    {Trees, _, _} = tx_pool:data(),
    TN = trees:name(TreeName),
    Root = trees:TN(Trees),
    {RootHash, Value, Proof} = TN:get(ID, Root),
    Proof2 = proof_packer(Proof),
    {ok, {return, RootHash, Value, Proof2}};
doit({market_data, OID}) ->
    OB = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    {ok, {Expires, keys:pubkey(), Period}};
doit({trade, Account, Price, Type, Amount, OID, SSPK, Fee}) ->
    %make sure they pay a fee in channel for having their trade listed. 
    %make sure they paid enough to afford the shares.
    BetLocation = constants:oracle_bet(),
    OB = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    SC = market:market_smart_contract(BetLocation, OID, Type, Expires, Price, keys:pubkey(), Period, Amount, OID),
    SSPK2 = channel_feeder:trade(Account, Price, Type, Amount, OID, SSPK, Fee),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    Order = order_book:make_order(Account, Price, Type, Amount),
    order_book:add(Order, OID),
    {ok, SSPK2};
doit({remove_trade, AccountID, Price, Type, Amount, OID, SSPK}) ->
    %make sure they signed.
    %make sure they paid enough of a fee.
    %give them their money back
    %don't remove trades that are already being matched.
    ok;
    
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
 
    %Proof2 = list_to_tuple([proof|tuple_to_list(Proof)]),
many_headers(M, _) when M < 1 -> [];
many_headers(Many, N) ->    
    [block:block_to_header(block:read_int(N))|
     many_headers(Many-1, N+1)].
    
    
minus([T|X], T) -> X;
minus([A|T], X) -> [A|minus(T, X)].
