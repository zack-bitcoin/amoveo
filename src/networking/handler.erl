
-module(handler).

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

handle(Req, State) ->
    %{Length, Req2} = cowboy_req:body_length(Req),
    {ok, Data, Req3} = cowboy_req:body(Req),
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
-define(WORD, 10000000).%10 megabytes.
doit({pubkey}) -> {ok, keys:pubkey()};
%doit({height}) -> {ok, block_tree:height()};
%doit({total_coins}) -> {ok, block_tree:total_coins()};
doit({give_block, SignedBlock}) -> 
    block_absorber:doit(SignedBlock),
    {ok, 0};
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
    P2 = download_blocks:tuples2lists(P),
    {ok, P2};
doit({peers, Peers}) ->
    peers:add(Peers),
    {ok, 0};
doit({txs}) -> 
    {_,_,Txs} = tx_pool:data(),
    {ok, Txs};
doit({txs, Txs}) -> 
    download_blocks:absorb_txs(Txs),
    {ok, 0};
doit({id}) -> {ok, keys:id()};
doit({top}) -> 
    Top = block:read(top:doit()),
    Height = block:height(Top),
    {ok, Top, Height};
doit({test}) -> 
    {test_response};
doit({min_channel_ratio}) ->
    {ok, free_constants:min_channel_ratio()};
doit({new_channel, STx, SSPK}) ->
    unlocked = keys:status(),
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
    SPK = channel_manager:read(CID),
    {ok, SPK};
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
    Channels = trees:channels(Trees),
    {Amount, _, _} = spk:run(fast, SS, SPK, Height, 0, Accounts, Channels),
    {Tx, _} = channel_team_close_tx:make(CID, Accounts, Channels, Amount, Fee),
    tx_pool_feeder:absorb(keys:sign(STx, Accounts)),
    {ok, ok};
doit({locked_payment, SSPK}) ->
    R = channel_feeder:lock_spend(SSPK),
    {ok, R};
doit({channel_simplify, SS, SSPK}) ->
    Return = channel_feeder:simplify(SS, SSPK),
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
    
doit({dice, 1, Other, Commit, Amount}) ->
    %Eventually we need to charge them a big enough fee to cover the cost of watching for them to close the channel without us. 
    {ok, CD} = channel_manager:read(Other),
    [] = channel_feeder:script_sig_me(CD),
    [] = channel_feeder:script_sig_them(CD),
    {MyCommit, Secret} = secrets:new(),
    SSPK = channel_feeder:make_bet(Other, dice, [Amount, Commit, MyCommit], Secret),
    {ok, SSPK, MyCommit};
doit({dice, 2, ID, SSPK, SS}) ->
    channel_feeder:update_to_me(SSPK),
    io:fwrite("handler dice 2 "),
    disassembler:doit(SS),
    {SSPKsimple, MySecret} = channel_feeder:make_simplification(ID, dice, [SS]),
    {ok, SSPKsimple, MySecret};
doit({dice, 3, _ID, SSPK}) ->
    channel_feeder:update_to_me(SSPK),
    {ok, 0};
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
many_headers(M, N) when M < 1 -> [];
many_headers(Many, N) ->    
    [block:block_to_header(block:read_int(N))|
     many_headers(Many-1, N+1)].
    
    
