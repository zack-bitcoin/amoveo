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
    {ok, block:pow_block(block:read_int(N))};
    %{ok, block_tree:read_int(N)};
doit({tophash}) -> {ok, top:doit()};
doit({recent_hash, H}) -> {ok, block_tree:is_key(H)};
doit({peers}) ->
    P = peers:all(),
    P2 = download_blocks:tuples2lists(P),
    {ok, P2};
doit({peers, Peers}) ->
    peers:add(Peers),
    {ok, 0};
%doit({tx_absorb, Tx}) -> 
%    {ok, tx_pool_feeder:absorb(Tx)};
doit({txs}) -> 
    {_,_,_,Txs} = tx_pool:data(),
    {ok, Txs};
doit({txs, Txs}) -> 
    download_blocks:absorb_txs(Txs),
    {ok, 0};


doit({id}) -> {ok, keys:id()};

%doit({balance, ID}) ->
%    {ok, accounts:balance(block_tree:account(ID))};
doit({top}) -> 
    Top = block:read(top:doit()),
    Height = block:height(Top),
    %TopHash = block:hash(Top),
    {ok, Top, Height};

doit({create_account, Pub, Amount, Fee}) -> 
    {ok, create_account_tx:create_account(Pub, Amount, Fee)};
doit({spend, To, Amount, Fee}) ->
    spend_tx:spend(To, Amount, Fee);
doit({create_channel, Partner, Bal1, Bal2, Type, Fee}) ->
    to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee);
doit({to_channel, IP, Port, Inc1, Inc2, Fee}) ->
    {ok, ServerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(ServerId)),
    to_channel_tx:to_channel(ChId, Inc1, Inc2, Fee);
doit({close_channel, ChId, Amount, Nonce, Fee}) ->
    channel_block_tx:close_channel(ChId, Amount, Nonce, Fee);
doit({test}) -> 
    {test_response};
%doit({account, Id}) -> {ok, account:read(Id)};
doit(X) ->
    io:fwrite("I can't handle this \n"),
    io:fwrite(packer:pack(X)), %unlock2
    {error}.
    

    
