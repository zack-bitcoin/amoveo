
-module(internal_handler).

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", packer:pack({pubkey})}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    %io:fwrite("internal handler "),
    %io:fwrite(Data),
    %io:fwrite("\n"),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, D, Req),
    {ok, Req2, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
%-define(POP, <<1,6,3,87,3,5>>).

%doit({sign, Tx}) -> 
%    {Trees,_,_} = tx_pool:data(),
%    Accounts = trees:accounts(Trees),
%    {ok, keys:sign(Tx, Accounts)};


doit({Key}) ->
    {ok, easy:Key()};
doit({Key, Arg1}) ->
    {ok, easy:Key(Arg1)};
doit({Key, Arg1, Arg2}) ->
    {ok, easy:Key(Arg1, Arg2)};
doit({Key, A, B, C}) ->
    {ok, easy:Key(A, B, C)};
doit({Key, A, B, C, D}) ->
    {ok, easy:Key(A, B, C, D)};
doit({Key, A, B, C, D, E}) ->
    {ok, easy:Key(A, B, C, D, E)};
doit({Key, A, B, C, D, E, F}) ->
    {ok, easy:Key(A, B, C, D, E, F)};
doit({Key, A, B, C, D, E, F, G}) ->
    {ok, easy:Key(A, B, C, D, E, F, G)};
%doit({balance}) ->
%    {ok, accounts:balance(block_tree:account(keys:id()))};
%doit({create_account, Address, Amount}) -> 
%    easy:create_account(Address, Amount),
    %tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Address, Amount, Fee, ID, Accounts))),
%    {ok, ok};
%doit({spend, To, Amount}) ->
%    easy:spend(To, Amount),
%    {ok, ok};
%doit({mine_block}) -> 
%    block:mine_blocks(1, 100000000);
%doit({mine_block, Many, Times}) -> 
%    block:mine_blocks(Many, Times);
%doit({close_channel, IP, Port}) ->
%    easy:channel_close(IP, Port
%    {ok, PeerId} = talker:talk({id}, IP, Port),
%    {ok, CD} = channel_manager:read(PeerId),
%    SPK = testnet_sign:data(channel_feeder:them(CD)),
%    {Trees,_,_} = tx_pool:data(),
%    Height = block:height(block:read(top:doit())),
%    SS = channel_feeder:script_sig_them(CD),
%    {Amount, _, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
%    CID = spk:cid(SPK),

%%    Fee = free_constants:tx_fee(),
%    {Tx, _} = channel_team_close_tx:make(CID, Trees, Amount, [], Fee),
%    Accounts = trees:accounts(Trees),
%    STx = keys:sign(Tx, Accounts),
%    {ok, SSTx} = talker:talk({close_channel, CID, keys:id(), SS, STx}, IP, Port),
%tx_pool_feeder:absorb(SSTx),
%    {ok, 0};
%doit({channel_solo_close, Other}) ->
%    Fee = free_constants:tx_fee(),
%    {Trees,_,_} = tx_pool:data(),
%    Accounts = trees:accounts(Trees),
%    {ok, CD} = channel_manager:read(Other),
%    SSPK = channel_feeder:them(CD),
%    SS = channel_feeder:script_sig_them(CD),
%    {Tx, _} = channel_solo_close:make(keys:id(), Fee, keys:sign(SSPK, Accounts), SS, Trees),
%    STx = keys:sign(Tx, Accounts),
%    tx_pool_feeder:absorb(STx),
%    {ok, ok};
    
%doit({add_peer, IP, Port}) ->
%    peers:add(IP, Port);
%doit({sync, IP, Port}) ->
%    MyHeight = block:height(block:read(top:doit())),
%    download_blocks:sync(IP, Port, MyHeight);
%doit({top}) -> 
%    Top = block:read(top:doit()),
%    Height = block:height(Top),
%    TopHash = block:hash(Top),
%    {ok, TopHash, Height};
%doit({pubkey}) -> {ok, keys:pubkey()};
%doit({address}) -> {ok, testnet_sign:pubkey2address(keys:pubkey())};
%doit({address, X}) -> {ok, testnet_sign:pubkey2address(X)};
%doit({id}) -> {ok,  keys:id()};
%doit({channel_ids, Partner}) -> {ok, channel_manager:id(Partner)};
%doit({new_pubkey, Password}) -> 
%    keys:new(Password);
%doit({test}) -> 
%    {test_response};
%doit({channel_spend, IP, Port, Amount}) ->
%    easy:channel_spend(IP, Port, Amount),
%    {ok, 0};
    
%doit({new_channel_with_server, IP, Port, CID, Bal1, Bal2, Fee, Delay}) ->
    %make sure we don't already have a channel with this peer.
%    easy:new_channel_with_server(IP, Port, CID, Bal1, Bal2, Fee, Delay),
%    {ok, 0};
%doit({learn_secret, Secret, Code}) ->
%    secrets:add(Code, Secret),
%    {ok, 0};
%doit({pull_channel_state, IP, Port}) ->
    %If your channel partner has a script sig that you don't know about, this is how you download it
%    easy:pull_channel_state(IP, Port),
%    {ok, 0};
%doit({bet_unlock, IP, Port}) ->
    %look at the list of contracts that can be spent, see if the answers are in secrets.erl
%    easy:bet_unlock(IP, Port),
%    {ok, 0};
    
%doit({lightning_spend, IP, Port, Recipient, Amount, Fee}) ->
    %payment is routed through this server, and to the recipient.
%    easy:lightning_spend(IP, Port, Recipient, Amount, Fee),
%    {ok, 0};
%doit({lightning_spend, IP, Port, Recipient, Amount, Fee, Code, SS}) ->
%    easy:lightning_spend(IP, Port, Recipient, Amount, Fee, Code, SS),
%    {ok, 0};
%doit({channel_keys}) -> {ok, channel_manager:keys()};
%doit({halt}) -> {ok, testnet_sup:stop()};
%doit({key_status}) -> {ok, list_to_binary(atom_to_list(keys:status()))};
%doit({key_unlock, Password}) -> {ok, list_to_binary(atom_to_list(keys:unlock(Password)))};
%doit({keys_id_update, ID}) -> 
%    keys:update_id(ID),
%    {ok, 0};
%doit({key_new, Password}) -> 
%    keys:new(Password),
%    {ok, 0};

doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {error}.
    
