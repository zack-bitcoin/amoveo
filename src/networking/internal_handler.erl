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
-define(POP, <<1,6,3,87,3,5>>).
doit({sign, Tx}) -> 
    {Accounts, _,_,_} = tx_pool:data(),
    {ok, keys:sign(Tx, Accounts)};


doit({balance}) ->
    {ok, accounts:balance(block_tree:account(keys:id()))};
doit({create_account, Address, Amount, ID}) -> 
    easy:create_account(Address, Amount, ID),
    %tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Address, Amount, Fee, ID, Accounts))),
    {ok, ok};
doit({spend, To, Amount}) ->
    easy:spend(To, Amount),
    {ok, ok};
doit({mine_block, Many, Times}) -> 
    block:mine_blocks(Many, Times);
doit({close_channel, IP, Port}) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = testnet_sign:data(channel_feeder:them(CD)),
    {Accounts,Channels,_,_} = tx_pool:data(),
    Height = block:height(block:read(top:doit())),
    SS = channel_feeder:script_sig_them(CD),
    {Amount, _} = spk:run(fast, SS, SPK, Height, 0, Accounts, Channels),
    CID = spk:cid(SPK),
    Fee = free_constants:tx_fee(),
    {Tx, _} = channel_team_close_tx:make(CID, Accounts, Channels, Amount, Fee),
    STx = keys:sign(Tx, Accounts),
    talker:talk({close_channel, CID, keys:id(), SS, STx}, IP, Port),
    {ok, 0};
doit({dice, Amount, IP, Port}) ->
    {ok, Other} = talker:talk({id}, IP, Port),
    {Commit, Secret} = secrets:new(),
    MyID = keys:id(),
    {ok, SSPK, OtherCommit} = talker:talk({dice, 1, MyID, Commit, Amount}, IP, Port),
    SSPK2 = channel_feeder:agree_bet(dice, SSPK, [Amount, Commit, OtherCommit], Secret),%should store partner's info into channel manager.
    %ok;%comment below this line for testing channel_slash txs.
    SPK = testnet_sign:data(SSPK2),
    SS1 = dice:make_ss(SPK, Secret),
    {ok, SSPKsimple, TheirSecret} = talker:talk({dice, 2, MyID, SSPK2, SS1}, IP, Port), %SSPKsimple doesn't include the bet. the result of the bet instead is recorded.
    %ok;%comment below this line for testing channel_slash txs.
    SS = dice:resolve_ss(SPK, Secret, TheirSecret),%
    SSPK2simple = channel_feeder:agree_simplification(dice, SSPKsimple, [SS]),
    SPKsimple = testnet_sign:data(SSPKsimple),
    SPKsimple = testnet_sign:data(SSPK2simple),
    talker:talk({dice, 3, MyID, SSPK2simple}, IP, Port);
doit({channel_solo_close, Other}) ->
    Fee = free_constants:tx_fee(),
    {Accounts,Channels,_,_} = tx_pool:data(),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SS = channel_feeder:script_sig_them(CD),
    {Tx, _} = channel_solo_close:make(keys:id(), Fee, keys:sign(SSPK, Accounts), SS, Accounts, Channels),
    STx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(STx),
    {ok, ok};
    
doit({add_peer, IP, Port}) ->
    peers:add(IP, Port);
doit({sync, IP, Port}) ->
    MyHeight = block:height(block:read(top:doit())),
    download_blocks:sync(IP, Port, MyHeight);
doit({top}) -> 
    Top = block:read(top:doit()),
    Height = block:height(Top),
    TopHash = block:hash(Top),
    {ok, TopHash, Height};
doit({pubkey}) -> {ok, keys:pubkey()};
doit({address}) -> {ok, testnet_sign:pubkey2address(keys:pubkey())};
doit({address, X}) -> {ok, testnet_sign:pubkey2address(X)};
doit({id}) -> {ok,  keys:id()};
doit({channel_ids, Partner}) -> {ok, channel_manager:id(Partner)};
doit({new_pubkey, Password}) -> 
    keys:new(Password);
doit({test}) -> 
    {test_response};
doit({channel_spend, IP, Port, Amount}) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    %CID = peers:cid(peers:read(IP, Port)),
    %io:fwrite("CID is "),
    %io:fwrite(integer_to_list(CID)),
    %io:fwrite("\n"),
    {ok, CD} = channel_manager:read(PeerId),
    OldSPK = testnet_sign:data(channel_feeder:them(CD)),
    ID = keys:id(),
    {Accounts, _,_,_} = tx_pool:data(),
    SPK = spk:get_paid(OldSPK, ID, -Amount), 
    Payment = keys:sign(SPK, Accounts),
    %channel_feeder:update_to_me(SPK),
    M = {channel_payment, Payment, Amount},
    {ok, Response} = talker:talk(M, IP, Port),
    %maybe verify the signature of Response here?
    channel_feeder:spend(Response, -Amount),
    {ok, ok};
    
doit({new_channel, IP, Port, CID, Bal1, Bal2, Rent, Fee, Delay}) ->
    %make sure we don't already have a channel with this peer.
    unlocked = keys:status(),
    undefined = peers:cid(peers:read(IP, Port)),
    Acc1 = keys:id(),
    {ok, Acc2} = talker:talk({id}, IP, Port),
    Entropy = channel_feeder:entropy([Acc1, Acc2]) + 1,
    {Accounts, _,_,_} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Accounts, Acc1, Acc2, Bal1, Bal2, Rent, Entropy, Fee, Delay),
    SPK = new_channel_tx:spk(Tx, free_constants:channel_delay()),
    STx = keys:sign(Tx, Accounts),
    SSPK = keys:sign(SPK, Accounts),
    Msg = {new_channel, STx, SSPK},
    {ok, SSTx, S2SPK} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    peers:set_cid(IP, Port, CID),
    channel_feeder:new_channel(Tx, S2SPK, Accounts),
    {ok, ok};
doit({channel_keys}) -> {ok, channel_manager:keys()};
doit({block_tree_account, Id}) -> {ok, block_tree:account(Id)};
doit({halt}) -> {ok, testnet_sup:stop()};
doit({key_status}) -> {ok, list_to_binary(atom_to_list(keys:status()))};
doit({key_unlock, Password}) -> {ok, list_to_binary(atom_to_list(keys:unlock(Password)))};
doit({keys_id_update, ID}) -> 
    keys:update_id(ID),
    {ok, 0};
doit({key_new, Password}) -> 
    keys:new(Password),
    {ok, 0};

doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {error}.
