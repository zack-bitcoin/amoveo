-module(internal_handler).

-export([init/3, handle/2, terminate/3, doit/1, got_secret/3]).
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
    %tx_pool_feeder:absorb(keys:sign(spend_tx:spend(To, Amount, Fee)));
    {ok, ok};
doit({mine_block, Many, Times}) -> 
    %{_,_,_,Txs} = tx_pool:data(),
    block:mine_blocks(Many, Times);
    %Block = block:make(top:doit(), Txs, keys:id()),
    %PowBlock = block:mine(Block, 10000000),
    %block_absorber:doit(PowBlock);
%doit({create_channel, Partner, Bal1, Bal2, Type, Fee}) ->
%    keys:sign(to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee));
%doit({to_channel, IP, Port, Inc1, Inc2, Fee}) ->
%    {ok, ServerId} = talker:talk({id}, IP, Port),
%    ChId = hd(channel_manager:id(ServerId)),
%    SignedTx = keys:sign(to_channel_tx:to_channel(ChId, Inc1, Inc2, Fee)),
%    talker:talk({to_channel, SignedTx}, IP, Port);
%doit({close_channel, ChId, Amount, Nonce, Fee}) ->
    %keys:sign(channel_block_tx:close_channel(ChId, Amount, Nonce, Fee));
doit({close_channel, IP, Port}) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    %find Amount from channel manager.
    {ok, CD} = channel_manager:read(PeerId),
    SPK = testnet_sign:data(channel_feeder:them(CD)),
    {Accounts,Channels,_,_} = tx_pool:data(),
    Height = block:height(block:read(top:doit())),
    SS = channel_feeder:script_sig(CD),
    {Amount, _} = spk:run(SS, SPK, Height, 0, Accounts, Channels),
    %state = new_state(),
    %{BetAmount, _,_,_} = chalang:run(SS, spk:bets(SPK), spk:time_gas(SPK), spk:space_gas(SPK), constants:fun_limit(), constants:var_limit(), State),
    %Amount = spk:amount(SPK) + BetAmount,
    CID = spk:cid(SPK),
    Fee = free_constants:tx_fee(),
    {Tx, _} = channel_team_close_tx:make(CID, Accounts, Channels, Amount, Fee),
    STx = keys:sign(Tx, Accounts),
    talker:talk({close_channel, CID, keys:id(), SS, STx}, IP, Port),
    {ok, 0};
doit({channel_bet, "dice", Vars, IP, Port}) ->
    {ok, Other} = talker:talk({id}, IP, Port),

    SSPK = channel_feeder:absorb_bet(Other, dice, Vars),
    SSPK2 = talker:talk({bet, "dice", SSPK, Vars}, IP, Port),
    io:fwrite(packer:pack(SSPK2));
    %io:fwrite("channel bet here"),
    %channel_feeder:bet(dice, SSPK2, Vars);
    
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
doit({get_msg, IP, Port}) ->
    {ok, ServerId} = talker:talk({id}, IP, Port),
    Out= case talker:talk({pop_hashes, keys:id()}, IP, Port) of
	     {ok, <<"empty">>} -> <<"no messages">>;
	     {ok, T} -> absorb_msgs(T, IP, Port, ServerId)
	 end,
    {ok, Out};
doit({read_msg, Id, Index}) -> {ok, inbox:read(Id, Index)};
doit({msg_ids, Id}) -> {ok, inbox:msg_ids(Id)};
doit({msg_peers}) -> {ok, inbox:peers()};
doit({msg_delete, Id, Index}) -> {ok, inbox:delete(Id, Index)};
doit({msg_delete, Id}) -> {ok, inbox:delete(Id)};
doit({register, IP, Port}) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    {ok, Amount} = talker:talk({register_cost}, IP, Port),
    Payment = channel_manager_feeder:spend(ChId, Amount),
    Msg = {register, Payment, keys:id()},
    talker:talk(Msg, IP, Port),
    {ok, ok};
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
    Payment = keys:sign(spk:get_paid(OldSPK, ID, -Amount), Accounts),
    M = {channel_payment, Payment, Amount},
    {ok, Response} = talker:talk(M, IP, Port),
    %maybe verify the signature of Response here?
    channel_feeder:spend(Response, -Amount),
    {ok, ok};
    
%doit({send_msg, IP, Port, To, M, Seconds}) ->
%    Acc = block_tree:account(To),
%%    Pub = accounts:pub(Acc),
%    Msg = encryption:send_msg(M, Pub),
%    {ok, Amount} = talker:talk({mail_cost, size(Msg), Seconds}),
%    {ok, PeerId} = talker:talk({id}, IP, Port),
%    ChId = hd(channel_manager:id(PeerId)), 
%    Payment = channel_manager_feeder:spend(ChId, Amount),
%    Foo = {send, Payment, keys:id(), To, Msg, Seconds},
%    {ok, Response} = talker:talk(Foo, IP, Port),
%    channel_manager_feeder:recieve(ChId, -Amount, Response),
%    inbox:get_helper(To, M),
%    {ok, ok};
doit({new_channel, IP, Port, CID, Bal1, Bal2, Rent, Fee}) ->
    %make sure we don't already have a channel with this peer.
    undefined = peers:cid(peers:read(IP, Port)),
    Acc1 = keys:id(),
    {ok, Acc2} = talker:talk({id}, IP, Port),
    Entropy = channel_feeder:entropy([Acc1, Acc2]) + 1,
    {Accounts, _,_,_} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Accounts, Acc1, Acc2, Bal1, Bal2, Rent, Entropy, Fee),
    SPK = new_channel_tx:spk(Tx, free_constants:channel_delay()),
    STx = keys:sign(Tx, Accounts),
    SSPK = keys:sign(SPK, Accounts),
    Msg = {new_channel, STx, SSPK},
    {ok, SSTx, S2SPK} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    peers:set_cid(IP, Port, CID),
    channel_feeder:new_channel(Tx, S2SPK, Accounts),
    {ok, ok};
doit({lightning_spend, IP, Port, Partner, Amount}) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)),
    SecretHash = secrets:new(),
    Payment = channel_manager:new_hashlock(PeerId, Amount, SecretHash),
    Bet = channel_block_tx:bet_code(hd(channel_block_tx:bets(testnet_sign:data(Payment)))),
    BetHash = hash:doit(Bet),
    channel_partner:store(ChId, Payment),
    Acc = block_tree:account(Partner),
    Secret = secrets:read(SecretHash),
    Msg = encryption:send_msg({secret, Secret}, accounts:pub(Acc)),
    {ok, SignedCh} = talker:talk({locked_payment, keys:id(), Partner, Payment, Amount, SecretHash, BetHash, Msg}, IP, Port),
    channel_manager_feeder:spend_locked_payment(ChId, SignedCh, Amount, SecretHash),
    {ok, SecretHash};
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
%doit({make_channel, IP, Port, MyBal, OtherBal, Rent, Fee, CID}) ->
    %CID = channel:empty_id(),
%{Accounts, _,_,_} = tx_pool:data(),
%{ok, Acc2} = talker:talk({id}, IP, Port),
%    ID = keys:id(),
%    Entropy = channel_feeder:entropy(CID, [ID, Acc2]) + 1,
%    {Tx, _} = new_channel_tx:make(CID, Accounts, keys:id(), Acc2, MyBal, OtherBal, Rent, Entropy, Fee),
%    STx = keys:sign(Tx, Accounts),
%    talker:talk({new_channel, STx}, IP, Port),
%    {ok, 0};
    
doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {error}.
got_secret(Secret, IP, Port) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    UH = channel_manager_feeder:create_unlock_hash(ChId, Secret),
    channel_partner:store(ChId, UH),
    {ok, NewCh} = talker:talk({unlock, ChId, Secret, UH}, IP, Port),
    channel_manager_feeder:unlock_hash(ChId, Secret, NewCh).
absorb_msgs([], _, _, _) -> ok;
absorb_msgs([H|T], IP, Port, ServerId) -> 
    case talker:talk({pop, keys:id(), H}, IP, Port) of
	{ok, {unlock, Payment}} ->
	    {unlock, Payment2, ChId, Secret} = Payment,
	    Return = channel_manager_feeder:unlock_hash(ChId, Secret, Payment2),
	    channel_partner:store(ChId, Return),
	    talker:talk({unlock2, Return, ChId, Secret}, IP, Port);
	{ok, {locked_payment, Payment}} ->
	    {locked_payment, P, ChIdFrom, Amount, SecretHash, BetHash, Emsg} = Payment,
	    Return = channel_manager_feeder:recieve_locked_payment(ChIdFrom, P, Amount, SecretHash),
	    channel_partner:store(ChIdFrom, Return),
	    talker:talk({locked_payment2, Return, Amount, SecretHash, BetHash}, IP, Port),
	    M = encryption:get_msg(Emsg),
	    {secret, Secret} = encryption:msg(M),
	    got_secret(Secret, IP, Port);
	{ok, {pop_response, EMsg, Refund}} ->
	    NewCh = channel_manager_feeder:recieve(hd(channel_manager:id(ServerId)), 0, Refund),
	    ChId = hd(channel_manager:id(ServerId)),
	    channel_partner:store(ChId, NewCh),
	    talker:talk({update_channel, Refund, NewCh}, IP, Port),
	    inbox:get(EMsg);
	X -> 
	    io:fwrite("internal handler get msg bad "),
	    io:fwrite(packer:pack(X)),
	    io:fwrite("\n")
    end,
    absorb_msgs(T, IP, Port, ServerId).
