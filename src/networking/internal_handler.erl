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
doit({sign, Tx}) -> {ok, keys:sign(Tx)};


doit({balance}) ->
    {ok, accounts:balance(block_tree:account(keys:id()))};
doit({channel_balance, IP, Port}) ->
    {ok, ServerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(ServerId)),
    OnChannel = block_tree:channel(ChId),
    OffChannel = channel_manager:read_channel(ChId),
    A1 = channels:acc1(OnChannel),
    A2 = channels:acc2(OnChannel),
    {Bal, Sign} = 
	case keys:id() of
	    A1 -> {channels:bal1(OnChannel), 1};
	    A2 -> {channels:bal2(OnChannel), -1}
	end,
    BetAmounts = channel_manager:bet_amounts(OffChannel),
    {ok, Bal + (Sign * channel_block_tx:amount(OffChannel)) - BetAmounts};
doit({channel_balance2, IP, Port}) ->
    %This looks identical to the one above. It should probably be deleted.
    %balance.js uses it.
    {ok, ServerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(ServerId)),
    OnChannel = block_tree:channel(ChId),
    OffChannel = channel_manager:read_channel(ChId),
    A1 = channels:acc1(OnChannel),
    A2 = channels:acc2(OnChannel),
    {Bal, Sign} = 
	case keys:id() of
	    A2 -> {channels:bal1(OnChannel), 1};
	    A1 -> {channels:bal2(OnChannel), -1}
	end,
    BetAmounts = channel_manager:bet_amounts(OffChannel),
    {ok, Bal + (Sign * channel_block_tx:amount(OffChannel)) - BetAmounts};
doit({create_account, Pub, Amount, Fee}) -> 
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub, Amount, Fee))),
    {ok, ok};
doit({spend, To, Amount, Fee}) ->
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(To, Amount, Fee)));
doit({buy_block}) -> 
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))), 
    block_tree:buy_block();
doit({create_channel, Partner, Bal1, Bal2, Type, Fee}) ->
    keys:sign(to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee));
doit({to_channel, IP, Port, Inc1, Inc2, Fee}) ->
    {ok, ServerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(ServerId)),
    SignedTx = keys:sign(to_channel_tx:to_channel(ChId, Inc1, Inc2, Fee)),
    talker:talk({to_channel, SignedTx}, IP, Port);
doit({close_channel, ChId, Amount, Nonce, Fee}) ->
    keys:sign(channel_block_tx:close_channel(ChId, Amount, Nonce, Fee));
doit({sync, IP, Port}) ->
    download_blocks:sync(IP, Port);
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
    ChId = hd(channel_manager:id(PeerId)), 
    Payment = channel_manager_feeder:spend(ChId, Amount),
    M = {channel_spend, Payment, keys:id()},
    {ok, Response} = talker:talk(M, IP, Port),
    channel_manager_feeder:recieve(ChId, -Amount, Response),
    {ok, ok};
    
doit({send_msg, IP, Port, To, M, Seconds}) ->
    Acc = block_tree:account(To),
    Pub = accounts:pub(Acc),
    Msg = encryption:send_msg(M, Pub),
    {ok, Amount} = talker:talk({mail_cost, size(Msg), Seconds}),
    {ok, PeerId} = talker:talk({id}, IP, Port),
    ChId = hd(channel_manager:id(PeerId)), 
    Payment = channel_manager_feeder:spend(ChId, Amount),
    Foo = {send, Payment, keys:id(), To, Msg, Seconds},
    {ok, Response} = talker:talk(Foo, IP, Port),
    channel_manager_feeder:recieve(ChId, -Amount, Response),
    inbox:get_helper(To, M),
    {ok, ok};
doit({new_channel, IP, Port, Bal1, Bal2, Fee}) ->
    {ok, Partner} = talker:talk({id}, IP, Port),
    Type = <<"delegated_2">>,
    Tx = keys:sign(to_channel_tx:create_channel(Partner, Bal1, Bal2, Type, Fee)),
    Msg = {new_channel, Tx},
    {ok, Ch} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(Ch),
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
