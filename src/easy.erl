-module(easy).
-compile(export_all).

-define(Fee, free_constants:tx_fee()).

sync() ->
    spawn(fun() -> sync3() end).
height() ->    
    block:height(block:read(top:doit())).

sync3() ->
    Height = height(),
    download_blocks:sync_all(peers:all(), Height),
    sync2(Height, 600).
sync2(_Height, 0) -> 
    ok;
    %download_blocks:sync_txs(peers:all());
sync2(Height, N) ->
    timer:sleep(100),
    Height2 = block:height(block:read(top:doit())),
    if
	Height2 > Height -> 
	    timer:sleep(1400),
	    sync();
	true -> sync2(Height, N-1)
   end. 
   
tx_maker(F) -> 
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = F(Trees),
    case keys:sign(Tx, Accounts) of
	{error, locked} -> 
	    io:fwrite("your password is locked. use `keys:unlock(\"PASSWORD1234\")` to unlock it"),
	    ok;
	Stx -> tx_pool_feeder:absorb(Stx)
    end.
create_account(NewAddr, Amount, ID) ->
    create_account(NewAddr, Amount, ?Fee, ID).
create_account(NewAddr, Amount, Fee, ID) ->
    F = fun(Trees) ->
		create_account_tx:make(NewAddr, to_int(Amount), Fee, keys:id(), ID, Trees) end,
    tx_maker(F).
spend(ID, Amount) ->
    K = keys:id(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    A = to_int(Amount),
	    spend(ID, A, ?Fee)
    end.
spend(ID, Amount, Fee) ->
    F = fun(Trees) ->
		spend_tx:make(ID, Amount, Fee, keys:id(), Trees, []) end,
    tx_maker(F).
    
delete_account(ID) ->
    delete_account(ID, ?Fee).
delete_account(ID, Fee) ->
    F = fun(Trees) ->
		delete_account_tx:make(keys:id(), ID, Fee, Trees) end,
    tx_maker(F).

repo_account(ID) ->   
    repo_account(ID, ?Fee).
repo_account(ID, Fee) ->   
    F = fun(Trees) ->
		repo_tx:make(ID, Fee, keys:id(), Trees) end,
    tx_maker(F).
new_channel(Bal1, Bal2) ->
    new_channel(Bal1, Bal2, ?Fee, 10).
new_channel(Bal1, Bal2, Fee, Delay) ->
    {Trees, _,_} = tx_pool:data(),
    Channels = trees:channels(Trees),
    CID = new_channel2(1, Channels),
    B1 = to_int(Bal1),
    B2 = to_int(Bal2),
    new_channel_tx(constants:server_ip(), 
		constants:server_port(), 
		CID, B1, B2, Fee, Delay).
new_channel2(ID, Channels) ->
    <<X:8>> = crypto:strong_rand_bytes(1),
    case channels:get(ID+X, Channels) of
	{_, empty, _} -> ID+X;
	X -> new_channel2(ID+256, Channels)
    end.
new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Delay) ->
    new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, ?Fee, Delay).
new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Fee, Delay) ->
    %the delay is how many blocks you have to wait to close the channel if your partner disappears.
    %delay is also how long you have to stop your partner from closing at the wrong state.
    %entropy needs to be a different number every time you make a new channel with the same partner.
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = new_channel_tx:make(CID, Trees, keys:id(), Acc2, Bal1, Bal2, Entropy, Delay, Fee),
    keys:sign(Tx, Accounts).
    
new_channel_with_server(IP, Port, CID, Bal1, Bal2, Fee, Delay) ->
    undefined = peers:cid(peers:read(IP, Port)),
    Acc1 = keys:id(),
    {ok, Acc2} = talker:talk({id}, IP, Port),
    Entropy = channel_feeder:entropy([Acc1, Acc2]) + 1,
    {Trees,_,_} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Trees, Acc1, Acc2, Bal1, Bal2, Entropy, Fee, Delay),
    SPK = new_channel_with_server:spk(Tx, free_constants:channel_delay()),
    Accounts = trees:accounts(Trees),
    STx = keys:sign(Tx, Accounts),
    SSPK = keys:sign(SPK, Accounts),
    Msg = {new_channel, STx, SSPK},
    {ok, SSTx, S2SPK} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    peers:set_cid(IP, Port, CID),
    channel_feeder:new_channel(Tx, S2SPK, Accounts),
    ok.
channel_balance() ->
    I = integer_channel_balance(),
    pretty_display(I).
integer_channel_balance() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SPK = testnet_sign:data(SSPK),
    SS = channel_feeder:script_sig_them(CD),
    {Trees, NewHeight, _Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    Channels = trees:accounts(Trees),
    {Amount, _} = spk:run(fast, SS, SPK, NewHeight, 0, Accounts, Channels),
    CID = spk:cid(SPK),
    {_, Channel, _} = channels:get(CID, Channels),
    channels:bal1(Channel)-Amount.
dice(Amount) ->
    unlocked = keys:status(),
    A = to_int(Amount),
    internal_handler:doit({dice, A, constants:server_ip(), constants:server_port()}).
close_channel_with_server() ->
    internal_handler:doit({close_channel, constants:server_ip(), constants:server_port()}).
solo_close_channel() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    internal_handler:doit({channel_solo_close, Other}).
channel_timeout() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    Fee = free_constants:tx_fee(),
    {Trees,_,_} = tx_pool:data(),
    {ok, CD} = channel_manager:read(Other),
    CID = channel_feeder:cid(CD),
    {Tx, _} = channel_timeout:make(keys:id(), Trees, CID, Fee),
    Accounts = trees:accounts(Trees),
    Stx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(Stx).
    
to_int(X) ->
    round(X * constants:token_decimals()).

grow_channel(CID, Bal1, Bal2) ->
    grow_channel(CID, Bal1, Bal2, ?Fee).
grow_channel(CID, Bal1, Bal2, Fee) ->
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = grow_channel_tx:make(CID, Trees, to_int(Bal1), to_int(Bal2), Fee),
    Accounts = trees:accounts(Trees),
    keys:sign(Tx, Accounts).

channel_team_close(CID, Amount) ->
    channel_team_close(CID, Amount, ?Fee).
channel_team_close(CID, Amount, Fee) ->
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    keys:sign(channel_team_close_tx:make(CID, Trees, Amount, Fee), Accounts).

channel_repo(CID, Fee) ->
    F = fun(Trees) ->
		channel_repo_tx:make(keys:id(), CID, Fee, Trees) end,
    tx_maker(F).

channel_solo_close(CID, Fee, SPK, ScriptSig) ->
    F = fun(Trees) ->
		channel_solo_close:make(keys:id(), CID, Fee, SPK, ScriptSig, Trees) end,
    tx_maker(F).

channel_timeout(CID, Fee) ->
    F = fun(Trees) ->
		channel_timeout_tx:make(keys:id(), Trees, CID, Fee) end,
    tx_maker(F).

channel_slash(CID, Fee, SPK, SS) ->
    F = fun(Trees) ->
		channel_slash_tx:make(keys:id(), CID, Fee, SPK, SS, Trees) end,
    tx_maker(F).
new_difficulty_oracle(Start, ID, Difficulty) ->
    new_difficulty_oracle(?Fee, Start, ID, Difficulty).
new_difficulty_oracle(Fee, Start, ID, Difficulty) ->
    %used to measure the difficulty at which negative and positive shares are worth the same
    F = fun(Trees) ->
		oracle_new_tx:make(keys:id(), Fee, <<"">>, Start, ID, Difficulty, 0, 0, 0, Trees) end,
    tx_maker(F).
oracle_bet(OID, Type, Amount) ->
    oracle_bet(?Fee, OID, Type, Amount).
oracle_bet(Fee, OID, Type, Amount) ->
    F = fun(Trees) ->
		oracle_bet_tx:make(keys:id(), Fee, OID, Type, to_int(Amount), Trees)
	end,
    tx_maker(F).
oracle_close(OID) ->
    oracle_close(?Fee, OID).
oracle_close(Fee, OID) ->
    F = fun(Trees) ->
		oracle_close_tx:make(keys:id(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_shares(OID) ->
    oracle_shares(?Fee, OID).
oracle_shares(Fee, OID) ->
    F = fun(Trees) ->
		oracle_shares_tx:make(keys:id(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_unmatched(OracleID, OrderID) ->
    oracle_unmatched(?Fee, OracleID, OrderID).
oracle_unmatched(Fee, OracleID, OrderID) ->
    F = fun(Trees) ->
		oracle_unmatched_tx:make(keys:id(), Fee, OracleID, OrderID, Trees)
	end,
    tx_maker(F).

account(ID) ->
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    case accounts:get(ID, Accounts) of
	{_,empty,_} ->
	    io:fwrite("this account does not yet exist\n"),
	    accounts:new(-1,0,0,0);
	{_, A, _} -> A
    end.

account() -> account(keys:id()).
integer_balance() -> accounts:balance(account()).
balance() ->
    I = integer_balance(),
    pretty_display(I).
pretty_display(I) ->
    F = I / constants:token_decimals(),
    io_lib:format("~.8f", [F]).
off() -> testnet_sup:stop().

%mine() ->
%    mine:start().
    %mine(10000000000).
%mine(N) -> 
    %sync(),
    %block:mine_blocks(N, 100000, 30). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
test() ->
    %create_account(Address, 10, 2),
    %delete_account(2),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    create_account(NewAddr, 0.0000001, 2),
    timer:sleep(100),
    test_txs:mine_blocks(1),
    repo_account(2),
    timer:sleep(100),
    create_account(NewAddr, 10, 2),
    timer:sleep(100),
    spend(2, 1),
    timer:sleep(100),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    Ctx = new_channel_tx(1, 2, 1, 1, 1, 1),
    Stx = testnet_sign:sign_tx(Ctx, NewPub, NewPriv, 2, Accounts),
    tx_pool_feeder:absorb(Stx),
    timer:sleep(100),
    Ctx2 = grow_channel(1, 0.1, 0.1),
    Stx2 = testnet_sign:sign_tx(Ctx2, NewPub, NewPriv, 2, Accounts),
    tx_pool_feeder:absorb(Stx2),
    test_txs:mine_blocks(1),
    timer:sleep(100),
    {_Trees, Height, _} = tx_pool:data(),
    Difficulty = constants:initial_difficulty(),
    new_difficulty_oracle(Height+1, 1, Difficulty),
    timer:sleep(100),
    test_txs:mine_blocks(2),
    timer:sleep(100),
    oracle_bet(1, bad, 10.0),
    timer:sleep(100),
    test_txs:mine_blocks(2),
    oracle_close(1),
    timer:sleep(100),
    oracle_shares(1),
    timer:sleep(100),
    oracle_unmatched(1, 2),
    timer:sleep(100),
    tx_pool:data().
    
    
