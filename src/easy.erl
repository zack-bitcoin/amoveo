-module(easy).
-compile(export_all).

-define(Fee, free_constants:tx_fee()).
-define(IP, {46,101,103,165}).
-define(Port, 8080).

height() ->    
    block:height(block:read(top:doit())).
top() ->
    TopHash = top:doit(),
    Height = height(),
    {top, TopHash, Height}.
    
sign(Tx) ->
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    keys:sign(Tx, Accounts).
    
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
create_account(NewAddr, Amount) ->
    io:fwrite("easy create account \n"),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    ID = find_id(accounts, Accounts),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(ca, Governance),
    create_account(NewAddr, Amount, ?Fee + Cost, ID).
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
	    {Trees, _, _} = tx_pool:data(),
	    Governance = trees:governance(Trees),
	    Cost = governance:get_value(spend, Governance),
	    spend(ID, A, ?Fee+Cost)
    end.
spend(ID, Amount, Fee) ->
    F = fun(Trees) ->
		spend_tx:make(ID, Amount, Fee, keys:id(), Trees, []) end,
    tx_maker(F).
    
delete_account(ID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(da, Governance),
    delete_account(ID, ?Fee+Cost).
delete_account(ID, Fee) ->
    F = fun(Trees) ->
		delete_account_tx:make(keys:id(), ID, Fee, Trees) end,
    tx_maker(F).

repo_account(ID) ->   
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(repo, Governance),
    repo_account(ID, ?Fee+Cost).
repo_account(ID, Fee) ->   
    F = fun(Trees) ->
		repo_tx:make(ID, Fee, keys:id(), Trees) end,
    tx_maker(F).

%new_channel(Bal1, Bal2) ->
%    {Trees, _, _} = tx_pool:data(),
%    Governance = trees:governance(Trees),
%    Cost = governance:get_value(nc, Governance),
%    new_channel(Bal1, Bal2, ?Fee+Cost, 10).
%new_channel(Bal1, Bal2, Fee, Delay) ->
%    {Trees, _,_} = tx_pool:data(),

%%    Channels = trees:channels(Trees),
%    CID = new_channel2(1, Channels),
%    B1 = to_int(Bal1),
%    B2 = to_int(Bal2),
%    new_channel_tx(constants:server_ip(), 
%		constants:server_port(), 
%		CID, B1, B2, Fee, Delay).
%new_channel2(ID, Channels) ->
%    <<X:8>> = crypto:strong_rand_bytes(1),
%    case channels:get(ID+X, Channels) of
%	{_, empty, _} -> ID+X;

%%	X -> new_channel2(ID+256, Channels)
%    end.
new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Delay) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(nc, Governance),
    new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, ?Fee+Cost, Delay).
new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Fee, Delay) ->
    %the delay is how many blocks you have to wait to close the channel if your partner disappears.
    %delay is also how long you have to stop your partner from closing at the wrong state.
    %entropy needs to be a different number every time you make a new channel with the same partner.
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = new_channel_tx:make(CID, Trees, keys:id(), Acc2, Bal1, Bal2, Entropy, Delay, Fee),
    keys:sign(Tx, Accounts).
    
new_channel_with_server(Bal1, Bal2, Delay) ->
    {Trees, _, _} = tx_pool:data(),
    Channels = trees:channels(Trees),
    CID = find_id(channels, Channels),
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(nc, Governance),
    new_channel_with_server(?IP, ?Port, CID, Bal1, Bal2, ?Fee+Cost, Delay).
find_id(Name, Tree) ->
    find_id(Name, 1, Tree).
find_id(Name, N, Tree) ->
    case Name:get(N, Tree) of
	{_, empty, _} -> N;
	_ -> find_id(Name, N+1, Tree)
    end.
new_channel_with_server(IP, Port, CID, Bal1, Bal2, Fee, Delay) ->
    PR = peers:read(IP, Port),
    if
	<<"none">> == PR -> ok;
	true ->
	    PC = peers:cid(PR),
	    if 
		undefined == PC -> ok;
		true -> PR = PC
	    end
    end,
    Acc1 = keys:id(),
    {ok, Acc2} = talker:talk({id}, IP, Port),
    Entropy = channel_feeder:entropy([Acc1, Acc2]) + 1,
    {Trees,_,_} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Trees, Acc1, Acc2, Bal1, Bal2, Entropy, Delay, Fee),
    SPK = new_channel_tx:spk(Tx, free_constants:channel_delay()),
    Accounts = trees:accounts(Trees),
    STx = keys:sign(Tx, Accounts),
    SSPK = keys:sign(SPK, Accounts),
    Msg = {new_channel, STx, SSPK},
    {ok, SSTx, S2SPK} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    channel_feeder:new_channel(Tx, S2SPK, Accounts),
    ok.
pull_channel_state() ->
    pull_channel_state(?IP, ?Port).
pull_channel_state(IP, Port) ->
    {ok, ServerID} = talker:talk({id}, IP, Port),
    {ok, CD0} = channel_manager:read(ServerID),
    true = channel_feeder:live(CD0),
    SPKME = channel_feeder:me(CD0),
    CID = spk:cid(SPKME),
    {ok, CD, ThemSPK} = talker:talk({spk, CID}, IP, Port),
    Return = channel_feeder:they_simplify(ServerID, ThemSPK, CD),
    talker:talk({channel_sync, keys:id(), Return}, IP, Port),
    decrypt_msgs(channel_feeder:emsg(CD)),
    bet_unlock(IP, Port),
    ok.
decrypt_msgs([]) ->
    [];
decrypt_msgs([{msg, _, Msg, _}|T]) ->
    [Msg|decrypt_msgs(T)];
decrypt_msgs([Emsg|T]) ->
    [Secret, Code] = keys:decrypt(Emsg),
    learn_secret(Secret, Code),
    decrypt_msgs(T).
%learn_secrets([]) ->
%    ok;
%learn_secrets([[Secret, Code]|T]) ->
%    learn_secret(Secret, Code),
%    learn_secrets(T).
learn_secret(Secret, Code) ->
    secrets:add(Code, Secret).
add_secret(Code, Secret) ->
    pull_channel_state(),
    secrets:add(Code, Secret),
    bet_unlock().
bet_unlock() ->
    bet_unlock(?IP, ?Port).
bet_unlock(IP, Port) ->
    {ok, ServerID} = talker:talk({id}, IP, Port),
    {ok, CD0} = channel_manager:read(ServerID),
    CID = channel_feeder:cid(CD0),
    [{Secrets, SPK}] = channel_feeder:bets_unlock([ServerID]),
    io:fwrite("teach secrets \n"),
    teach_secrets(keys:id(), Secrets, IP, Port),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    talker:talk({channel_sync, keys:id(), keys:sign(SPK, Accounts)}, IP, Port),
    {ok, _CD, ThemSPK} = talker:talk({spk, CID}, IP, Port),
    channel_feeder:update_to_me(ThemSPK, ServerID),
    ok.
teach_secrets(_, [], _, _) -> ok;
teach_secrets(ID, [{secret, Secret, Code}|Secrets], IP, Port) ->
    talker:talk({learn_secret, ID, Secret, Code}, IP, Port),
    teach_secrets(ID, Secrets, IP, Port).
channel_spend(Amount) ->
    channel_spend(?IP, ?Port, Amount).
channel_spend(IP, Port, Amount) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    OldSPK = testnet_sign:data(channel_feeder:them(CD)),
    ID = keys:id(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    SPK = spk:get_paid(OldSPK, ID, -Amount), 
    Payment = keys:sign(SPK, Accounts),
    M = {channel_payment, Payment, Amount},
    {ok, Response} = talker:talk(M, IP, Port),
    channel_feeder:spend(Response, -Amount),
    ok.
-define(LFee, free_constants:lightning_fee()).
lightning_spend(Recipient, Pubkey, Amount) ->
    lightning_spend(?IP, ?Port, Recipient, Pubkey, Amount, ?LFee).
lightning_spend(IP, Port, Recipient, Pubkey, Amount, Fee) ->
    {Code, SS} = secrets:new_lightning(),
    lightning_spend(IP, Port, Recipient, Pubkey, Amount, Fee, Code, SS).
lightning_spend(IP, Port, Recipient, Pubkey, Amount, Fee, Code, SS) ->
    {ok, ServerID} = talker:talk({id}, IP, Port),
    %ChannelID,
    ESS = keys:encrypt([SS, Code], Pubkey),
    SSPK = channel_feeder:make_locked_payment(ServerID, Amount+Fee, Code, []),
    {ok, SSPK2} = talker:talk({locked_payment, SSPK, Amount, Fee, Code, keys:id(), Recipient, ESS}, IP, Port),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK2, Accounts), Accounts),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    %store SSPK2 in channel manager, it is their most recent signature.
    {ok, CD} = channel_manager:read(ServerID),
    CID = channel_feeder:cid(CD),
    Entropy = channel_feeder:entropy(CD),
    ThemSS = channel_feeder:script_sig_them(CD),
    MeSS = channel_feeder:script_sig_me(CD),
    NewCD = channel_feeder:new_cd(SPK, SSPK2, [<<>>|MeSS], [<<>>|ThemSS], Entropy, CID),
    channel_manager:write(ServerID, NewCD),
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
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(gc, Governance),
    grow_channel(CID, Bal1, Bal2, ?Fee+Cost).
grow_channel(CID, Bal1, Bal2, Fee) ->
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = grow_channel_tx:make(CID, Trees, to_int(Bal1), to_int(Bal2), Fee),
    Accounts = trees:accounts(Trees),
    keys:sign(Tx, Accounts).

channel_team_close(CID, Amount) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(ctc, Governance),
    channel_team_close(CID, Amount, ?Fee+Cost).
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
new_question_oracle(Start, Question, DiffOracleID)->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    ID = find_id(oracles, Oracles),
    new_question_oracle(Start, Question, DiffOracleID, ID).
new_question_oracle(Start, Question, DiffOracleID, ID)->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    {_, Recent, _} = oracles:get(DiffOracleID, Oracles),
    Difficulty = oracles:difficulty(Recent) div 2,
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_new, Governance),
    F = fun(Trs) ->
		oracle_new_tx:make(keys:id(), ?Fee+Cost, Question, Start, ID, Difficulty, DiffOracleID, 0, 0, Trs) end,
    tx_maker(F).
new_difficulty_oracle(Start, Difficulty) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_new, Governance),
    Oracles = trees:oracles(Trees),
    ID = find_id(oracles, Oracles),
    new_difficulty_oracle(?Fee+Cost, Start, ID, Difficulty).
new_difficulty_oracle(Fee, Start, ID, Difficulty) ->
    %used to measure the difficulty at which negative and positive shares are worth the same
    F = fun(Trees) ->
		oracle_new_tx:make(keys:id(), Fee, <<"">>, Start, ID, Difficulty, 0, 0, 0, Trees) end,
    tx_maker(F).
new_governance_oracle(Start, GovName, GovAmount, DiffOracleID) ->
    GovNumber = governance:name2number(GovName),
    F = fun(Trs) ->
		Oracles = trees:oracles(Trs),
		ID = find_id(oracles, Oracles),
		{_, Recent, _} = oracles:get(DiffOracleID, Oracles),
		Difficulty = oracles:difficulty(Recent) div 2,
		Governance = trees:governance(Trs),
		Cost = governance:get_value(oracle_new, Governance),
		oracle_new_tx:make(keys:id(), ?Fee + Cost, <<>>, Start, ID, Difficulty, DiffOracleID, GovNumber, GovAmount, Trs) end,
    tx_maker(F).
    
oracle_bet(OID, Type, Amount) when is_integer(Type) ->
    T = case Type of
	    0 -> true;
	    1 -> false;
	    2 -> bad
	end,
    oracle_bet(OID, T, Amount);
oracle_bet(OID, Type, Amount) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_bet, Governance),
    oracle_bet(?Fee+Cost, OID, Type, Amount).
oracle_bet(Fee, OID, Type, Amount) ->
    F = fun(Trees) ->
		oracle_bet_tx:make(keys:id(), Fee, OID, Type, to_int(Amount), Trees)
	end,
    tx_maker(F).
oracle_close(OID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_close, Governance),
    oracle_close(?Fee+Cost, OID).
oracle_close(Fee, OID) ->
    F = fun(Trees) ->
		oracle_close_tx:make(keys:id(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_shares(OID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_shares, Governance),
    oracle_shares(?Fee+Cost, OID).
oracle_shares(Fee, OID) ->
    F = fun(Trees) ->
		oracle_shares_tx:make(keys:id(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_unmatched(OracleID, OrderID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(unmatched, Governance),
    oracle_unmatched(?Fee+Cost, OracleID, OrderID).
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
    I = case keys:id() of
	    -1 -> 0;
	    _ -> integer_balance()
	end,
    %pretty_display(I),
    I.

pretty_display(I) ->
    F = I / constants:token_decimals(),
    io:fwrite(hd(io_lib:format("~.8f", [F]))),
    io:fwrite("\n").
mempool() ->
    {_, _, Txs} = tx_pool:data(),
    Txs.
off() -> testnet_sup:stop().
mine_block() ->
    block:mine_blocks(1, 100000).
mine_block(Many, Times) ->
    block:mine_blocks(Many, Times).
channel_close() ->
    channel_close(?IP, ?Port).
channel_close(IP, Port) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(ctc, Governance),
    channel_close(IP, Port, ?Fee+Cost).
channel_close(IP, Port, Fee) ->
    {ok, PeerId} = talker:talk({id}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = testnet_sign:data(channel_feeder:them(CD)),
    {Trees,_,_} = tx_pool:data(),
    Height = block:height(block:read(top:doit())),
    SS = channel_feeder:script_sig_them(CD),
    {Amount, _, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
    CID = spk:cid(SPK),
    {Tx, _} = channel_team_close_tx:make(CID, Trees, Amount, [], Fee),
    Accounts = trees:accounts(Trees),
    STx = keys:sign(Tx, Accounts),
    {ok, SSTx} = talker:talk({close_channel, CID, keys:id(), SS, STx}, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    0.
channel_solo_close(IP, Port) ->
    {ok, Other} = talker:talk({id}, IP, Port),
    channel_solo_close(Other).
channel_solo_close(Other) ->
    Fee = free_constants:tx_fee(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SS = channel_feeder:script_sig_them(CD),
    {Tx, _} = channel_solo_close:make(keys:id(), Fee, keys:sign(SSPK, Accounts), SS, Trees),
    STx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(STx),
    0.
add_peer(IP, Port) ->
    peers:add(IP, Port),
    0.
sync(IP, Port) ->
    MyHeight = block:height(block:read(top:doit())),
    download_blocks:sync(IP, Port, MyHeight),
    0.
pubkey() ->
    keys:pubkey().
address() ->
    address(pubkey()).
address(Pub) ->
    testnet_sign:pubkey2address(Pub).
id() ->
    keys:id().
new_pubkey(Password) ->    
    keys:new(Password).
test() ->
    {test_response}.
channel_keys() ->
    channel_manager:keys().
keys_status() ->
    list_to_binary(atom_to_list(keys:status())).
keys_unlock(Password) ->
    keys:unlock(Password),
    0.
keys_id_update(ID) ->
    keys:update_id(ID),
    0.
keys_new(Password) ->
    keys:new(Password),
    0.
market_match(OID) ->
    %check that we haven't matched too recently. (otherwise we lose all our money in all the channels.)
    {_PriceDeclaration, _Accounts} = order_book:match(OID),
    %update a bunch of channels. 
    %store the price declaration in the channel_manager.
    ok.
new_market(OID, Expires, Period) -> 
    %for now lets use the oracle id as the market id. this wont work for combinatorial markets.
    order_book:new_market(OID, Expires, Period).
    %set up an order book.
    %turn on the api for betting.
trade(Price, Type, Amount, OID, Fee) ->
    trade(Price, Type, Amount, OID, Fee, ?IP, ?Port).
trade(Price, Type, A, OID, Fee, IP, Port) ->
    Amount = to_int(A),
    {ok, ServerID} = talker:talk({id}, IP, Port),
    {ok, {Expires, 
	  Pubkey, %pubkey of market maker
	  Period}} = 
	talker:talk({market_data, OID}, IP, Port),
    BetLocation = constants:oracle_bet(),
    MarketID = OID,
    %type is true or false or one other thing...
    SC = market:market_smart_contract(BetLocation, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID),
    SSPK = channel_feeder:trade(Amount, SC, ServerID, OID),
    {ok, SSPK2} =
	talker:talk({trade, 
		     keys:id(),
		     Price,
		     Type,
		     Amount,
		     OID,
		     SSPK, 
		     Fee}, IP, Port),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    channel_feeder:update_to_me(SSPK2, ServerID).
    
    

%mine() ->
%    mine:start().
    %mine(10000000000).
%mine(N) -> 
    %sync(),
    %block:mine_blocks(N, 100000, 30). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
test_it_out() ->
    %create_account(Address, 10, 2),
    %delete_account(2),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    create_account(NewAddr, 0.0000001),
    timer:sleep(100),
    test_txs:mine_blocks(1),
    repo_account(2),
    timer:sleep(100),
    create_account(NewAddr, 10),
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
    new_difficulty_oracle(Height+1, Difficulty),
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
    
    
