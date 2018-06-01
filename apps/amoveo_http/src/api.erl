-module(api).
-compile(export_all).
-define(Fee, element(2, application:get_env(amoveo_core, tx_fee))).
-define(IP, constants:server_ip()).
-define(Port, constants:server_port()).
-include("../../amoveo_core/src/records.hrl").
dump_channels() ->
    channel_manager:dump().
keys_status() -> keys:status().
load_key(Pub, Priv, Brainwallet) ->
    keys:load(Pub, Priv, Brainwallet).
height() ->    
    (headers:top())#header.height.
height(1) ->
    block:height().
block(1, N) ->
    B = block:get_by_height(N),
    B#block.txs;
block(3, N) ->
    Txs = tl(block(1, N)),
    Txids = lists:map(
	      fun(Tx) -> hash:doit(testnet_sign:data(Tx)) end, 
	      Txs),
    [Txs, Txids];
block(2, H) ->
    block:get_by_hash(H).
top() ->
    TopHeader = headers:top(),
    Height = TopHeader#header.height,
    {top, TopHeader, Height}.
top(1) ->
    H = headers:top_with_block(),
    [H, block:hash(H)].
sign(Tx) -> keys:sign(Tx).
tx_maker0(Tx) -> 
    case keys:sign(Tx) of
	{error, locked} -> 
	    io:fwrite("your password is locked. use `keys:unlock(\"PASSWORD1234\")` to unlock it"),
	    ok;
	Stx -> 
	    tx_pool_feeder:absorb(Stx),
	    Peers = peers:all(),
	    %spawn(fun() ->
		%	  lists:map(fun(P) -> 
		%			    timer:sleep(200),
		%			    spawn(fun() -> talker:talk({txs, [Stx]}, P) end) end, Peers)
		%	  end),
	    %ok
	    hash:doit(Tx)
    end.
create_account(NewAddr, Amount) ->
    Cost = trees:dict_tree_get(governance, create_acc_tx),
    create_account(NewAddr, Amount, ?Fee + Cost).
create_account(NewAddr, Amount, Fee) when size(NewAddr) == 65 ->
    Tx = create_account_tx:make_dict(NewAddr, Amount, Fee, keys:pubkey()),
    tx_maker0(Tx);
create_account(N, A, F) ->
    M = base64:decode(N),
    create_account(M, A, F).
coinbase(_) ->
    K = keys:pubkey(),
    tx_maker0(coinbase_tx:make_dict(K)).
spend(ID0, Amount) ->
    ID = decode_pubkey(ID0),
    K = keys:pubkey(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    B = trees:dict_tree_get(accounts, ID),
            if 
               (B == empty) ->
                    create_account(ID, Amount);
                true ->
		    Cost = trees:dict_tree_get(governance, spend),
                    spend(ID, Amount, ?Fee+Cost)
            end
    end.
spend(ID0, Amount, Fee) ->
    ID = decode_pubkey(ID0),
    tx_maker0(spend_tx:make_dict(ID, Amount, Fee, keys:pubkey())).
delete_account(ID0) ->
    ID = decode_pubkey(ID0),
    Cost = trees:dict_tree_get(governance, delete_acc_tx),
    delete_account(ID, ?Fee + Cost).
delete_account(ID0, Fee) ->
    ID = decode_pubkey(ID0),
    tx_maker0(delete_account_tx:make_dict(ID, keys:pubkey(), Fee)).
new_channel_tx(CID, Acc2, Bal1, Bal2, Delay) ->
    Cost = trees:dict_tree_get(governance, nc),
    new_channel_tx(CID, Acc2, Bal1, Bal2, ?Fee+Cost, Delay).
new_channel_tx(CID, Acc2, Bal1, Bal2, Fee, Delay) ->
    %the delay is how many blocks you have to wait to close the channel if your partner disappears.
    %delay is also how long you have to stop your partner from closing at the wrong state.
    Tx = new_channel_tx:make_dict(CID, keys:pubkey(), Acc2, Bal1, Bal2, Delay, Fee),
    keys:sign(Tx).
new_channel_with_server(Bal1, Bal2, Delay, Expires) ->
    new_channel_with_server(Bal1, Bal2, Delay, Expires, ?IP, ?Port).
new_channel_with_server(Bal1, Bal2, Delay, Expires, IP, Port) ->
    CID = find_id2(),
    Cost = trees:dict_tree_get(governance, nc),
    new_channel_with_server(IP, Port, CID, Bal1, Bal2, ?Fee+Cost, Delay, Expires),
    CID.
find_id2() -> find_id2(1, 1).
find_id2(_, _) -> crypto:strong_rand_bytes(32).
%find_id(Name, Tree) ->
%    find_id(Name, 1, Tree).
%find_id(Name, N, Tree) ->
%    case Name:get(N, Tree) of
%	{_, empty, _} -> N;
%	_ -> find_id(Name, N+1, Tree)
%    end.
new_channel_with_server(IP, Port, CID, Bal1, Bal2, Fee, Delay, Expires) ->
    Acc1 = keys:pubkey(),
    {ok, Acc2} = talker:talk({pubkey}, IP, Port),
    Tx = new_channel_tx:make_dict(CID, Acc1, Acc2, Bal1, Bal2, Delay, Fee),
    {ok, ChannelDelay} = application:get_env(amoveo_core, channel_delay),
    {ok, TV} = talker:talk({time_value}, IP, Port),%We need to ask the server for their time_value.
    %make sure the customer is aware of the time_value before they click this button. Don't request time_value now, it should have been requested earlier.
    LifeSpan = Expires - api:height(),
    true = LifeSpan > 0,
    CFee = TV * (Delay + LifeSpan) * (Bal1 + Bal2) div 100000000,
    SPK0 = new_channel_tx:spk(Tx, ChannelDelay),
    SPK = SPK0#spk{amount = CFee},
    STx = keys:sign(Tx),
    SSPK = keys:sign(SPK),
    Msg = {new_channel, STx, SSPK, Expires},
    {ok, [SSTx, S2SPK]} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    channel_feeder:new_channel(Tx, S2SPK, Expires),
    0.
pull_channel_state() ->
    pull_channel_state(?IP, ?Port).
pull_channel_state(IP, Port) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    {ok, [CD, ThemSPK]} = talker:talk({spk, keys:pubkey()}, IP, Port),
    case channel_manager:read(ServerID) of
        error  -> 
            %This trusts the server and downloads a new version of the state from them. It is only suitable for testing and development. Do not use this in production.
            SPKME = CD#cd.them,
            true = testnet_sign:verify(keys:sign(ThemSPK)),
            SPK = testnet_sign:data(ThemSPK),
            SPK = testnet_sign:data(SPKME),
            true = keys:pubkey() == element(2, SPK),
            NewCD = CD#cd{me = SPK, them = ThemSPK, ssme = CD#cd.ssthem, ssthem = CD#cd.ssme},
            channel_manager:write(ServerID, NewCD);
        {ok, CD0} ->
            true = CD0#cd.live,
            Return = channel_feeder:they_simplify(ServerID, ThemSPK, CD),
            talker:talk({channel_sync, keys:pubkey(), Return}, IP, Port),
            decrypt_msgs(CD#cd.emsg),
            bet_unlock(IP, Port),
	    {ok, CD2} = channel_manager:read(ServerID),
	    CD3 = CD2#cd{emsg = []},
	    channel_manager:write(ServerID, CD3),
	    Return2 = keys:sign(CD2#cd.me),
            talker:talk({channel_sync, keys:pubkey(), Return2}, IP, Port),
            ok
    end.
channel_state() -> 
    channel_manager:read(hd(channel_manager:keys())).
decrypt_msgs([]) ->
    [];
decrypt_msgs([{msg, _, Msg, _}|T]) ->
    [Msg|decrypt_msgs(T)];
decrypt_msgs([Emsg|T]) ->
    [Secret, Code, Amount] = keys:decrypt(Emsg),
    learn_secret(Secret, Code, Amount),
    decrypt_msgs(T).
learn_secret(Secret, Code, Amount) ->
    secrets:add(Code, Secret, Amount).
%add_secret(Code, Secret) ->
%    ok = pull_channel_state(?IP, ?Port),
%    secrets:add(Code, Secret),
%    ok = bet_unlock(?IP, ?Port).
bet_unlock(IP, Port) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    [{Secrets, _SPK}] = channel_feeder:bets_unlock([ServerID]),
    teach_secrets(keys:pubkey(), Secrets, IP, Port),
    {ok, [_CD, ThemSPK]} = talker:talk({spk, keys:pubkey()}, IP, Port),
    channel_feeder:update_to_me(ThemSPK, ServerID),
    ok.
teach_secrets(_, [], _, _) -> ok;
teach_secrets(ID, [{secret, Secret, Code}|Secrets], IP, Port) ->
    talker:talk({learn_secret, ID, Secret, Code}, IP, Port),
    teach_secrets(ID, Secrets, IP, Port).
channel_spend(Amount) ->
    channel_spend(?IP, ?Port, Amount).
channel_spend(IP, Port, Amount) ->
    {ok, PeerId} = talker:talk({pubkey}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    OldSPK = testnet_sign:data(CD#cd.them),
    ID = keys:pubkey(),
    SPK = spk:get_paid(OldSPK, ID, -Amount), 
    Payment = keys:sign(SPK),
    M = {channel_payment, Payment, Amount},
    {ok, Response} = talker:talk(M, IP, Port),
    channel_feeder:spend(Response, -Amount),
    ok.
lightning_spend(Pubkey, Amount) ->
    {ok, LFee} = application:get_env(amoveo_core, lightning_fee),
    lightning_spend(?IP, ?Port, Pubkey, Amount, LFee).
lightning_spend(IP, Port, Pubkey, Amount) ->
    lightning_spend(IP, Port, Pubkey, Amount, ?Fee).
lightning_spend(IP, Port, Pubkey, Amount, Fee) ->
    {Code, SS} = secrets:new_lightning(Amount),
    lightning_spend(IP, Port, Pubkey, Amount, Fee, Code, SS).
lightning_spend(IP, Port, Pubkey, Amount, Fee, Code, SS) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    ESS = keys:encrypt([SS, Code, Amount], Pubkey),
    SSPK = channel_feeder:make_locked_payment(ServerID, Amount+Fee, Code),
    {ok, SSPK2} = talker:talk({locked_payment, SSPK, Amount, Fee, Code, keys:pubkey(), Pubkey, ESS}, IP, Port),
    true = testnet_sign:verify(keys:sign(SSPK2)),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    channel_manager_update(ServerID, SSPK2, spk:new_ss(compiler_chalang:doit(<<>>), [])),
    ok.
channel_manager_update(ServerID, SSPK2, DefaultSS) ->
    %store SSPK2 in channel manager, it is their most recent signature.
    {ok, CD} = channel_manager:read(ServerID),
    SPK = testnet_sign:data(SSPK2),
    NewCD = CD#cd{me = SPK, them = SSPK2, ssme = [DefaultSS|CD#cd.ssme], ssthem = [DefaultSS|CD#cd.ssthem]},
    channel_manager:write(ServerID, NewCD),
    ok.
channel_balance() ->
    channel_balance({127,0,0,1}, constants:server_port()).
channel_balance(Ip, Port) ->
    {Balance, _} = integer_channel_balance(Ip, Port),
    Balance.
channel_balance2(Ip, Port) ->
    {_, Bal} = integer_channel_balance(Ip, Port),
    Bal.
integer_channel_balance(Ip, Port) ->
    {ok, Other} = talker:talk({pubkey}, Ip, Port),
    
    {ok, CD} = channel_manager:read(Other),
    SSPK = CD#cd.them,
    SPK = testnet_sign:data(SSPK),
    %SS = CD#cd.ssthem,
    TP = tx_pool:get(),
    NewHeight = TP#tx_pool.height,
    Amount = SPK#spk.amount,
    BetAmounts = sum_bets(SPK#spk.bets),
    CID = SPK#spk.cid,
    Channel = trees:dict_tree_get(channels, CID),
    {channels:bal1(Channel)+Amount, channels:bal2(Channel)-Amount-BetAmounts}.
sum_bets([]) -> 0;
sum_bets([B|T]) ->
    B#bet.amount + sum_bets(T).
pretty_display(I) ->
    {ok, TokenDecimals} = application:get_env(amoveo_core, token_decimals),
    F = I / TokenDecimals,
    [Formatted] = io_lib:format("~.8f", [F]),
    Formatted.
channel_team_close(CID, Amount) ->
    Cost = trees:dict_tree_get(governance, ctc),
    channel_team_close(CID, Amount, ?Fee+Cost).
channel_team_close(CID, Amount, Fee) ->
    Tx = channel_team_close_tx:make_dict(CID, Amount, Fee),
    keys:sign(Tx).
channel_timeout() ->
    channel_timeout(constants:server_ip(), constants:server_port()).
channel_timeout(Ip, Port) ->
    {ok, Other} = talker:talk({pubkey}, Ip, Port),
    {ok, Fee} = application:get_env(amoveo_core, tx_fee),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Dict = (tx_pool:get())#tx_pool.dict,
    {ok, CD} = channel_manager:read(Other),
    CID = CD#cd.cid,
    {Tx, _} = channel_timeout_tx:make_dict(keys:pubkey(), Trees, CID, [], Fee, Dict),
    case keys:sign(Tx) of
        {error, locked} ->
            io:fwrite("your password is locked");
        Stx ->
            tx_pool_feeder:absorb(Stx)
    end.
channel_slash(_CID, Fee, SPK, SS) ->
    tx_maker0(channel_slash_tx:make_dict(keys:pubkey(), Fee, SPK, SS)).
new_question_oracle(Start, Question)->
    ID = find_id2(),
    new_question_oracle(Start, Question, ID).

new_question_oracle(Start, Question, ID)->
    Q2 = if
	     is_list(Question) -> list_to_binary(Question);
	     true -> Question
	 end,
    Cost = trees:dict_tree_get(governance, oracle_new),
    tx_maker0(oracle_new_tx:make_dict(keys:pubkey(), ?Fee+Cost, Q2, Start, ID, 0, 0)),
    ID.
new_governance_oracle(GovName, GovAmount) ->
    GovNumber = governance:name2number(GovName),
    ID = find_id2(),
    %Recent = trees:dict_tree_get(oracles, DiffOracleID),
    Cost = trees:dict_tree_get(governance, oracle_new),
    Tx = oracle_new_tx:make_dict(keys:pubkey(), ?Fee + Cost, <<>>, 0, ID, GovNumber, GovAmount),
    tx_maker0(Tx),
    ID.
oracle_bet(OID, Type, Amount) ->
    Cost = trees:dict_tree_get(governance, oracle_bet),
    oracle_bet(?Fee+Cost, OID, Type, Amount).
oracle_bet(Fee, OID, Type, Amount) ->
    tx_maker0(oracle_bet_tx:make_dict(keys:pubkey(), Fee, OID, Type, Amount)).
oracle_close(OID) ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Dict = (tx_pool:get())#tx_pool.dict,
    Cost = trees:dict_tree_get(governance, oracle_close, Dict, Trees),
    oracle_close(?Fee+Cost, OID).
oracle_close(Fee, OID) ->
    tx_maker0(oracle_close_tx:make_dict(keys:pubkey(), Fee, OID)).
oracle_winnings(OID) ->
    Cost = trees:dict_tree_get(governance, oracle_winnings),
    oracle_winnings(?Fee+Cost, OID).
oracle_winnings(Fee, OID) ->
    tx_maker0(oracle_winnings_tx:make_dict(keys:pubkey(), Fee, OID)).
oracle_unmatched(OracleID) ->
    Cost = trees:dict_tree_get(governance, unmatched),
    oracle_unmatched(?Fee+Cost, OracleID).
oracle_unmatched(Fee, OracleID) ->
    tx_maker0(oracle_unmatched_tx:make_dict(keys:pubkey(), Fee, OracleID)).
account(P) ->
    Pubkey = decode_pubkey(P),
    trees:dict_tree_get(accounts, Pubkey).
account() -> account(keys:pubkey()).
confirmed_balance(P) ->
    Pubkey = decode_pubkey(P),
    Root = confirmed_root:read(),
    Block = block:get_by_hash(Root),
    Trees = Block#block.trees,
    Accounts = trees:accounts(Trees),
    {_, V, _} = accounts:get(Pubkey, Accounts),
    B1 = V#acc.balance,
    V2 = account(Pubkey),
    B2 = V2#acc.balance,
    min(B1, B2).
decode_pubkey(P) when size(P) == 65 -> P;
decode_pubkey(P) when is_list(P) -> 
    decode_pubkey(base64:decode(P));
decode_pubkey(P) when ((size(P) > 85) and (size(P) < 90)) -> 
    decode_pubkey(base64:decode(P)).
    
integer_balance() -> 
    A = account(),
    case A of
        empty -> 0;
        A -> A#acc.balance
    end.
balance() -> integer_balance().
mempool() -> lists:reverse((tx_pool:get())#tx_pool.txs).
halt() -> off().
off() ->
    testnet_sup:stop(),
    ok = application:stop(amoveo_core),
    ok = application:stop(amoveo_http).
mine_block() ->
    block:mine(10000000).
    %potential_block:save(),
    %block:mine(1, 100000).
mine_block(0, Times) -> ok;
mine_block(Periods, Times) ->
    %potential_block:save(),
    %PB = block:top(),
    %Top = block:block_to_header(PB),
    %Txs = (tx_pool:get())#tx_pool.txs,
    %Block = block:make(Top, Txs, PB#block.trees, keys:pubkey()),
    %block:mine(Block, Times),

    timer:sleep(100),
    block:mine(Times),
    mine_block(Periods-1, Times).
mine_block(_, _, _) -> %only creates a headers, no blocks.
%This is only used for testing purposes.
    PB = potential_block:read(),
    WB = block:mine2(PB, 1000000),
    Header = block:block_to_header(WB),
    headers:absorb([Header]).
channel_close() ->
    channel_close(?IP, ?Port).
channel_close(IP, Port) ->
    Cost = trees:dict_tree_get(governance, ctc),
    channel_close(IP, Port, ?Fee+Cost).
channel_close(IP, Port, Fee) ->
    {ok, PeerId} = talker:talk({pubkey}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = testnet_sign:data(CD#cd.them),
    Dict = (tx_pool:get())#tx_pool.dict,
    Height = (block:get_by_hash(headers:top()))#block.height,
    SS = CD#cd.ssthem,
    SS = [],
    {Amount, _Nonce, _Delay} = spk:dict_run(fast, SS, SPK, Height, 0, Dict),
    CID = SPK#spk.cid,
    Channel = trees:dict_tree_get(channels, CID),
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    {ok, TV} = talker:talk({time_value}, IP, Port),%We need to ask the server for their time_value.
    Expires = CD#cd.expiration,
    LifeSpan= max(0, Expires - Height),
    CFee = TV * LifeSpan * (Bal1 + Bal2) div 100000000,
    Tx = channel_team_close_tx:make_dict(CID, Amount-CFee,Fee),
    STx = keys:sign(Tx),
    {ok, SSTx} = talker:talk({channel_close, CID, keys:pubkey(), SS, STx}, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    0.
channel_solo_close() -> channel_solo_close({127,0,0,1}, 3010).
channel_solo_close(IP, Port) ->
    {ok, Other} = talker:talk({pubkey}, IP, Port),
    channel_solo_close(Other).
channel_solo_close(Other) ->
    Fee = free_constants:tx_fee(),
    {ok, CD} = channel_manager:read(Other),
    SSPK = CD#cd.them,
    SS = CD#cd.ssthem,
    Tx = channel_solo_close:make_dict(keys:pubkey(), Fee, keys:sign(SSPK), SS),
    STx = keys:sign(Tx),
    tx_pool_feeder:absorb(STx),
    ok.
channel_solo_close(_CID, Fee, SPK, ScriptSig) ->
    tx_maker0(channel_solo_close:make_dict(keys:pubkey(), Fee, SPK, ScriptSig)).
add_peer(IP, Port) ->
    peers:add({IP, Port}),
    0.
%sync() -> sync(?IP, ?Port).
sync() -> 
    spawn(fun() -> sync:start() end),
    0.
sync(IP, Port) -> 
    spawn(fun() -> sync:start([{IP, Port}]) end),
    0.
sync(2, IP, Port) ->
    spawn(fun() -> sync:get_headers({IP, Port}) end),
    0.
keypair() -> keys:keypair().
pubkey() -> base64:encode(keys:pubkey()).
new_pubkey(Password) -> keys:new(Password).
new_keypair() -> testnet_sign:new_key().
test() -> {test_response}.
channel_keys() -> channel_manager:keys().
keys_lock() ->
    keys:lock(),
    0.
keys_unlock() ->
    keys:lock(),
    0.
keys_unlock(Password) ->
    keys:unlock(Password),
    0.
keys_new(Password) ->
    keys:new(Password),
    0.
market_match(OID) ->
    order_book:match_all([OID]),
    {ok, ok}.
settle_bets() ->
    channel_feeder:bets_unlock(channel_manager:keys()),
    {ok, ok}.
new_market(OID, Expires, Period) -> %<<5:256>>, 4000, 5
    %sets up an order book.
    %turns on the api for betting.
    %for now lets use the oracle id as the market id. this wont work for combinatorial markets.
    TPG = tx_pool:get(),
    Height = TPG#tx_pool.height,
    {ok, Confirmations} = application:get_env(amoveo_core, confirmations_needed),
    OldBlock = block:get_by_height(Height - Confirmations),
    OldTrees = OldBlock#block.trees,
    io:fwrite("api oid is "),
    io:fwrite(packer:pack([OID, OldTrees, Height-Confirmations])),
    io:fwrite("\n"),
    false = empty == trees:dict_tree_get(oracles, OID, dict:new(), OldTrees),%oracle existed confirmation blocks ago.
    
    order_book:new_market(OID, Expires, Period).
trade(Price, Type, Amount, OID, Height) ->
    trade(Price, Type, Amount, OID, Height, ?IP, ?Port).
trade(Price, Type, Amount, OID, Height, IP, Port) ->
    trade(Price, Type, Amount, OID, Height, ?Fee*2, IP, Port).
trade(Price, Type, A, OID, Height, Fee, IP, Port) ->
    Amount = A,
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    {ok, {Expires, 
	  Pubkey, %pubkey of market maker
	  Period}} = 
	talker:talk({market_data, OID}, IP, Port),
    BetLocation = constants:oracle_bet(),
    MarketID = OID,
    %type is true or false or one other thing...
    MyHeight = api:height(),
    true = Height =< MyHeight,
    SC = market:market_smart_contract(BetLocation, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID, Height),
    SSPK = channel_feeder:trade(Amount, Price, SC, ServerID, OID),
    Msg = {trade, keys:pubkey(), Price, Type, Amount, OID, SSPK, Fee},
    Msg = packer:unpack(packer:pack(Msg)),%sanity check
    {ok, SSPK2} =
	talker:talk(Msg, IP, Port),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    channel_manager_update(ServerID, SSPK2, market:unmatched(OID)),
    0.
cancel_trade(N) ->
    cancel_trade(N, ?IP, ?Port).
cancel_trade(N, IP, Port) ->
    %the nth bet in the channel (starting at 2) is an unmatched trade that we want to cancel.
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    channel_feeder:cancel_trade(N, ServerID, IP, Port),
    0.
combine_cancel_assets() ->
    combine_cancel_assets(?IP, ?Port).
combine_cancel_assets(IP, Port) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    channel_feeder:combine_cancel_assets(ServerID, IP, Port),
    0.
txs({IP, Port}) ->
    txs(IP, Port).
txs(IP, Port) ->
    sync:trade_txs({IP, Port}),
    0.
-define(mining, "data/mining_block.db").
work(Nonce, _) ->
    Block = potential_block:check(),
    Height = Block#block.height,
    F2 = forks:get(2),
    N = if
	       F2 > Height -> 
		<<X:256>> = Nonce,
		X;
	       true -> 
		<<X:184>> = Nonce,
		X
	end,
    Block2 = Block#block{nonce = N},
    %io:fwrite("work block hash is "),
    %io:fwrite(packer:pack(hash:doit(block:hash(Block)))),
    %io:fwrite(packer:pack(hash:doit(block:hash(Block2)))),
    io:fwrite("pool found a block"),
    io:fwrite("\n"),
    Header = block:block_to_header(Block2),
    headers:absorb([Header]),%uses call
    headers:absorb_with_block([Header]),%uses call
    %block_absorber:save(Block2),
    block_organizer:add([Block2]),
    %spawn(fun() -> 
    %timer:sleep(1000),
    potential_block:save(),
    %sync:start() ,
	%  end),
    0.
mining_data() ->
    normal = sync_mode:check(),
    Block = potential_block:read(),
    %io:fwrite("mining data block hash is "),
    %io:fwrite(packer:pack(hash:doit(block:hash(Block)))),
    %io:fwrite("\n"),
    F2 = forks:get(2),
    Height = Block#block.height,
    Entropy = if
	       F2 > Height -> 32;
	       true -> 23
	   end,
    [hash:doit(block:hash(Block)),
     crypto:strong_rand_bytes(Entropy), 
     %headers:difficulty_should_be(Top)].
     Block#block.difficulty].
sync_normal() ->
    sync_mode:normal(),
    0.
sync_quick() ->
    sync_mode:quick(),
    0.
   
mining_data(X) ->
    mining_data(X, 30).
mining_data(X, Start) ->
    L = lists:map(fun(N) -> round(block:hashrate_estimate(N)) end, lists:seq(Start, block:height(), X)).

pubkey(Pubkey, Many, TopHeight) ->
    amoveo_utils:address_history(quiet, Pubkey, Many, TopHeight).
%curl -i -d '["pubkey", "BEwcawKx5oZFOmp1533TqDzUl76fOeLosDl+hwv6rZ50tLSQmMyW/87saj3D5qBtJI4lLsILllpRlT8/ppuNaPM=", 100, 18000]' http://localhost:8081
