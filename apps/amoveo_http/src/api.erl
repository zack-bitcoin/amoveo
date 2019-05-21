-module(api).
-compile(export_all).
-export([new_market/6]).
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
block_hash(N) ->    
    %returns the hash of block N
    B = block:get_by_height(N),
    block:hash(B).
ewah(Start, End) ->
    headers:ewah_range(Start, End).
top() ->
    TopHeader = headers:top(),
    Height = TopHeader#header.height,
    {top, TopHeader, Height}.
top(1) ->
    H = headers:top_with_block(),
    [H, block:hash(H)].
sign(Tx) -> keys:sign(Tx).
tx_maker0(Tx) -> 
    case sync_mode:check() of
	quick ->
	    S = "error, you need to be in sync mode normal to make txs",
	    io:fwrite(S),
	    ok;
	normal ->
	    case keys:sign(Tx) of
		{error, locked} -> 
		    io:fwrite("your password is locked. use `keys:unlock(\"PASSWORD1234\")` to unlock it"),
		    ok;
		Stx -> 
		    ok = tx_pool_feeder:absorb(Stx),
		    hash:doit(Tx)
	    end
    end.
create_account(NewAddr, Amount) ->
    case keys:status() of
        locked -> {error, "need to decrypt private key"};
        unlocked ->
            Cost = trees:get(governance, create_acc_tx),
            create_account(NewAddr, Amount, ?Fee + Cost)
    end.
create_account(NewAddr, Amount, Fee) when size(NewAddr) == 65 ->
    Tx = create_account_tx:make_dict(NewAddr, Amount, Fee, keys:pubkey()),
    tx_maker0(Tx);
create_account(N, A, F) ->
    M = base64:decode(N),
    create_account(M, A, F).
coinbase(_) ->
    K = keys:pubkey(),
    tx_maker0(coinbase_tx:make_dict(K)).
spend(L) when is_list(L) ->
    Txs = multi_spend(L),
    Fee = multi_fee(Txs),
    MTx = multi_tx:make_dict(keys:pubkey(), Txs, Fee),
    tx_maker0(MTx).
multi_spend([]) -> [];
multi_spend([{Amount, Pubkey}|T]) ->
    ID = decode_pubkey(Pubkey),
    K = keys:pubkey(),
    Tx = if
	     ID == K -> io:fwrite("don't spend to yourself\n"),
			1 = 2;
	     true ->
		 B = trees:get(accounts, ID),
		 if
		     (B == empty) -> 
			 create_account_tx:make_dict(ID, Amount, 0, K);
		     true -> 
			 spend_tx:make_dict(ID, Amount, 0, K)
		 end
	 end,
    [Tx|multi_spend(T)].
multi_fee([]) -> 0;
multi_fee([H|T]) ->
    Type = element(1, H),
    Cost = trees:get(governance, Type),
    Cost + multi_fee(T) + ?Fee.


spend(ID0, Amount) ->
    ID = decode_pubkey(ID0),
    K = keys:pubkey(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    B = trees:get(accounts, ID),
            if 
               (B == empty) ->
                    create_account(ID, Amount);
                true ->
		    Cost = trees:get(governance, spend),
                    spend(ID, Amount, ?Fee+Cost)
            end
    end.
spend(ID0, Amount, Fee) ->
    ID = decode_pubkey(ID0),
    case keys:status() of
        locked -> {error, "need to decrypt private key"};
        unlocked ->
            tx_maker0(spend_tx:make_dict(ID, Amount, Fee, keys:pubkey()))
    end.
delete_account(ID0) ->
    ID = decode_pubkey(ID0),
    Cost = trees:get(governance, delete_acc_tx),
    delete_account(ID, ?Fee + Cost).
delete_account(ID0, Fee) ->
    ID = decode_pubkey(ID0),
    tx_maker0(delete_account_tx:make_dict(ID, keys:pubkey(), Fee)).
new_channel_tx(CID, Acc2, Bal1, Bal2, Delay) ->
    Cost = trees:get(governance, nc),
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
    Cost = trees:get(governance, nc),
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
signed(Signed, Pub) ->
    X = element(2, Signed),
    S = element(3, Signed),
    sign:verify_sig(X, S, Pub).
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
    _NewHeight = TP#tx_pool.height,
    Amount = SPK#spk.amount,
    BetAmounts = sum_bets(SPK#spk.bets),
    CID = SPK#spk.cid,
    Channel = trees:get(channels, CID),
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
    Cost = trees:get(governance, ctc),
    channel_team_close(CID, Amount, ?Fee+Cost).
channel_team_close(CID, Amount, Fee) ->
    Tx = channel_team_close_tx:make_dict(CID, Amount, Fee),
    %keys:sign(Tx).
    Tx.
channel_timeout(1) -> %if you are running a channel node server.
    channels:close_many().
    
channel_timeout() ->
    channel_timeout(constants:server_ip(), constants:server_port()).
channel_timeout(Ip, Port) ->
    {ok, Other} = talker:talk({pubkey}, Ip, Port),
    {ok, Fee} = application:get_env(amoveo_core, tx_fee),
    {ok, CD} = channel_manager:read(Other),
    CID = CD#cd.cid,
    {Tx, _} = channel_timeout_tx:make_dict(keys:pubkey(), CID, Fee),
    case keys:sign(Tx) of
        {error, locked} ->
            io:fwrite("your password is locked");
        Stx ->
            tx_pool_feeder:absorb(Stx)
    end.
channel_slash(_CID, Fee, SPK, SS) ->
    tx_maker0(channel_slash_tx:make_dict(keys:pubkey(), Fee, SPK, SS)).
new_scalar_oracle(Start, Question) ->
    new_scalar_oracle(Start, Question, 10).
new_scalar_oracle(Start, Question, Many) ->
    <<ID:256>> = find_id2(),
    new_scalar_oracle(Start, Question, ID, Many).
new_scalar_oracle(Start, Question, ID, Many) when is_binary(ID) ->
    <<IDN:256>> = ID,
    new_scalar_oracle(Start, Question, IDN, Many);
new_scalar_oracle(Start, Question, ID, Many) ->
    Many = 10,
    Q2 = if
	     is_list(Question) -> list_to_binary(Question);
	     true -> Question
	 end,
    Cost = trees:get(governance, oracle_new),
    nso2(keys:pubkey(), ?Fee+Cost+20, Q2, Start, ID, 0, Many),
    <<ID:256>>.
-define(GAP, 0).%how long to wait between the limits when the different oracles of a scalar market can be closed.
nso2(_Pub, _C, _Q, _S, _ID, Many, Many) -> 0;
nso2(Pub, C, Q, S, ID, Many, Limit) -> 
    io:fwrite("nso2\n"),
    Q2 = <<Q/binary, (list_to_binary("  most significant is bit 0. this is bit number: "))/binary, (list_to_binary(integer_to_list(Many)))/binary>>,
    Tx = oracle_new_tx:make_dict(Pub, C, Q2, S, <<ID:256>>, 0, 0),
    io:fwrite(packer:pack(Tx)),
    %tx_pool_feeder:absorb(keys:sign(Tx)),
    tx_maker0(Tx),
    nso2(Pub, C, Q, S+?GAP, ID+1, Many+1, Limit).
    
new_question_oracle(Start, Question)->
    ID = find_id2(),
    new_question_oracle(Start, Question, ID).

new_question_oracle(Start, Question, ID)->
    Q2 = if
	     is_list(Question) -> list_to_binary(Question);
	     true -> Question
	 end,
    Cost = trees:get(governance, oracle_new),
    tx_maker0(oracle_new_tx:make_dict(keys:pubkey(), ?Fee+Cost, Q2, Start, ID, 0, 0)),
    ID.
new_governance_oracle(GovName, GovAmount) ->
    GovNumber = governance:name2number(GovName),
    ID = find_id2(),
    %Recent = trees:get(oracles, DiffOracleID),
    Cost = trees:get(governance, oracle_new),
    Tx = oracle_new_tx:make_dict(keys:pubkey(), ?Fee + Cost, <<>>, 0, ID, GovNumber, GovAmount),
    tx_maker0(Tx),
    ID.
oracle_bet(OID, Type, Amount) ->
    Cost = trees:get(governance, oracle_bet),
    oracle_bet(?Fee+Cost, OID, Type, Amount).
oracle_bet(Fee, OID, Type, Amount) ->
    tx_maker0(oracle_bet_tx:make_dict(keys:pubkey(), Fee, OID, Type, Amount)).
minimum_scalar_oracle_bet(OID, N) ->
    true = is_integer(N),
    true = (N > -1),
    true = (N < 1024),
    Amount = trees:get(governance, oracle_question_liquidity) + 1,
    Bits = lists:reverse(to_bits(N, 10)),
    <<OIDN:256>> = OID,
    msob2(OIDN, Amount, 0, Bits).
scalar_oracle_close(OID) ->
    <<OIDN:256>> = OID,
    scalar_oracle_close2(OIDN, 10).
scalar_oracle_close2(_, 0) -> ok;
scalar_oracle_close2(OIDN, N) ->
    oracle_close(<<OIDN:256>>),
    scalar_oracle_close2(OIDN+1, N-1).
msob2(_, _, _, []) -> ok;
msob2(OIDN, Amount, N, [H|T]) ->
    spawn(fun() ->
                  oracle_bet(<<(OIDN + N):256>>, H, Amount)
          end),
    timer:sleep(200),
    msob2(OIDN, Amount, N+1, T).
to_bits(_, 0) -> [];
to_bits(X, N) when (0 == (X rem 2)) ->
    [2|to_bits(X div 2, N-1)];
to_bits(X, N) ->
    Y = (X - 1) div 2,
    [1|to_bits(Y, N-1)].
    
oracle_close(OID) ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Dict = (tx_pool:get())#tx_pool.dict,
    Cost = trees:get(governance, oracle_close, Dict, Trees),
    oracle_close(?Fee+Cost, OID).
oracle_close(Fee, OID) ->
    tx_maker0(oracle_close_tx:make_dict(keys:pubkey(), Fee, OID)).
oracle_winnings(OID) ->
    Cost = trees:get(governance, oracle_winnings),
    oracle_winnings(?Fee+Cost, OID).
oracle_winnings(Fee, OID) ->
    tx_maker0(oracle_winnings_tx:make_dict(keys:pubkey(), Fee, OID)).
scalar_oracle_winnings(OID) -> 
    Cost = trees:get(governance, oracle_winnings),
    scalar_oracle_winnings(?Fee+Cost, OID).
scalar_oracle_winnings(Fee, OID) -> %for scalar oracles
    <<OIDN:256>> = OID,
    scalar_oracle_winnings(Fee, OIDN, 10).
scalar_oracle_winnings(_, _, 0) -> 0;
scalar_oracle_winnings(Fee, OIDN, N) ->
    oracle_winnings(Fee, <<OIDN:256>>),
    scalar_oracle_winnings(Fee, OIDN + 1, N - 1).
oracle_unmatched(OracleID) ->
    Cost = trees:get(governance, unmatched),
    oracle_unmatched(?Fee+Cost, OracleID).
oracle_unmatched(Fee, OracleID) ->
    tx_maker0(oracle_unmatched_tx:make_dict(keys:pubkey(), Fee, OracleID)).
scalar_oracle_unmatched(OID) -> 
    Cost = trees:get(governance, unmatched),
    scalar_oracle_unmatched(?Fee+Cost, OID).
scalar_oracle_unmatched(Fee, OID) -> %for scalar oracles
    <<OIDN:256>> = OID,
    scalar_oracle_unmatched(Fee, OIDN, 10).
scalar_oracle_unmatched(_, _, 0) -> 0;
scalar_oracle_unmatched(Fee, OIDN, N) ->
    oracle_unmatched(Fee, <<OIDN:256>>),
    scalar_oracle_unmatched(Fee, OIDN + 1, N - 1).
tree_common(TreeName, ID) ->
    X = trees:get(TreeName, ID),
    case X of
        empty -> 0;
        _ -> X
    end.
tree_common(TreeName, ID, BlockHash) ->
    B = block:get_by_hash(BlockHash),
    %T = block:trees(B),
    T = B#block.trees,
    X = trees:get(TreeName, ID, dict:new(), T),
    case X of
        empty -> 0;
        _ -> X
    end.
    
governance(ID) ->
    tree_common(governance, ID).
governance(ID, BlockHash) ->
    tree_common(governance, ID, BlockHash).
channel(ID) ->
    tree_common(channels, ID).
channel(ID, BlockHash) ->
    tree_common(channel, ID, BlockHash).
existence(ID) ->
    tree_common(existence, ID).
existence(ID, BlockHash) ->
    tree_common(existence, ID, BlockHash).
oracle(ID) ->
    tree_common(oracles, ID).
oracle(ID, BlockHash) ->
    tree_common(oracle, ID, BlockHash).
account(P) ->
    Pubkey = decode_pubkey(P),
    tree_common(accounts, Pubkey).
account(P, BlockHash) ->
    Pubkey = decode_pubkey(P),
    tree_common(oracle, Pubkey, BlockHash).
account() -> account(keys:pubkey()).
confirmed_balance(P) ->
    Pubkey = decode_pubkey(P),
    M = max(api:height() - 10, 1),
    Block = block:get_by_height(M),
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
    sync:stop(),
    timer:sleep(1000),
    testnet_sup:stop(),
    ok = application:stop(amoveo_core),
    ok = application:stop(amoveo_http).
test_mine_blocks(S) ->
    spawn(fun() -> test_mine_blocks2(S) end).
test_mine_blocks2(S) ->
    mine_block(),
    timer:sleep(S*1000),
    case sync_mode:check() of
        normal ->
            test_mine_blocks(S);
        _ -> ok
    end.
mine_block() ->
    block:mine(10000000).
    %potential_block:save(),
    %block:mine(1, 100000).
mine_block(0, _Times) -> ok;
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
    Cost = trees:get(governance, ctc),
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
    Channel = trees:get(channels, CID),
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
    {ok, CD} = channel_manager:read(Other),
    SSPK = CD#cd.them,
    SS = CD#cd.ssthem,
    Tx = channel_solo_close:make_dict(keys:pubkey(), ?Fee, keys:sign(SSPK), SS),
    STx = keys:sign(Tx),
    tx_pool_feeder:absorb(STx),
    ok.
channel_solo_close(_CID, Fee, SPK, ScriptSig) ->
    tx_maker0(channel_solo_close:make_dict(keys:pubkey(), Fee, SPK, ScriptSig)).
add_peer(IP, Port) ->
    peers:add({IP, Port}),
    0.
%sync() -> sync(?IP, ?Port).
%curl -d '["sync", 2, [x,x,x,x], 8080]' localhost:8081
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
    keys:unlock(""),
    0.
keys_unlock(Password) ->
    keys:unlock(Password),
    0.
keys_new(Password) ->
    %WARNING!!! THIS DELETES YOUR PRIVATE KEY!!! 
    %there is a different command for saving your 
    %private key and deleting your password.
    keys:new(Password),
    0.
market_match(OID) ->
    %maybe this should be removed? does it break the order book?
    order_book:match_all([OID]),
    {ok, ok}.
settle_bets() ->
    channel_feeder:bets_unlock(channel_manager:keys()),
    {ok, ok}.
not_empty_oracle(_, 0, _) -> true;
not_empty_oracle(OID, Many, OldTrees) ->
    io:fwrite("not empty oracle "),
    io:fwrite(integer_to_list(Many)),
    io:fwrite("\n"),
    X = trees:get(oracles, <<OID:256>>, dict:new(), OldTrees),
    (not (empty == X)) and not_empty_oracle(OID+1, Many-1, OldTrees).
new_market(OID, Expires, Period, LL, UL, Many) -> %<<5:256>>, 4000, 5
    true = LL > -1,
    true = LL < UL,
    true = UL < (round(math:pow(2, Many))),
    TPG = tx_pool:get(),
    Height = TPG#tx_pool.height,
    {ok, Confirmations} = application:get_env(amoveo_core, confirmations_needed),
    OldBlock = block:get_by_height(Height - Confirmations),
    OldTrees = OldBlock#block.trees,
    <<OIDN:256>> = OID,
    true = not_empty_oracle(OIDN, Many, OldTrees),
    order_book:new_scalar_market(OID, Expires, Period, LL, UL, Many).
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
    false = empty == trees:get(oracles, OID, dict:new(), OldTrees),%oracle existed confirmation blocks ago.
    
    order_book:new_market(OID, Expires, Period).
as_binary(Size, B) ->
    if
        is_binary(B) ->
            S = size(B),
            if
                S == Size -> B;
                true -> base64:decode(B)
            end;
        true -> base64:decode(B)
    end.
                    
trade(Price, Type, Amount, OID, Height) ->
    trade(Price, Type, Amount, OID, Height, ?IP, ?Port).
trade(Price, Type, Amount, OID, Height, IP, Port) ->
    trade(Price, Type, Amount, OID, Height, ?Fee*2, IP, Port).
trade(Price, Type, A, OID, Height, Fee, IP, Port) ->
    Amount = A,
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    {ok, {OB, Question}} = talker:talk({oracle, OID}, IP, Port),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    Pubkey = ServerID,
    OBData = order_book:ob_type(OB),%include this
    MyHeight = api:height(),
    true = Height =< MyHeight,
    MarketID = OID,
%    {ok, {Expires, 
%	  Pubkey, %pubkey of market maker
%	  Period}} = 
%	talker:talk({market_data, OID}, IP, Port),
    SC = channel_feeder:contract_market(OBData, OID, Type, Expires, Price, ServerID, Period, Amount, OID, Height),
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
    %io:fwrite("pool found a block"),
    %io:fwrite("\n"),
    Header = block:block_to_header(Block2),
    headers:absorb([Header]),%uses call
    %headers:absorb_with_block([Header]),%uses call
    %block_absorber:save(Block2),
    block_organizer:add([Block2]),
    %spawn(fun() -> 
    %timer:sleep(1000),
    potential_block:save(),
    %sync:start() ,
	%  end),
    0.
mining_data() ->
    case sync_mode:check() of
	quick -> 0;
	normal ->
	    Block = potential_block:read(),
    %io:fwrite("mining data block hash is "),
    %io:fwrite(packer:pack(hash:doit(block:hash(Block)))),
    %io:fwrite("\n"),
	    case Block of
		"" ->
		    ok;
		    %timer:sleep(100),
		    %mining_data();
		_ ->
		    F2 = forks:get(2),
		    Height = Block#block.height,
		    Entropy = if
				  F2 > Height -> 32;
				  true -> 23
			      end,
		    [hash:doit(block:hash(Block)),
		     crypto:strong_rand_bytes(Entropy), 
     %headers:difficulty_should_be(Top)].
		     Block#block.difficulty]
	    end
    end.
orders(OID) ->
    %if the OID is encoded, we should decode it to binary form.
    Oracle = trees:get(oracles, OID),
    X = oracles:orders(Oracle),
    IDs = orders:all(X),
    lists:map(fun(Y) -> orders:get(Y, X) end, IDs).
oracles() ->
    oracles:all().
channels_from(Address) ->
    CA = channels:all(),
    channels_from2(CA, Address).
channels_from2([], _) -> [];
channels_from2([X|T], Address) ->
    B1 =  element(3, X) == base64:decode(Address),
    B2 =  element(4, X) == base64:decode(Address),
    if
        (B1 or B2) -> [X|channels_from2(T, Address)];
        true -> channels_from2(T, Address)
    end.
    
		      

sync_normal() ->
    sync_mode:normal(),
    0.
sync_quick() ->
    sync_mode:quick(),
    0.
   
mining_data(X) ->
    mining_data(X, 30).
mining_data(X, Start) ->
    lists:map(fun(N) -> round(block:hashrate_estimate(N)) end, lists:seq(Start, block:height(), X)).

pubkey(Pubkey, Many, TopHeight) ->
    amoveo_utils:address_history(quiet, Pubkey, Many, TopHeight).
%curl -i -d '["pubkey", "BEwcawKx5oZFOmp1533TqDzUl76fOeLosDl+hwv6rZ50tLSQmMyW/87saj3D5qBtJI4lLsILllpRlT8/ppuNaPM=", 100, 18000]' http://localhost:8081

atomic_payment(To, Commit, Amount) ->
    %we can use this api call without syncing the blockchain.

    ok.%returns new_channel offer
    
atomic_receive(NC, Secret, Amount) ->
    %verify the new_channel offer has the correct commit in it, and that it is sending the correct amount.
    %They put 21 in the contract, we put 1. If we fail to reveal the secret, it all goes to them after a delay of like 1 day.
    %if we do reveal the secret, then we get a contract for stable bitcoin. revealing the secret also allows them to unlock the bitcoin we sent them.
    ok.%if it succeeds, it publishes the new_channel tx, and once we have a confirmation, it uses the secret to update our channel state with our partner
