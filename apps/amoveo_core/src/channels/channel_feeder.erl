%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/3, spend/2, close/2, lock_spend/7,
	 agree_bet/4, garbage/0, combine_cancel_assets/3,
	 new_channel_check/1, other/1,
	 update_to_me/2, new_cd/6,
	 make_locked_payment/3, they_simplify/3,
	 bets_unlock/1, trade/5, trade/7,
         cancel_trade/4, cancel_trade_server/3,
         combine_cancel_assets_server/2,
	 contract_market/10
	 ]).
-include("../records.hrl").
new_cd(Me, Them, SSMe, SSThem, CID, Expiration) ->
    #cd{me = Me, them = Them, ssthem = SSThem, ssme = SSMe, live = true, cid = CID, expiration = Expiration}.
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) ->
    %check if any of the channels haven't existed in the last free_constants:fork_tolerance() blocks. If it hasn't, then delete it.
    Keys = channel_manager:keys(),
    {C, OldC} = c_oldc(),
    garbage_helper(Keys, C, OldC),
    {noreply, X};
handle_cast({new_channel, Tx, SSPK, Expires}, X) ->
    %a new channel with our ID was just created on-chain. We should record an empty SPK in this database so we can accept channel payments.
    SPK = testnet_sign:data(SSPK),
    CD = #cd{me = SPK, them = SSPK, cid = new_channel_tx:cid(Tx), expiration = Expires},
    channel_manager:write(other(Tx), CD),
    {noreply, X};
handle_cast({close, SS, STx}, X) ->
    %closing the channel at it's current SPK
    Tx = testnet_sign:data(STx),
    OtherID = other(Tx),
    {ok, CD} = channel_manager:read(OtherID),
    true = CD#cd.live,
    SPKM = CD#cd.me,
    A1 = SPKM#spk.acc1, 
    A2 = SPKM#spk.acc2,
    SPK = testnet_sign:data(CD#cd.them),
    A3 = channel_team_close_tx:acc1(Tx),
    A4 = channel_team_close_tx:acc2(Tx),
    K = keys:pubkey(),
    true = (((A1 == A3) and (A2 == A4)) or ((A1 == A4) and (A2 == A3))),
    Direction = if
		    K == A1 -> -1;
		    K == A2 -> 1;
		    true -> K = A1
		end,
    DemandedAmount = channel_team_close_tx:amount(Tx),
    TP = tx_pool:get(),
    TD = TP#tx_pool.dict,
    Height = TP#tx_pool.height,
    {CalculatedAmount, NewNonce, _} = spk:dict_run(safe, SS, SPK, Height, 0, TD),
    {OldAmount, OldNonce, _} = spk:dict_run(safe, CD#cd.ssthem, SPK, Height, 0, TD),
    if
	NewNonce > OldNonce -> ok; 
	NewNonce == OldNonce ->
	    true = Direction*(CalculatedAmount - OldAmount) >= 0; %otherwise we would prefer to publish our data instead of signing theirs, and we would profit.
	true -> NewNonce = OldNonce %crash
    end,
    true = Direction*(DemandedAmount - CalculatedAmount) >= 0,%They shouldn't take any more money than we calculated they can take
    {ok, OldCD} = channel_manager:read(OtherID),
    NewCD = OldCD#cd{live = false},
    true = other(A1, A2) == OtherID,
    channel_manager:write(OtherID, NewCD),
    tx_pool_feeder:absorb(keys:sign(STx)),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({combine_cancel_assets, TheirPub, IP, Port}, _From, X) ->
    {ok, OldCD} = channel_manager:read(TheirPub),
    {SSPK, NewSS} = combine_cancel_common(OldCD),
    Msg = {combine_cancel_assets, keys:pubkey(), SSPK},
    Msg = packer:unpack(packer:pack(Msg)),
    {ok, SSPK2} = talker:talk(Msg, IP, Port),
    true = testnet_sign:verify(keys:sign(SSPK2)),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = NewSS, ssthem = NewSS},
    channel_manager:write(TheirPub, NewCD),
    {reply, ok, X};
handle_call({combine_cancel_assets_server, TheirPub, SSPK2}, _From, X) ->
    {ok, OldCD} = channel_manager:read(TheirPub),
    {SSPK, NewSS} = combine_cancel_common(OldCD),
    SPK = testnet_sign:data(SSPK),
    SPK2 = testnet_sign:data(SSPK2),
    io:fwrite("combine cancel assets spks should match\n"),
    io:fwrite(packer:pack(SPK)),%didn't close any bets
    io:fwrite("\n"),
    io:fwrite(packer:pack(SPK2)),%closes 2 bets. this is correct. This was generated in javascript.
    io:fwrite("\n"),
    SPK = SPK2,
    Bets = (OldCD#cd.me)#spk.bets,
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = NewSS, ssthem = NewSS},
    channel_manager:write(TheirPub, NewCD),
    {reply, SSPK, X};
handle_call({cancel_trade_server, N, TheirPub, SSPK2}, _From, X) ->
    {ok, OldCD} = channel_manager:read(TheirPub),
    SSPK = cancel_trade_common(N, OldCD), 
    SPK = testnet_sign:data(SSPK),
    SPK2 = testnet_sign:data(SSPK2),
    SPK = SPK2,
    Bets = (OldCD#cd.me)#spk.bets,
    Bet = element(N-1, list_to_tuple(Bets)),
    {Type, Price} = Bet#bet.meta,
    CodeKey = Bet#bet.key,
    OID = case CodeKey of
	      {market, 1, _, _, _, _, Z} -> Z;
	      {market, 2, _, _, _, _, Y, _, _} -> Y
	  end,
    %{market, 1, _, _, _, _, OID} = CodeKey,
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = spk:remove_nth(N-1, OldCD#cd.ssme),
                     ssthem = spk:remove_nth(N-1, OldCD#cd.ssthem)},
    Type2 = case Type of
                1 -> buy;
                2 -> sell
            end,
    ok = order_book:remove(TheirPub, Type2, Price, OID),
    channel_manager:write(TheirPub, NewCD),
    {reply, keys:sign(SSPK2), X};
handle_call({cancel_trade, N, TheirPub, IP, Port}, _From, X) ->
    {ok, OldCD} = channel_manager:read(TheirPub),
    SSPK = cancel_trade_common(N, OldCD),

    Msg = {cancel_trade, keys:pubkey(), N, SSPK},
    Msg = packer:unpack(packer:pack(Msg)),
    {ok, SSPK2} = talker:talk(Msg, IP, Port),
    true = testnet_sign:verify(keys:sign(SSPK2)),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = spk:remove_nth(N-1, OldCD#cd.ssme),
                     ssthem = spk:remove_nth(N-1, OldCD#cd.ssthem)},
    channel_manager:write(TheirPub, NewCD),
    {reply, ok, X};
handle_call({trade, ID, Price, Type, Amount, OID, SSPK, Fee}, _From, X) ->%id is an account pubkey
    TP = tx_pool:get(),
    Height = TP#tx_pool.height,
    io:fwrite("channel feeder trade\n"),
    io:fwrite(packer:pack(SSPK)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(keys:sign(SSPK))),
    io:fwrite("\n"),
    io:fwrite("channel feeder serialized\n"),
    io:fwrite(base64:encode(sign:serialize(testnet_sign:data(SSPK)))),
    io:fwrite("\n"),
    true = testnet_sign:verify(keys:sign(SSPK)),%breaks here
    true = Amount > 0,
    {ok, LF} = application:get_env(amoveo_core, lightning_fee),
    true = Fee > LF,
    <<_:256>> = OID,
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    SC = contract_market(order_book:ob_type(OB), OID, Type, Expires, Price, keys:pubkey(), Period, Amount, OID, Height),
    %BetLocation = constants:oracle_bet(),
    %SC = market:market_smart_contract(BetLocation, OID, Type, Expires, Price, keys:pubkey(), Period, Amount, OID, Height),
    {ok, OldCD} = channel_manager:read(ID),
    ChannelExpires = OldCD#cd.expiration,
    true = Expires < ChannelExpires,%The channel has to be open long enough for the market to close.
    SSPK2 = trade(Amount, Price, SC, ID, OID),%bad
    SPK = testnet_sign:data(SSPK),
    SPK2 = testnet_sign:data(SSPK2),%bad
    io:fwrite("channel feeder spks \n"),
    io:fwrite(packer:pack(SPK)), %bet amount 500
    io:fwrite("\n"),
    io:fwrite(packer:pack(SPK2)), %bet amount 750
    io:fwrite("\n"),
    SPK = SPK2,
    DefaultSS = market:unmatched(OID),
    SSME = [DefaultSS|OldCD#cd.ssme],
    SSThem = [DefaultSS|OldCD#cd.ssthem],
    %Dict = TP#tx_pool.dict,
    %spk:dict_run(fast, SSME, SPK, Height, 0, Dict),%sanity test
    %spk:dict_run(fast, SSThem, SPK, Height, 0, Dict),%sanity test
    NewCD = OldCD#cd{them = SSPK, me = SPK, 
		     ssme = SSME, ssthem = SSThem},
    Channel = trees:get(channels, NewCD#cd.cid),
    SPKAmount = SPK#spk.amount,
    BetAmount = api:sum_bets(SPK#spk.bets),
    true = 0 < (channels:bal1(Channel) + SPKAmount),
    true = 0 < (channels:bal2(Channel) - SPKAmount - BetAmount),
    channel_manager:write(ID, NewCD),
    {reply, keys:sign(SSPK), X};
handle_call({lock_spend, SSPK, Amount, Fee, Code, Sender, Recipient, ESS}, _From, X) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    true = testnet_sign:verify(keys:sign(SSPK)),
    true = Amount > 0,
    {ok, LightningFee} = application:get_env(amoveo_core, lightning_fee),
    true = Fee > LightningFee,
    Return = make_locked_payment(Sender, Amount+Fee, Code),
    SPK = testnet_sign:data(SSPK),%first is from them
    SPK22 = testnet_sign:data(Return),
    io:fwrite("lock spend compare spks "),
    io:fwrite(packer:pack([SPK, SPK22])),
    SPK = SPK22,
    {ok, OldCD} = channel_manager:read(Sender),
    OldSPK = testnet_sign:data(OldCD#cd.them),
    %Use OldSPK to see if they have enough money to make this payment.
    NewCD = OldCD#cd{them = SSPK, me = SPK, 
		     ssme = [spk:new_ss(<<>>, [])|OldCD#cd.ssme],
		     ssthem = [spk:new_ss(<<>>, [])|OldCD#cd.ssme]},
    channel_manager:write(Sender, NewCD),
    arbitrage:write(Code, [Sender, Recipient]),
    Channel2 = make_locked_payment(Recipient, -Amount, Code),
    {ok, OldCD2} = channel_manager:read(Recipient),
    NewCD2 = OldCD2#cd{me = testnet_sign:data(Channel2),
		       ssme = [spk:new_ss(<<>>, [])|OldCD2#cd.ssme],
		       emsg = [ESS|OldCD2#cd.emsg]},
    channel_manager:write(Recipient, NewCD2),
    {reply, Return, X};
handle_call({spend, SSPK, Amount}, _From, X) ->
%giving us money in the channel.
    true = testnet_sign:verify(keys:sign(SSPK)),
    SPK = testnet_sign:data(SSPK),
    Other = other(SPK),
    {ok, OldCD} = channel_manager:read(Other),
    true = OldCD#cd.live,
    OldSPK = OldCD#cd.me,
    SPK = spk:get_paid(OldSPK, keys:pubkey(), Amount),
    Return = keys:sign(SPK),
    NewCD = OldCD#cd{them = SSPK, me = SPK},
    channel_manager:write(Other, NewCD),
    {reply, Return, X};
handle_call({update_to_me, SSPK, From}, _From, X) ->
    %this updates our partner's side of the channel state to include the bet that we already included.
    MyID = keys:pubkey(),
    SPK = testnet_sign:data(SSPK),
    Acc1 = SPK#spk.acc1,
    Acc2 = SPK#spk.acc2,
    From = case MyID of
	Acc1 -> Acc2;
	Acc2 -> Acc1;
	X -> X = Acc1
    end,	
    true = testnet_sign:verify(keys:sign(SSPK)),
    {ok, OldCD} = channel_manager:read(From),
    if
	SPK == OldCD#cd.me -> ok;
	true -> 
	    io:fwrite("update to me does not match\n"),
	    io:fwrite(packer:pack(SPK)),%still has a bet in it.
	    io:fwrite(packer:pack(OldCD#cd.me)),%still has a bet in it.
	    1=2
    end,
    SPK = OldCD#cd.me,
    NewCD = OldCD#cd{them = SSPK, ssthem = OldCD#cd.ssme},
    channel_manager:write(From, NewCD),
    {reply, 0, X};
handle_call({they_simplify, From, ThemSPK, CD}, _FROM, X) ->
    %if your partner found a way to close the channel at a higher nonced state, or a state that they think you will find preferable, then this is how you request the proof from them, and then update your data about the channel to reflect this new information.
    %send your partner a signed copy of the spk so that they can update to the current state.
    {ok, CD0} = channel_manager:read(From),
    true = CD0#cd.live,
    SPKME = CD0#cd.me,
    SSME = CD0#cd.ssme,
    true = testnet_sign:verify(keys:sign(ThemSPK)),
    true = CD#cd.live,
    NewSPK = testnet_sign:data(ThemSPK),
    NewSPK = CD#cd.me,
    SS = CD#cd.ssme,
    SS4 = CD#cd.ssthem,%is this wrong?
    B2 = spk:force_update(SPKME, SSME, SS4),
    CID = CD#cd.cid,
    NewCD = CD#cd{me = NewSPK, them = ThemSPK, ssthem = SS, ssme = SS},
    io:fwrite("channel feeder B2, {newspk, ss} comparison\n"),
    io:fwrite(packer:pack(B2)),
    io:fwrite("\n"),
    io:fwrite(packer:pack({NewSPK, SS})),
    io:fwrite("\n"),
    Return2 = 
	if
	    (B2 == {NewSPK, SS}) ->
%if they find a way to unlock funds, then give it to them.
		Return = keys:sign(NewSPK),
		channel_manager:write(From, NewCD),
		Return;
	    true ->
		B = spk:is_improvement(SPKME, SSME, NewSPK, SS),
		if
		    B ->
	    %if they give free stuff, then accept.
			Return = keys:sign(NewSPK),
			channel_manager:write(From, NewCD),
			Return;
		    true ->
			{SS5, Return} = simplify_helper(From, SS4),%this should get rid of one of the bets. %using spk:bet_unlock/2
			SPK = testnet_sign:data(ThemSPK),
			SPK2 = testnet_sign:data(Return),
			SPK = SPK2,
			Data = new_cd(SPK, ThemSPK, SS5, SS5, CID, CD#cd.expiration),
			channel_manager:write(From, Data),
			Return
		end
	end,
    {reply, Return2, X};
handle_call(_, _From, X) -> {reply, X, X}.
new_channel(Tx, SSPK, Expires) ->
    gen_server:cast(?MODULE, {new_channel, Tx, SSPK, Expires}).
spend(SPK, Amount) -> 
    gen_server:call(?MODULE, {spend, SPK, Amount}).
close(SS, Tx) ->
    gen_server:cast(?MODULE, {close, SS, Tx}).
lock_spend(SSPK, Amount, Fee, SecretHash, Sender, Recipient, ESS) ->
    %first check that this channel is in the on-chain state with sufficient depth
    gen_server:call(?MODULE, {lock_spend, SSPK, Amount, Fee, SecretHash, Sender, Recipient, ESS}).
trade(ID, Price, Type, Amount, OID, SSPK, Fee) ->
    gen_server:call(?MODULE, {trade, ID, Price, Type, Amount, OID, SSPK, Fee}).
combine_cancel_assets(TheirPub, IP, Port) ->
    gen_server:call(?MODULE, {combine_cancel_assets, TheirPub, IP, Port}).
combine_cancel_assets_server(TheirPub, SSPK2) ->
    gen_server:call(?MODULE, {combine_cancel_assets_server, TheirPub, SSPK2}).
cancel_trade(N, TheirPub, IP, Port) ->
    gen_server:call(?MODULE, {cancel_trade, N, TheirPub, IP, Port}).
cancel_trade_server(N, TheirPub, SSPK2) ->
    gen_server:call(?MODULE, {cancel_trade_server, N, TheirPub, SSPK2}).
update_to_me(SSPK, From) ->
    gen_server:call(?MODULE, {update_to_me, SSPK, From}).
agree_bet(Name, SSPK, Vars, Secret) -> 
    gen_server:call(?MODULE, {agree_bet, Name, SSPK, Vars, Secret}).
garbage() ->
    gen_server:cast(?MODULE, garbage).
garbage_helper([], _C, _OldC) -> ok;
garbage_helper([H|T], C, OldC) -> 
    throw(untested_code_error),
    {ok, CD} = channel_manager:read(H),
    if
	CD#cd.live -> ok;
	true ->
	    SPK = CD#cd.me,
	    B = depth_check2(SPK, C, OldC), 
	    case B of
		neither -> %If it has been deleted in both places
		    channel_manager:write(H, #cd{});
		_ -> ok
	    end
    end,
    garbage_helper(T, C, OldC).
c_oldc() ->
    Top = block:get_by_hash(headers:top()),
    Height = Top#block.height,
    {ok, ForkTolerance} = application:get_env(amoveo_core, fork_tolerance),
    OldHeight = Height - ForkTolerance,
    true = OldHeight > 0,
    Old = block:get_by_height(OldHeight),
    C = block:channels(Top),
    OldC = block:channels(Old),
    {C, OldC}.
depth_check2(SPK, C, OldC) -> 
    PartnerID = other(SPK),
    Channel = channels:get(PartnerID, C),
    OldChannel = channels:get(PartnerID, OldC),
    A11 = channels:acc1(Channel),
    A12 = channels:acc1(OldChannel),
    A21 = channels:acc2(Channel),
    A22 = channels:acc2(OldChannel),
    K = keys:pubkey(),
    B = ((K == A11) or (K == A21)),
    Both = (A11 == A12) %both is true if the channel has existed a long time.
	and (A21 == A22)
	and B,
    One = (B or (K == A12) or (K == A22)) %One is true if the channel is young, or old and gone.
	and ((A11 == A12) or (A21 == A22)),
    if
	Both -> both;
	One -> one;
	true -> neither
    end.
other(X) when element(1, X) == signed ->
    other(testnet_sign:data(X));
other(SPK) when element(1, SPK) == spk ->
    other(SPK#spk.acc1, SPK#spk.acc2);
other(Tx) when element(1, Tx) == ctc ->
    other(channel_team_close_tx:acc1(Tx),
	  channel_team_close_tx:acc2(Tx));
other(Tx) when element(1, Tx) == nc ->
    other(new_channel_tx:acc1(Tx),
	  new_channel_tx:acc2(Tx)).
other(Aid1, Aid2) ->
    K = keys:pubkey(),
    Out = if
	Aid1 == K -> Aid2;
	Aid2 == K -> Aid1;
	true -> Aid1 = K
    end,
    Out.
new_channel_check(Tx) ->
    %make sure we aren't already storing a channel with the same partner.
    Other = other(Tx),
    case channel_manager:read(Other) of
	{ok, CD} ->
	    true = CD#cd.me == [];%this is the case if it was deleted before
	error -> true %we have never used this CID partner combo before.
    end.
they_simplify(From, SSPK, CD) ->
    gen_server:call(?MODULE, {they_simplify, From, SSPK, CD}).
simplify_helper(From, SS) ->
    {ok, CD} = channel_manager:read(From),
    SPK = CD#cd.me,
    %this is giving the new SS to bet_unlock. channel_feeder:bets_unlock feeds the old SS and old SPK to it.
    {SSRemaining, NewSPK, _, _} = spk:bet_unlock(SPK, SS),
    Return = keys:sign(NewSPK),
    {SSRemaining, Return}. 
make_locked_payment(To, Amount, Code) -> 
    {ok, CD} = channel_manager:read(To),
    SPK = CD#cd.me,
    Bet = spk:new_bet(Code, Code, Amount),%code is the key for looking it up.
    NewSPK = spk:apply_bet(Bet, 0, SPK, 1000, 1000),
    keys:sign(NewSPK).
trade(Amount, Price, Bet, Other, _OID) ->
    {ok, CD} = channel_manager:read(Other),
    SPK = CD#cd.me,
    CID = SPK#spk.cid,
    {ok, TimeLimit} = application:get_env(amoveo_core, time_limit),
    {ok, SpaceLimit} = application:get_env(amoveo_core, space_limit),
    CGran = constants:channel_granularity(),
    A = (Amount * Price) div CGran,
    SPK2 = spk:apply_bet(Bet, -A, SPK, TimeLimit div 10 , SpaceLimit),
    keys:sign(SPK2).
cancel_trade_common(N, OldCD) ->
    SPK = OldCD#cd.me,
    SS = element(N-1, list_to_tuple(OldCD#cd.ssme)),
    true = ((SS#ss.code) == <<0,0,0,0,4>>),%this is what it looks like when a bet is unmatched
    SPK2 = spk:remove_bet(N-1, SPK),
    SPK3 = SPK2#spk{nonce = SPK2#spk.nonce + 1000000},
    keys:sign(SPK3).
matchable(Bet, SS) ->%combine-cancelable.
    io:fwrite("matchable\n"),
    SSC = SS#ss.code,
    BK = Bet#bet.key,
    {Direction, Price} = Bet#bet.meta,
    Price2 = SS#ss.meta,
    if 
        SSC == <<0,0,0,0,4>> -> 
	    io:fwrite("unmatched open order cannot be combine canceled.\n"),
	    false; %unmatched open order cannot be combine canceled.
        ((not(size(BK) == 7)) and (not(size(BK) == 9)))-> 
	    io:fwrite("not a market contract.\n"),
	    false; %not a market contract
        not(element(1, BK) == market) -> 
	    io:fwrite("not a market contract 2.\n"),
	    false; %not a market contract
        ((not(element(2, BK) == 1)) and (not(element(2, BK) == 2)))-> 
	    io:fwrite("not a standard market type.\n"),
	    false; %not a standard market contract
        Price2 == Price -> 
	    io:fwrite("bet is partially matched"),
	    false; %bet is only partially matched.
        true -> true
    end.
combine_cancel_common(OldCD) ->
    %someday, if we wanted to unlock money in a partially matched trade, we would probably also have to adjust some info in the order book. This is risky, so lets not do it yet.
    SPK = OldCD#cd.me,
    Bets = SPK#spk.bets,
    {NewBets, NewSSMe} = combine_cancel_common2(Bets, OldCD#cd.ssme, [], []),
    N = length(Bets) - length(NewBets),
    M = N * 1000000,
    %M = 0,
    SPK2 = SPK#spk{bets = NewBets, nonce = SPK#spk.nonce + M},
    %identify matched trades in the same market that go in opposite directions. remove the same amount from opposite directions to unlock liquidity.

    {keys:sign(SPK2), NewSSMe}.
combine_cancel_common2([], [], A, B) ->
    %O((number of bets)^2) in time.
    %comparing every pair of bets.
    {lists:reverse(A), lists:reverse(B)};
combine_cancel_common2([Bet|BT], [SSM|MT], OB, OM) ->
    Amount = Bet#bet.amount,
    if
        Amount == 0 -> 
            combine_cancel_common2(BT, MT, OB, OM);
        true ->
            B = matchable(Bet, SSM),
            if
                B -> combine_cancel_common3(Bet, SSM, BT, MT, OB, OM);
                true -> combine_cancel_common2(BT, MT, [Bet|OB], [SSM|OM])
            end
    end.
combine_cancel_common3(Bet, SSM, BT, MT, OB, OM) ->
    %check if bet can combine with any others, if it can, reduce the amounts of both accordinly.
    %if any amount goes to zero, remove that bet and it's SS entirely.
    {BK, SK, BF, MF} = combine_cancel_common4(Bet, SSM, BT, MT, [], []),
    combine_cancel_common2(BF, MF, BK ++ OB, SK ++ OM).
combine_cancel_common4(Bet, SSM, [], [], BO, MO) ->
    Amount = Bet#bet.amount,
    if
        Amount == 0 -> {[], [], BO, MO};
        true -> {[Bet], [SSM], BO, MO}
    end;
combine_cancel_common4(Bet, SSM, [BH|BT], [MH|MT], BO, MO) ->
    Amount = Bet#bet.amount,
    {Direction1, _} = Bet#bet.meta,
    {Direction2, _} = BH#bet.meta,
    Key1 = Bet#bet.key,
    Key2 = BH#bet.key,
    OID2 = element(7, Key2),
    OID = element(7, Key1),
    B = matchable(BH, MH),
    if
        Amount == 0 -> 
            {[], [], 
             lists:reverse([BH|BT]) ++ BO,
             lists:reverse([MH|MT]) ++ MO};
        not(B) or
        not(OID == OID2) or %must be same market to match
        (Direction1 == Direction2) or %must be opposite directions to match
	not(element(2, Key1) == element(2, Key2)) -> %must be same kind of market to match
            io:fwrite("not matchable or different oracle, or different direction \n"),
            combine_cancel_common4(Bet, SSM, BT, MT, [BH|BO], [MH|MO]);
        true -> 
            A1 = Bet#bet.amount,
            A2 = BH#bet.amount,
            if
                A1 == A2 -> 
                    {[], [],
                     lists:reverse(BT) ++ BO,
                     lists:reverse(MT) ++ MO};
                A1 > A2 ->
                    Bet2 = Bet#bet{amount = A1 - A2},
                    combine_cancel_common4(Bet2, SSM, BT, MT, BO, MO);
                A1 < A2 -> 
                    BH2 = BH#bet{amount = A2 - A1},
                    {[], [], lists:reverse(BT) ++ [BH2] ++ BO,
                     lists:reverse([MH|MT]) ++ MO}
            end
    end.
bets_unlock(X) -> 
    %io:fwrite("bets unlock\n"),
    bets_unlock2(X, []).
bets_unlock2([], Out) -> Out;
bets_unlock2([ID|T], OutT) ->
    %io:fwrite("bets unlock2\n"),
    {ok, CD0} = channel_manager:read(ID),
    true = CD0#cd.live,
    SPKME = CD0#cd.me,
    SSOld = CD0#cd.ssme,
    {NewSS, SPK, Secrets, SSThem} = spk:bet_unlock(SPKME, SSOld),
    NewCD = CD0#cd{me = SPK, ssme = NewSS, ssthem = SSThem},
    channel_manager:write(ID, NewCD),
    Out = {Secrets, SPK},
    bets_unlock2(T, [Out|OutT]).

contract_market(OBData, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID, Height) ->
    case OBData of
	{scalar, LL, UL, 10} -> 
	    BetLocation = constants:scalar_oracle_bet(),
	    scalar_market:market_smart_contract(BetLocation, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID, Height, LL, UL);
	{binary} ->
	    BetLocation = constants:oracle_bet(),
	    market:market_smart_contract(BetLocation, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID, Height)
    end.

    
