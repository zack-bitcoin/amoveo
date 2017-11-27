%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/3,spend/2,close/2,lock_spend/7,
	 agree_bet/4,garbage/0,entropy/1,
	 new_channel_check/1,
	 cid/1,them/1,script_sig_them/1,me/1,
	 script_sig_me/1,
	 update_to_me/2, new_cd/6, 
	 make_locked_payment/3, live/1, they_simplify/3,
	 bets_unlock/1, emsg/1, trade/5, trade/7,
         cancel_trade/4, cancel_trade_server/3,
         combine_cancel_assets/3,
         combine_cancel_assets_server/2
	 ]).
-record(cd, {me = [], %me is the highest-nonced SPK signed by this node.
	     them = [], %them is the highest-nonced SPK signed by the other node. 
	     ssme = [], %ss is the highest nonced ScriptSig that works with me
	     ssthem = [], %ss is the highest nonced ScriptSig that works with them. 
	     emsg = [],
	     live = true,
	     entropy = 0,
	     cid}). %live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.
emsg(X) ->
    X#cd.emsg.
live(X) ->
    X#cd.live.
new_cd(Me, Them, SSMe, SSThem, Entropy, CID) ->
    #cd{me = Me, them = Them, ssthem = SSThem, ssme = SSMe, live = true, entropy = Entropy, cid = CID}.
me(X) -> X#cd.me.
cid({ok, CD}) -> cid(CD);
cid(X) when is_binary(X) ->
    %{ok, CD} = channel_manager:read(X),
    %cid(CD);
    %{ok, CD} = 
    cid(channel_manager:read(X));
    %CD#cd.cid;
cid(X) when is_record(X, cd) -> X#cd.cid;
cid(error) -> undefined;
cid(X) -> cid(other(X)).
them(X) -> X#cd.them.
script_sig_them(X) -> X#cd.ssthem.
script_sig_me(X) -> X#cd.ssme.
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
handle_cast({new_channel, Tx, SSPK, _Accounts}, X) ->
    %a new channel with our ID was just created on-chain. We should record an empty SPK in this database so we can accept channel payments.
    SPK = testnet_sign:data(SSPK),
    %Delay = spk:delay(SPK),
    %SPK2 = new_channel_tx:spk(Tx, Delay),%doesn't move the money
    CD = #cd{me = SPK, them = SSPK, entropy = spk:entropy(SPK), cid = new_channel_tx:id(Tx)},
    channel_manager:write(other(Tx), CD),
    {noreply, X};
handle_cast({close, SS, STx}, X) ->
    %closing the channel at it's current SPK
    Tx = testnet_sign:data(STx),
    OtherID = other(Tx),
    {ok, CD} = channel_manager:read(OtherID),
    true = CD#cd.live,
    SPKM = CD#cd.me,
    A1 = spk:acc1(SPKM), 
    A2 = spk:acc2(SPKM),
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
    {Trees, Height, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {CalculatedAmount, NewNonce, _} = spk:run(safe, SS, SPK, Height, 0, Trees),
    {OldAmount, OldNonce, _} = spk:run(safe, CD#cd.ssthem, SPK, Height, 0, Trees),
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
    io:fwrite("channel feeder spks "),
    io:fwrite(packer:pack({testnet_sign:data(SSPK), NewSS})),
    io:fwrite("\n"),
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
    io:fwrite("channel feeder spks "),
    io:fwrite(packer:pack({testnet_sign:data(SSPK),
                           testnet_sign:data(SSPK2)})),
    io:fwrite("\n"),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    Bets = spk:bets(me(OldCD)),
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = NewSS, ssthem = NewSS},
    channel_manager:write(TheirPub, NewCD),
    {reply, SSPK, X};
handle_call({cancel_trade_server, N, TheirPub, SSPK2}, _From, X) ->
    {ok, OldCD} = channel_manager:read(TheirPub),
    SSPK = cancel_trade_common(N, OldCD), 
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    Bets = spk:bets(me(OldCD)),
    Bet = element(N-1, list_to_tuple(Bets)),
    {Type, Price} = spk:bet_meta(Bet),
    CodeKey = spk:key(Bet),
    {market, 1, _, _, _, _, OID} = CodeKey,
    NewCD = OldCD#cd{them = SSPK2, me = SPK,
                     ssme = spk:remove_nth(N-1, OldCD#cd.ssme),
                     ssthem = spk:remove_nth(N-1, OldCD#cd.ssthem)},
    Type2 = case Type of
                1 -> buy;
                2 -> sell
            end,
    ok = order_book:remove(TheirPub, Type2, Price, OID),
    channel_manager:write(TheirPub, NewCD),
    {reply, SSPK, X};
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
handle_call({trade, ID, Price, Type, Amount, OID, SSPK, Fee}, _From, X) ->
    {Trees,Height,_} = tx_pool:data(),
    true = testnet_sign:verify(keys:sign(SSPK)),
    true = Amount > 0,
    {ok, LF} = application:get_env(ae_core, lightning_fee),
    true = Fee > LF,
    {ok, OB} = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    BetLocation = constants:oracle_bet(),
    io:fwrite("market_smart contract amount is "),
    io:fwrite(integer_to_list(Amount)),
    io:fwrite("\n"),
    SC = market:market_smart_contract(BetLocation, OID, Type, Expires, Price, keys:pubkey(), Period, Amount, OID),
    CodeKey = market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID),
    SSPK2 = trade(Amount, Price, SC, ID, OID),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    {ok, OldCD} = channel_manager:read(ID),
    DefaultSS = market:unmatched(OID),
    SSME = [DefaultSS|OldCD#cd.ssme],
    SSThem = [DefaultSS|OldCD#cd.ssthem],
    spk:run(fast, SSME, SPK, Height, 0, Trees),%sanity test
    spk:run(fast, SSThem, SPK, Height, 0, Trees),%sanity test
    NewCD = OldCD#cd{them = SSPK, me = SPK, 
		     ssme = SSME, ssthem = SSThem},
    %arbitrage:write(CodeKey, [ID]),
    channel_manager:write(ID, NewCD),
    {reply, SSPK2, X};
handle_call({lock_spend, SSPK, Amount, Fee, Code, Sender, Recipient, ESS}, _From, X) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK)),
    true = Amount > 0,
    {ok, LightningFee} = application:get_env(ae_core, lightning_fee),
    true = Fee > LightningFee,
    Return = make_locked_payment(Sender, Amount+Fee, Code),
    SPK = testnet_sign:data(SSPK),
    SPK22 = testnet_sign:data(Return),
    
    SPK = SPK22,
    {ok, OldCD} = channel_manager:read(Sender),
    NewCD = OldCD#cd{them = SSPK, me = SPK, 
		     ssme = [spk:new_ss(<<>>, [])|OldCD#cd.ssme],
		     ssthem = [spk:new_ss(<<>>, [])|OldCD#cd.ssme]},
		     %ssthem = [spk:new_ss(<<>>, [])|OldCD#cd.ssthem]},
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
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    From = case MyID of
	Acc1 -> Acc2;
	Acc2 -> Acc1;
	X -> X = Acc1
    end,	
    true = testnet_sign:verify(keys:sign(SSPK)),
    {ok, OldCD} = channel_manager:read(From),
    SPK2 = OldCD#cd.me,
    SPK = SPK2,
    NewCD = OldCD#cd{them = SSPK, ssthem = OldCD#cd.ssme},
    channel_manager:write(From, NewCD),
    {reply, 0, X};
handle_call({they_simplify, From, ThemSPK, CD}, _FROM, X) ->
    %if your partner found a way to close the channel at a higher nonced state, or a state that they think you will find preferable, then this is how you request the proof from them, and then update your data about the channel to reflect this new information.
    %send your partner a signed copy of the spk so that they can update to the current state.
    io:fwrite("the simplify 01 \n"),
    {ok, CD0} = channel_manager:read(From),
    true = live(CD0),
    SPKME = me(CD0),
    SSME = script_sig_me(CD0),
    true = testnet_sign:verify(keys:sign(ThemSPK)),
    true = live(CD),
    NewSPK = testnet_sign:data(ThemSPK),
    NewSPK = me(CD),
    io:fwrite("the simplify 02 \n"),
    SS = script_sig_me(CD),
    SS4 = script_sig_them(CD),
    Entropy = entropy(CD),
    io:fwrite("they simplify about to force update "),
    io:fwrite(packer:pack({SSME, SS4, SPKME})),
    io:fwrite("\n"),
    B2 = spk:force_update(SPKME, SSME, SS4),
    io:fwrite(packer:pack({channel_feeder, B2, {NewSPK, SS}})),
    io:fwrite("\n"),
    CID = CD#cd.cid,
    io:fwrite("the simplify 03 \n"),
    Return2 = 
	if
	    (B2 == {NewSPK, SS}) ->
%if they find a way to unlock funds, then give it to them.
		Return = keys:sign(NewSPK),
		NewCD = new_cd(NewSPK, ThemSPK, SS, SS, Entropy, CID),
		channel_manager:write(From, NewCD),
		Return;
	    true ->
		B = spk:is_improvement(SPKME, SSME,
				       NewSPK, SS),
		if
		    B ->
	    %if they give free stuff, then accept.
			Return = keys:sign(NewSPK),
			NewCD = new_cd(NewSPK, ThemSPK, SS, SS, Entropy, CID),
			channel_manager:write(From, NewCD),
			Return;
		    true ->
			{SS5, Return} = simplify_helper(From, SS4),%this should get rid of one of the bets.
			SPK = testnet_sign:data(ThemSPK),
			SPK2 = testnet_sign:data(Return),
                        io:fwrite(packer:pack({compare_spks, SPK})), %this has 1 bet in it
                        io:fwrite("\n"),
                        io:fwrite(packer:pack({compare_spks2, SPK2})), %this has 2 bets in it
                        io:fwrite("\n"),
			SPK = SPK2,
			Data = new_cd(SPK, ThemSPK, SS5, SS5, Entropy, CID),
			channel_manager:write(From, Data),
			Return
		end
	end,
    {reply, Return2, X};
handle_call(_, _From, X) -> {reply, X, X}.

new_channel(Tx, SSPK, Accounts) ->
    gen_server:cast(?MODULE, {new_channel, Tx, SSPK, Accounts}).
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
		neither -> 
	 %If it has been deleted in both places
		    %CID = spk:cid(SPK),
		    channel_manager:write(H, #cd{entropy = CD#cd.entropy}); 
		    %channel_manager:delete(CID);
		_ -> ok
	    end
    end,
    garbage_helper(T, C, OldC).
   
c_oldc() ->
    Top = block:get_by_hash(headers:top()),
    Height = block:height(Top),
    {ok, ForkTolerance} = application:get_env(ae_core, fork_tolerance),
    OldHeight = Height - ForkTolerance,
    true = OldHeight > 0,
    Old = block:get_by_height(OldHeight),
    C = block:channels(Top),
    OldC = block:channels(Old),
    {C, OldC}.
depth_check2(SPK, C, OldC) -> 
    %CID = spk:cid(SPK),
    PartnerID = other(SPK),
    Channel = channels:get(PartnerID, C),
    OldChannel = channels:get(PartnerID, OldC),
    E1 = spk:entropy(SPK),
    E2 = channels:entropy(Channel),
    E3 = channels:entropy(OldChannel),
    A11 = channels:acc1(Channel),
    A12 = channels:acc1(OldChannel),
    A21 = channels:acc2(Channel),
    A22 = channels:acc2(OldChannel),
    K = keys:pubkey(),
    B = ((K == A11) or (K == A21)),
    Both = (E1 == E2)  %both is true if the channel has existed a long time.
	and (E1 == E3) 
	and (A11 == A12) 
	and (A21 == A22)
	and B,
    One = (B or (K == A12) or (K == A22)) %One is true if the channel is young, or old and gone.
	and ((E1 == E2) or (E1 == E3))
	and ((A11 == A12) or (A21 == A22)),
    if
	Both -> both;
	One -> one;
	true -> neither
    end.

other(X) when element(1, X) == signed ->
    other(testnet_sign:data(X));
other(SPK) when element(1, SPK) == spk ->
    other(spk:acc1(SPK), spk:acc2(SPK));
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
    
	    
entropy([Aid1, Aid2]) ->
    Other = other(Aid1, Aid2),
    entropy(Other);
entropy(CD) when is_record(CD, cd)->
    CD#cd.entropy;
entropy(Other) ->
    case channel_manager:read(Other) of
	{ok, CD} ->  
	    CD#cd.entropy;
	error -> 1
    end.
new_channel_check(Tx) ->
    %make sure we aren't already storing a channel with the same CID/partner combo.
    Other = other(Tx),
    %CID = new_channel_tx:id(Tx),
    case channel_manager:read(Other) of
	{ok, CD} ->
	    true = CD#cd.me == [],%this is the case if it was deleted before
	    OldEntropy = CD#cd.entropy,
	    NewEntropy = new_channel_tx:entropy(Tx),
	    NewEntropy>OldEntropy;
	error -> true %we have never used this CID partner combo before.
    end.

they_simplify(From, SSPK, CD) ->
    gen_server:call(?MODULE, {they_simplify, From, SSPK, CD}).
    
simplify_helper(From, SS) ->
    {ok, CD} = channel_manager:read(From),
    SPK = CD#cd.me,
    %this is giving the new SS to bet_unlock. channel_feeder:bets_unlock feeds the old SS and old SPK to it.
    %spk:run(fast, SS, OldSPK
    io:fwrite("simplify_helper "),
    io:fwrite(packer:pack({sh, SPK, SS})),
    io:fwrite("\n"),
    {SSRemaining, NewSPK, _, _} = spk:bet_unlock(SPK, SS),
    io:fwrite(packer:pack({sh2, NewSPK})),
    io:fwrite("\n"),
    Return = keys:sign(NewSPK),
    {SSRemaining, Return}. 

make_locked_payment(To, Amount, Code) -> 
	 %look up our current SPK,
    {ok, CD} = channel_manager:read(To),
    SPK = CD#cd.me,
    %OldSPK = CD#cd.them,
    %SPK = testnet_sign:data(OldSPK),
    Bet = spk:new_bet(Code, Code, Amount),
    NewSPK = spk:apply_bet(Bet, 0, SPK, 1000, 1000),
    {Trees, _, _} = tx_pool:data(),
    keys:sign(NewSPK).
trade(Amount, Price, Bet, Other, OID) ->
    {ok, CD} = channel_manager:read(Other),
    Prove = [{oracles, OID}],
    %Bet = spk:new_bet(Code, Amount, Prove),
    io:fwrite("trade bet is "),
    io:fwrite(packer:pack(Bet)),
    io:fwrite("\n"),
    SPK = channel_feeder:me(CD),
    CID = spk:cid(SPK),
    {ok, TimeLimit} = application:get_env(ae_core, time_limit),
    {ok, SpaceLimit} = application:get_env(ae_core, space_limit),
    CGran = constants:channel_granularity(),
    A = (Amount * Price) div CGran,
    SPK2 = spk:apply_bet(Bet, -A, SPK, TimeLimit div 10 , SpaceLimit),
    keys:sign(SPK2).
cancel_trade_common(N, OldCD) ->
    SPK = channel_feeder:me(OldCD),
    SS = element(N-1, list_to_tuple(OldCD#cd.ssme)),
    true = spk:ss_code(SS) == <<0,0,0,0,4>>,%this is what it looks like when a bet is unmatched
    SPK2 = spk:remove_bet(N-1, SPK),
    keys:sign(SPK2).
matchable(Bet, SS) ->
    SSC = spk:ss_code(SS),
    BK = spk:key(Bet),
    {Direction, Price} = spk:bet_meta(Bet),
    Price2 = spk:ss_meta(SS),
    if 
        SSC == <<0,0,0,0,4>> -> 
            io:fwrite("not cancelable because it is an open order.\n"),
            false; %this means it is unmatched.
        not(size(BK) == 7) -> false; %this means it is not a market contract
        not(element(1, BK) == market) -> false; %this means it is not a market contract
        not(element(2, BK) == 1) -> false; %this means it is not a standard market contract
        Price2 == Price -> 
            io:fwrite("not cancelable because it is a partially open order."),
            false; %this means that the bet is only partially matched.
        true ->  io:fwrite("is matchable \n"),
            true
    end.
combine_cancel_common(OldCD) ->
    %someday, if we wanted to unlock money in a partially matched trade, we would probably also have to adjust some info in the order book. This is risky, so lets not do it yet.
    SPK = channel_feeder:me(OldCD),
    Bets = spk:bets(SPK),
    {NewBets, NewSSMe} = combine_cancel_common2(Bets, OldCD#cd.ssme, [], []),
    SPK2 = spk:update_bets(SPK, NewBets),
    %identify matched trades in the same market that go in opposite directions. remove the same amount from opposite directions to unlock liquidity.

    {keys:sign(SPK2), NewSSMe}.
combine_cancel_common2([], [], A, B) ->
    %O((number of bets)^2) in time.
    %comparing every pair of bets.
    io:fwrite("combine cancel common finish "),
    io:fwrite(packer:pack([length(A), length(B)])),
    io:fwrite("\n"),
    {lists:reverse(A), lists:reverse(B)};
combine_cancel_common2([Bet|BT], [SSM|MT], OB, OM) ->
    io:fwrite("combine cancel common 2\n"),
    Amount = spk:bet_amount(Bet),
    if
        Amount == 0 -> 
            io:fwrite("amount is 0\n"),
            combine_cancel_common2(BT, MT, OB, OM);
        true ->
            io:fwrite("amount is >0\n"),
            B = matchable(Bet, SSM),
            if
                B -> combine_cancel_common3(Bet, SSM, BT, MT, OB, OM);
                true -> combine_cancel_common2(BT, MT, [Bet|OB], [SSM|OM])
            end
    end.
combine_cancel_common3(Bet, SSM, BT, MT, OB, OM) ->
    io:fwrite("combine cancel common 3\n"),
    %check if bet can combine with any others, if it can, reduce the amounts of both accordinly.
    %if any amount goes to zero, remove that bet and it's SS entirely.
    {BK, SK, BF, MF} = combine_cancel_common4(Bet, SSM, BT, MT, [], []),
    combine_cancel_common2(BF, MF, BK ++ OB, SK ++ OM).
combine_cancel_common4(Bet, SSM, [], [], BO, MO) ->
    Amount = spk:bet_amount(Bet),
    if
        Amount == 0 -> 
            io:fwrite("combine cancel common4 amount 0 1\n");
            {[], [], BO, MO};
        true -> {[Bet], [SSM], BO, MO}
    end;
combine_cancel_common4(Bet, SSM, [BH|BT], [MH|MT], BO, MO) ->
    Amount = spk:bet_amount(Bet),
    {Direction1, _} = spk:bet_meta(Bet),
    {Direction2, _} = spk:bet_meta(BH),
    Key1 = spk:key(Bet),
    Key2 = spk:key(BH),
    OID2 = element(7, Key2),
    OID = element(7, Key1),
    B = matchable(BH, MH),
    if
        Amount == 0 -> 
            io:fwrite("combine cancel common4 amount 0 2\n"),
            {[], [], 
             lists:reverse([BH|BT]) ++ BO,
             lists:reverse([MH|MT]) ++ MO};
        not(B) or
        not(OID == OID2) or %must be same market to match
        (Direction1 == Direction2) -> %must be opposite directions to match
            io:fwrite("not matchable or different oracle, or different direction \n"),
            combine_cancel_common4(Bet, SSM, BT, MT, [BH|BO], [MH|MO]);
        true -> 
            A1 = spk:bet_amount(Bet),
            A2 = spk:bet_amount(BH),
            if
                A1 == A2 -> 
                    io:fwrite("match both away\n"),
                    {[], [],
                     lists:reverse(BT) ++ BO,
                     lists:reverse(MT) ++ MO};
                A1 > A2 ->
                    io:fwrite("match 1 away \n"),
                    Bet2 = spk:update_bet_amount(Bet, A1 - A2),
                    combine_cancel_common4(Bet2, SSM, BT, MT, BO, MO);
                A1 < A2 -> 
                    io:fwrite("match other away \n"),
                    BH2 = spk:update_bet_amount(BH, A2 - A1),
                    {[], [], lists:reverse(BT) ++ [BH2] ++ BO,
                     lists:reverse([MH|MT]) ++ MO}
            end
    end.
    
bets_unlock(X) -> 
    bets_unlock2(X, []).
bets_unlock2([], Out) -> Out;
bets_unlock2([ID|T], OutT) ->
    {ok, CD0} = channel_manager:read(ID),
    true = live(CD0),
    SPKME = me(CD0),
    SSOld = script_sig_me(CD0),
    {NewSS, SPK, Secrets, SSThem} = spk:bet_unlock(SPKME, SSOld),
    
    NewCD = CD0#cd{me = SPK, ssme = NewSS, ssthem = SSThem},
    channel_manager:write(ID, NewCD),
    Out = {Secrets, SPK},
    bets_unlock2(T, [Out|OutT]).
