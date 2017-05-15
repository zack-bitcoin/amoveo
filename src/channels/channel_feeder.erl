%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/3,spend/2,close/2,lock_spend/6,
	 agree_bet/4,garbage/0,entropy/1,
	 new_channel_check/1,
	 cid/1,them/1,script_sig_them/1,me/1,script_sig_me/1,
	 make_bet/4, update_to_me/1, new_cd/6, 
	 make_locked_payment/4, live/1, they_simplify/3
	 ]).
-record(cd, {me = [], %me is the highest-nonced SPK signed by this node.
	     them = [], %them is the highest-nonced SPK signed by the other node. 
	     ssthem = [], %ss is the highest nonced ScriptSig that works with them. 
	     ssme = [], %ss is the highest nonced ScriptSig that works with me
	     live = true,
	     entropy = 0,
	     cid}). %live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.
live(X) ->
    X#cd.live.
new_cd(Me, Them, SSThem, SSMe, Entropy, CID) ->
    #cd{me = Me, them = Them, ssthem = SSThem, ssme = SSMe, live = true, entropy = Entropy, cid = CID}.
me(X) -> X#cd.me.
cid(X) when is_integer(X) ->
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
    Delay = spk:delay(SPK),
    SPK2 = new_channel_tx:spk(Tx, Delay),%doesn't move the money
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
    K = keys:id(),
    true = (((A1 == A3) and (A2 == A4)) or ((A1 == A4) and (A2 == A3))),
    Direction = if
		    K == A1 -> -1;
		    K == A2 -> 1;
		    true -> K = A1
		end,
    DemandedAmount = channel_team_close_tx:amount(Tx),
    TotalCoins = 0,
    {Trees, Height, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    %State = spk:chalang_state(Height, 0, Trees),
    %Bets = spk:bets(SPK),
    %Governance = trees:governance(Trees),
    %FunLimit = governance:get_value(fun_limit, Governance),
    %VarLimit = governance:get_value(var_limit, Governance),
    {CalculatedAmount, NewNonce, [], _} = spk:run(safe, SS, SPK, Height, 0, Trees),
    %{CalculatedAmount, NewNonce, [], _, _} = chalang:run(SS, Bets, free_constants:gas_limit(), free_constants:gas_limit(), FunLimit, VarLimit, State),
    {OldAmount, OldNonce, [], _} = spk:run(safe, CD#cd.ssthem, SPK, Height, 0, Trees),
    %{OldAmount, OldNonce, [], _, _} = chalang:run(CD#cd.ssthem, Bets, free_constants:gas_limit(), free_constants:gas_limit(), FunLimit, VarLimit, State),
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
    tx_pool_feeder:absorb(keys:sign(STx, Accounts)),
    {noreply, X};
handle_cast
(_, X) -> {noreply, X}.
handle_call({lock_spend, SSPK, Amount, Fee, Code, Sender, Recipient}, _From, X) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    true = Amount > 0,
    true = Fee > free_constants:lightning_fee(),
    Return = make_locked_payment(Sender, Amount+Fee, Code, []),
    SPK = testnet_sign:data(SSPK),
    SPK2 = testnet_sign:data(Return),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(Return),
    {ok, OldCD} = channel_manager:read(Sender),
    NewCD = OldCD#cd{them = SSPK, me = SPK},
    channel_manager:write(Sender, NewCD),
    
    arbitrage:write(Code, [Sender, Recipient]),

    Channel2 = make_locked_payment(Recipient, -Amount, Code, []),
    {ok, OldCD2} = channel_manager:read(Recipient),
    NewCD2 = OldCD2#cd{me = testnet_sign:data(Channel2)},
    channel_manager:write(Recipient, NewCD2),
    {reply, Return, X};
handle_call({spend, SSPK, Amount}, _From, X) ->
%giving us money in the channel.
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    SPK = testnet_sign:data(SSPK),
    Other = other(SPK),
    {ok, OldCD} = channel_manager:read(Other),
    true = OldCD#cd.live,
    OldSPK = OldCD#cd.me,
    SPK = spk:get_paid(OldSPK, keys:id(), Amount),
    Return = keys:sign(SPK, Accounts),
    NewCD = OldCD#cd{them = SSPK, me = SPK},
    channel_manager:write(Other, NewCD),
    {reply, Return, X};
%handle_call({update_to_me, SSPK}, _From, X) ->
%    %this updates our partner's side of the channel state to include the bet that we already included.
%    {Trees,_,_} = tx_pool:data(),
%    Accounts = trees:accounts(Trees),
%    MyID = keys:id(),
%    SPK = testnet_sign:data(SSPK),
%    Acc1 = spk:acc1(SPK),
%    Acc2 = spk:acc2(SPK),
%    Other = case MyID of
%	Acc1 -> Acc2;
%	Acc2 -> Acc1;
%	X -> X = Acc1
%    end,	
%    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
%    {ok, OldCD} = channel_manager:read(Other),
%    Mine = OldCD#cd.me,
%    update_to_me_internal(keys:sign(Mine, Accounts), SSPK),
%    {reply, 0, X};
%handle_call({make_bet, Other, Name, Vars, Secret}, _From, X) ->
    %this puts the bet on the version of the channel state that we signed.
%    Z = make_bet_internal(Other, Name, Vars, Secret),
%    {reply, Z, X};
%handle_call({agree_bet, Name, SSPK, Vars, Secret}, _From, X) ->
    %This is like make_bet and update_bet_to_me combined
%    {Trees,_,_} = tx_pool:data(),
%    Accounts = trees:accounts(Trees),
%    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
%    SPK = testnet_sign:data(SSPK),
%    Other = other(SPK),
%    Return = make_bet_internal(Other, Name, Vars, Secret),
%    update_to_me_internal(Return, SSPK),
%    {reply, Return, X};
handle_call({they_simplify, From, SSPK, SS}, _FROM, X) ->
    %if your partner found a way to close the channel at a higher nonced state, or a state that they think you will find preferable, then this is how you request the proof from them, and then update your data about the channel to reflect this new information.
    %send your partner a signed copy of the spk so that they can update to the current state.
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    {Return, SS2} = simplify_helper(From, SS),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(Return),
    %L = arbitrage:check(Code),
    %Arbitrage,
    %look up in arbitrage if we can use this same SS to simplify other channels, if we can, then do it.
    {ok, CD} = channel_manager:read(From),
    CID = spk:cid(CD#cd.me),
    Data = new_cd(SPK, SSPK, SS2, SS2, entropy(CD), CID),
    channel_manager:write(CID, Data),
    {reply, {SS2, Return}, X};
handle_call({we_simplify, Them, SS}, _FROM, X) ->
    {Return, SS2} = simplify_helper(Them, SS),
    {ok, CD} = channel_manager:read(Them),
    CID = spk:cid(CD#cd.me),
    OldSSPK = CD#cd.them,
    SPK = testnet_sign:data(Return),
    Data = new_cd(SPK, OldSSPK, SS2, SS, entropy(CD), CID),
    channel_manager:write(CID, Data),
    {reply, Return, X};
%handle_call({make_simplification, Other, Name, OtherSS}, _From, X) ->
%    Z = make_simplification_internal(Other, Name, OtherSS),
%    {reply, Z, X};
%handle_call({agree_simplification, Name, SSPK, OtherSS}, _From, X) ->
%    {Trees,_,_} = tx_pool:data(),
%    Accounts = trees:accounts(Trees),
%    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
%    SPK = testnet_sign:data(SSPK),
%    Other = other(SPK),
%    {Return, _OurSecret} = make_simplification_internal(Other, Name, OtherSS),
%    update_to_me_internal(Return, SSPK),
%    {reply, Return, X};
handle_call(_, _From, X) -> {reply, X, X}.
update_to_me_internal(OurSPK, SSPK) ->
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(OurSPK),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    Other = other(SPK),
    {ok, OldCD} = channel_manager:read(Other),
    NewCD = OldCD#cd{them = SSPK, ssthem = OldCD#cd.ssme},
    channel_manager:write(Other, NewCD).
   
make_simplification_internal(Other, dice, OtherSS) ->
    %calculate who won the dice game, give them the money.
    {ok, OldCD} = channel_manager:read(Other),
    true = OldCD#cd.live,
    Them = OldCD#cd.them,
    SPK = testnet_sign:data(Them),
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    {Trees,_,_} = tx_pool:data(),
    
    {Amount, _Nonce, _SS, OurSecret} = channel_solo_close:next_ss(Other, OtherSS, SPK, Acc1, Acc2, Trees),
    NewSPK = spk:settle_bet(SPK, [], Amount),
    NewCD = OldCD#cd{me = NewSPK, ssme = []},
    channel_manager:write(Other, NewCD),
    Accounts = trees:accounts(Trees),
    {keys:sign(NewSPK, Accounts), OurSecret}.%we should also return our secret.

make_bet_internal(Other, dice, Vars, Secret) ->%this should only be called by the channel_feeder gen_server, because it updates the channel_manager.
    %SSme = dice:make_ss(SPK, Secret),
    %io:fwrite("channel feeder make_bet_internal. other is "),
    %io:fwrite(integer_to_list(Other)),
    %io:fwrite("\n"),
    {ok, OldCD} = channel_manager:read(Other),
    true = OldCD#cd.live,
    Them = OldCD#cd.them,
    OldSPK = testnet_sign:data(Them),
    Bets = free_constants:bets(),
    SPK = get_bet(dice, Bets, Vars, OldSPK),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    SSme = dice:make_ss(SPK, Secret),
    NewCD = OldCD#cd{me = SPK, ssme = [SSme]},
    channel_manager:write(Other, NewCD),
    keys:sign(SPK, Accounts).

make_bet(Other, Name, Vars, Secret) ->
    gen_server:call(?MODULE, {make_bet, Other, Name, Vars, Secret}).
new_channel(Tx, SSPK, Accounts) ->
    %io:fwrite("channel feeder inserting channel $$$$$$$$$$$$$$$$$$$$$$$$$$"),
    gen_server:cast(?MODULE, {new_channel, Tx, SSPK, Accounts}).
spend(SPK, Amount) -> 
    gen_server:call(?MODULE, {spend, SPK, Amount}).
close(SS, Tx) ->
    gen_server:cast(?MODULE, {close, SS, Tx}).
lock_spend(SSPK, Amount, Fee, SecretHash, Sender, Recipient) ->
    %first check that this channel is in the on-chain state with sufficient depth
    gen_server:call(?MODULE, {lock_spend, SSPK, Amount, Fee, SecretHash, Sender, Recipient}).
update_to_me(SSPK) ->
    gen_server:call(?MODULE, {update_to_me, SSPK}).
    
    
agree_bet(Name, SSPK, Vars, Secret) -> 
    gen_server:call(?MODULE, {agree_bet, Name, SSPK, Vars, Secret}).
garbage() ->
    gen_server:cast(?MODULE, garbage).
garbage_helper([], _C, _OldC) -> ok;
garbage_helper([H|T], C, OldC) -> 
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
    Top = block:read(top:doit()),
    Height = block:height(Top),
    OldHeight = Height - free_constants:fork_tolerance(),
    true = OldHeight > 0,
    Old = block:read_int(OldHeight),
    C = block:channels(Top),
    OldC = block:channels(Old),
    {C, OldC}.
depth_check(SPK) -> 
    {C, OldC} = c_oldc(),
    depth_check2(SPK, C, OldC).
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
    K = keys:id(),
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

get_bet(Name, [{Name, Loc}|_], Vars, SPK) ->
    %io:fwrite("get_bets"),
    get_bet2(Name, Loc, Vars, SPK);
get_bet(Name, [_|T], Vars, SPK) -> get_bet(Name, T, Vars, SPK).
get_bet2(dice, Loc, [Amount, Commit1, Commit2], SPK) ->
    %check that Amount is in a reasonable range based on the channel state.
    %we need my balance from channels:get, and from the Amount from the most recent spk they signed.
    CID = spk:cid(SPK),
    {Trees,_,_} = tx_pool:data(),
    Channels = trees:channels(Trees),
    {_, OldChannel, _} = channels:get(CID, Channels),
    %0 = channels:rent(OldChannel),%otherwise they could attack us by making a bet where the amount they could lose is slightly smaller.
    NewHeight = block:height(block:read(top:doit())),
    Channel = channels:update(0, CID, Trees, none, 0, 0,0,0, channels:delay(OldChannel), NewHeight),
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    A = spk:amount(SPK),
    true = (Bal1-A) >= Amount, 
    true = (Bal2+A) >= Amount,  %This checks that neither of us can have negative amounts of money.
    Front = "macro Amount int " ++ integer_to_list(Amount) ++ " ; \n
             macro Commit1 binary " ++ integer_to_list(size(Commit1))++" "++ binary_to_list(base64:encode(Commit1)) ++ " ; \n
             macro Commit2 binary " ++ integer_to_list(size(Commit2))++ " " ++ binary_to_list(base64:encode(Commit2)) ++ " ; \n
",
    %io:fwrite("channel feeder front is \n"),
    %io:fwrite(Front),
    Bet = compile:doit(Loc, Front),
    [] = spk:bets(SPK),%for now we only make 1 bet per customer at a time, otherwise it would be possible for a customer to make us check their complicated script over and over on each bet, to see if it can close any of them.
    spk:apply_bet(Bet, SPK, 1000, 1000).
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
    K = keys:id(),
    Out = if
	Aid1 == K -> Aid2;
	Aid2 == K -> Aid1;
	true -> Aid1 == K
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

%make_simplification(Other, Name, OtherSS) ->
%    gen_server:call(?MODULE, 
%		    {make_simplification, 
%		     Other, 
%		     Name, 
%		     OtherSS}).
%agree_simplification(Name, SSPK, OtherSS) ->
%    gen_server:call(?MODULE, 
%		    {agree_simplification,
%		     Name, 
%		     SSPK,
%		     OtherSS}).
    
we_simplify(From, SS) ->
    gen_server:call(?MODULE, {we_simplify, From, SS}).
they_simplify(From, SSPK, SS) ->
    gen_server:call(?MODULE, {they_simplify, From, SSPK, SS}).
simplify_helper(From, SS) ->
    {ok, CD} = channel_manager:read(From),
    OldSS = CD#cd.ssme,
    SPK = CD#cd.me,
    {Trees, Height, _} = tx_pool:data(),
    {_, N1, _, _} = spk:run(fast, OldSS, SPK, Height, 0, Trees),
    {Amount, N2, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
    true = N2 > N1,
    %look up our old SS, and our oldSPK. apply both SSs to the oldSPK See if the new one has a higher nonce, if it does, then continue
    %else, if the nonce is the same, and it improves our position, by unlocking funds for example, then continue
    %else, refuse the upgrade.
    N = diff(SS, OldSS, 0),
    SS2 = remove_i(N, SS),
    Bets = spk:bets(SPK),
    NewSPK = spk:settle_bet(SPK, remove_i(N, Bets), 0),
    {CheckAmount, _, _, _} = spk:run(fast, SS2, NewSPK, Height, 0, Trees),
    NewSPK2 = spk:settle_bet(SPK, remove_i(N, Bets), Amount - CheckAmount),
    Return = keys:sign(NewSPK2),
    {SS2, Return}.
remove_i(0, [_|T]) ->
    T;
remove_i(N, [A|T]) ->
    [A|remove_i(N-1, T)].
diff([A|T], [A|Q], N) ->
    diff(T, Q, N+1);
diff(_, _, N) ->
    N.

make_locked_payment(To, Amount, Code, Prove) -> 
	 %look up our current SPK,
    {ok, CD} = channel_manager:read(To),
    OldSPK = CD#cd.them,
    Bet = spk:new_bet(Code, Amount, Prove),
    Time = free_constants:gas_limit(),
    Space = free_constants:space_limit(),
    SPK = testnet_sign:data(OldSPK),
    NewSPK = spk:apply_bet(Bet, 0, SPK, Time, Space),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    Out = keys:sign(NewSPK, Accounts),
    Out.
