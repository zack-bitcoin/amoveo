%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/3,spend/2,close/2,lock_spend/1,
	 agree_bet/4,garbage/0,entropy/1,
	 new_channel_check/1,
	 cid/1,them/1,script_sig_them/1,me/1,script_sig_me/1,
	 make_bet/4, update_to_me/1,
	 make_simplification/3,agree_simplification/3]).
-record(cd, {me = [], %me is the highest-nonced SPK signed by this node.
	     them = [], %them is the highest-nonced SPK signed by the other node. 
	     ssthem = [], %ss is the highest nonced ScriptSig that works with them. 
	     ssme = [], %ss is the highest nonced ScriptSig that works with me
	     live = true,
	     entropy = 0,
	     cid}). %live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.
me(X) -> X#cd.me.
cid(X) when is_integer(X) ->
    cid(channel_manager:read(X));
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
handle_cast({new_channel, Tx, SSPK, Accounts}, X) ->
    %a new channel with our ID was just created on-chain. We should record an empty SPK in this database so we can accept channel payments.
    SPK = testnet_sign:data(SSPK),
    SPK = new_channel_tx:spk(Tx, spk:delay(SPK)),%doesn't move the money
    %CID = spk:cid(SPK),
    CD = #cd{me = SPK, them = SSPK, entropy = spk:entropy(SPK)},
    %io:fwrite("adding new channel to manager at "),
    %io:fwrite(integer_to_list(spk:cid(SPK))),
    %io:fwrite(" with acc "),
    %io:fwrite(integer_to_list(other(Tx))),
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
    {Accounts, Channels, Height, _} = tx_pool:data(),
    State = chalang:new_state(TotalCoins, Height, 0, <<0:(8*hash:hash_depth())>>, Accounts, Channels),
    Bets = spk:bets(SPK),
    {CalculatedAmount, NewNonce, _, _} = chalang:run(SS, Bets, free_constants:gas_limit(), free_constants:gas_limit(), constants:fun_limit(), constants:var_limit(), State),

    {OldAmount, OldNonce, _, _} = chalang:run(CD#cd.ssthem, Bets, free_constants:gas_limit(), free_constants:gas_limit(), constants:fun_limit(), constants:var_limit(), State),
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
handle_call({spend, SSPK, Amount}, _From, X) ->
%giving us money in the channel.
    {Accounts, _,_,_} = tx_pool:data(),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    SPK = testnet_sign:data(SSPK),
    Other = other(SPK),
    {ok, OldCD} = channel_manager:read(Other),
    true = OldCD#cd.live,
    OldSPK = OldCD#cd.me,
    SPK = spk:get_paid(testnet_sign:data(OldSPK), keys:id(), Amount),
    Return = keys:sign(SPK, Accounts),
    NewCD = OldCD#cd{them = SSPK, me = Return},
    channel_manager:write(Other, NewCD),
    {reply, Return, X};
handle_call({update_to_me, SSPK}, _From, X) ->
    %this updates our partner's side of the channel state to include the bet that we already included.
    {Accounts, _,_,_} = tx_pool:data(),
    MyID = keys:id(),
    SPK = testnet_sign:data(SSPK),
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    Other = case MyID of
	Acc1 -> Acc2;
	Acc2 -> Acc1;
	X -> X = Acc1
    end,	
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    {ok, OldCD} = channel_manager:read(Other),
    Mine = OldCD#cd.me,
    update_to_me_internal(keys:sign(Mine, Accounts), SSPK),
    {reply, 0, X};
handle_call({make_bet, Other, Name, Vars, Secret}, _From, X) ->
    %this puts the bet on the version of the channel state that we signed.
    Z = make_bet_internal(Other, Name, Vars, Secret),
    {reply, Z, X};
handle_call({agree_bet, Name, SSPK, Vars, Secret}, _From, X) ->
    %This is like make_bet and update_bet_to_me combined
    {Accounts, _,_,_} = tx_pool:data(),
    testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    SPK = testnet_sign:data(SSPK),
    Other = other(SPK),
    Return = make_bet_internal(Other, Name, Vars, Secret),
    update_to_me_internal(Return, SSPK),
    {reply, Return, X};
handle_call({make_simplification, Other, Name, OtherSS}, _From, X) ->
    Z = make_simplification_internal(Other, Name, OtherSS),
    {reply, Z, X};
handle_call({agree_simplification, Name, SSPK, OtherSS}, _From, X) ->
    {Accounts, _,_,_} = tx_pool:data(),
    true = testnet_sign:verify(keys:sign(SSPK, Accounts), Accounts),
    SPK = testnet_sign:data(SSPK),
    Other = other(SPK),
    {Return, _OurSecret} = make_simplification_internal(Other, Name, OtherSS),
    update_to_me_internal(Return, SSPK),
    {reply, Return, X};
handle_call(_, _From, X) -> {reply, X, X}.
update_to_me_internal(OurSPK, SSPK) ->
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(OurSPK),
    {Accounts, _,_,_} = tx_pool:data(),
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
    {Accounts, Channels,_,_} = tx_pool:data(),
    {Amount, _Nonce, _SS, OurSecret} = channel_solo_close:next_ss(Other, OtherSS, SPK, Acc1, Acc2, Accounts, Channels),
    NewSPK = spk:settle_bet(SPK, [], Amount),
    NewCD = OldCD#cd{me = NewSPK, ssme = []},
    channel_manager:write(Other, NewCD),
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
    {Accounts, _,_,_} = tx_pool:data(),
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
lock_spend(_SPK) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    %first check that this channel is in the on-chain state with sufficient depth
    %we need the arbitrage gen_server to exist first, before we can do this.
    ok.
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
    Channel = channel:get(PartnerID, C),
    OldChannel = channel:get(PartnerID, OldC),
    E1 = spk:entropy(SPK),
    E2 = channel:entropy(Channel),
    E3 = channel:entropy(OldChannel),
    A11 = channel:acc1(Channel),
    A12 = channel:acc1(OldChannel),
    A21 = channel:acc2(Channel),
    A22 = channel:acc2(OldChannel),
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
    %we need my balance from channel:get, and from the Amount from the most recent spk they signed.
    CID = spk:cid(SPK),
    {_Accounts,Channels,_,_} = tx_pool:data(),
    {_, OldChannel, _} = channel:get(CID, Channels),
    0 = channel:rent(OldChannel),%otherwise they could attack us by making a bet where the amount they could lose is slightly smaller.
    NewHeight = block:height(block:read(top:doit())),
    Channel = channel:update(CID, Channels, none, 0, 0,0,0, channel:delay(OldChannel), NewHeight),
    Bal1 = channel:bal1(Channel),
    Bal2 = channel:bal2(Channel),
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

make_simplification(Other, Name, OtherSS) ->
    gen_server:call(?MODULE, 
		    {make_simplification, 
		     Other, 
		     Name, 
		     OtherSS}).
agree_simplification(Name, SSPK, OtherSS) ->
    gen_server:call(?MODULE, 
		    {agree_simplification,
		     Name, 
		     SSPK,
		     OtherSS}).
    

