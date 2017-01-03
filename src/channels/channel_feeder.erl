%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/1,spend/2,close/3,lock_spend/1,
	 bet/3,garbage/0]).
-record(cd, {me = [], %me is the highest-nonced SPK signed by this node.
	     them = [], %them is the highest-nonced SPK signed by the other node. 
	     sst = [], 
%sst is the highest nonced ScriptSig that works with them.
	     live = true}). %live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.
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
handle_cast({new_channel, Tx}, X) ->
    %a new channel with our ID was just created on-chain. We should record an empty SPK in this database so we can accept channel payments.
    SPK = new_channel_tx:spk(Tx),%doesn't move the money
    CID = spk:cid(SPK),
    CD = #cd{me = keys:sign(SPK)},
    channel_manager:write(CID, CD),
    {noreply, X};
handle_cast({close, CID, SS, STx}, X) ->
    %closing the channel at it's current SPK
    CD = channel_manager:read(CID),
    true = CD#cd.live,
    SPKM = CD#cd.me,
    A1 = spk:acc1(SPKM), 
    A2 = spk:acc2(SPKM),
    SPK = CD#cd.them,
    Tx = testnet_sign:data(STx),
    A3 = channel_team_close:aid1(Tx),
    A4 = channel_team_close:aid2(Tx),
    K = keys:id(),
    true = (((A1 == A3) and (A2 == A4)) or ((A1 == A4) and (A2 == A3))),
    Direction = if
		    K == A1 -> -1;
		    K == A2 -> 1;
		    true -> K = A1
		end,
    DemandedAmount = channel_team_close:amount(Tx),
    TotalCoins = 0,
    {Accounts, Channels, Height, _} = tx_pool:data(),
    State = chalang:new_state(TotalCoins, Height, 0, <<0:(8*hash:hash_depth())>>, Accounts, Channels),
    Bets = spk:bets(SPK),
    {CalculatedAmount, NewNonce, _, _} = chalang:run(SS, Bets, free_constants:gas_limit(), free_constants:gas_limit(), constants:fun_limit(), constants:var_limit(), State),

    {OldAmount, OldNonce, _, _} = chalang:run(CD#cd.sst, Bets, free_constants:gas_limit(), free_constants:gas_limit(), constants:fun_limit(), constants:var_limit(), State),
    if
	NewNonce > OldNonce -> ok; 
	NewNonce == OldNonce ->
	    true = Direction*(CalculatedAmount - OldAmount) >= 0; %otherwise we would prefer to publish our data instead of signing theirs, and we would profit.
	true -> NewNonce = OldNonce %crash
    end,
    true = Direction*(DemandedAmount - CalculatedAmount) >= 0,%They shouldn't take any more money than we calculated they can take
    OldCD = channel_manager:read(CID),
    NewCD = OldCD#cd{live = false},
    channel_manager:write(CID, NewCD),
    tx_pool_feeder:absorb(keys:sign(STx)),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({spend, SSPK, Amount}, _From, X) ->
%giving us money in the channel.
    SPK = testnet_sign:data(SSPK),
    both = depth_check(SPK), 
    CID = spk:cid(SPK), 
    OldCD = channel_manager:read(CID),
    true = OldCD#cd.live,
    OldSPK = OldCD#cd.me,
    SPK = get_paid(OldSPK, keys:id(), Amount),
    Return = keys:sign(SPK),
    NewCD = OldCD#cd{them = SSPK, me = Return},
    channel_manager:write(CID, NewCD),
    {reply, Return, X};
handle_call({bet, Name, SSPK, Vars}, _From, X) ->
%doing one of the bets that we offer.
    SPK = testnet_sign:data(SSPK),
    CID = spk:cid(SPK),
    both = depth_check(SPK), 
    OldCD = channel_manager:read(CID),
    true = OldCD#cd.live,
    OldSPK = testnet_sign:data(OldCD#cd.them),
    Bets = free_variables:bets(),
    Bet = get_bet(Name, Bets),
    SPK = apply_bet(Bet, OldSPK, Vars),
    Return = keys:sign(SPK),
    NewCD = OldCD#cd{them = SSPK, me = Return},
    channel_manager:write(CID, NewCD),
    {reply, Return, X};
handle_call(_, _From, X) -> {reply, X, X}.
new_channel(Tx) ->
    gen_server:cast(?MODULE, {new_channel, Tx}).
spend(SPK, Amount) ->
    gen_server:call(?MODULE, {spend, SPK, Amount}).
close(CID, SS, Tx) ->
    gen_server:cast(?MODULE, {close, CID, SS, Tx}).
lock_spend(_SPK) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    %first check that this channel is in the on-chain state with sufficient depth
    %we need the arbitrage gen_server to exist first, before we can do this.
    ok.
bet(Name, SPK, Vars) -> 
    gen_server:call(?MODULE, {bet, Name, SPK, Vars}).
garbage() ->
    gen_server:cast(?MODULE, garbage).
garbage_helper([], _C, _OldC) -> ok;
garbage_helper([H|T], C, OldC) -> 
    CD = channel_manager:read(H),
    if
	CD#cd.live -> ok;
	true ->
	    SPK = CD#cd.me,
	    B = depth_check2(SPK, C, OldC), 
	    case B of
		neither -> 
	 %If it has been deleted in both places
		    CID = spk:cid(SPK),
		    channel_manager:delete(CID);
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
    CID = spk:cid(SPK),
    Channel = channel:get(CID, C),
    OldChannel = channel:get(CID, OldC),
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
       

get_bet(Name, [{Name, Val}|_]) ->
    Val;
get_bet(Name, [_|T]) -> get_bet(Name, T).

apply_bet(_Bet, _SPK, _Vars) -> 
%vars is a tuple of variables that get inserted into the free spots in the bet.
    ok.
get_paid(_SPK, _MyID, _Amount) ->
    ok.

  
