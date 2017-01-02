%this is the only thing that contact the channel_manager. That way, we are safe from race-conditions on updating the channel state.
-module(channel_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 new_channel/1,spend/1,close/3,lock_spend/1,
	 bet/2,garbage/0]).
-record(cd, {me = [], them = [], live = true}).%them is the highest-nonced SPK signed by the other node. me is the highest-nonced SPK signed by this node.
%live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) ->
    %check if any of the channels haven't existed in the last free_constants:fork_tolerance() blocks. If it hasn't, then delete it.
    Keys = channel_manager:keys(),
    Top = block:read(top:doit()),
    Height = block:height(Top),
    OldHeight = Height - free_constants:fork_tolerance(),
    true = OldHeight > 0,
    Old = block:read_int(OldHeight),
    C = block:channels(Top),
    OldC = block:channels(Old),
    garbage_helper(Keys, C, OldC),
    {noreply, X};
handle_cast({new_channel, Tx}, X) ->
    %a new channel with our ID was just created on-chain.
    0 = Tx + X,
    {noreply, X};
handle_cast({close, SPK, SS, Tx}, X) ->
%closing the channel at it's current state
%If we have a higher-nonced state signed by them, then don't close at this state.
%check that this SPK+SS combo results in the price in Tx.
    %We need to put a flag on this channel, so we don't accept payments from it anymore. We should keep this data around, so we can keep broadcasting to close the channel as long as it takes to get closed.

    0 = SPK + SS + Tx,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({spend, SPK}, _From, X) ->
%giving us money in the channel.
%check that they gave us money, and that nothing else changed.
    0 = SPK,
    {noreply, X};
handle_call({bet, Name, SSPK}, _From, X) ->
%doing one of the bets that we offer.
    SPK = testnet_sign:data(SSPK),
    CID = spk:cid(SPK),
    true = depth_check(SPK), %checks that this channel is in the on-chain state with sufficient depth.
    %free_constants:fork_tolerance()
    OldCD = channel_manager:read(CID),
    OldSPK = testnet_sign:data(OldCD#cd.them),
    Bets = free_variables:bets(),
    Bet = get_bet(Name, Bets),
    SPK = apply_bet(Bet, OldSPK),
    Return = keys:sign(SPK),
    NewCD = #cd{them = SSPK, me = Return},
    channel_manager:write(CID, NewCD),
    {reply, Return, X};
handle_call(_, _From, X) -> {reply, X, X}.
new_channel(Tx) ->
    gen_server:cast(?MODULE, {new_channel, Tx}).
spend(SPK) ->
    gen_server:call(?MODULE, {spend, SPK}).
close(SPK, SS, Tx) ->
    gen_server:cast(?MODULE, {close, SPK, SS, Tx}).
lock_spend(_SPK) ->
%giving us money conditionally, and asking us to forward it with a similar condition to someone else.
    %first check that this channel is in the on-chain state with sufficient depth
    %we need the arbitrage gen_server to exist first, before we can do this.
    ok.
bet(Name, SPK) -> 
    gen_server:call(?MODULE, {bet, Name, SPK}).
garbage() ->
    gen_server:cast(?MODULE, garbage).
garbage_helper([], _C, _OldC) -> ok;
garbage_helper([H|T], C, OldC) -> 
    CD = channel_manager:read(H),
    if
	CD#cd.live -> ok;
	true ->
	    SPK = CD#cd.me,
	    _CID = spk:cid(SPK)
    end,
    garbage_helper(T, C, OldC).
    
depth_check(_) ->
    ok.
get_bet(_,_) ->
    ok.
apply_bet(_,_) ->
    ok.

