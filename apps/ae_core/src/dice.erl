-module(dice).
-export([make_ss/2, resolve_ss/3, doit/1]).

%from handler
doit({dice, 1, Other, Commit, Amount}) ->
    %Eventually we need to charge them a big enough fee to cover the cost of watching for them to close the channel without us. 
    {ok, CD} = channel_manager:read(Other),
    [] = channel_feeder:script_sig_me(CD),
    [] = channel_feeder:script_sig_them(CD),
    {MyCommit, Secret} = secrets:new(),
    SSPK = channel_feeder:make_bet(Other, dice, [Amount, Commit, MyCommit], Secret),
    {ok, SSPK, MyCommit};
doit({dice, 2, ID, SSPK, SS}) ->
    channel_feeder:update_to_me(SSPK),
    io:fwrite("handler dice 2 "),
    disassembler:doit(SS),
    {SSPKsimple, MySecret} = channel_feeder:make_simplification(ID, dice, [SS]),
    {ok, SSPKsimple, MySecret};
doit({dice, 3, _ID, SSPK}) ->
    channel_feeder:update_to_me(SSPK),
    {ok, 0};


%from internal_handler
doit({dice, Amount, IP, Port}) ->
    %{ok, Other} = talker:talk({id}, IP, Port),
    {Commit, Secret} = secrets:new(),
    MyID = keys:id(),
    {ok, SSPK, OtherCommit} = talker:talk({dice, 1, MyID, Commit, Amount}, IP, Port),
    SSPK2 = channel_feeder:agree_bet(dice, SSPK, [Amount, Commit, OtherCommit], Secret),%should store partner's info into channel manager.
    %ok;%comment below this line for testing channel_slash txs.
    SPK = testnet_sign:data(SSPK2),
    SS1 = dice:make_ss(SPK, Secret),
    {ok, SSPKsimple, TheirSecret} = talker:talk({dice, 2, MyID, SSPK2, SS1}, IP, Port), %SSPKsimple doesn't include the bet. the result of the bet instead is recorded.
    %ok;%comment below this line for testing channel_slash txs.
    SS = dice:resolve_ss(SPK, Secret, TheirSecret),%
    SSPK2simple = channel_feeder:agree_simplification(dice, SSPKsimple, [SS]),
    SPKsimple = testnet_sign:data(SSPKsimple),
    SPKsimple = testnet_sign:data(SSPK2simple),
    talker:talk({dice, 3, MyID, SSPK2simple}, IP, Port).
make_ss(SPK, Secret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    MyID = keys:id(),
    N = case MyID of
	    Acc1 -> 1;
	    Acc2 -> 2;
	    X -> X = Acc1
	end,
    S = size(Secret),
    compiler_chalang:doit("binary " ++ integer_to_list(S) ++ " " ++ binary_to_list(base64:encode(Secret)) ++ " int " ++ integer_to_list(N) ++ " ").

resolve_ss(SPK, Secret, TheirSecret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    MyID = keys:id(),
    {S1, S2} = case MyID of
	    Acc1 -> {Secret, TheirSecret};
	    Acc2 -> {TheirSecret, Secret};
	    X -> X = Acc1
	end,
    T1 = integer_to_list(size(S1)),
    T2 = integer_to_list(size(S2)),
    S1s = binary_to_list(base64:encode(S1)),
    S2s = binary_to_list(base64:encode(S2)),
    S = " binary " ++ T1 ++ " " ++ S1s ++ " binary " ++ T2 ++ " "++ S2s ++ " int 3 ",
    compiler_chalang:doit(S).
