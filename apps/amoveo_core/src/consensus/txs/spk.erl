-module(spk).
-export([new/8, apply_bet/5, get_paid/3, run/6, dict_run/6,
         chalang_state/3, new_bet/3, new_bet/4, 
	 is_improvement/4, bet_unlock/2, force_update/3,
         new_ss/2, remove_bet/2, remove_nth/2, 
         verify_sig/3, sign/2, hash/1,
	 test/0, test2/0
	]).

%We want channel that are using the same contract to be able to calculate a contract hash that is the same. This makes it easier to tell if 2 channels are betting on the same thing.
%Each contract should output an amount between 0 and constants:channel_granularity(), which is the portion of the money that goes to one of the participants. Which participant it signifies depends on what value is stored in a flag.
%each contract needs a value saying how much of the money is locked into that contract.

-include("../../records.hrl").
%SPK is where we hold the channel contracts. They are turing complete smart contracts.
%Besides the SPK, there is the ScriptSig. Both participants of the channel sign the SPK, neither signs the SS.

verify_sig(S, Pub1, Pub2) ->
    X = element(4, S),
    Z = case X of
            {2, Sig2} ->
                io:fwrite("spk more \n"),
                {2, Sig1} = element(3, S),
                %Serialized = sign:serialize(element(2, S)),
                %H = hash:doit(Serialized),
                H = hash(element(2, S)),
                B1 = testnet_sign:verify_sig(H, Sig1, Pub1),
                B2 = testnet_sign:verify_sig(H, Sig2, Pub2),
                B1 and B2;
                %S2 = setelement(2, S, H),
                %S3 = setelement(3, S2, Sig1),
                %setelement(4, S3, Sig2);
            _ -> testnet_sign:verify(S)
        end.
    %testnet_sign:verify(Z).
sign(S, N) ->
    if
        (element(1, S) == signed) ->
 %If it already has a old type signature, then do that. Otherwise, make a new type signature.
            Bool = (is_binary(element(3, S)) or is_binary(element(4, S))),
            if
                Bool-> keys:sign(S);
                true -> sign2(S, N)
            end;
        true -> sign2({signed, S, [], []}, N)
    end.
hash(X) -> hash:doit(sign:serialize(X)).
sign2(S, N) -> 
    {signed, Data, B3, B4} = S,
    NewData = hash(Data),
    io:fwrite("spk sign2\n"),
    %io:fwrite(packer:pack(Sa1)),
    io:fwrite("\n"),
    Sig = {2, keys:raw_sign(NewData)},
    case N of
        1 -> {signed, Data, Sig, B4};
        2 -> {signed, Data, B3, Sig}
    end.
            
remove_bet(N, SPK) ->
    NewBets = remove_nth(N, SPK#spk.bets),
    B = element(N, list_to_tuple(SPK#spk.bets)),
    A = case B#bet.meta of
            0 -> 0;
            {_Direction, Price} -> 
                CGran = constants:channel_granularity(),
                Amount = B#bet.amount,
                (Amount * Price) div CGran
        end,
    SPK#spk{bets = NewBets, amount = SPK#spk.amount + A}.
remove_nth(N, _) when N < 1 -> 1=2;
remove_nth(1, [A|B]) -> B;
remove_nth(N, [A|B]) -> [A|remove_nth(N-1, B)].

prove_facts([], _) ->%we need to return an empty list here.
    compiler_chalang:doit(<<" nil ">>);
prove_facts(X, Trees) ->
    A = <<"macro [ nil ;
	macro , swap cons ;
	macro ] swap cons reverse ;
        [">>,
    B = prove_facts2(X, Trees),
    compiler_chalang:doit(<<A/binary, B/binary>>).
prove_facts2([], _) ->
    <<"]">>;
prove_facts2([{Tree, Key}|T], Trees) when is_integer(Key)->
    ID = tree2id(Tree),
    Branch = trees:Tree(Trees),
    {_, Data, _} = Tree:get(Key, Branch),
    SerializedData = Tree:serialize(Data),
    Size = size(SerializedData),
    A = "[int " ++ integer_to_list(ID) ++ 
	", int " ++ integer_to_list(Key) ++%burn and existence store by hash, not by integer.
	", binary " ++
	integer_to_list(Size) ++ " " ++
	binary_to_list(base64:encode(Tree:serialize(Data)))++ 
	"]",
    A2 = list_to_binary(A),
    B = prove_facts2(T, Trees),
    C = case T of
	    [] -> <<>>;
	    _ -> <<", ">>
		     end,
    <<A2/binary, C/binary, B/binary>>;
prove_facts2([{Tree, Key}|T], Trees) ->
    ID = tree2id(Tree),
    Branch = trees:Tree(Trees),
    {_, Data, _} = Tree:get(Key, Branch),
    SerializedData = Tree:serialize(Data),
    Size = size(SerializedData),
    A = "[int " ++ integer_to_list(ID) ++ 
	", binary " ++
	integer_to_list(size(Key)) ++ " " ++
	binary_to_list(base64:encode(Key)) ++
	", binary " ++
	integer_to_list(Size) ++ " " ++
	binary_to_list(base64:encode(Tree:serialize(Data)))++ 
	"]",%this comma is used one too many times.
    A2 = list_to_binary(A),
    B = prove_facts2(T, Trees),
    C = case T of
	    [] -> <<>>;
	    _ -> <<", ">>
		     end,
    <<A2/binary, C/binary, B/binary>>.

tree2id(accounts) -> 1;
tree2id(channels) -> 2;
tree2id(existence) -> 3;
tree2id(oracles) -> 5;
tree2id(governance) -> 6.

new_ss(Code, Prove) ->
    #ss{code = Code, prove = Prove}.
new_bet(Code, Key, Amount) ->
    new_bet(Code, Key, Amount, 0).
new_bet(Code, Key, Amount, Meta) ->
    #bet{code = Code, key = Key, amount = Amount, meta = Meta}.
new(Acc1, Acc2, CID, Bets, SG, TG, Nonce, Delay) ->
    <<_:256>> = CID,
    #spk{acc1 = Acc1, acc2 = Acc2,
	 bets = Bets, space_gas = SG, time_gas = TG,
	 cid = CID, nonce = Nonce, delay = Delay}.
bet_unlock(SPK, SS) ->
    %io:fwrite("spk bet unlock\n"),
    Bets = SPK#spk.bets,
    %check if we have the secret to unlock each bet.
    %unlock the ones we can, and return an SPK with the remaining bets and the new amount of money that is moved.
    {Remaining, AmountChange, SSRemaining, Secrets, Dnonce, SSThem} = bet_unlock2(Bets, [], 0, SS, [], [], 0, []),
    {lists:reverse(SSRemaining),
     SPK#spk{bets = lists:reverse(Remaining),
	     amount = SPK#spk.amount + (AmountChange),
	     nonce = SPK#spk.nonce + Dnonce},
     Secrets, SSThem}.
bet_unlock2([], B, A, [], SS, Secrets, Nonce, SSThem) ->
    {B, A, SS, Secrets, Nonce, lists:reverse(SSThem)};
bet_unlock2([Bet|T], B, A, [SS|SSIn], SSOut, Secrets, Nonce, SSThem) ->
    Key = Bet#bet.key, 
    case secrets:read(Key) of
	<<"none">> -> 
            %io:fwrite("no secret known\n"),
	    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
	{SS2, Amount} -> 
	    %Just because a bet is removed doesn't mean all the money was transfered. We should calculate how much of the money was transfered.
            %io:fwrite("we have a secret\n"),
	    %io:fwrite(packer:pack(SS2)),% the browswer is making it look like this: {[{<<"code">>,[2,0,0,0,32,98,87,250,109,44,40,33,174,78,71,84,176,34,104,226,87,251,254,27,121,249,5,18,185,76,16,255,4,134,1,189,94]},{<<"prove">>,[]},{<<"meta">>,[]}]}
%in erlang it makes: {ss, <<binary>>, [], 0}
            TP = tx_pool:get(),
            Trees = TP#tx_pool.block_trees,
            Height = TP#tx_pool.height,
	    State = chalang_state(Height, 0, Trees),
	    {ok, FunLimit} = application:get_env(amoveo_core, fun_limit),
	    {ok, VarLimit} = application:get_env(amoveo_core, var_limit),
	    {ok, BetGasLimit} = application:get_env(amoveo_core, bet_gas_limit),
	    true = chalang:none_of(SS2#ss.code),
	    F = prove_facts(SS#ss.prove, Trees),
	    C = Bet#bet.code,
	    Code = <<F/binary, C/binary>>,
	    Data = chalang:data_maker(BetGasLimit, BetGasLimit, VarLimit, FunLimit, SS2#ss.code, Code, State, constants:hash_size()),
	    Data2 = chalang:run5(SS2#ss.code, Data),
	    Data3 = chalang:run5(Code, Data2),
	    case Data3 of
		{error, _E} -> 
		    io:fwrite("spk bet unlock, ss doesn't work\n"),
		    io:fwrite(packer:pack(SS2)),
		    io:fwrite("\n"),
                    %io:fwrite("spk bet_unlock2 chalang run third\n"),
		    Data4 = chalang:run5(SS#ss.code, Data),
                    %io:fwrite("spk bet_unlock2 chalang run fourth\n"),
		    Y = chalang:run5(Code, Data4),
		    case Y of
			{error, E2} ->
			    io:fwrite("bet unlock2 ERROR"),
			    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
			Z -> 
			    bet_unlock3(Z, T, B, A, Bet, SSIn, SSOut, SS, Secrets, Nonce, SSThem)
		    end;
		X -> 
                    if%this if clause seems unnecessary.
                        is_integer(Amount) ->
                            true = (abs(Amount) == abs(Bet#bet.amount));
                        true -> ok
                    end,
                    bet_unlock3(X, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem)
	    end
    end.
bet_unlock3(Data5, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem) ->
    %io:fwrite("spk bet_unlock3\n"),
    [<<ContractAmount0:32>>, <<Nonce2:32>>, <<Delay:32>>|_] = chalang:stack(Data5),
   if
        Delay > 0 ->
	   io:fwrite("delay is "),
	   io:fwrite(integer_to_list(Delay)),
	   io:fwrite(". Delay >0, keep the bet.\n"),
	   io:fwrite("nonce .\n"),
	   io:fwrite(integer_to_list(Nonce2)),
	   io:fwrite("amount .\n"),
	   io:fwrite(integer_to_list(ContractAmount0)),
	   io:fwrite("\n"),
	   bet_unlock2(T, [Bet|B], A, SSIn, [SS2|SSOut], Secrets, Nonce, [SS2|SSThem]);
       true -> 
	   CGran = constants:channel_granularity(),
	   ContractAmount = if
				ContractAmount0 > CGran ->
				    ContractAmount0 - round(math:pow(2, 32));
				true -> ContractAmount0
			    end,
	   %io:fwrite("delay <1, remove it.\n"),
	   true = ContractAmount =< CGran,
	   true = ContractAmount >= -CGran,
	   A3 = ContractAmount * Bet#bet.amount div CGran,
	   Key = Bet#bet.key, 
	   bet_unlock2(T, B, A+A3, SSIn, SSOut, [{secret, SS2, Key}|Secrets], Nonce + Nonce2, [SS2|SSThem])
   end.
	    
apply_bet(Bet, Amount, SPK, Time, Space) ->
%bet is binary, the SPK portion of the script.
%SPK is the old SPK, we output the new one.
    SPK#spk{bets = [Bet|SPK#spk.bets], 
	    nonce = SPK#spk.nonce + 1, 
	    time_gas = SPK#spk.time_gas + Time, 
	    space_gas = max(SPK#spk.space_gas, Space), 
	    amount = SPK#spk.amount + Amount}.
get_paid(SPK, ID, Amount) -> %if Amount is positive, that means money is going to Aid2.
    Aid1 = SPK#spk.acc1,
    Aid2 = SPK#spk.acc2,
    D = case ID of
	Aid1 -> -1;
	Aid2 -> 1;
	_ -> ID = Aid1
    end,
    SPK#spk{amount = (SPK#spk.amount + (D*Amount)), 
	    nonce = SPK#spk.nonce + 1}.
	    
dict_run(Mode, SS, SPK, Height, Slash, Dict) ->
    State = chalang_state(Height, Slash, 0),
    {Amount, NewNonce, Delay, _} = dict_run2(Mode, SS, SPK, State, Dict),
    {Amount + SPK#spk.amount, NewNonce + SPK#spk.nonce, Delay}.
%dict_run2(fast, SS, SPK, State, Dict) ->
dict_run2(_, SS, SPK, State, Dict) ->
    FunLimit = governance:dict_get_value(fun_limit, Dict),
    VarLimit = governance:dict_get_value(var_limit, Dict),
    true = is_list(SS),
    Bets = SPK#spk.bets,
    Delay = SPK#spk.delay,
    run(SS, 
	Bets,
	SPK#spk.time_gas,
	SPK#spk.space_gas,
	FunLimit,
	VarLimit,
	State, 
	Delay);
dict_run2(safe, SS, SPK, State, Dict) -> %unused.
    %will not crash. if the thread that runs the code crashes, or takes too long, then it returns {-1,-1,-1,-1}
    S = self(),
    spawn(fun() ->
		  X = dict_run2(fast, SS, SPK, State, Dict),
		  S ! X
	  end),
    spawn(fun() ->
                  {ok, A} = application:get_env(amoveo_core, smart_contract_runtime_limit),
		  timer:sleep(A),%wait enough time for the chalang contracts to finish
		  S ! error
	  end),
    receive 
	Z -> Z
    end.
    
run(Mode, SS, SPK, Height, Slash, Trees) ->
    State = chalang_state(Height, Slash, Trees),
    {Amount, NewNonce, Delay, _} = run2(Mode, SS, SPK, State, Trees),
    {Amount + SPK#spk.amount, NewNonce + SPK#spk.nonce, Delay}.
run2(fast, SS, SPK, State, Trees) -> 
    Governance = trees:governance(Trees),
    FunLimit = governance:get_value(fun_limit, Governance),
    VarLimit = governance:get_value(var_limit, Governance),
    true = is_list(SS),
    Bets = SPK#spk.bets,
    Delay = SPK#spk.delay,
    run(SS, 
	Bets,
	SPK#spk.time_gas,
	SPK#spk.space_gas,
	FunLimit,
	VarLimit,
	State, 
	Delay);
run2(safe, SS, SPK, State, Trees) -> 
    %will not crash. if the thread that runs the code crashes, or takes too long, then it returns {-1,-1,-1,-1}
    S = self(),
    spawn(fun() ->
		  X = run2(fast, SS, SPK, State, Trees),
		  S ! X
	  end),
    spawn(fun() ->
		  timer:sleep(5000),%wait enough time for the chalang contracts to finish
		  S ! error
	  end),
    receive 
	Z -> Z
    end.
chalang_state(Height, Slash, _) ->	    
    chalang:new_state(Height, Slash, 0).
run(ScriptSig, Codes, OpGas, RamGas, Funs, Vars, State, SPKDelay) ->
    run(ScriptSig, Codes, OpGas, RamGas, Funs, Vars, State, 0, 0, SPKDelay).

run([], [], OpGas, _, _, _, _, Amount, Nonce, Delay) ->
    {Amount, Nonce, Delay, OpGas};
run([SS|SST], [Code|CodesT], OpGas, RamGas, Funs, Vars, State, Amount, Nonce, Delay) ->
    {A2, N2, Delay2, EOpGas} = 
	run3(SS, Code, OpGas, RamGas, Funs, Vars, State),
    run(SST, CodesT, EOpGas, RamGas, Funs, Vars, State, A2+Amount, N2+Nonce, max(Delay, Delay2)).
run3(SS, Bet, OpGas, RamGas, Funs, Vars, State) ->
    ScriptSig = SS#ss.code,
    true = chalang:none_of(ScriptSig),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    %F = prove_facts(Bet#bet.prove, Trees),
    F = prove_facts(SS#ss.prove, Trees),
    C = Bet#bet.code,
    Code = <<F/binary, C/binary>>,  
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, ScriptSig, Code, State, constants:hash_size()),
    {Amount0, Nonce, Delay, Data2} = chalang_error_handling(ScriptSig, Code, Data),
    CGran = constants:channel_granularity(),
    
    %io:fwrite(packer:pack({stack, Amount, Nonce, Delay})),
    %io:fwrite("\n"),
    A3 = Amount0 * Bet#bet.amount div CGran,
    {A3, Nonce, Delay,
     chalang:time_gas(Data2)
    }.
force_update(SPK, SSOld, SSNew) ->
    F = tx_pool:get(),
    Trees = F#tx_pool.block_trees,
    Dict = F#tx_pool.dict,
    Height = F#tx_pool.height,
    L = length(SSOld),
    L2 = length(SSNew),
    if 
	not(L == L2) -> false;
	true ->
	    {_, NonceOld,  _} =  run(fast, SSOld, SPK, Height, 0, Trees),
						%{_, NonceOld,  _} =  dict_run(fast, SSOld, SPK, Height, 0, Dict),
						%we can't use dict here, because not all the information we need is stored in the dict.
	    {_, NonceNew,  _} =  run(fast, SSNew, SPK, Height, 0, Trees),
						%{_, NonceNew,  _} =  dict_run(fast, SSNew, SPK, Height, 0, Dict),
	    if
		NonceNew >= NonceOld ->
		    {NewBets, FinalSS, Amount, Nonce} = force_update2(SPK#spk.bets, SSNew, [], [], 0, 0),
						%NewSPK = SPK#spk{bets = NewBets, amount = (SPK#spk.amount + (Amount div 2)), nonce = (SPK#spk.nonce + Nonce)},
		    NewSPK = SPK#spk{bets = NewBets, amount = (SPK#spk.amount + (Amount)), nonce = (SPK#spk.nonce + Nonce)},
		    {NewSPK, FinalSS};
		true -> false
	    end
    end.
force_update2([], [], NewBets, NewSS, A, Nonce) ->
    {NewBets, NewSS, A, Nonce};
force_update2([Bet|BetsIn], [SS|SSIn], BetsOut, SSOut, Amount, Nonce) ->
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    Height = TP#tx_pool.height,
    State = chalang_state(Height, 0, Trees),
    {ok, FunLimit} = application:get_env(amoveo_core, fun_limit),
    {ok, VarLimit} = application:get_env(amoveo_core, var_limit),
    {ok, BetGasLimit} = application:get_env(amoveo_core, bet_gas_limit),
    true = chalang:none_of(SS#ss.code),
    F = prove_facts(SS#ss.prove, Trees),
    C = Bet#bet.code,
    Code = <<F/binary, C/binary>>,
    Data = chalang:data_maker(BetGasLimit, BetGasLimit, VarLimit, FunLimit, SS#ss.code, Code, State, constants:hash_size()),
    {ContractAmount, N, Delay, _} = chalang_error_handling(SS#ss.code, Code, Data),
    if
	%Delay > 50 ->
	Delay > 0 ->
	    force_update2(BetsIn, SSIn, [Bet|BetsOut], [SS|SSOut], Amount, Nonce);
	true ->
	    CGran = constants:channel_granularity(),
	    true = ContractAmount =< CGran,
	    A = ContractAmount * Bet#bet.amount div CGran,
	    io:fwrite("force update 2 amounts \n"),
	    io:fwrite(packer:pack([ContractAmount, Bet#bet.amount, A])),
	    io:fwrite("\n"),
	    force_update2(BetsIn, SSIn, BetsOut, SSOut, Amount + A, Nonce + N)
    end.
    
is_improvement(OldSPK, OldSS, NewSPK, NewSS) ->
    TP = tx_pool:get(),
    BlockTrees = TP#tx_pool.block_trees,
    Height = TP#tx_pool.height,
    {ok, SpaceLimit} = application:get_env(amoveo_core, space_limit),
    {ok, TimeLimit} = application:get_env(amoveo_core, time_limit),
    SG = NewSPK#spk.space_gas,
    TG = NewSPK#spk.time_gas,
    true = SG =< SpaceLimit,
    true = TG =< TimeLimit,
    {_, Nonce2, Delay2} =  run(fast, NewSS, NewSPK, Height, 0, BlockTrees),
    {_, Nonce1, _} =  run(fast, OldSS, OldSPK, Height, 0, BlockTrees),
    io:fwrite("spk is improvement "),
    io:fwrite(packer:pack([Nonce1, Nonce2, NewSS, OldSS])),
    io:fwrite("\n"),
    if
	Nonce2 > Nonce1 ->
	    Bets2 = NewSPK#spk.bets,
	    Bets1 = OldSPK#spk.bets,
    %{ok, MaxChannelDelay} = application:get_env(amoveo_core, max_channel_delay),
    %io:fwrite("delay2 is "),
    %io:fwrite(packer:pack(Delay2)),
    %io:fwrite("\n"),
    %true = Delay2 =< MaxChannelDelay,
	    Amount2 = NewSPK#spk.amount,
	    Amount1 = OldSPK#spk.amount,
	    NewSPK = OldSPK#spk{bets = Bets2,
				space_gas = SG,
				time_gas = TG,
				amount = Amount2,
				nonce = NewSPK#spk.nonce},
	    CID = NewSPK#spk.cid,
	    <<_:256>> = CID,
	    Channel = trees:get(channels, CID),
	    KID = keys:pubkey(),
	    Acc1 = channels:acc1(Channel),
	    Acc2 = channels:acc2(Channel),
	    Profit = 
		if
		    KID == Acc1 ->
			Amount2 - Amount1;
		    KID == Acc2 ->
			Amount1 - Amount2
		end,
	    LT = length(Bets2) - length(Bets1),
	    if
		(Bets1 == Bets2) and 
		Profit > 0 -> 
	%if they give us money for no reason, then accept
		    true; 
	%BL2 == BL1 ->
	%if they give us all the money from a bet, then accept
	    %find the missing bet.
	    %{Good, Amount} = find_extra(NewSPK#spk.bets, OldSPK#spk.bets),
	    %Good verifies that bets were only removed, not added.
	    %amount is the total volume of money controlled by those bets that were removed.
	    %(Profit == Amount) and Good;
		(Profit >= 0) and %costs nothing
		(LT > 0)->
	    %if we have the same or greater amount of money, and they make a bet that possibly gives us more money, then accept it.
		    [NewBet|T] = Bets2,
		    BetAmount = NewBet#bet.amount,
		    PotentialGain = 
			case KID of
			    Acc1 -> -BetAmount;
			    Acc2 -> BetAmount
			end,
	    %We should look up the fee to leave a channel open, and check that the extra money beyond obligations is enough to keep the channel open long enough to close it without their help.
	    %The problem is that we can't know how long the delay is in all the contracts.
		    Obligations1 = obligations(1, Bets2),
		    Obligations2 = obligations(2, Bets2),
		    ChannelBal1 = channels:bal1(Channel),
		    ChannelBal2 = channels:bal2(Channel),
		    if
			(T == Bets1) and %only add one more bet
			(PotentialGain > 0) and %potentially gives us money
			(Obligations2 =< ChannelBal2) and
			(Obligations1 =< ChannelBal1)
			-> true; 
			true -> false
		    end;
		true -> false
	    end;
	true -> false
    end.
obligations(_, []) -> 0;
obligations(1, [A|T]) ->
    B = A#bet.amount,
    C = if
	    B>0 -> B;
	    true -> 0
	end,
    C + obligations(1, T);
obligations(2, [A|T]) ->
    B = A#bet.amount,
    C = if
	    B<0 -> -B;
	    true -> 0
	end,
    C + obligations(2, T).
vm(SS, State) ->
    {ok, TimeLimit} = application:get_env(amoveo_core, time_limit),
    {ok, SpaceLimit} = application:get_env(amoveo_core, space_limit),
    {ok, FunLimit} = application:get_env(amoveo_core, fun_limit),
    {ok, VarLimit} = application:get_env(amoveo_core, var_limit),
    chalang:vm(SS, TimeLimit, SpaceLimit, FunLimit, VarLimit, State).
-define(error_amount, 0).
-define(error_delay, 10000000).
-define(error_nonce, 0).
chalang_error_handling(SS, Code, Data) ->
    Default = {?error_amount, ?error_nonce, ?error_delay, Data},
    case chalang:run5(SS, Data) of
        {error, S} ->
            io:fwrite("script sig has an error when executed: "),
            io:fwrite(S),
            io:fwrite("\n"),
	    Default;
        Data2 ->
            case chalang:run5(Code, Data2) of
                {error, S2} ->
                    io:fwrite("code has an error when executed with that script sig: "),
                    io:fwrite(S2),
                    io:fwrite("\n"),
		    Default;
                Data3 ->
		    Stack = chalang:stack(Data3),
		    Max = round(math:pow(2, 32)),
		    case Stack of
                    [<<Amount:32>>|
                     [<<Nonce:32>>|
                      [<<Delay:32>>|_]]] ->
			    CGran = constants:channel_granularity(),
    %negative numbers are stored in the highest 10000 values that can be stored in the 4-byte value.
			    if
				((Amount > CGran) and (Amount < (Max - CGran))) -> Default;
				(Amount < 0) -> Default;
				(Amount > Max) -> Default;
				(Amount > CGran) ->
				    {Amount - Max, Nonce, Delay, Data3};
				true ->
				    {Amount, Nonce, Delay, Data3}
			    end;
			_ -> Default
		    end
            end
    end.
test2() ->
    {ok, CD} = channel_manager:read(hd(channel_manager:keys())),
    SSME = CD#cd.ssme,
    SPK = CD#cd.me,
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    Height = TP#tx_pool.height,
    run(fast, SSME, SPK, Height, 0, Trees).
test() ->
    %test prove_facts.
    BlockTrees = (tx_pool:get())#tx_pool.block_trees,
    Pub = constants:master_pub(),
    GovID = 2,
    Code = prove_facts([{governance, GovID},{accounts, Pub}], BlockTrees),
    State = chalang_state(1, 0, BlockTrees),
    [[[<<6:32>>, <<GovID:32>>, Gov5], %6th tree is governance. 5th thing is "delete channel reward"
      [<<1:32>>, BPub, Acc1]]] = %1st tree is accounts. 1 is for account id 1.
	chalang:vm(Code, 100000, 100000, 1000, 1000, State),
    Govern5 = trees:get(governance, GovID),
    Account1 = trees:get(accounts, constants:master_pub()),
    %io:fwrite(packer:pack([governance:deserialize(Gov5), Govern5])),
    %[-6,["gov",2,1100,0],889981]
    Acc1 = accounts:serialize(Account1),
    %Govern5 = governance:element(3, governance:deserialize(Gov5)),
    %Gov5 = governance:serialize(Govern5),
    success.
    
    
