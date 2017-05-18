-module(spk).
-export([acc1/1,acc2/1,entropy/1,
	 bets/1,space_gas/1,time_gas/1,
	 new/9,cid/1,amount/1, 
	 nonce/1,apply_bet/5,get_paid/3,
	 run/6,settle_bet/4,chalang_state/3,
	 prove/1, new_bet/3, delay/1,
	 is_improvement/4, bet_unlock/2,
	 code/1,
	 test/0
	]).
-record(bet, {code, amount, prove}).
%We want channel that are using the same contract to be able to calculate a contract hash that is the same. This makes it easier to tell if 2 channels are betting on the same thing.
%Each contract should output an amount between 0 and constants:channel_granularity(), which is the portion of the money that goes to one of the participants. Which participant it signifies depends on what value is stored in a flag.
%So each contract needs a value saying how much of the money is locked into that contract.
-record(spk, {acc1, acc2, entropy, 
	      bets, space_gas, time_gas, 
	      cid, amount = 0, nonce = 0,
	      delay = 0
	     }).
%scriptpubkey is the name that Satoshi gave to this part of the transactions in bitcoin.
%This is where we hold the channel contracts. They are turing complete smart contracts.
%Besides the SPK, there is the ScriptSig. Both participants of the channel sign the SPK, only one signs the SS.
delay(X) -> X#spk.delay.
acc1(X) -> X#spk.acc1.
acc2(X) -> X#spk.acc2.
bets(X) -> X#spk.bets.
entropy(X) -> X#spk.entropy.
space_gas(X) -> X#spk.space_gas.
time_gas(X) -> X#spk.time_gas.
cid(X) -> X#spk.cid.
amount(X) -> X#spk.amount.
nonce(X) -> X#spk.nonce.

bet_amount(X) -> X#bet.amount.
prove(X) -> X#bet.prove.
code(X) -> X#bet.code.

prove_facts([], _) ->
    <<>>;
prove_facts(X, Trees) ->
	   %[int 5,int 6,int 7] 
    A = <<"macro [ nil ;
	macro , swap cons ;
	macro ] swap cons reverse ;
        [">>,
    B = prove_facts2(X, Trees),
    compiler_chalang:doit(<<A/binary, B/binary>>).
prove_facts2([], _) ->
    <<"]">>;
prove_facts2([{Tree, Key}|T], Trees) ->
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
	"]",%this comma is used one too many times.
    A2 = list_to_binary(A),
    B = prove_facts2(T, Trees),
    C = case T of
	    [] -> <<>>;
	    _ -> <<", ">>
		     end,
    <<A2/binary, C/binary, B/binary>>.

tree2id(accounts) ->1;
tree2id(channels) -> 2;
tree2id(existence) -> 3;
tree2id(burn) -> 4;
tree2id(oracles) -> 5;
tree2id(governance) -> 6.

new_bet(Code, Amount, Prove) ->
    #bet{code = Code, amount = Amount, prove = Prove}.
new(Acc1, Acc2, CID, Bets, SG, TG, Nonce, Delay, Entropy) ->
    %Prove = many([], length(Bets)),
    #spk{acc1 = Acc1, acc2 = Acc2, entropy = Entropy,
	 bets = Bets, space_gas = SG, time_gas = TG,
	 cid = CID, nonce = Nonce, delay = Delay
	}.
bet_unlock(SPK, SS) ->
    Bets = SPK#spk.bets,
    %check if we have the secret to unlock each bet.
    %unlock the ones we can, and return an SPK with the remaining bets and the new amount of money that is moved.
    {Remaining, AmountChange, SSRemaining, Secrets, Dnonce, SSThem} = bet_unlock2(Bets, [], 0, SS, [], [], 0, []),
    {lists:reverse(SSRemaining),
     SPK#spk{bets = lists:reverse(Remaining),
	     amount = SPK#spk.amount + AmountChange,
	     nonce = SPK#spk.nonce + Dnonce},
     Secrets, SSThem}.
bet_unlock2([], B, A, [], SS, Secrets, Nonce, SSThem) ->
    {B, A, SS, Secrets, Nonce, lists:reverse(SSThem)};
bet_unlock2([Bet|T], B, A, [SS|SSIn], SSOut, Secrets, Nonce, SSThem) ->
    Code = Bet#bet.code, 
    case secrets:read(Code) of
	<<"none">> -> 
	    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
	SS2 -> 
	    %Just because a bet is removed doesn't mean all the money was transfered. We should calculate how much of the money was transfered.
	    {Trees, Height, _} = tx_pool:data(),
	    State = chalang_state(Height, 0, Trees),
	    FunLimit = free_constants:fun_limit(),
	    VarLimit = free_constants:var_limit(),
	    {ContractAmount, Nonce2, ShareRoot, Delay, _OpGas} =
		run([SS2], [Bet], 
		    free_constants:bet_gas_limit(),
		    free_constants:bet_gas_limit(),
		    FunLimit, VarLimit, State, 0),
	    ShareRoot = [],%deal with lightning shares later.
	    bet_unlock2(T, B, A+ContractAmount, SSIn, SSOut, [{secret, SS2, Code}|Secrets], Nonce + Nonce2, [SS2|SSThem])
    end.
	    
	    
	    
    
many(_, 0) -> [];
many(X, N) -> [X|many(X, N-1)].
    
apply_bet(Bet, Amount, SPK, Time, Space) ->
%bet is binary, the SPK portion of the script.
%SPK is the old SPK, we output the new one.
    SPK#spk{bets = [Bet|SPK#spk.bets], 
	    nonce = SPK#spk.nonce + 1, 
	    time_gas = SPK#spk.time_gas + Time, 
	    space_gas = max(SPK#spk.space_gas, Space), 
	    amount = SPK#spk.amount + Amount}.
settle_bet(SPK, Bets, Amount, N) ->
    SPK#spk{bets = Bets, amount = Amount, nonce = SPK#spk.nonce + N}.
get_paid(SPK, MyID, Amount) -> %if Amount is positive, that means money is going to Aid2.
    Aid1 = SPK#spk.acc1,
    Aid2 = SPK#spk.acc2,
    D = case MyID of
	Aid1 -> -1;
	Aid2 -> 1;
	_ -> MyID = Aid1
    end,
    SPK#spk{amount = (SPK#spk.amount + (D*Amount)), 
	    nonce = SPK#spk.nonce + 1}.
	    
run(Mode, SS, SPK, Height, Slash, Trees) ->
    %Accounts = trees:accounts(Trees),
    %Channels = trees:channels(Trees),
    %State = chalang:new_state(0, Height, Slash, 0, Accounts, Channels),
    State = chalang_state(Height, Slash, Trees),
    {Amount, NewNonce, CodeShares, Delay, _} = run2(Mode, SS, SPK, State, Trees),
    true = NewNonce < 1000,
    Shares = shares:from_code(CodeShares),
    {Amount + SPK#spk.amount, NewNonce + (1000 * SPK#spk.nonce), Shares, Delay}.
run2(fast, SS, SPK, State, Trees) -> 
    Governance = trees:governance(Trees),
    FunLimit = governance:get_value(fun_limit, Governance),
    VarLimit = governance:get_value(var_limit, Governance),
    true = is_list(SS),
    Bets = SPK#spk.bets,
    %Scripts = bets2scripts(Bets, Trees),
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
%bets2scripts([], _) -> [];
%bets2scripts([B|T], Trees) ->
%    F = prove_facts(B#bet.prove, Trees),
%    C = B#bet.code,
%    [<<F/binary, C/binary>>|bets2scripts(T, Trees)].
chalang_state(Height, Slash, Trees) ->	    
    chalang:new_state(Height, Slash, Trees).
run(ScriptSig, Codes, OpGas, RamGas, Funs, Vars, State, SPKDelay) ->
    run(ScriptSig, Codes, OpGas, RamGas, Funs, Vars, State, 0, 0, SPKDelay, []).

run([], [], OpGas, RamGas, Funs, Vars, State, Amount, Nonce, Delay, ShareRoot) ->
    {Amount, Nonce, ShareRoot, Delay, OpGas};
run([SS|SST], [Code|CodesT], OpGas, RamGas, Funs, Vars, State, Amount, Nonce, Delay, Share0) ->
    {A2, N2, Share, Delay2, EOpGas} = 
	run3(SS, Code, OpGas, RamGas, Funs, Vars, State),
    run(SST, CodesT, EOpGas, RamGas, Funs, Vars, State, A2+Amount, N2+Nonce, max(Delay, Delay2), Share ++ Share0).
run3(ScriptSig, Bet, OpGas, RamGas, Funs, Vars, State) ->
    %compiler_chalang:print_binary(ScriptSig),
    true = chalang:none_of(ScriptSig),
    {Trees, _, _} = tx_pool:data(),
    F = prove_facts(Bet#bet.prove, Trees),
    C = Bet#bet.code,
    Code = <<F/binary, C/binary>>,  
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, ScriptSig, Code, State),
    Data2 = chalang:run5([ScriptSig], Data),
    Data3 = chalang:run5([Code], Data2),
    [ShareRoot|
     [<<Amount:32>>|
       [<<Nonce:32>>|
	[<<Delay:32>>|_]]]] = chalang:stack(Data3),%#d.stack,
    CGran = constants:channel_granularity(),
    true = Amount <= CGran,
    A3 = Amount * Bet#bet.amount div CGran,
    {A3, Nonce, ShareRoot, Delay,
     chalang:time_gas(Data3)
    }.

is_improvement(OldSPK, OldSS, NewSPK, NewSS) ->
    {Trees, Height, _} = tx_pool:data(),
    {_, Nonce2, _, Delay2} =  run(fast, NewSS, NewSPK, Height, 0, Trees),
    {_, Nonce1, _, _} =  run(fast, OldSS, OldSPK, Height, 0, Trees),
    true = Nonce2 > Nonce1,
    Bets2 = NewSPK#spk.bets,
    Bets1 = OldSPK#spk.bets,
    SG = NewSPK#spk.space_gas,
    TG = NewSPK#spk.time_gas,
    true = Delay2 < free_constants:max_channel_delay(),
    true = SG < free_constants:space_limit(),
    true = TG < free_constants:time_limit(),
    Amount2 = NewSPK#spk.amount,
    Amount1 = OldSPK#spk.amount,
    NewSPK = OldSPK#spk{bets = Bets2,
			space_gas = SG,
			time_gas = TG,
			amount = Amount2,
			nonce = NewSPK#spk.nonce},
    CID = NewSPK#spk.cid,
    Channels = trees:channels(Trees),
    {_, Channel, _} = channels:get(CID, Channels),
    KID = keys:id(),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    Profit = 
	if
	    KID == Acc1 ->
		Amount2 - Amount1;
	    KID == Acc2 ->
		Amount1 - Amount2
	end,
    LT = length(Bets2),
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
	(Profit >= 0) and (LT > 0)->
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
		(Profit >= 0) and %costs nothing
		(T == Bets1) and %only add one more bet
		(PotentialGain > 0) and %potentially gives us money
		(Obligations2 =< ChannelBal2) and
		(Obligations1 =< ChannelBal1)
		-> true; 
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
    
find_extra(New, Old) -> 
    find_extra(New, Old, 0).
find_extra([], [], Amount) ->
    {true, Amount};
find_extra(_, [], Amount) ->
    {false, Amount};
find_extra(B, [A|T], Amount) ->
    C = is_in(A, B),
    if
	C ->
	    find_extra(remove(A, B), T, Amount);
	true ->
	    find_extra(B, T, Amount + abs(A#bet.amount))
    end.
is_in(A, []) -> false;
is_in(A, [A|_]) -> true;
is_in(A, [_|C]) -> is_in(A, C).
remove(A, [A|T]) -> T;
remove(A, [B|T]) -> 
    [B|remove(A, T)].

	
test() ->
    %test prove_facts.
    {Trees, _, _} = tx_pool:data(),
    Code = prove_facts([{governance, 5},{accounts, 1}], Trees),
    State = chalang_state(1, 0, Trees), 
    [[[<<6:32>>, <<5:32>>, Gov5], %6th tree is governance. 5th thing is "delete channel reward"
      [<<1:32>>, <<1:32>>, Acc1]]] = %1st tree is accounts. 1 is for account id 1.
	chalang:vm(Code, 100000, 100000, 1000, 1000, State),
    Governance = trees:governance(Trees),
    {_, Govern5, _} = governance:get(5, Governance),
    Accounts = trees:accounts(Trees),
    {_, Account1, _} = accounts:get(1, Accounts),
    Acc1 = accounts:serialize(Account1),
    Gov5 = governance:serialize(Govern5),
    success.
    
    
