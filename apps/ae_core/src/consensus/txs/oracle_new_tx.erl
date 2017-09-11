-module(oracle_new_tx).
-export([doit/3, go/3, make/10, from/1, id/1, recent_price/1]).
-record(oracle_new, {from = 0, 
		     nonce = 0, 
		     fee = 0, 
		     question = <<>>, 
		     start, 
		     id, 
		     recent_price, %if this is a governance oracle, or if it is asking a question, then we need to reference another oracle that closed recently with the state "bad". We reference it so we know the current price of shares.
		     difficulty, 
		     governance, 
		     governance_amount}).
%This asks the oracle a question.
%The oracle can only answer true/false questions.
%Running the oracle costs a fee which is used as a reward to get people to use the oracle.
%The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
%The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
%The oracle has a start-date written in it. Trading doesn't start until the start-date.
%The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.
from(X) -> X#oracle_new.from.
recent_price(X) -> X#oracle_new.recent_price.
id(X) -> X#oracle_new.id.
make(From, Fee, Question, Start, ID, Difficulty, Recent, Governance, GovAmount, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = accounts:get(From, Accounts),
    Tx = #oracle_new{from = From, nonce = accounts:nonce(Acc) + 1, fee = Fee, question = Question, start = Start, id = ID, recent_price = Recent, difficulty = Difficulty, governance = Governance, governance_amount = GovAmount},
    {Tx, []}.
doit(Tx, Trees0, NewHeight) ->
    %If the question is <<"">>, let it run.
    %If the question is not <<"">>, then they need to show that a different oracle with the question "" recently returned "bad", and the difficulty of this oracle is 1/2 as high as that oracle.
    Oracles = trees:oracles(Trees0),
    GovTree = trees:governance(Trees0),
    Gov = Tx#oracle_new.governance,
    GovAmount = Tx#oracle_new.governance_amount,
    GCL = governance:get_value(governance_change_limit, GovTree),
    true = GovAmount > -1,
    true = GovAmount < GCL,
    Question = Tx#oracle_new.question,
    {_, Recent, _} = oracles:get(Tx#oracle_new.recent_price,Oracles),
    Trees = 
	case Gov of
	    0 -> 
		GovAmount = 0,
		Trees0;
	    G ->
		true = GovAmount > 0,
		3 = oracles:result(Recent),
		GD = governance:get_value(governance_delay, GovTree),
		true = NewHeight - oracles:done_timer(Recent) < GD,
		Dif = oracles:difficulty(Recent),
		Dif = Tx#oracle_new.difficulty,
		Question = <<"">>,
		{_, GVar, _} = governance:get(G, GovTree),
		false = governance:is_locked(GVar),
		NewGovTree = governance:lock(G, GovTree),
		%NewGovTree = governance:write(NewGVar, GovTree),
		trees:update_governance(Trees0, NewGovTree)
	end,
    Governance = trees:governance(Trees),
    ok = case Question of
	     <<"">>-> ok;
	     Q -> 
		 %get the recent oracle, make sure it's question was <<"">>, make sure our difficulty is half as high as that difficulty.
		 MQS = governance:get_value(maximum_question_size, Governance),
		 true = size(Q) < MQS,
		 0 = GovAmount,
		 Di = oracles:difficulty(Recent) div 2,
		 Di = Tx#oracle_new.difficulty,
		 3 = oracles:result(Recent),
		 QD = governance:get_value(question_delay, Governance),
		 true = NewHeight - oracles:done_timer(Recent) < QD,
		 ok
	 end,
    Accounts = trees:accounts(Trees),
    From = Tx#oracle_new.from,
    OIL = governance:get_value(oracle_initial_liquidity, Governance),
    Facc = accounts:update(From, Trees, -Tx#oracle_new.fee-OIL, Tx#oracle_new.nonce, NewHeight),
    NewAccounts = accounts:write(Facc, Accounts),
    Starts = Tx#oracle_new.start,
    OFL = governance:get_value(oracle_future_limit, Governance),
    %true = (Starts - NewHeight) < constants:oracle_future_limit(),
    true = (Starts - NewHeight) < OFL,
    ID = Tx#oracle_new.id,
    Question = Tx#oracle_new.question,
    true = is_binary(Question),
    QH = testnet_hasher:doit(Question),
    Diff = Tx#oracle_new.difficulty,
    ON = oracles:new(ID, QH, Starts, From, Diff, Gov, GovAmount, Trees),
    {_, empty, _} = oracles:get(ID, Oracles),
    NewOracles = oracles:write(ON, Oracles),
    Trees2 = trees:update_oracles(Trees, NewOracles),
    trees:update_accounts(Trees2, NewAccounts).
go(Tx, Dict, NewHeight) ->
    Gov = Tx#oracle_new.governance,
    GovAmount = Tx#oracle_new.governance_amount,
    GCL = governance:dict_get_value(governance_change_limit, Dict),
    true = GovAmount > -1,
    true = GovAmount < GCL,
    Question = Tx#oracle_new.question,
    Dict2 = 
        case Gov of
            0 ->
                GovAmount = 0,
                Dict;
            G ->
                Recent = oracles:dict_get(Tx#oracle_new.recent_price, Dict),
                true = GovAmount > 0,
                3 = oracles:result(Recent),
                GD = governance:dict_get_value(governance_delay, Dict),
                true = NewHeight - oracles:done_timer(Recent) < GD,
		Dif = oracles:difficulty(Recent),
		Dif = Tx#oracle_new.difficulty,
		Question = <<"">>,
                GVar = governance:dict_get(G, Dict),
                false = governance:is_locked(GVar),
                governance:dict_lock(G, Dict)
        end,
    ok = case Question of
             <<"">> -> ok;
             Q ->
                 Recent2 = oracles:dict_get(Tx#oracle_new.recent_price, Dict),
                 MQS = governance:dict_get_value(maximum_question_size, Dict2),
                 true = size(Q) < MQS,
                 0 = GovAmount,
		 Di = oracles:difficulty(Recent2) div 2,
		 Di = Tx#oracle_new.difficulty,
		 3 = oracles:result(Recent2),
		 QD = governance:dict_get_value(question_delay, Dict2),
                 true = NewHeight - oracles:done_timer(Recent2) < QD,
                 ok
         end,
    From = Tx#oracle_new.from,
    OIL = governance:dict_get_value(oracle_initial_liquidity, Dict2),
    Facc = accounts:dict_update(From, Dict2, -Tx#oracle_new.fee-OIL, Tx#oracle_new.nonce, NewHeight),
    Dict3 = accounts:dict_write(Facc, Dict2),
    Starts = Tx#oracle_new.start,
    OFL = governance:dict_get_value(oracle_future_limit, Dict3),
    true = (Starts - NewHeight) < OFL,
    ID = Tx#oracle_new.id,
    Question = Tx#oracle_new.question,
    true = is_binary(Question),
    QH = testnet_hasher:doit(Question),
    Diff = Tx#oracle_new.difficulty,
    ON = oracles:dict_new(ID, QH, Starts, From, Diff, Gov, GovAmount, Dict),
    empty = oracles:dict_get(ID, Dict),
    Dict4 = oracles:dict_write(ON, Dict3).
    
    
