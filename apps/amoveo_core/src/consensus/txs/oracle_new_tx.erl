-module(oracle_new_tx).
-export([go/4, make/7, make_dict/6, make_dict/7, from/1, id/1, governance/1, id_generator/1, id_generator2/4, scalar_q_maker/3, scalar_keys/2, scalar_keys/1]).
-include("../../records.hrl").
%-export([go/4, make/7, make_dict/6, make_dict/7, from/1, id/1, governance/1, id_generator2/4]).
%-include("../../records.hrl").
%This asks the oracle a question.
%The oracle can only answer true/false questions.
%Running the oracle costs a fee which is used as a reward to get people to use the oracle.
%The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
%The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
%The oracle has a start-date written in it. Trading doesn't start until the start-date.
%The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.
from(X) -> X#oracle_new.from.
id(X) -> X#oracle_new.id.

scalar_keys(OID) ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    S = Oracle#oracle.starts,
    scalar_keys(OID, S).
scalar_keys(OID, Start) ->
    lists:reverse(scalar_keys2(OID, Start, 1, [{oracles, OID}])).
scalar_keys2(_, _, 10, Out) -> Out;
scalar_keys2(OID, Start, I, Out) ->
    QuestionN = scalar_q_maker(I, 0, OID),
    OID2 = id_generator2(Start, 0, 0, QuestionN),
    scalar_keys2(OID, Start, I+1, [{oracles, OID2}|Out]).

id_generator2(Start, Gov, GA, Question) ->
    true = is_binary(Question),
    id_generator3(Start, Gov, GA, hash:doit(Question)).
id_generator3(Start, Gov, GA, QH) ->
    S = <<Start:32,Gov:32,GA:32,QH/binary>>,
    hash:doit(S).
%id_generator2(Start, Gov, GA, Question) ->
%    QH = hash:doit(Question),
%    B = <<Start:32,Gov:32,GA:32,QH/binary>>,
%    hash:doit(<<Start:32,Gov:32,GA:32,QH/binary>>).
id_generator(Tx) ->
    id_generator2(Tx#oracle_new.start,
                  Tx#oracle_new.governance,
                  Tx#oracle_new.governance_amount,
                  Tx#oracle_new.question).
scalar_q_maker(0, Question, _OID1) -> Question;
scalar_q_maker(10, Question, _OID1) -> Question;
scalar_q_maker(Many, _Question, OID1) when ((Many > 0) and (Many < 10))->
    <<(list_to_binary("scalar "))/binary, 
      (base64:encode(OID1))/binary, 
      (list_to_binary(" bit number "))/binary, 
      (list_to_binary (integer_to_list(Many)))/binary>>.
%id_generator_old(Tx) ->
%    hash:doit(<<(Tx#oracle_new.start):32,
%               (Tx#oracle_new.governance):32,
%               (Tx#oracle_new.governance_amount):32,
%               (Tx#oracle_new.question)/binary>>).
governance(X) -> X#oracle_new.governance.
make_dict(From, Fee, Question, Start, Governance, GovAmount) ->
    Acc = trees:get(accounts, From),
    Tx0 = #oracle_new{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, question = Question, start = Start, governance = Governance, governance_amount = GovAmount},
    ID = id_generator(Tx0),
    Tx0#oracle_new{id = ID}.
make_dict(From, Fee, Question, Start, ID, Governance, GovAmount) ->
    1=2,
    <<_:256>> = ID,
    Acc = trees:get(accounts, From),
    Tx0 = #oracle_new{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, question = Question, start = Start, governance = Governance, governance_amount = GovAmount},
    %ID = id_generator(Tx0),
    Tx0#oracle_new{id = ID}.
    
make(From, Fee, Question, Start, Governance, GovAmount, Trees) ->
    %<<_:256>> = ID,
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = accounts:get(From, Accounts),
    Tx0 = #oracle_new{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, question = Question, start = Start, governance = Governance, governance_amount = GovAmount},
    ID = id_generator(Tx0),
    Tx = Tx0#oracle_new{id = ID},
    {Tx, []}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    ID = Tx#oracle_new.id,
    F24 = forks:get(24),
    if 
        NewHeight > F24 ->
            %io:fwrite("oracle new tx go \n"),
            %io:fwrite(packer:pack(ID)),
            %io:fwrite("\n"),
            ID = id_generator(Tx);
        true -> ok
    end,
    F52 = forks:get(52) > NewHeight,
    if
        F52 ->
            empty = oracles:dict_get(ID, Dict, NewHeight);
        true ->
            1=2
    end,
    Gov = Tx#oracle_new.governance,
    GovAmount = Tx#oracle_new.governance_amount,
    GCL = governance:dict_get_value(governance_change_limit, Dict, NewHeight),
    true = GovAmount > -1,
    true = GovAmount < GCL,
    Question = Tx#oracle_new.question,
    %Starts = Tx#oracle_new.start,
    false = (NewHeight == forks:get(5)),
    {Dict2, Starts, OIL} = 
        case Gov of
            0 ->
                GovAmount = 0,
		FG5 = NewHeight > (forks:get(5)),
		%FG5 = false,
		L1 = if
			 FG5 ->
			     governance:dict_get_value(oracle_question_liquidity, Dict, NewHeight);
			 true ->
			     governance:dict_get_value(oracle_initial_liquidity, Dict, NewHeight)%
		     end,
		{Dict, Tx#oracle_new.start, L1};
	    _ ->
		%make sure the oracle starts now, and has no delay.
                true = GovAmount > 0,
		Question = <<"">>,
                GVar = governance:dict_get(Gov, Dict),
                false = governance:is_locked(GVar),
		FG1 = forks:get(1),%
		L2 = governance:dict_get_value(oracle_initial_liquidity, Dict, NewHeight),
		if%
		    FG1 < NewHeight -> %
			{governance:dict_lock(Gov, Dict), NewHeight, L2};%
		    true ->%
			{governance:dict_lock(Gov, Dict), max(NewHeight, Tx#oracle_new.start), L2}
		end%
        end,
    FG31 = forks:get(31),
    if
        NewHeight < FG31 ->
            false = Starts < NewHeight;
        true -> ok
    end,
    ok = case Question of
             <<"">> -> ok;
             _ ->
                 MQS = governance:dict_get_value(maximum_question_size, Dict2, NewHeight),
                 F47_activated = forks:get(47) < NewHeight,
                 SizeLimit = if
                                 F47_activated ->
                                     MQS * 10;
                                 true -> MQS
                             end,
                 true = size(Question) < SizeLimit,
                 0 = GovAmount,
		 ok
	 end,
    From = Tx#oracle_new.from,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Tx#oracle_new.nonce),
    %Nonce = if
	%	NonceCheck -> Tx#oracle_new.nonce;
	%	true -> none
	%    end,
    Facc = accounts:dict_update(From, Dict2, -Tx#oracle_new.fee-OIL, Nonce),
    Dict3 = accounts:dict_write(Facc, Dict2),
    %OFL = governance:dict_get_value(oracle_future_limit, Dict3),
    %true = (Starts - NewHeight) < OFL,
    Question = Tx#oracle_new.question,
    true = is_binary(Question),
    QH = hash:doit(Question),
    oracle_questions:store(QH, Question),
    F10 = NewHeight > forks:get(10),
    ON = oracles:new(ID, QH, Starts, From, Gov, GovAmount, Dict, F10, NewHeight),
    Dict4 = if
		F10 -> 
		    unmatched:dict_empty_book(ID, Dict3);
		true -> Dict3%
	    end,
    oracles:dict_write(ON, Dict4).
    
    
