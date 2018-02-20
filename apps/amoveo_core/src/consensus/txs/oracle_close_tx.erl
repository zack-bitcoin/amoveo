-module(oracle_close_tx).
-export([make/4, make_dict/3, go/3, from/1, oracle_id/1]).
-record(oracle_close, {from, nonce, fee, oracle_id}).
-include("../../records.hrl").
%If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
%This ends betting in the market.
%The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.
from(X) -> X#oracle_close.from.
oracle_id(X) -> X#oracle_close.oracle_id.
make_dict(From, Fee, OID) ->
    Acc = trees:dict_tree_get(accounts, From),
    #oracle_close{from = From, fee = Fee, oracle_id = OID, nonce = Acc#acc.nonce + 1}.
    
make(From, Fee, OID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = accounts:get(From, Accounts),
    Tx = #oracle_close{from = From, fee = Fee, oracle_id = OID, nonce = Acc#acc.nonce + 1},
    {Tx, []}.
        
go(Tx, Dict, NewHeight) ->
    From = Tx#oracle_close.from,
    txs:developer_lock(From, NewHeight, Dict),
    Acc = accounts:dict_update(From, Dict, -Tx#oracle_close.fee, Tx#oracle_close.nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    OID = Tx#oracle_close.oracle_id,
    Oracle = oracles:dict_get(OID, Dict2),
    true = Oracle#oracle.starts =< NewHeight,
    OIL = governance:dict_get_value(oracle_initial_liquidity, Dict2),
    VolumeCheck = orders:dict_significant_volume(Dict2, OID, OIL),
    Result = if
		 VolumeCheck -> Oracle#oracle.type;
		 true -> 3
	     end,
    Oracle3 = Oracle#oracle{done_timer = NewHeight, result = Result},
    Dict4 = oracles:dict_write(Oracle3, Dict2),
    Gov = Oracle3#oracle.governance,
    MOT = governance:dict_get_value(maximum_oracle_time, Dict4),
    Dict5 = 
        case Gov of
            0 ->
		%is not a governance oracle.
		B1 = Oracle#oracle.done_timer < NewHeight,
		B2 = Oracle3#oracle.starts + MOT < NewHeight,
		true = (B1 or B2),
		Dict4;
	    G ->
                %io:fwrite("governance branch\n"),
		GA = Oracle3#oracle.governance_amount,
		case Result of
		    1 -> 
                        %io:fwrite("gov 1\n"),
			true = Oracle#oracle.done_timer < NewHeight,
			governance:dict_change(Gov, GA, Dict4);
		    2 ->
                        %io:fwrite("gov 2\n"),
			true = Oracle#oracle.done_timer < NewHeight,
			governance:dict_change(Gov, -GA, Dict4);
		    3 -> 
                        %io:fwrite("gov 3\n"),
			true = Oracle3#oracle.starts + MOT < NewHeight,
			governance:dict_unlock(G, Dict4)
                end
        end,
    Oracle4 = oracles:dict_get(OID, Dict5),
    OracleType = Oracle4#oracle.type,
    LoserType = 
	case OracleType of
	    1 -> 2;
	    2 -> 1;
	    3 -> 1
	end,
    OBTx = {oracle_bet, Oracle4#oracle.creator, 
	  none, 0, OID, LoserType, 
	  OIL div 2},
    Dict6 = oracle_bet_tx:go2(OBTx, Dict5, NewHeight),%maybe this is bad. maybe we only want to update the one account.
    spawn(fun() ->
                  timer:sleep(100),
                  api:settle_bets(),
                  ok
                  end),
    Dict6.
    
    
    
                
