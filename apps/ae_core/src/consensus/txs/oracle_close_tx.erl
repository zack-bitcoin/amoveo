-module(oracle_close_tx).
-export([make/4, go/3, doit/3, from/1, oracle_id/1]).
-record(oracle_close, {from, nonce, fee, oracle_id}).
%If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
%This ends betting in the market.
%The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.
from(X) -> X#oracle_close.from.
oracle_id(X) -> X#oracle_close.oracle_id.
make(From, Fee, OID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = accounts:get(From, Accounts),
    Tx = #oracle_close{from = From, fee = Fee, oracle_id = OID, nonce = accounts:nonce(Acc) + 1},
    {Tx, []}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Acc = accounts:update(Tx#oracle_close.from, Trees, -Tx#oracle_close.fee, Tx#oracle_close.nonce, NewHeight),
    NewAccounts = accounts:write(Acc, Accounts),

    OID = Tx#oracle_close.oracle_id,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    true = oracles:starts(Oracle) =< NewHeight,
    %if the volume of orders in the oracle is too low, then set the oracle:type to 3.
    %Result = oracles:type(Oracle),
    Orders0 = oracles:orders(Oracle),
    VolumeCheck = orders:significant_volume(Orders0, Trees),
    Result = if
		 VolumeCheck -> oracles:type(Oracle);
		 true -> 3
	     end,
    Oracle2 = oracles:set_result(Oracle, Result),
    Oracle3 = oracles:set_done_timer(Oracle2, NewHeight),
    Oracles2 = oracles:write(Oracle3, Oracles),
    Trees2 = trees:update_accounts(trees:update_oracles(Trees, Oracles2), NewAccounts),
    Gov = oracles:governance(Oracle3),
    Governance = trees:governance(Trees),
    %Mot = governance:get_value(minimum_oracle_time, Governance),
    %DT = oracles:done_timer(Oracle),
    %true = NewHeight - Mot < DT,
    MOT = governance:get_value(maximum_oracle_time, Governance),
    Trees3 = 
	case Gov of
	    0 -> 
		%is not a governance oracle.
		%io:fwrite(packer:pack({done_timer, oracles:done_timer(Oracle), oracles:starts(Oracle3)})),
		%io:fwrite("\n"),
		B1 = oracles:done_timer(Oracle) < NewHeight,
		B2 = oracles:starts(Oracle3) + MOT < NewHeight,
		true = (B1 or B2),
		Trees2;
	    G ->
		GA = oracles:governance_amount(Oracle3),
		case Result of
		    1 -> 
			1=2,
			true = oracles:done_timer(Oracle3) < NewHeight,
			Gov2=governance:change(G, GA, Gov),
			trees:update_governance(Gov2, Trees2);
		    2 ->
			1=2,
			true = oracles:done_timer(Oracle3) < NewHeight,
			Gov2=governance:change(G, -GA,Gov),
			trees:update_governance(Gov2, Trees2);
		    3 -> 
			true = oracles:starts(Oracle3) + MOT < NewHeight,
			Gov2 = governance:unlock(G, Gov),
			trees:update_governance(Gov2, Trees2)
			%Trees2
		end
	end,
    OraclesEE = trees:oracles(Trees3),
    {_, Oracle4, _} = oracles:get(OID, OraclesEE),
    OracleType = oracles:type(Oracle4),
    LoserType = 
	case OracleType of
	    1 -> 2;
	    2 -> 1;
	    3 -> 1
	end,
    OBTx = {oracle_bet, oracles:creator(Oracle4), 
	  none, 0, OID, LoserType, 
	  constants:oracle_initial_liquidity()},
    Trees4 = oracle_bet_tx:doit2(OBTx, Trees3, NewHeight),
    Accounts4 = trees:accounts(Trees4),
    trees:update_accounts(Trees3, Accounts4).
go(Tx, Dict, NewHeight) ->
    Acc = accounts:dict_update(Tx#oracle_close.from, Dict, -Tx#oracle_close.fee, Tx#oracle_close.nonce, NewHeight),
    Dict2 = accounts:dict_write(Acc, Dict),
    OID = Tx#oracle_close.oracle_id,
    Oracle = oracles:dict_get(OID, Dict2),
    true = oracles:starts(Oracle) =< NewHeight,
    OIL = governance:dict_get_value(oracle_initial_liquidity, Dict2),
    VolumeCheck = orders:dict_significant_volume(Dict2, OID, OIL),
    Result = if
		 VolumeCheck -> oracles:type(Oracle);
		 true -> 3
	     end,
    Oracle2 = oracles:set_result(Oracle, Result),
    Oracle3 = oracles:set_done_timer(Oracle2, NewHeight),
    Dict3 = oracles:dict_write(Oracle3, Dict2),
    Gov = oracles:governance(Oracle3),
    MOT = governance:dict_get_value(maximum_oracle_time, Dict3),
    Dict4 = 
        case Gov of
            0 ->
		%is not a governance oracle.
		B1 = oracles:done_timer(Oracle) < NewHeight,
		B2 = oracles:starts(Oracle3) + MOT < NewHeight,
		true = (B1 or B2),
		Dict3;
	    G ->
		GA = oracles:governance_amount(Oracle3),
		case Result of
		    1 -> 
			true = oracles:done_timer(Oracle3) < NewHeight,
			governance:dict_change(GA, Gov, Dict3);
		    2 ->
			true = oracles:done_timer(Oracle3) < NewHeight,
			governance:dict_change(-GA, Gov, Dict3);
		    3 -> 
			true = oracles:starts(Oracle3) + MOT < NewHeight,
			governance:dict_unlock(G, Dict3)
                end
        end,
    Oracle4 = oracles:dict_get(OID, Dict4),
    OracleType = oracles:type(Oracle4),
    LoserType = 
	case OracleType of
	    1 -> 2;
	    2 -> 1;
	    3 -> 1
	end,
    OBTx = {oracle_bet, oracles:creator(Oracle4), 
	  none, 0, OID, LoserType, 
	  constants:oracle_initial_liquidity()},
    oracle_bet_tx:go2(OBTx, Dict4, NewHeight).%maybe this is bad. maybe we only want to update the one account and it's bets.
    
                
