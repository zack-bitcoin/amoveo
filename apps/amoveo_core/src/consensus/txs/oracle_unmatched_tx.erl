-module(oracle_unmatched_tx).
-export([make/4, make_dict/3, go/4, from/1, oracle_id/1]).
%If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.
-record(unmatched, {from, nonce, fee, oracle_id}).
-include("../../records.hrl").

from(X) -> X#unmatched.from.
oracle_id(X) -> X#unmatched.oracle_id.
           
make_dict(From, Fee, OracleID) ->
    Acc = trees:get(accounts, From),
    #unmatched{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, oracle_id = OracleID}.
    
make(From, Fee, OracleID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #unmatched{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, oracle_id = OracleID},
    {Tx, [Proof]}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    OracleID = Tx#unmatched.oracle_id,
    Oracle = oracles:dict_get(OracleID, Dict),
    Result = Oracle#oracle.result,
    false = Result == 0,
    AID = Tx#unmatched.from,
    F10 = NewHeight > forks:get(10),
    UMT = if%
	      F10  -> unmatched;
	      true -> orders%
	  end,%
    Order = UMT:dict_get({key, AID, OracleID}, Dict),
    Amount = UMT:amount(Order),
    Dict2 = UMT:dict_remove(AID, OracleID, Dict),
    Nonce = if
		NonceCheck -> Tx#unmatched.nonce;
		true -> none
	    end,
    Facc = accounts:dict_update(AID, Dict2, Amount - Tx#unmatched.fee, Nonce),
    accounts:dict_write(Facc, Dict2).
