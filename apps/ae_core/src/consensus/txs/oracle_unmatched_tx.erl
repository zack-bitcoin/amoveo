-module(oracle_unmatched_tx).
-export([make/4, go/3, from/1, oracle_id/1]).
%If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.
-record(unmatched, {from, nonce, fee, oracle_id}).
-include("../../records.hrl").

from(X) -> X#unmatched.from.
oracle_id(X) -> X#unmatched.oracle_id.
           
make(From, Fee, OracleID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #unmatched{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, oracle_id = OracleID},
    {Tx, [Proof]}.

go(Tx, Dict, NewHeight) ->
    OracleID = Tx#unmatched.oracle_id,
    AID = Tx#unmatched.from,
    Order = orders:dict_get({key, AID, OracleID}, Dict),
    Amount = orders:amount(Order),
    Dict2 = orders:dict_remove(AID, OracleID, Dict),
    Facc = accounts:dict_update(AID, Dict2, Amount - Tx#unmatched.fee, Tx#unmatched.nonce),
    accounts:dict_write(Facc, Dict2).
