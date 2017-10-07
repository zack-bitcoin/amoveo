-module(oracle_unmatched_tx).
-export([make/4, go/3, from/1, oracle_id/1]).
%If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.
-record(unmatched, {from, nonce, fee, oracle_id}).

from(X) -> X#unmatched.from.
oracle_id(X) -> X#unmatched.oracle_id.
           
make(From, Fee, OracleID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #unmatched{from = From, nonce = accounts:nonce(Acc) + 1, fee = Fee, oracle_id = OracleID},
    {Tx, [Proof]}.

go(Tx, Dict, NewHeight) ->
    OracleID = Tx#unmatched.oracle_id,
    AID = Tx#unmatched.from,
    %Oracle = oracles:dict_get(OracleID, Oracles),
    io:fwrite("oracle unmatched 0\n"),
    Order = orders:dict_get({key, AID, OracleID}, Dict),
    io:fwrite("oracle unmatched order is "),
    io:fwrite(packer:pack(Order)),
    io:fwrite("\n"),
    io:fwrite("oracle unmatched 1\n"),
    Amount = orders:amount(Order),
    io:fwrite("oracle unmatched 2\n"),
    Dict2 = orders:dict_remove(AID, OracleID, Dict),
    Facc = accounts:dict_update(AID, Dict2, Amount - Tx#unmatched.fee, Tx#unmatched.nonce, NewHeight),
    io:fwrite("oracle unmatched 5\n"),
    accounts:dict_write(Facc, Dict2).
