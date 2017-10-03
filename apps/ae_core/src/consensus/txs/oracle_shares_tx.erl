-module(oracle_shares_tx).
-export([go/3, make/4, from/1, oracle_id/1]).

%If you bet in an oracle, and the oracle has closed, this is how you get your shares out.
%If you bet on the winning outcome, then you get positive shares. If you bet on one of the losing outcomes, then you get negative shares.
%[you can read about shares here](docs/shares.md)
%The difficulty of the shares was announced when the oracle was launched.
-record(oracle_shares, {from, nonce, fee, oracle_id}).
from(X) -> X#oracle_shares.from.
oracle_id(X) -> X#oracle_shares.oracle_id.
make(From, Fee, OID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #oracle_shares{from = From, nonce = accounts:nonce(Acc) + 1, fee = Fee, oracle_id = OID},
    {Tx, [Proof]}.
go(Tx, Dict, NewHeight) ->
    OID = Tx#oracle_shares.oracle_id,
    Oracle = oracles:dict_get(OID, Dict),
    Result = oracles:result(Oracle),
    false = Result == 0,
    AID = Tx#oracle_shares.from,
    Bet = oracle_bets:dict_get({key, AID, OID}, Dict),
    Reward = oracle_bets:reward(Bet, Result, NewHeight),
    Acc2 = accounts:dict_update(AID, Dict, -Tx#oracle_shares.fee + Reward, Tx#oracle_shares.nonce, NewHeight),
    Dict2 = accounts:dict_write(Acc2, Dict),
    oracle_bets:dict_delete({key, AID, OID}, Dict2).
    
    
    
    
