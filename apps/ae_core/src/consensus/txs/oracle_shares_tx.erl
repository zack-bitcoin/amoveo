-module(oracle_shares_tx).
-export([doit/3, go/3, make/4, from/1, oracle_id/1]).

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
doit(Tx, Trees, NewHeight) ->
    OID = Tx#oracle_shares.oracle_id,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    Result = oracles:result(Oracle),
    false = Result == 0,
    %DT = oracles:done_timer(Oracle),
    Governance = trees:governance(Trees),
    %MOT = governance:get_value(minimum_oracle_time, Governance),
    %true = NewHeight - MOT < DT,
    AID = Tx#oracle_shares.from,
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = accounts:get(AID, Accounts),
    Bets = accounts:bets(Acc),
    {_, Bet, _} = oracle_bets:get(OID, Bets),
    Reward = oracle_bets:reward(Bet, Result, NewHeight),
    Acc2 = accounts:update(AID, Trees, -Tx#oracle_shares.fee + Reward, Tx#oracle_shares.nonce, NewHeight),
    %transform their bets into shares.

    %Shares = accounts:shares(Acc),
    %Acc2 = accounts:receive_shares(Acc, B2Shares, NewHeight, Trees),
    Bets2 = oracle_bets:delete(OID, Bets),
    Acc3 = accounts:update_bets(Acc2, Bets2),
    
    Accounts2 = accounts:write(Accounts, Acc3),
    trees:update_accounts(Trees, Accounts2).
go(Tx, Dict, NewHeight) ->
    Dict.
