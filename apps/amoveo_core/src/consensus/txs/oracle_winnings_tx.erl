-module(oracle_winnings_tx).
-export([go/4, make/4, make_dict/3, from/1, oracle_id/1]).
-include("../../records.hrl").

%If you bet in an oracle, and the oracle has closed, this is how you get your winnings out.
%If you bet on the winning outcome, then you get positive winnings. If you bet on one of the losing outcomes, then you get negative winnings.
%The difficulty of the winnings was announced when the oracle was launched.
-record(oracle_winnings, {from, nonce, fee, oracle_id}).
from(X) -> X#oracle_winnings.from.
oracle_id(X) -> X#oracle_winnings.oracle_id.
make_dict(From, Fee, OID) ->
    Acc = trees:get(accounts, From),
    #oracle_winnings{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, oracle_id = OID}.
    
make(From, Fee, OID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #oracle_winnings{from = From, nonce = Acc#acc.nonce + 1, fee = Fee, oracle_id = OID},
    {Tx, [Proof]}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    OID = Tx#oracle_winnings.oracle_id,
    Oracle = oracles:dict_get(OID, Dict),
    Result = Oracle#oracle.result,
    false = Result == 0,
    AID = Tx#oracle_winnings.from,
    F10 = NewHeight > forks:get(10),
    UMT = if%
	      F10  -> matched;
	      true -> oracle_bets%
	  end,%
    Bet = UMT:dict_get({key, AID, OID}, Dict),
    Reward = UMT:reward(Bet, Result, NewHeight),
    Nonce = if
		NonceCheck -> Tx#oracle_winnings.nonce;
		true -> none
	    end,
    Acc2 = accounts:dict_update(AID, Dict, -Tx#oracle_winnings.fee + Reward, Nonce),
    Dict2 = accounts:dict_write(Acc2, Dict),
    UMT:dict_delete({key, AID, OID}, Dict2).
    
    
    
    
