-module(sub_spend_tx).
-export([go/4, make_dict/6, from/1, to/1]).
-include("../../records.hrl").
from(X) -> X#sub_spend_tx.from.
to(X) -> X#sub_spend_tx.to. 
make_dict(To, Amount, Fee, Contract, Type, From) ->
    Acc = trees:get(accounts, From),
    #sub_spend_tx{from = From, nonce = Acc#acc.nonce + 1, to = To, amount = Amount, fee = Fee, contract = Contract, type = Type}.
	    
go(Tx, Dict, NewHeight, NonceCheck) ->
    true = NewHeight > forks:get(32),
    #sub_spend_tx{
    from = From,
    to = To,
    amount = A,
    nonce = Nonce0,
    type = N,
    contract = CID,
    fee = Fee
   } = Tx,
    false = From == To,
%    Nonce = if
%		NonceCheck -> Nonce0;
%		true -> none
%	    end,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Tx#sub_spend_tx.nonce),
    FromKey = sub_accounts:make_key(From, CID, N),
    ToKey = sub_accounts:make_key(To, CID, N),
    
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    F2acc = sub_accounts:dict_update(FromKey, Dict, -A, none),
    
    Dict2 = accounts:dict_write(Facc, Dict),
    Dict3 = sub_accounts:dict_write(F2acc, Dict2),
    
    OA = sub_accounts:dict_get(ToKey, Dict),
    Tacc = 
        case OA of
            empty -> sub_accounts:new(To, A, CID, N);
            _ -> sub_accounts:dict_update(ToKey, Dict, A, none)
        end,
    sub_accounts:dict_write(Tacc, Dict3).
