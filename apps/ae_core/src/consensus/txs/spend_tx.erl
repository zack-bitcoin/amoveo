-module(spend_tx).
-export([doit/3, go/3, make/6, from/1, to/1]).
-record(spend, {from = 0, nonce = 0, fee = 0, to = 0, amount = 0, shares = []}).
from(X) -> X#spend.from.
to(X) -> X#spend.to. 
make(To, Amount, Fee, From, Trees, _Shares) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    {_, _Acc2, Proof2} = accounts:get(To, Accounts),
    Tx = #spend{from = From, nonce = accounts:nonce(Acc) + 1, to = To, amount = Amount, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Trees, NewHeight) ->
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc = accounts:update(From, Trees, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight),
    Tacc = accounts:update(To, Trees, A, none, NewHeight),
    Accounts = trees:accounts(Trees),
    Accounts2 = accounts:write(Facc, Accounts),
    NewAccounts = accounts:write(Tacc, Accounts2),
    trees:update_accounts(Trees, NewAccounts).
go(Tx, Dict, NewHeight) ->
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc = accounts:dict_update(From, Dict, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight),
    Tacc = accounts:dict_update(To, Dict, A, none, NewHeight),
    Dict2 = accounts:dict_write(Facc, Dict),
    accounts:dict_write(Tacc, Dict2).
