-module(delete_account_tx).
-export([doit/3, make/4, from/1, to/1]).
-record(da, {from = 0, nonce = 0, fee = 0, to = 0}).
from(X) -> X#da.from.
to(X) -> X#da.to.
make(To, ID, Fee, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Facc, Fproof} = accounts:get(ID, Accounts),
    {_, Tacc, Tproof} = accounts:get(To, Accounts),
    false = Tacc == empty,
    Tx = #da{from = ID, nonce = accounts:nonce(Facc) + 1,
	     to = To, fee = Fee},
    {Tx, [Fproof, Tproof]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    From = Tx#da.from,
    To = Tx#da.to,
    false = From == To,
    {_, Facc, _} = accounts:get(From, Accounts),
    A = accounts:balance(Facc),
    Amount = A-Tx#da.fee,
    true = Amount > 0,
    Governance = trees:governance(Trees),
    DAR = governance:get_value(delete_account_reward, Governance),
    Tacc = accounts:update(To, Trees, Amount+DAR, none, NewHeight),
    _ = accounts:update(From, Trees, 0, Tx#da.nonce, NewHeight),
    Accounts2 = accounts:write(Accounts, Tacc),
    NewAccounts = accounts:delete(From, Accounts2),
    trees:update_accounts(Trees, NewAccounts).

