-module(delete_account_tx).
-export([doit/4, make/4]).
-record(da, {from = 0, nonce = 0, fee = 0, to = 0}).
make(To, ID, Fee, Accounts) ->
    {_, Facc, Fproof} = account:get(ID, Accounts),
    {_, Tacc, Tproof} = account:get(To, Accounts),
    false = Tacc == empty,
    Tx = #da{from = ID, nonce = account:nonce(Facc) + 1,
	     to = To, fee = Fee},
    {Tx, [Fproof, Tproof]}.
doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#da.from,
    To = Tx#da.to,
    false = From == To,
    {_, Facc, _} = account:get(From, Accounts),
    A = account:balance(Facc),
    Amount = A-Tx#da.fee,
    true = Amount > 0,
    Tacc = account:update(To, Accounts, Amount+constants:delete_account_reward(), none, NewHeight),
    _ = account:update(From, Accounts, 0, Tx#da.nonce, NewHeight),
    Accounts2 = account:write(Accounts, Tacc),
    NewAccounts = account:delete(From, Accounts2),
    {Channels, NewAccounts}.

