-module(spend_tx).
-export([doit/3, make/6]).
-record(spend, {from = 0, nonce = 0, fee = 0, to = 0, amount = 0, shares = []}).
make(To, Amount, Fee, Id, Accounts, Shares) ->
    {_, Acc, Proof} = account:get(Id, Accounts),
    {_, _Acc2, Proof2} = account:get(To, Accounts),
    Tx = #spend{from = Id, nonce = account:nonce(Acc) + 1, to = To, amount = Amount, shares = Shares, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc = account:update(From, Accounts, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight),
    Facc2 = account:send_shares(Facc, Tx#spend.shares, NewHeight),
    Tacc = account:update(To, Accounts, A, none, NewHeight),
    Tacc2 = account:receive_shares(Tacc, Tx#spend.shares, NewHeight),
    Accounts2 = account:write(Accounts, Facc2),
    NewAccounts = account:write(Accounts2, Tacc2),
    trees:update_accounts(Trees, NewAccounts).
