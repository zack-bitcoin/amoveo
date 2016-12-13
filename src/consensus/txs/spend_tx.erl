-module(spend_tx).
-export([doit/4, spend/5]).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0, fee = 0}).
spend(To, Amount, Fee, Id, Accounts) ->
    {_, Acc, Proof} = account:get(Id, Accounts),
    {_, _Acc2, Proof2} = account:get(To, Accounts),
    Tx = #spend{from = Id, nonce = account:nonce(Acc) + 1, to = To, amount = Amount, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc2 = account:update(From, Accounts, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight),
    Tacc2 = account:update(To, Accounts, A, none, NewHeight),
    Accounts2 = account:write(Accounts, Facc2, From),
    NewAccounts = account:write(Accounts2, Tacc2, To),
   {Channels, NewAccounts}. 
