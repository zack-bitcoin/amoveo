-module(spend_tx).
-export([doit/4, spend/5]).
-record(spend, {from = 0, nonce = 0, to = 0, amount = 0, fee = 0}).
spend(To, Amount, Fee, Id, Accounts) ->
    {_, Acc, Proof} = trie:get(Id, Accounts, accounts),
    {_, _Acc2, Proof2} = trie:get(To, Accounts, accounts),
    Tx = #spend{from = Id, nonce = account:nonce(Acc) + 1, to = To, amount = Amount, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    {_, Facc, _} = trie:get(From, Accounts, accounts),
    {_, Tacc, _} = trie:get(To, Accounts, accounts),
    Facc2 = account:update(Facc, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight, []),
    Tacc2 = account:update(Tacc, A, none, NewHeight, []),
    Accounts2 = account:overwrite(Accounts, Facc2, From),
    NewAccounts = account:overwrite(Accounts2, Tacc2, To),
   {Channels, NewAccounts}. 
