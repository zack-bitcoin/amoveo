%similar to slasher.
%Each account needs a minimum amount of money.
%If you can provide evidence that someone doesn't have enough money left to validate, then you can get a reward for deleting their account.

-module(repo_tx).
-export([doit/4, make/4]).
-record(repo, {from = 0, nonce = 0, fee = 0, target = 0}).
make(Target, Fee, Id, Accounts) ->
    {_, A, Proof} = account:get(Id, Accounts),
    {_, _, Proof2} = account:get(Target, Accounts),
    %NB = account:now_balance(T, 0, Height),
    %true = NB < 0,
    Nonce = account:nonce(A),
    Tx = #repo{from = Id, nonce = Nonce + 1, target = Target, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#repo.from,
    To = Tx#repo.target,
    false = From == To,
    {_, Tacc, _} = account:get(To, Accounts),
    NB = account:now_balance(Tacc, 0, NewHeight),
    true = NB =< 0,
    Facc = account:update(From, Accounts, constants:delete_account_reward(), Tx#repo.nonce, NewHeight),
    Accounts2 = account:write(Accounts, Facc),
    Accounts3 = account:delete(To, Accounts2),
    {Channels, Accounts3}.
