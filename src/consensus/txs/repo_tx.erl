%similar to slasher.
%Each account needs a minimum amount of money.
%If you can provide evidence that someone doesn't have enough money left to validate, then you can get a reward for deleting their account.

-module(repo_tx).
-export([doit/3, make/4]).
-record(repo, {from = 0, nonce = 0, fee = 0, target = 0}).
make(Target, Fee, Id, Accounts) ->
    {_, A, Proof} = account:get(Id, Accounts),
    {_, _, Proof2} = account:get(Target, Accounts),
    %NB = account:now_balance(T, 0, Height),
    %true = NB < 0,
    Nonce = account:nonce(A),
    Tx = #repo{from = Id, nonce = Nonce + 1, target = Target, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    From = Tx#repo.from,
    To = Tx#repo.target,
    false = From == To,
    {_, Tacc, _} = account:get(To, Accounts),
    NB = account:now_balance(Tacc, 0, NewHeight, Trees),
    true = NB =< 0,
    Governance = trees:governance(Trees),
    DAR = governance:get_value(delete_account_reward, Governance),
    Facc = account:update(From, Trees, DAR, Tx#repo.nonce, NewHeight),
    Accounts2 = account:write(Accounts, Facc),
    Accounts3 = account:delete(To, Accounts2),
    trees:update_accounts(Trees, Accounts3).
