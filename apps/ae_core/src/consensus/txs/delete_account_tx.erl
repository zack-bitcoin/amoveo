-module(delete_account_tx).
-export([doit/3, go/3, new/4, from/1, to/1]).
-record(delete_acc_tx, {from = 0,
                        nonce = 0,
                        fee = 0,
                        to = 0}).

from(X) -> X#delete_acc_tx.from.
to(X) -> X#delete_acc_tx.to.
new(To, ID, Fee, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, FromAccount, FromProof} = accounts:get(ID, Accounts),
    {_, ToAccount, ToProof} = accounts:get(To, Accounts),
    false = ToAccount == empty,
    Tx = #delete_acc_tx{from = ID,
                        nonce = accounts:nonce(FromAccount) + 1,
                        to = To,
                        fee = Fee},
    {Tx, [FromProof, ToProof]}.

doit(Tx, Trees, NewHeight) ->
    Accounts0 = trees:accounts(Trees),

    From = Tx#delete_acc_tx.from,
    To = Tx#delete_acc_tx.to,
    Nonce = Tx#delete_acc_tx.nonce,
    AccountFee = Tx#delete_acc_tx.fee,

    false = From == To,

    {_, FromAccount, _} = accounts:get(From, Accounts0),
    Balance = accounts:balance(FromAccount),
    Amount = Balance - AccountFee,
    true = Amount > 0,

    Governance = trees:governance(Trees),
    GovernanceReward = governance:get_value(delete_account_reward, Governance),

    ToAccount = accounts:update(To, Trees, Amount + GovernanceReward, none, NewHeight),
    _UpdatedAccount = accounts:update(From, Trees, 0, Nonce, NewHeight),
    Accounts = accounts:write(Accounts0, ToAccount),
    NewAccounts = accounts:delete(From, Accounts),

    trees:update_accounts(Trees, NewAccounts).
go(Tx, Trees, NewHeight) ->
    ok.
