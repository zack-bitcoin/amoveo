-module(create_account_tx).
-export([doit/3, go/3, new/5, from/1, pubkey/1]).
-record(create_acc_tx, {from = 0,
                        nonce = 0,
                        fee = 0,
                        pubkey = <<>>,
                        amount = 0}).

from(X) ->
    X#create_acc_tx.from.
pubkey(X) ->
    X#create_acc_tx.pubkey.

new(Pub, Amount, Fee, From, Trees) -> %To is a new ID. set it to any unused ID.
    PS = size(Pub),
    PS = size(From),
    PS = constants:pubkey_size(),
    Accounts = trees:accounts(Trees),
    {_, Account, Proof} = accounts:get(From, Accounts),
    Tx = #create_acc_tx{from = From,
                        nonce = accounts:nonce(Account) + 1,
                        pubkey = Pub,
                        amount = Amount,
                        fee = Fee},
    {Tx, [Proof]}.

doit(Tx, Trees, NewHeight) ->
    Accounts0 = trees:accounts(Trees),

    Pub = Tx#create_acc_tx.pubkey,
    Amount = Tx#create_acc_tx.amount,
    From = Tx#create_acc_tx.from,
    Nonce = Tx#create_acc_tx.nonce,
    AccountFee = Tx#create_acc_tx.fee,

    {_RH, empty, _Proof} = accounts:get(Pub, Accounts0),

    Governance = trees:governance(Trees),
    GovernanceFee = governance:get_value(create_account_fee, Governance),

    Account = accounts:update(From, Trees, -Amount - AccountFee - GovernanceFee, Nonce, NewHeight),

    NewAccount = accounts:new(Pub, Amount, NewHeight),
    Accounts = accounts:write(Accounts0, NewAccount),
    NewAccounts = accounts:write(Accounts, Account),

    trees:update_accounts(Trees, NewAccounts).
go(Tx, Trees, NewHeight) ->
    ok.
