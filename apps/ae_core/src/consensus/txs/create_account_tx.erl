-module(create_account_tx).
-export([go/3, new/5, from/1, pubkey/1]).
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
go(Tx, Dict, NewHeight) ->
    Pub = Tx#create_acc_tx.pubkey,
    Amount = Tx#create_acc_tx.amount,
    From = Tx#create_acc_tx.from,
    Nonce = Tx#create_acc_tx.nonce,
    AccountFee = Tx#create_acc_tx.fee,
    empty = accounts:dict_get(Pub, Dict),
    Account = accounts:dict_update(From, Dict, -Amount - AccountFee, Nonce, NewHeight),
    NewAccount = accounts:new(Pub, Amount, NewHeight),
    Dict2 = accounts:dict_write(Account, Dict),
    accounts:dict_write(NewAccount, Dict2).
