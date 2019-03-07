-module(delete_account_tx).
-export([go/4, new/4, make_dict/3, from/1, to/1]).
-record(delete_acc_tx, {from = 0,
                        nonce = 0,
                        fee = 0,
                        to = 0}).
-include("../../records.hrl").

from(X) -> X#delete_acc_tx.from.
to(X) -> X#delete_acc_tx.to.
make_dict(To, ID, Fee) ->
    From = trees:get(accounts, ID),
    ToAcc = trees:get(accounts, To),
    false = ToAcc == empty,
    #delete_acc_tx{from = ID,
		   nonce = From#acc.nonce + 1,
		   to = To,
		   fee = Fee}.
    
new(To, ID, Fee, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, FromAccount, FromProof} = accounts:get(ID, Accounts),
    {_, ToAccount, ToProof} = accounts:get(To, Accounts),
    false = ToAccount == empty,
    Tx = #delete_acc_tx{from = ID,
                        nonce = FromAccount#acc.nonce + 1,
                        to = To,
                        fee = Fee},
    {Tx, [FromProof, ToProof]}.

go(Tx, Dict, NewHeight, _) ->
    From = Tx#delete_acc_tx.from,
    %txs:developer_lock(From, NewHeight, Dict),
    To = Tx#delete_acc_tx.to,
    Nonce = Tx#delete_acc_tx.nonce,
    AccountFee = Tx#delete_acc_tx.fee,
    false = From == To,
    FromAccount = accounts:dict_get(From, Dict),
    Balance = FromAccount#acc.balance,
    Amount = Balance - AccountFee,
    true = Amount > 0,
    ToAccount = accounts:dict_update(To, Dict, Amount, none),
    _UpdatedAccount = accounts:dict_update(From, Dict, 0, Nonce),
    Dict2 = accounts:dict_write(ToAccount, Dict),
    accounts:dict_delete(From, Dict2).
