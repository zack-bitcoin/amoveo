-module(create_account_tx).
-export([go/4, new/5, make_dict/4, from/1, pubkey/1]).
-include("../../records.hrl").
from(X) -> X#create_acc_tx.from.
pubkey(X) -> X#create_acc_tx.pubkey.
make_dict(Pub, Amount, Fee, From) ->
    PS = size(Pub),
    PS = size(From),
    PS = constants:pubkey_size(),
    Account = trees:get(accounts, From),
    #create_acc_tx{from = From,
		   nonce = Account#acc.nonce + 1,
		   pubkey = Pub,
		   amount = Amount,
		   fee = Fee}.
    
new(Pub, Amount, Fee, From, Trees) -> %To is a new ID. set it to any unused ID.
    PS = size(Pub),
    PS = size(From),
    PS = constants:pubkey_size(),
    Accounts = trees:accounts(Trees),
    {_, Account, Proof} = accounts:get(From, Accounts),
    Tx = #create_acc_tx{from = From,
                        nonce = Account#acc.nonce + 1,
                        pubkey = Pub,
                        amount = Amount,
                        fee = Fee},
    {Tx, [Proof]}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    From = Tx#create_acc_tx.from,
    %txs:developer_lock(From, NewHeight, Dict),
    Pub = Tx#create_acc_tx.pubkey,
    Amount = Tx#create_acc_tx.amount,
    true = (Amount > (-1)),
    Nonce = if
		NonceCheck -> Tx#create_acc_tx.nonce;
		true -> none
	    end,
    AccountFee = Tx#create_acc_tx.fee,
    empty = accounts:dict_get(Pub, Dict),
    Account = accounts:dict_update(From, Dict, -Amount - AccountFee, Nonce),
    NewAccount = accounts:new(Pub, Amount),
    Dict2 = accounts:dict_write(Account, Dict),
    accounts:dict_write(NewAccount, Dict2).
