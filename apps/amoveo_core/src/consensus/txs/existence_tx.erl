-module(existence_tx).
-export([go/4, make/4, make_dict/3, from/1, commit/1]).
-record(ex, {from, nonce = 0, fee = 0, commit = 0}).
-include("../../records.hrl").

from(X) -> X#ex.from.
commit(X) -> X#ex.commit.
make_dict(From, Fee, Data) ->
    true = is_binary(Data),
    32 = size(Data),
    Acc = trees:get(accounts, From),
    Nonce = Acc#acc.nonce + 1,
    #ex{from = From,fee=Fee,nonce=Nonce,commit=Data}.
make(From, Fee, Data, Trees) ->
    true = is_binary(Data),
    32 = size(Data),
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Nonce = Acc#acc.nonce + 1,
    Tx = #ex{from = From,fee=Fee,nonce=Nonce,commit=Data},
    {Tx, [Proof]}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    From = Tx#ex.from,
    C = Tx#ex.commit,
    D = existence:new(C, NewHeight),
    empty = existence:dict_get(C,Dict),
    Dict2 = existence:dict_write(D, Dict),
    Nonce = if
		NonceCheck -> Tx#ex.nonce;
		true -> none
	    end,
    Acc=accounts:dict_update(From, Dict, -Tx#ex.fee,Nonce),
    accounts:dict_write(Acc, Dict2).

    
