-module(existence_tx).
-export([go/3, make/4, from/1, commit/1]).
-record(ex, {from, nonce = 0, fee = 0, commit = 0}).

from(X) -> X#ex.from.
commit(X) -> existence:hash(X#ex.commit).
make(From, Fee, C, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Nonce = accounts:nonce(Acc) + 1,
    %C = existence:new(testnet_hasher:doit(Data)),
    Tx = #ex{from = From,fee=Fee,nonce=Nonce,commit=C},
    {Tx, [Proof]}.
go(Tx, Dict, NewHeight) ->
    From = Tx#ex.from,
    C = Tx#ex.commit,
    empty = existence:dict_get(existence:hash(C),Dict),
    Dict2 = existence:dict_write(C, Dict),
    Acc = accounts:dict_update(From, Dict, -Tx#ex.fee, Tx#ex.nonce, NewHeight),
    accounts:dict_write(Acc, Dict2).

    
