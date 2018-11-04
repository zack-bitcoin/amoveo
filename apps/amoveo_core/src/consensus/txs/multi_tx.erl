-module(multi_tx).
-export([go/4, 
	 %make/2, 
	 make_dict/3, from/1, txs/1]).
-include("../../records.hrl").
from(X) -> X#multi_tx.from.
txs(X) -> X#multi_tx.txs.

make_dict(From, Txs, Fee) ->
    %replace from and nonce in each sub-tx with a 0.
    Acc = trees:get(accounts, From),
    Txs2 = zero_accounts_nonces(Txs),
    #multi_tx{from = From, nonce = Acc#acc.nonce + 1, txs = Txs2, fee = Fee}.
zero_accounts_nonces([]) -> [];
zero_accounts_nonces([H|T]) ->
    H2 = setelement(2, H, 0),
    H3 = setelement(3, H2, 0),
    H4 = setelement(4, H3, 0),
    [H4|zero_accounts_nonces(T)].
go(Tx, Dict, NewHeight, _) ->
    F = forks:get(4),
    true = NewHeight >= F,
    From = Tx#multi_tx.from,
    Txs = Tx#multi_tx.txs,
    true = length(Txs) > 0,
    Dict1 = sub_txs(Txs, From, Dict, NewHeight),
    Fee = Tx#multi_tx.fee,
    Facc = accounts:dict_update(From, Dict1, -Fee, Tx#multi_tx.nonce),
    accounts:dict_write(Facc, Dict1).
sub_txs([], From, Dict, _) -> Dict;
sub_txs([H|T], From, Dict, NewHeight) ->
    Type = element(1, H),
    ok = case Type of
	     spend -> ok;
	     create_acc_tx -> ok
	 end,
    0 = element(2, H),
    0 = element(3, H),
    0 = element(4, H),
    M = txs:key2module(Type),
    H2 = setelement(2, H, From),
    Dict2 = M:go(H2, Dict , NewHeight, none),
    sub_txs(T, From, Dict2, NewHeight).
     
