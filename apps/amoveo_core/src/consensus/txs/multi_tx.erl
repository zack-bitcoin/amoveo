-module(multi_tx).
-export([go/4, 
	 %make/2, 
	 make_dict/2, from/1]).
-include("../../records.hrl").
from(X) -> X#multi_tx.from.
make_dict(From, Txs) ->
    Acc = trees:dict_tree_get(accounts, From),
    #multi_tx{from = From, nonce = Acc#acc.nonce + 1, txs = Txs}.
go(Tx, Dict, NewHeight, _) ->
    From = Tx#multi_tx.from,
    Txs = Tx#multi_tx.txs,
    Dict1 = sub_txs(Txs, From, Dict, NewHeight),
    Fee = Tx#multi_tx.fee,
    Facc = accounts:dict_update(From, Dict1, -Fee, Tx#multi_tx.nonce),
    Dict2 = accounts:dict_write(Facc, Dict1).
sub_txs([], From, Dict, _) -> Dict;
sub_txs([H|T], From, Dict, NewHeight) ->
    Type = element(1, H),
    false = testnet_sign:type_check(Type), %filter to make sure they aren't using a tx that needs 2 signatures.
    false = (Type == multi_tx),%don't embed multi_tx inside multi_tx
    M = txs:key2module(Type),
    H2 = setelement(2, H, From),
    Dict2 = M:go(H2, Dict , NewHeight, none),
    sub_txs(T, From, Dict2, NewHeight).
     
