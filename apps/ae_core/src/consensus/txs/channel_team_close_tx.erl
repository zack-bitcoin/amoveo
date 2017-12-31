%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx).
-export([go/3, make/4, acc1/1, acc2/1, fee/1, amount/1,
         aid1/1, aid2/1, id/1]).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0}).
-include("../../records.hrl").
aid1(X) -> X#ctc.aid1.
aid2(X) -> X#ctc.aid2.
id(X) -> X#ctc.id.
amount(Tx) -> Tx#ctc.amount.
fee(Tx) -> Tx#ctc.fee.
acc1(Tx) -> Tx#ctc.aid1.
acc2(Tx) -> Tx#ctc.aid2.
make(ID,Trees,Amount,Fee) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    {_, C, CProof} = channels:get(ID, Channels),
    A1 = channels:acc1(C),
    A2 = channels:acc2(C),
    {_, Acc1, Proof1} = accounts:get(A1, Accounts),
    {_, _, Proof2} = accounts:get(A2, Accounts),
    Nonce = Acc1#acc.nonce,
    Tx = #ctc{id = ID, aid1 = A1, aid2 = A2, 
	     fee = Fee, nonce = Nonce+1, 
	     amount = Amount},
    {Tx, [CProof, Proof1, Proof2]}.
    
go(Tx, Dict, NewHeight) ->
    ID = Tx#ctc.id,
    OldChannel = channels:dict_get(ID, Dict),
    0 = channels:closed(OldChannel),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    Dict2 = channels:dict_delete(ID, Dict),
    Bal1 = channels:bal1(OldChannel),
    Bal2 = channels:bal2(OldChannel),
    Amount = Tx#ctc.amount,
    Acc1 = accounts:dict_update(Aid1, Dict, Bal1 + Amount, Tx#ctc.nonce, NewHeight),
    Acc2 = accounts:dict_update(Aid2, Dict, Bal2 - Amount, none, NewHeight),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    accounts:dict_write(Acc2, Dict3).
