%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx).
-export([go/4, make/4, make_dict/3, acc1/1, acc2/1, fee/1, amount/1,
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
make_dict(ID,Amount,Fee) ->
    C = trees:get(channels, ID),
    A1 = channels:acc1(C),
    A2 = channels:acc2(C),
    Acc1 = trees:get(accounts, A1),
    Nonce = Acc1#acc.nonce,
    #ctc{id = ID, aid1 = A1, aid2 = A2, 
	 fee = Fee, nonce = Nonce+1, 
	 amount = Amount}.
    
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
    
go(Tx, Dict, NewHeight, _) ->
    %io:fwrite("team close 0\n"),
    ID = Tx#ctc.id,
    OldChannel = channels:dict_get(ID, Dict),
    %io:fwrite("team close 1\n"),
    0 = channels:closed(OldChannel),
    %io:fwrite("team close 2\n"),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    %io:fwrite("team close 3\n"),
    %Dict2 = channels:dict_delete(ID, Dict),
    F17 = forks:get(17),
    Dict2 = if 
                NewHeight > F17 -> 
                    NewChannel = channels:dict_update(ID, Dict, none, 0, 0, 0, 0, NewHeight, true),
                    channels:dict_write(NewChannel, Dict);
                true ->
                    channels:dict_delete(ID, Dict)
            end,
    %io:fwrite("team close 4\n"),
    Bal1 = channels:bal1(OldChannel),
    Bal2 = channels:bal2(OldChannel),
    Amount = Tx#ctc.amount,
    HF = Tx#ctc.fee div 2,
    %io:fwrite("team close 5\n"),
    F16 = forks:get(16),
    if
        NewHeight > F16 ->
            true = (Bal1 + Amount) > -1,
            true = (Bal2 - Amount) > -1;
        true -> true
    end,
    Acc1 = accounts:dict_update(Aid1, Dict, Bal1 + Amount - HF, Tx#ctc.nonce),
    %io:fwrite("team close 6\n"),
    Acc2 = accounts:dict_update(Aid2, Dict, Bal2 - Amount - HF, none),
    %io:fwrite("team close 7\n"),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    %io:fwrite("team close 8\n"),
    accounts:dict_write(Acc2, Dict3).
