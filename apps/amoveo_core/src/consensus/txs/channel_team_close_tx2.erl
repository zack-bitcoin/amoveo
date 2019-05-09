%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx2).
-export([go/4, %make/4, 
         make_dict/3, acc1/1, acc2/1, fee/1, 
         amount1/1, amount2/1,
         aid1/1, aid2/1, id/1]).
-record(ctc2, {aid1 = 0, aid2 = 0, fee = 0,
               id = 0,
               amount1 = 0, amount2 = 0,
               upper_limit, lower_limit}).
-include("../../records.hrl").
aid1(X) -> X#ctc2.aid1.
aid2(X) -> X#ctc2.aid2.
id(X) -> X#ctc2.id.
amount1(Tx) -> Tx#ctc2.amount1.
amount2(Tx) -> Tx#ctc2.amount2.
fee(Tx) -> Tx#ctc2.fee.
acc1(Tx) -> Tx#ctc2.aid1.
acc2(Tx) -> Tx#ctc2.aid2.
make_dict(ID,Amount,Fee) ->
    C = trees:get(channels, ID),
    A1 = channels:acc1(C),
    A2 = channels:acc2(C),
    B1 = channels:bal1(C),
    B2 = channels:bal2(C),
    Acc1 = trees:get(accounts, A1),
    Height = block:height(),
    #ctc2{id = ID, aid1 = A1, aid2 = A2, 
          fee = Fee, 
          amount1 = B1 + Amount, 
          amount2 = B2 - Amount,
          upper_limit = Height + 6,
          lower_limit = Height}.
    
%make(ID,Trees,Amount,Fee) ->
%    Accounts = trees:accounts(Trees),
%    Channels = trees:channels(Trees),
%    {_, C, CProof} = channels:get(ID, Channels),
%    A1 = channels:acc1(C),
%    A2 = channels:acc2(C),
%    {_, Acc1, Proof1} = accounts:get(A1, Accounts),
%    {_, _, Proof2} = accounts:get(A2, Accounts),
%    Nonce = Acc1#acc.nonce,
%    Tx = #ctc2{id = ID, aid1 = A1, aid2 = A2, 
%	     fee = Fee, nonce = Nonce+1, 
%	     amount = Amount},
%    {Tx, [CProof, Proof1, Proof2]}.
    
go(Tx, Dict, NewHeight, _) ->
    %io:fwrite("team close 0\n"),
    ID = Tx#ctc2.id,
    OldChannel = channels:dict_get(ID, Dict),
    %io:fwrite("team close 1\n"),
    0 = channels:closed(OldChannel),
    %io:fwrite("team close 2\n"),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    Aid1 = Tx#ctc2.aid1,
    Aid2 = Tx#ctc2.aid2,
    %io:fwrite("team close 3\n"),
    %Dict2 = channels:dict_delete(ID, Dict),
    F17 = forks:get(17),
    true = NewHeight > F17,
    NewChannel = channels:dict_update(ID, Dict, none, 0, 0, 0, 0, NewHeight, true),
    Dict2 = channels:dict_write(NewChannel, Dict),
    %io:fwrite("team close 4\n"),
    Bal1 = channels:bal1(OldChannel),
    Bal2 = channels:bal2(OldChannel),
    Amount1 = Tx#ctc2.amount1,
    Amount2 = Tx#ctc2.amount2,
    true = Tx#ctc2.upper_limit >= NewHeight,
    true = Tx#ctc2.lower_limit =< NewHeight,
    true = Amount1 >= 0,
    true = Amount2 >= 0,
    true = ((Amount1 + Amount2) =< (Bal1 + Bal2)),
    HF = Tx#ctc2.fee div 2,
    Acc1 = accounts:dict_update(Aid1, Dict, Amount1 - HF, none),
    %io:fwrite("team close 6\n"),
    Acc2 = accounts:dict_update(Aid2, Dict, Amount2 - HF, none),
    %io:fwrite("team close 7\n"),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    %io:fwrite("team close 8\n"),
    accounts:dict_write(Acc2, Dict3).
