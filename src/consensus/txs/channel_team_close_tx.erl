%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx).
-export([doit/4, make/5, acc1/1, acc2/1, fee/1, amount/1]).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0}).
amount(Tx) -> Tx#ctc.amount.
fee(Tx) -> Tx#ctc.fee.
acc1(Tx) -> Tx#ctc.aid1.
acc2(Tx) -> Tx#ctc.aid2.
make(ID,Accounts,Channels,Amount,Fee) ->
    {_, C, CProof} = channel:get(ID, Channels),
    A1 = channel:acc1(C),
    A2 = channel:acc2(C),
    {_, Acc1, Proof1} = account:get(A1, Accounts),
    {_, _, Proof2} = account:get(A2, Accounts),
    Nonce = account:nonce(Acc1),
    Tx = #ctc{id = ID, aid1 = A1, aid2 = A2, 
	     fee = Fee, nonce = Nonce+1, 
	     amount = Amount},
    {Tx, [CProof, Proof1, Proof2]}.
    
doit(Tx, Channels,Accounts,NewHeight) ->
    ID = Tx#ctc.id,
    {_, OldChannel, _} = channel:get(ID, Channels),
    Aid1 = channel:acc1(OldChannel),
    Aid2 = channel:acc2(OldChannel),
    %ID = channel:id(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    NewChannels = channel:delete(ID, Channels),
    Bal1 = channel:bal1(OldChannel),
    Bal2 = channel:bal2(OldChannel),
    Amount = Tx#ctc.amount,
    Acc1 = account:update(Aid1, Accounts, Bal1 + Amount, Tx#ctc.nonce, NewHeight),
    Acc2 = account:update(Aid2, Accounts, Bal2 - Amount, none, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    NewAccounts = account:write(Accounts2, Acc2),
    {NewChannels, NewAccounts}.
    
    
