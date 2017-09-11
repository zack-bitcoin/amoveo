%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx).
-export([doit/3, go/3, make/5, acc1/1, acc2/1, fee/1, amount/1,
	 sum_share_amounts/1, aid1/1, aid2/1, id/1]).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0, 
	      shares}).
aid1(X) -> X#ctc.aid1.
aid2(X) -> X#ctc.aid2.
id(X) -> X#ctc.id.
amount(Tx) -> Tx#ctc.amount.
fee(Tx) -> Tx#ctc.fee.
acc1(Tx) -> Tx#ctc.aid1.
acc2(Tx) -> Tx#ctc.aid2.
make(ID,Trees,Amount,Shares,Fee) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    {_, C, CProof} = channels:get(ID, Channels),
    A1 = channels:acc1(C),
    A2 = channels:acc2(C),
    {_, Acc1, Proof1} = accounts:get(A1, Accounts),
    {_, _, Proof2} = accounts:get(A2, Accounts),
    Nonce = accounts:nonce(Acc1),
    Tx = #ctc{id = ID, aid1 = A1, aid2 = A2, 
	     fee = Fee, nonce = Nonce+1, 
	     amount = Amount, shares = Shares},
    {Tx, [CProof, Proof1, Proof2]}.
    
doit(Tx,Trees,NewHeight) ->
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    ID = Tx#ctc.id,
    {_, OldChannel, _} = channels:get(ID, Channels),
    false = channels:closed(OldChannel),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    %ID = channels:id(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    NewChannels = channels:delete(ID, Channels),
    Bal1 = channels:bal1(OldChannel),
    Bal2 = channels:bal2(OldChannel),
    ShareAmount = sum_share_amounts(Tx#ctc.shares) div 2,
    Amount = Tx#ctc.amount,
    true = Bal1 + Bal2 >= ShareAmount * 2,
    true = Bal1 + Amount - ShareAmount > 0,
    true = Bal2 - Amount - ShareAmount > 0,
    Acc1 = accounts:update(Aid1, Trees, Bal1 + Amount - ShareAmount, Tx#ctc.nonce, NewHeight),
    %Acc1a = accounts:send_shares(Acc1, Tx#ctc.shares, NewHeight, Trees),
    Acc2 = accounts:update(Aid2, Trees, Bal2 - Amount - ShareAmount, none, NewHeight),
    %Acc2a = accounts:receive_shares(Acc2, Tx#ctc.shares, NewHeight, Trees),
    Accounts2 = accounts:write(Acc1, Accounts),
    NewAccounts = accounts:write(Acc2, Accounts2),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
    
sum_share_amounts([]) -> 0;
sum_share_amounts([H|T]) -> 
    shares:amount(H)+
	sum_share_amounts(T).
go(Tx, Dict, NewHeight) ->
    ID = Tx#ctc.id,
    OldChannel = channels:dict_get(ID, Dict),
    false = channels:closed(OldChannel),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    %ID = channels:id(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    Dict2 = channels:dict_delete(ID, Dict),
    Bal1 = channels:bal1(OldChannel),
    Bal2 = channels:bal2(OldChannel),
    ShareAmount = sum_share_amounts(Tx#ctc.shares) div 2,
    Amount = Tx#ctc.amount,
    true = Bal1 + Bal2 >= ShareAmount * 2,
    true = Bal1 + Amount - ShareAmount > 0,
    true = Bal2 - Amount - ShareAmount > 0,
    Acc1 = accounts:dict_update(Aid1, Dict, Bal1 + Amount - ShareAmount, Tx#ctc.nonce, NewHeight),
    %Acc1a = accounts:send_shares(Acc1, Tx#ctc.shares, NewHeight, Trees),
    Acc2 = accounts:dict_update(Aid2, Dict, Bal2 - Amount - ShareAmount, none, NewHeight),
    %Acc2a = accounts:receive_shares(Acc2, Tx#ctc.shares, NewHeight, Trees),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    accounts:dict_write(Acc2, Dict3).
