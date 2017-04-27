%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_team_close_tx).
-export([doit/3, make/6, acc1/1, acc2/1, fee/1, amount/1,
	 sum_share_amounts/1]).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0, 
	      shares}).
amount(Tx) -> Tx#ctc.amount.
fee(Tx) -> Tx#ctc.fee.
acc1(Tx) -> Tx#ctc.aid1.
acc2(Tx) -> Tx#ctc.aid2.
make(ID,Accounts,Channels,Amount,Shares,Fee) ->
    {_, C, CProof} = channel:get(ID, Channels),
    A1 = channel:acc1(C),
    A2 = channel:acc2(C),
    {_, Acc1, Proof1} = account:get(A1, Accounts),
    {_, _, Proof2} = account:get(A2, Accounts),
    Nonce = account:nonce(Acc1),
    Tx = #ctc{id = ID, aid1 = A1, aid2 = A2, 
	     fee = Fee, nonce = Nonce+1, 
	     amount = Amount, shares = Shares},
    {Tx, [CProof, Proof1, Proof2]}.
    
doit(Tx,Trees,NewHeight) ->
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    ID = Tx#ctc.id,
    {_, OldChannel, _} = channel:get(ID, Channels),
    false = channel:closed(OldChannel),
    Aid1 = channel:acc1(OldChannel),
    Aid2 = channel:acc2(OldChannel),
    %ID = channel:id(OldChannel),
    Aid1 = Tx#ctc.aid1,
    Aid2 = Tx#ctc.aid2,
    false = Aid1 == Aid2,
    NewChannels = channel:delete(ID, Channels),
    Bal1 = channel:bal1(OldChannel),
    Bal2 = channel:bal2(OldChannel),
    ShareAmount = sum_share_amounts(Tx#ctc.shares) div 2,
    Amount = Tx#ctc.amount,
    true = Bal1 + Bal2 >= ShareAmount * 2,
    true = Bal1 + Amount - ShareAmount > 0,
    true = Bal2 - Amount - ShareAmount > 0,
    Acc1 = account:update(Aid1, Trees, Bal1 + Amount - ShareAmount, Tx#ctc.nonce, NewHeight),
    Acc1a = account:send_shares(Acc1, Tx#ctc.shares, NewHeight, Trees),
    Acc2 = account:update(Aid2, Trees, Bal2 - Amount - ShareAmount, none, NewHeight),
    Acc2a = account:receive_shares(Acc2, Tx#ctc.shares, NewHeight, Trees),
    Accounts2 = account:write(Accounts, Acc1a),
    NewAccounts = account:write(Accounts2, Acc2a),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
    
sum_share_amounts([]) -> 0;
sum_share_amounts([H|T]) -> 
    shares:amount(H)+
	sum_share_amounts(T).
