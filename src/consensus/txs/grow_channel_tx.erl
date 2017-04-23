-module(grow_channel_tx).
-export([doit/3, make/7, good/1]).
-record(gc, {acc1 = 0, acc2 = 0, fee = 0, nonce = 0, inc1 = 0, inc2 = 0, rent = 0, channel_nonce = none, id = -1}).
good(_Tx) ->
    %make sure they aren't taking our money.
    %check that it is still meeting the min_channel_ratio.
    %check that it is a valid transaction.
    true.
    
make(ID,Accounts,Channels,Inc1,Inc2,Rent,Fee) ->
    {_, C, CProof} = channel:get(ID, Channels),
    A1 = channel:acc1(C),
    A2 = channel:acc2(C),
    {_, Acc1, Proof1} = account:get(A1, Accounts),
    {_, _, Proof2} = account:get(A2, Accounts),
    Nonce = account:nonce(Acc1),
    Tx = #gc{id = ID, acc1 = A1, acc2 = A2, 
	     fee = Fee, nonce = Nonce+1, inc1 = Inc1,
	     inc2 = Inc2, rent = Rent},
    {Tx, [CProof, Proof1, Proof2]}.
    
doit(Tx,Trees,NewHeight) ->
    %it already exists with these two accounts.
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    ID = Tx#gc.id,
    {_, OldChannel, _} = channel:get(ID, Channels),
    0 = channel:slasher(OldChannel),
    false = channel:closed(OldChannel),
    0 = channel:amount(OldChannel),
    Aid1 = channel:acc1(OldChannel),
    Aid2 = channel:acc2(OldChannel),
    ID = channel:id(OldChannel),
    Aid1 = Tx#gc.acc1,
    Aid2 = Tx#gc.acc2,
    false = Aid1 == Aid2,
    Inc1 = Tx#gc.inc1,
    Inc2 = Tx#gc.inc2,
    true = Inc1 + Inc2 >= 0,
    %Rent = Tx#gc.rent,
    CNonce = Tx#gc.channel_nonce,
    0 = channel:amount(OldChannel),
    NewChannel = channel:update(0, ID, Channels, CNonce, Inc1, Inc2, 0, channel:delay(OldChannel), NewHeight, false, []),
    NewChannels = channel:write(NewChannel, Channels),
    Acc1 = account:update(Aid1, Accounts, -Inc1, Tx#gc.nonce, NewHeight),
    Acc2 = account:update(Aid2, Accounts, -Inc2, none, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    NewAccounts = account:write(Accounts2, Acc2),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
    
    
