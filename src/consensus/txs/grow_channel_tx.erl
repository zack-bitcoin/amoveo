-module(grow_channel_tx).
-export([doit/3, make/5, good/1]).
-record(gc, {acc1 = 0, acc2 = 0, fee = 0, nonce = 0, inc1 = 0, inc2 = 0, channel_nonce = none, id = -1}).
good(_Tx) ->
    %make sure they aren't taking our money.
    %check that it is still meeting the min_channel_ratio.
    %check that it is a valid transaction.
    true.
    
make(ID,Trees,Inc1,Inc2,Fee) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    {_, C, CProof} = channels:get(ID, Channels),
    A1 = channels:acc1(C),
    A2 = channels:acc2(C),
    {_, Acc1, Proof1} = accounts:get(A1, Accounts),
    {_, _, Proof2} = accounts:get(A2, Accounts),
    Nonce = accounts:nonce(Acc1),
    Tx = #gc{id = ID, acc1 = A1, acc2 = A2, 
	     fee = Fee, nonce = Nonce+1, inc1 = Inc1,
	     inc2 = Inc2},
    {Tx, [CProof, Proof1, Proof2]}.
    
doit(Tx,Trees,NewHeight) ->
    %it already exists with these two accounts.
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    ID = Tx#gc.id,
    {_, OldChannel, _} = channels:get(ID, Channels),
    0 = channels:slasher(OldChannel),
    false = channels:closed(OldChannel),
    Aid1 = channels:acc1(OldChannel),
    Aid2 = channels:acc2(OldChannel),
    ID = channels:id(OldChannel),
    Aid1 = Tx#gc.acc1,
    Aid2 = Tx#gc.acc2,
    false = Aid1 == Aid2,
    Inc1 = Tx#gc.inc1,
    Inc2 = Tx#gc.inc2,
    true = Inc1 + Inc2 >= 0,
    %Rent = Tx#gc.rent,
    CNonce = Tx#gc.channel_nonce,
    NewChannel = channels:update(0, ID, Trees, CNonce, Inc1, Inc2, 0, channels:delay(OldChannel), NewHeight, false, []),
    io:fwrite("grow channel tx new channel is "),
    io:fwrite(packer:pack(NewChannel)),
    io:fwrite("\n"),
    NewChannels = channels:write(NewChannel, Channels),
    Acc1 = accounts:update(Aid1, Trees, -Inc1, Tx#gc.nonce, NewHeight),
    Acc2 = accounts:update(Aid2, Trees, -Inc2, none, NewHeight),
    Accounts2 = accounts:write(Accounts, Acc1),
    NewAccounts = accounts:write(Accounts2, Acc2),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
    
    
