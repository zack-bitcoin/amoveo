-module(new_channel_tx).
-export([doit/4, make/9, good/1, spk/2, cid/1,
	 entropy/1]).
-record(nc, {acc1 = 0, acc2 = 0, fee = 0, nonce = 0, 
	     bal1 = 0, bal2 = 0, rent = 0, entropy = 0, 
	     id = -1}).
good(_Tx) ->
    MCR = free_constants:min_channel_ratio(),
    ok.
cid(Tx) -> Tx#nc.id.
entropy(Tx) -> Tx#nc.entropy.
spk(Tx, Delay) -> spk:new(Tx#nc.acc1, Tx#nc.acc2, Tx#nc.id,
			  [], 0,0,0, Delay, 0, 
			  Tx#nc.entropy).
make(ID,Accounts,Acc1,Acc2,Inc1,Inc2,Rent,Entropy,Fee) ->
    {_, A, Proof} = account:get(Acc1, Accounts),
    Nonce = account:nonce(A),
    {_, _, Proof2} = account:get(Acc2, Accounts),
    %Entropy = channel_feeder:entropy(ID, [Acc1, Acc2])+1,
    Tx = #nc{id = ID, acc1 = Acc1, acc2 = Acc2, 
	     fee = Fee, nonce = Nonce+1, bal1 = Inc1,
	     bal2 = Inc2, entropy = Entropy, rent = Rent
	     },
    {Tx, [Proof, Proof2]}.
				 
doit(Tx, Channels, Accounts, NewHeight) ->
    ID = Tx#nc.id,
    {_, empty, _} = channel:get(ID, Channels),
    Aid1 = Tx#nc.acc1,
    Aid2 = Tx#nc.acc2,
    false = Aid1 == Aid2,
    Bal1 = Tx#nc.bal1,
    true = Bal1 >= 0,
    Bal2 = Tx#nc.bal2,
    true = Bal2 >= 0,
    Rent = Tx#nc.rent,
    Entropy = Tx#nc.entropy,
    NewChannel = channel:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Entropy, Rent),
    NewChannels = channel:write(NewChannel, Channels),
    CCFee = constants:create_channel_fee() div 2,
    Acc1 = account:update(Aid1, Accounts, -Bal1-CCFee, Tx#nc.nonce, NewHeight),
    Acc2 = account:update(Aid2, Accounts, -Bal2-CCFee, none, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    NewAccounts = account:write(Accounts2, Acc2),
    K = keys:id(),
    if
	(K == Aid1) or (K == Aid2) ->
	    channel_feeder:new_channel(Tx, Accounts);
	true -> ok
    end,
    {NewChannels, NewAccounts}.
