-module(channel_timeout_tx).
-export([doit/4, make/5]).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.
make(ID,Accounts,Channels,CID,Fee) ->
    {_, Acc, Proof} = account:get(ID, Accounts),
    {_, _, Proofc} = channel:get(CID, Channels),
    Nonce = account:nonce(Acc),
    Tx = #timeout{aid = ID, nonce = Nonce + 1,
		  fee = Fee, cid = CID},
    {Tx, [Proof, Proofc]}.

doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#timeout.aid,
    CID = Tx#timeout.cid,
    {_, Channel, _} = channel:get(CID, Channels),
    LM = channel:last_modified(Channel),
    TD = NewHeight - LM,
    true = TD >= channel:delay(Channel),
    Mode = channel:mode(Channel),
    Aid1 = channel:acc1(Channel),
    Aid2 = channel:acc2(Channel),
    {Mode, AccB} = case From of
	       Aid1 -> {1, Aid2};
	       Aid2 -> {2, Aid1}
	   end,
    Acc1 = account:update(From, Accounts, channel:bal1(Channel)-Tx#timeout.fee, Tx#timeout.nonce, NewHeight),
    Acc2 = account:update(AccB, Accounts, channel:bal2(Channel), none, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    NewAccounts = account:write(Accounts2, Acc2),
    NewChannels = channel:delete(CID, Channels),
    {NewChannels, NewAccounts}.

