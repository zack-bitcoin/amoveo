-module(channel_timeout_tx).
-export([doit/4, make/5]).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.
make(ID,Accounts,Channels,CID,Fee) ->
    {_, Acc, Proof} = account:get(ID, Accounts),
    {_, Channel, Proofc} = channel:get(CID, Channels),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Accb = case ID of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = account:get(Accb, Accounts),
    Nonce = account:nonce(Acc),
    Tx = #timeout{aid = ID, nonce = Nonce + 1,
		  fee = Fee, cid = CID},
    {Tx, [Proof, Proof2, Proofc]}.

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
    Fee = Tx#timeout.fee,
    Nonce = Tx#timeout.nonce,
    {Mode, Acc1Fee, Acc2Fee, N1, N2} = 
	case From of
	    Aid1 -> {1, Fee, 0, Nonce, none};
	    Aid2 -> {2, 0, Fee, none, Nonce}
	end,
    Acc1 = account:update(Aid1, Accounts, channel:bal1(Channel)-Acc1Fee, N1, NewHeight),
    Acc2 = account:update(Aid2, Accounts, channel:bal2(Channel)-Acc2Fee, N2, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    NewAccounts = account:write(Accounts2, Acc2),
    NewChannels = channel:delete(CID, Channels),
    {NewChannels, NewAccounts}.

