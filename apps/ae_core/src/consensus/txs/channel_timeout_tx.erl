-module(channel_timeout_tx).
-export([doit/3, make/5, cid/1, aid/1, spk_aid1/1, spk_aid2/1]).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0, shares, spk_aid1, spk_aid2}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%you don't provide the channel state now, instead you use a channel_slash to provide that data.
cid(X) -> X#timeout.cid.
aid(X) -> X#timeout.aid.
spk_aid1(X) -> X#timeout.spk_aid1.
spk_aid2(X) -> X#timeout.spk_aid2.
    
    
make(ID,Trees,CID,_Shares,Fee) ->
    %shares is a list of shares.
    %The root hash of this list must match the hash stored in the channel
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    {_, Acc, Proof} = accounts:get(ID, Accounts),
    {_, Channel, Proofc} = channels:get(CID, Channels),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    Accb = case ID of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = accounts:get(Accb, Accounts),
    Nonce = accounts:nonce(Acc),
    Tx = #timeout{aid = ID, nonce = Nonce + 1,
		  fee = Fee, cid = CID, %shares = Shares,
                  spk_aid1 = Acc1, spk_aid2 = Acc2},
    {Tx, [Proof, Proof2, Proofc]}.

doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    From = Tx#timeout.aid,
    CID = Tx#timeout.cid,
    {_, Channel, _} = channels:get(CID, Channels),
    %CS = channels:shares(Channel),
    %true = shares:root_hash(CS) 
	%== shares:root_hash(shares:write_many(Tx#timeout.shares, 0, CS)),
    %make sure that the sum of shares in Tx#timeout.shares and the amount spent by the channel sum up to less than how much money was in the channel.
    false = channels:closed(Channel),
    CA = channels:amount(Channel),
    %false = CA == 0,
    LM = channels:last_modified(Channel),
    TD = NewHeight - LM,
    %io:fwrite("delay was "),
    %io:fwrite(integer_to_list(TD)),
    %io:fwrite(" channel is "),
    %io:fwrite(packer:pack(Channel)),
    true = TD >= channels:delay(Channel),
    %Mode = channels:mode(Channel),
    Aid1 = channels:acc1(Channel),
    Aid1 = Tx#timeout.spk_aid1,
    Aid2 = channels:acc2(Channel),
    Aid2 = Tx#timeout.spk_aid2,
    Amount = channels:amount(Channel),
    Fee = Tx#timeout.fee,
    %SR = channels:slash_reward(Channel),
    %ShareAmount = channel_team_close_tx:sum_share_amounts(Tx#timeout.shares) div 2,
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    %io:fwrite("channel timeout "),
    %io:fwrite({ShareAmount, Bal1, Bal2}),
    %true = Bal1 + Bal2 >= ShareAmount * 2,
    %true = Bal1 + Amount - ShareAmount > 0,
    %true = Bal2 - Amount - ShareAmount > 0,
    Acc1 = accounts:update(Aid1, Trees, Bal1-Amount, none, NewHeight),
    %Acc1a = accounts:send_shares(Acc1, Tx#timeout.shares, NewHeight, Trees),
    Acc2 = accounts:update(Aid2, Trees, Bal2+Amount, none, NewHeight),
    %Acc2a = accounts:receive_shares(Acc2, Tx#timeout.shares, NewHeight, Trees),
    Accounts2 = accounts:write(Accounts, Acc1),
    Accounts3 = accounts:write(Accounts2, Acc2),
    Trees3 = trees:update_accounts(Trees, Accounts3),
    Slasher = channels:slasher(Channel),
    Acc4 = accounts:update(From, Trees3, -Fee, none, NewHeight),
    NewAccounts = accounts:write(Accounts3, Acc4),
    NewChannels = channels:delete(CID, Channels),%
    Trees2 = trees:update_channels(Trees3, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).

