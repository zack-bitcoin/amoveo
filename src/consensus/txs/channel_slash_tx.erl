

-module(channel_slash_tx).
-export([doit/3, make/6]).
-record(cs, {from, nonce, fee = 0, 
	     scriptpubkey, scriptsig}).
make(From, Fee, ScriptPubkey, ScriptSig, Accounts,Channels) ->
    SPK = testnet_sign:data(ScriptPubkey),
    CID = spk:cid(SPK),
    {_, Acc, Proof1} = account:get(From, Accounts),
    {_, Channel, Proofc} = channel:get(CID, Channels),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Accb = case From of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = account:get(Accb, Accounts),
    Tx = #cs{from = From, nonce = account:nonce(Acc)+1, 
	      fee = Fee, 
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proof2, Proofc]}.

doit(Tx, Trees, NewHeight) ->
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#cs.from,
    %CID = Tx#cs.cid,
    SignedSPK = Tx#cs.scriptpubkey,
    SPK = testnet_sign:data(SignedSPK),
    CID = spk:cid(SPK),
    {_, OldChannel, _} = channel:get(CID, Channels),
    CA = channel:amount(OldChannel),
    false = CA == 0,
    %Channel = channel:update(CID, Channels, none, 0,0,channel:mode(OldChannel), channel:delay(OldChannel), NewHeight),
    true = testnet_sign:verify(SignedSPK, Accounts),
    Acc1 = channel:acc1(OldChannel),
    Acc2 = channel:acc2(OldChannel),
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    true = channel:entropy(OldChannel) == spk:entropy(SPK),
    %Mode = channel:mode(OldChannel),
    Fee = Tx#cs.fee,
    Nonce = Tx#cs.nonce,
    {Amount, NewCNonce, Shares} = spk:run(fast, Tx#cs.scriptsig, SPK, NewHeight, 1, Accounts, Channels),
    false = Amount == 0,
    true = NewCNonce > channel:nonce(OldChannel),
    %delete the channel. empty the channel into the accounts.
    %NewChannels = channel:delete(CID, Channels),
    true = (-1 < (channel:bal1(OldChannel)-Amount)),%channels can only delete money that was inside the channel.
    true = (-1 < (channel:bal2(OldChannel)+Amount)),
    NewChannel = channel:update(From, CID, Channels, NewCNonce, 0, 0, Amount, spk:delay(SPK), NewHeight, false, Shares), 
    NewChannels = channel:write(NewChannel, Channels),
    ID = Tx#cs.from,
    Account = account:update(ID, Accounts, -Fee, Nonce, NewHeight),
    NewAccounts = account:write(Accounts, Account), 
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
		      
