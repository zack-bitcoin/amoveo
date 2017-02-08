-module(channel_slash_tx).
-export([doit/4, make/6]).
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

doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#cs.from,
    %CID = Tx#cs.cid,
    SignedSPK = Tx#cs.scriptpubkey,
    SPK = testnet_sign:data(SignedSPK),
    CID = spk:cid(SPK),
    {_, OldChannel, _} = channel:get(CID, Channels),
    Channel = channel:update(CID, Channels, none, channel:rent(OldChannel), 0,0,channel:mode(OldChannel), channel:delay(OldChannel), NewHeight),
    true = testnet_sign:verify(SignedSPK, Accounts),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    true = channel:entropy(Channel) == spk:entropy(SPK),
    Mode = channel:mode(Channel),
    Fee = Tx#cs.fee,
    Nonce = Tx#cs.nonce,
    {Mode, Acc1Fee, Acc2Fee, N1, N2}
	= case From of
	      Acc1 -> {2, Fee, 0, Nonce, none};
	      Acc2 -> {1, 0, Fee, none, Nonce};
	      _ -> Acc1
	  end,
    {Amount, NewCNonce} = spk:run(fast, Tx#cs.scriptsig, SPK, NewHeight, 1, Accounts, Channels),
    true = NewCNonce > channel:nonce(Channel),
    %delete the channel. empty the channel into the accounts.
    NewChannels = channel:delete(CID, Channels),
    Account1 = account:update(Acc1, Accounts, channel:bal1(Channel)-Acc1Fee-Amount, N1, NewHeight),
    Account2 = account:update(Acc2, Accounts, channel:bal2(Channel)-Acc2Fee+Amount, N2, NewHeight),
    Accounts2 = account:write(Accounts, Account1),
    NewAccounts = account:write(Accounts2, Account2),
    
   {NewChannels, NewAccounts}. 
		      
