-module(channel_solo_close).
-export([doit/4, make/6, scriptpubkey/1]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).

scriptpubkey(X) -> X#csc.scriptpubkey.

make(From, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    true = is_list(ScriptSig),
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    {_, Acc, Proof1} = account:get(From, Accounts),
    {_, _Channel, Proofc} = channel:get(CID, Channels),
    
    Tx = #csc{from = From, nonce = account:nonce(Acc)+1, 
	      fee = Fee,
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#csc.from, 
    SPK = Tx#csc.scriptpubkey,
    CID = spk:cid(testnet_sign:data(SPK)),
    {_, Channel, _} = channel:get(CID, Channels),
    true = testnet_sign:verify(SPK, Accounts),
    ScriptPubkey = testnet_sign:data(SPK),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channel:entropy(Channel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    0 = channel:mode(Channel),
    Mode = case From of
	       Acc1 -> 1;
	       Acc2 -> 2
	   end,
    Slash = 0,%this flag tells whether it is a channel-slash transaction, or a solo-close transaction.
    {Amount, NewCNonce} = spk:run(Tx#csc.scriptsig, ScriptPubkey, NewHeight, Slash, Accounts, Channels),

    true = NewCNonce > channel:nonce(Channel),
    NewChannel = channel:update(CID, Channels, NewCNonce, 0, -(Amount), Amount, Mode, spk:delay(ScriptPubkey), NewHeight),
    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),

    {NewChannels, NewAccounts}.
