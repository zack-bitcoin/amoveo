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
    {_, OldChannel, _} = channel:get(CID, Channels),
    Channel = channel:update(CID, Channels, none, channel:rent(OldChannel), 0,0,0, channel:delay(OldChannel), NewHeight),
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
    SS = Tx#csc.scriptsig,
    {Amount, NewCNonce} = spk:run(fast, SS, ScriptPubkey, NewHeight, Slash, Accounts, Channels),

    true = NewCNonce > channel:nonce(Channel),
    NewChannel = channel:update(CID, Channels, NewCNonce, 0, -(Amount), Amount, Mode, spk:delay(ScriptPubkey), NewHeight),
    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    <<TheirSecret/binary, _>> = SS,
    spawn(fun() -> check_slash(From, Acc1, Acc2, TheirSecret, SPK, NewCNonce) end), %If our channel is closing somewhere we don't like, then we need to use a channel_slash transaction to stop them and save our money.
    {NewChannels, NewAccounts}.

check_slash(From, Acc1, Acc2, TheirSecret, SPK, Accounts, Channels, TheirNonce) ->
    %if our partner is trying to close our channel without us, and we have a ScriptSig that can close the channel at a higher nonce, then we should make a channel_slash_tx to do that.
    %From = MyID,

    {_, Nonce, SSM} = next_ss(From, TheirSecret, Acc1, Acc2, Accounts, Channels),
    %true = Nonce > TheirNonce,
    timer:sleep(40000),%we need to wait enough time to finish loading the current block before we make this tx
    %Depending
    {Accounts,Channels,_,_} = tx_pool:data(),
    {Tx, _} = channel_slash_tx:make(MyID, free_:fee(), SPK, SSM, Accounts, Channels),
    Stx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(Stx),
    easy:sync();
next_ss(From, TheirSS2, Acc1, Acc2, Accounts, Channels) ->
    CD = channel_manager:read(From),
    SS = channel_feeder:script_sig(CD),
    <<SS2/binary, _>> = SS,
    %<<TheirSS2/binary, _>> = TheirSS,
    MyID = keys:id(),
    {From, SSF} = case MyID of
		      Acc1 -> 
			  {Acc2, <<SS2/binary, TheirSS2/binary, 3>>};
		      Acc2 -> 
			  {Acc1, <<TheirSS2/binary, SS2/binary, 3>>};
		      true -> Acc1 = Acc2
		  end,
	%maybe it is possible to combine the ScriptSigs to make a higher nonce
    {Amount1, Nonce1} = spk:run(safe, SS, ScriptPubkey, NewHeight, Slash, Accounts, Channels),
    {Amount2, Nonce2} = spk:run(safe, SSF, ScriptPubkey, NewHeight, Slash, Accounts, Channels),
    NonceM = max(Nonce1, Nonce2),
    case NonceM of
	Nonce1 -> {Amount1, Nonce1, SS};
	Nonce2 -> {Amount2, Nonce2, SS2};
	_ -> Nonce1 = NonceM
    end.
