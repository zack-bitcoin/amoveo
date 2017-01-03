-module(channel_solo_close).
-export([doit/4, make/6]).
-record(csc, {from, nonce, fee = 0, 
	      cid, scriptpubkey, scriptsig}).

make(From, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    {_, Acc, Proof1} = account:get(From, Accounts),
    io:fwrite(packer:pack(ScriptPubkey)),
    io:fwrite("CID is "),
    io:fwrite(integer_to_list(CID)),
    io:fwrite("\n"),
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
    io:fwrite("CID is "),
    io:fwrite(integer_to_list(CID)),
    io:fwrite("\n"),
    {_, Channel, _} = channel:get(CID, Channels),
    true = testnet_sign:verify(SPK, Accounts),
    ScriptPubkey = testnet_sign:data(SPK),
    io:fwrite("channel is "),
    io:fwrite(packer:pack(Channel)),
    io:fwrite("\n"),
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
    State = chalang:new_state(0, NewHeight, Slash, <<0:(8*hash:hash_depth())>>, Accounts, Channels),
    {Amount, NewCNonce, _, _} = chalang:run(Tx#csc.scriptsig, spk:bets(ScriptPubkey), spk:time_gas(ScriptPubkey), spk:space_gas(ScriptPubkey), constants:fun_limit(), constants:var_limit(), State), 
    true = NewCNonce > channel:nonce(Channel),
    NewChannel = channel:update(CID, Channels, NewCNonce, 0, -(Amount), Amount, Mode, spk:delay(ScriptPubkey), NewHeight),
    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),

    {NewChannels, NewAccounts}.
