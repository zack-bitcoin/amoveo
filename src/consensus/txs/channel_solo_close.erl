-module(channel_solo_close).
-export([doit/4, make/7]).
-record(csc, {from = 0, nonce = 0, fee = 0, cid = 0, scriptpubkey = [], scriptsig = []}).

make(From, CID, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    {_, Acc, Proof1} = account:get(From, Accounts),
    {_, _, Proofc} = channel:get(CID, Channels),
    Tx = #csc{from = From, nonce = account:nonce(Acc)+1, 
	      fee = Fee, cid = CID, 
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#csc.from, 
    CID = Tx#csc.cid,
    {_, Channel, _} = channel:get(CID, Channels),
    SPK = Tx#csc.scriptpubkey,
    true = testnet_sign:verify(SPK, Accounts),
    ScriptPubkey = testnet_sign:data(SPK),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channel:entropy(Channel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    Mode = case From of
	       Acc1 -> 1;
	       Acc2 -> 2
	   end,
    Slash = 0,%this flag tells whether it is a channel-slash transaction, or a solo-close transaction.
    State = chalang:new_state(0, NewHeight, Slash, <<0:(8*hash:hash_depth())>>, Accounts, Channels),
    {Amount, NewCNonce, _, _} = chalang:run(Tx#csc.scriptsig, spk:code(ScriptPubkey), spk:time_gas(ScriptPubkey), spk:space_gas(ScriptPubkey), constants:fun_limit(), constants:var_limit(), State), 
    true = NewCNonce > channel:nonce(Channel),
    NewChannel = channel:update(CID, Channels, NewCNonce, 0, -(Amount), Amount, Mode, spk:delay(ScriptPubkey), NewHeight),
    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),

    {NewChannels, NewAccounts}.
