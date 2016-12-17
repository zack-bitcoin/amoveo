-module(channel_solo_close).
-export([doit/4, make/7]).
-record(csc, {from = 0, nonce = 0, fee = 0, id = 0, scriptpubkey = [], scriptsig = []}).

make(From, CID, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    {_, Acc, Proof1} = account:get(From, Accounts),
    {_, _, Proofc} = channel:get(CID, Channels),
    Tx = #csc{from = From, nonce = account:nonce(Acc)+1, 
	      fee = Fee, id = CID, 
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

doit(Tx, Channels, Accounts, NewHeight) ->
    %check that they both signed the scriptpubkey.
    %check that the scriptpubkey has the random entropy associated to this channel. That way we know this scriptpubkey was intended for this channel.    
    %change the channels mode from 0.
    Facc = account:update(Tx#csc.from, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    NewChannels = Channels,
    {NewChannels, NewAccounts}.
