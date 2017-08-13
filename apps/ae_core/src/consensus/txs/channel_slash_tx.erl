-module(channel_slash_tx).
-export([doit/3, make/5, is_tx/1, from/1, id/1]).
-record(cs, {from, nonce, fee = 0, 
	     scriptpubkey, scriptsig}).
from(X) -> X#cs.from.
id(X) -> 
    SPK = X#cs.scriptpubkey,
    spk:cid(testnet_sign:data(SPK)).
is_tx(Tx) ->
    is_record(Tx, cs).
make(From, Fee, ScriptPubkey, ScriptSig, Trees) ->
    Governance = trees:governance(Trees),
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    SPK = testnet_sign:data(ScriptPubkey),
    CID = spk:cid(SPK),
    true = spk:time_gas(SPK) < governance:get_value(time_gas, Governance),
    true = spk:space_gas(SPK) < governance:get_value(space_gas, Governance),
    {_, Acc, Proof1} = accounts:get(From, Accounts),
    {_, Channel, Proofc} = channels:get(CID, Channels),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    Accb = case From of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = accounts:get(Accb, Accounts),
    Tx = #cs{from = From, nonce = accounts:nonce(Acc)+1, 
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
    {_, OldChannel, _} = channels:get(CID, Channels),
    %CA = channels:amount(OldChannel),
    %false = CA == 0,
    true = testnet_sign:verify(SignedSPK),
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    true = channels:entropy(OldChannel) == spk:entropy(SPK),
    %Mode = channels:mode(OldChannel),
    Fee = Tx#cs.fee,
    Nonce = Tx#cs.nonce,
    {Amount, NewCNonce, _Shares, Delay} = spk:run(fast, Tx#cs.scriptsig, SPK, NewHeight, 1, Trees),
    %false = Amount == 0,
    true = NewCNonce > channels:nonce(OldChannel),
    %delete the channel. empty the channel into the accounts.
    %NewChannels = channels:delete(CID, Channels),
    true = (-1 < (channels:bal1(OldChannel)-Amount)),%channels can only delete money that was inside the channel.
    true = (-1 < (channels:bal2(OldChannel)+Amount)),
    
    NewChannel = channels:update(From, CID, Trees, NewCNonce, 0, 0, Amount, Delay, NewHeight, false), 
    NewChannels = channels:write(NewChannel, Channels),
    ID = Tx#cs.from,
    Account = accounts:update(ID, Trees, -Fee, Nonce, NewHeight),
    NewAccounts = accounts:write(Accounts, Account), 
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).
		      
