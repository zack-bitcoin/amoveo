-module(channel_slash_tx).
-export([go/4, make/5, make_dict/4, is_tx/1, from/1, id/1]).
-record(cs, {from, nonce, fee = 0, 
	     scriptpubkey, scriptsig}).
-include("../../records.hrl").
from(X) -> X#cs.from.
id(X) -> 
    SPK = X#cs.scriptpubkey,
    (testnet_sign:data(SPK))#spk.cid.
is_tx(Tx) ->
    is_record(Tx, cs).
make_dict(From, Fee, ScriptPubkey, ScriptSig) ->
    SPK = testnet_sign:data(ScriptPubkey),
    CID = SPK#spk.cid,
    T = governance,
    GTG = trees:get(T, time_gas),
    GSG = trees:get(T, space_gas),
    true = SPK#spk.time_gas < GTG,
    true = SPK#spk.time_gas < GSG,
    Acc = trees:get(accounts, From),
    Channel = trees:get(channels, CID),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    #cs{from = From, nonce = Acc#acc.nonce + 1, 
	fee = Fee, 
	scriptpubkey = ScriptPubkey, 
	scriptsig = ScriptSig}.
		  
make(From, Fee, ScriptPubkey, ScriptSig, Trees) ->
    Governance = trees:governance(Trees),
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    SPK = testnet_sign:data(ScriptPubkey),
    CID = SPK#spk.cid,
    true = SPK#spk.time_gas < governance:get_value(time_gas, Governance),
    true = SPK#spk.space_gas < governance:get_value(space_gas, Governance),
    {_, Acc, Proof1} = accounts:get(From, Accounts),
    {_, Channel, Proofc} = channels:get(CID, Channels),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    Accb = case From of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = accounts:get(Accb, Accounts),
    Tx = #cs{from = From, nonce = Acc#acc.nonce + 1, 
	      fee = Fee, 
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proof2, Proofc]}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    From = Tx#cs.from,
    SignedSPK = Tx#cs.scriptpubkey,
    SPK = testnet_sign:data(SignedSPK),
    CID = SPK#spk.cid,
    OldChannel = channels:dict_get(CID, Dict),
    0 = channels:closed(OldChannel),
    LM = channels:last_modified(OldChannel),
    true = LM < NewHeight,
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    true = spk:verify_sig(SignedSPK, Acc1, Acc2),%%
    %true = testnet_sign:verify(SignedSPK),%%
    Acc1 = SPK#spk.acc1,
    %Acc2 = SPK#spk.acc2,
    Fee = Tx#cs.fee,
    Nonce = if
		NonceCheck -> Tx#cs.nonce;
		true -> none
	    end,
    {Amount0, NewCNonce, Delay} = spk:dict_run(fast, Tx#cs.scriptsig, SPK, NewHeight, 1, Dict),
    F15 = forks:get(15),
    CB1OC = channels:bal1(OldChannel),
    CB2OC = channels:bal2(OldChannel),
    Amount = if
                 NewHeight > F15 -> min(CB1OC, max(-CB2OC, Amount0));
                 true -> Amount0
             end,
    CNOC = channels:nonce(OldChannel),
    NewChannel = channels:dict_update(CID, Dict, NewCNonce, 0, 0, Amount, Delay, NewHeight, false), 
    Dict2 = if
		(((NewCNonce > CNOC) and
		  (-1 < (CB1OC-Amount))) and
		 (-1 < (CB2OC+Amount))) ->
		    channels:dict_write(NewChannel, Dict);
		true -> Dict
	    end,
    ID = Tx#cs.from,
    Account = accounts:dict_update(ID, Dict, -Fee, Nonce),
    accounts:dict_write(Account, Dict2).
