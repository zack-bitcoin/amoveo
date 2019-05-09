-module(channel_timeout_tx).
-export([go/4, make/5, make_dict/3, cid/1, aid/1, spk_aid1/1, spk_aid2/1]).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0, spk_aid1, spk_aid2}).
-include("../../records.hrl").
%If your partner is not helping you, this is how you start the process of closing the channel. 
%you don't provide the channel state now, instead you use a channel_slash to provide that data.
cid(X) -> X#timeout.cid.
aid(X) -> X#timeout.aid.
spk_aid1(X) -> X#timeout.spk_aid1.
spk_aid2(X) -> X#timeout.spk_aid2.
    
   
make_dict(ID, CID, Fee) -> 
    Acc = trees:get(accounts, ID),
    Channel = trees:get(channels, CID),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    Nonce = Acc#acc.nonce,
    #timeout{aid = ID, nonce = Nonce + 1,
	     fee = Fee, cid = CID,
	     spk_aid1 = Acc1, spk_aid2 = Acc2}.
    
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
    Nonce = Acc#acc.nonce,
    Tx = #timeout{aid = ID, nonce = Nonce + 1,
		  fee = Fee, cid = CID, %shares = Shares,
                  spk_aid1 = Acc1, spk_aid2 = Acc2},
    {Tx, [Proof, Proof2, Proofc]}.

go(Tx, Dict, NewHeight, _) ->
    From = Tx#timeout.aid,
    CID = Tx#timeout.cid,
    Channel = channels:dict_get(CID, Dict),
    F12 = forks:get(12),
    F16 = forks:get(16),
    if
        ((NewHeight > 62233) and (NewHeight < F16)) ->
            1=2;%this can be deleted once fork 16 activates.
        NewHeight > F12 ->
            true = channels:nonce(Channel) > 1;
        true -> ok
    end,
    0 = channels:closed(Channel),
    LM = channels:last_modified(Channel),
    TD = NewHeight - LM,
    true = TD >= channels:delay(Channel),
    Aid1 = channels:acc1(Channel),
    Aid1 = Tx#timeout.spk_aid1,
    Aid2 = channels:acc2(Channel),
    Aid2 = Tx#timeout.spk_aid2,
    Amount = channels:amount(Channel),
    Fee = Tx#timeout.fee,
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    Dict2 = case accounts:dict_get(Aid1, Dict) of
                empty -> Dict;
                _ ->
                    Acc1 = accounts:dict_update(Aid1, Dict, Bal1-Amount, none),
                    accounts:dict_write(Acc1, Dict)
            end,
    Dict3 = case accounts:dict_get(Aid2, Dict2) of
                empty -> Dict2;
                _ ->
                    Acc2 = accounts:dict_update(Aid2, Dict2, Bal2+Amount, none),
                    accounts:dict_write(Acc2, Dict2)
            end,
    %Slasher = channels:slasher(Channel),
    Acc4 = accounts:dict_update(From, Dict3, -Fee, none),
    Dict4 = accounts:dict_write(Acc4, Dict3),
    F17 = forks:get(17),
    if 
        NewHeight > F17 -> 
            NewChannel = channels:dict_update(CID, Dict4, none, 0, 0, 0, 0, NewHeight, true),
            channels:dict_write(NewChannel, Dict4);
        true ->
            channels:dict_delete(CID, Dict4)
    end.
