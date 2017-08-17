-module(channel_solo_close).
-export([doit/3, go/3, make/5, from/1, id/1]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).

from(X) -> X#csc.from.
id(X) -> 
    SPK = X#csc.scriptpubkey,
    spk:cid(testnet_sign:data(SPK)).
make(From, Fee, ScriptPubkey, ScriptSig, Trees) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    true = is_list(ScriptSig),
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    {_, Acc, Proof1} = accounts:get(From, Accounts),
    {_, _Channel, Proofc} = channels:get(CID, Channels),
    
    Tx = #csc{from = From, nonce = accounts:nonce(Acc)+1, 
	      fee = Fee,
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

doit(Tx, Trees, NewHeight) ->
    Governance = trees:governance(Trees),
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#csc.from, 
    SPK = Tx#csc.scriptpubkey,
    ScriptPubkey = testnet_sign:data(SPK),
    true = spk:time_gas(ScriptPubkey) < governance:get_value(time_gas, Governance),
    true = spk:space_gas(ScriptPubkey) < governance:get_value(space_gas, Governance),
    CID = spk:cid(testnet_sign:data(SPK)),
    {_, OldChannel, _} = channels:get(CID, Channels),
    0 = channels:amount(OldChannel),
    true = testnet_sign:verify(SPK),
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channels:entropy(OldChannel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    SS = Tx#csc.scriptsig,
    {Amount, NewCNonce, Delay} = spk:run(fast, SS, ScriptPubkey, NewHeight, 0, Trees),
    %false = Amount == 0,
    true = NewCNonce > channels:nonce(OldChannel),
    %SharesRoot = shares:root_hash(shares:write_many(Shares, 0)),
    NewChannel = channels:update(From, CID, Trees, NewCNonce, 0, 0, Amount, Delay, NewHeight, false),

    true = (-1 < (channels:bal1(NewChannel)-Amount)),
    true = (-1 < (channels:bal2(NewChannel)+Amount)),

    NewChannels = channels:write(NewChannel, Channels),
    Facc = accounts:update(From, Trees, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = accounts:write(Accounts, Facc),
    Trees2 = trees:update_channels(Trees, NewChannels),
    Trees3 = trees:update_accounts(Trees2, NewAccounts),
    spawn(fun() -> check_slash(From, Trees3, NewAccounts,NewHeight, NewCNonce) end), 
   %If our channel is closing somewhere we don't like, then we should try to use a channel_slash transaction to save our money.
    Trees3.
check_slash(From, Trees, Accounts, NewHeight, TheirNonce) ->
    case channel_manager:read(From) of
	error -> 
	    %io:fwrite("not in channel manager\n"),
	    ok;
	{ok, CD} ->
	    SPK = channel_feeder:them(CD),
	    SS = channel_feeder:script_sig_them(CD), 
	    {_, CDNonce, _} = 
		spk:run(fast, 
			SS,
			testnet_sign:data(SPK),
			NewHeight, 1, Trees),
	    %io:fwrite("\n "),
	    %io:fwrite("channel solo close "),
	    %io:fwrite(packer:pack({csc, CDNonce, TheirNonce, CD})),
	    %io:fwrite("\n "),
	    if
		CDNonce > TheirNonce ->
		    %io:fwrite("CDNONCE BIGGER\n"),
		    Governance = trees:governance(Trees),
		    GovCost = governance:get_value(cs, Governance),
		    {Tx, _} = channel_slash_tx:make(keys:pubkey(), free_constants:tx_fee() + GovCost, keys:sign(SPK), SS, Trees),
		    Stx = keys:sign(Tx),
		    %io:fwrite(packer:pack({stx, Stx})),
		    %io:fwrite("\n "),
		    tx_pool_feeder:absorb(Stx);
		true -> ok
	    end
    end.
go(Tx, Dict, NewHeight) ->
    From = Tx#csc.from, 
    SPK = Tx#csc.scriptpubkey,
    ScriptPubkey = testnet_sign:data(SPK),
    TimeGas = governance:dict_get_value(time_gas, Dict),
    SpaceGas = governance:dict_get_value(space_gas, Dict),
    true = spk:time_gas(ScriptPubkey) < TimeGas,
    true = spk:space_gas(ScriptPubkey) < SpaceGas,
    CID = spk:cid(testnet_sign:data(SPK)),
    OldChannel = channels:dict_get(CID, Dict),
    0 = channels:amount(OldChannel),
    true = testnet_sign:verify(SPK),
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channels:entropy(OldChannel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    SS = Tx#csc.scriptsig,
    {Amount, NewCNonce, Delay} = spk:dict_run(fast, SS, ScriptPubkey, NewHeight, 0, Dict),
    %false = Amount == 0,
    true = NewCNonce > channels:nonce(OldChannel),
    %SharesRoot = shares:root_hash(shares:write_many(Shares, 0)),
    NewChannel = channels:dict_update(From, CID, Dict, NewCNonce, 0, 0, Amount, Delay, NewHeight, false),

    true = (-1 < (channels:bal1(NewChannel)-Amount)),
    true = (-1 < (channels:bal2(NewChannel)+Amount)),
    Dict2 = channels:dict_write(NewChannel, Dict),
    Facc = accounts:dict_update(From, Dict, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    Dict3 = accounts:dict_write(Facc, Dict2),
    spawn(fun() -> dict_check_slash(From, Dict3, NewHeight, NewCNonce) end), 
   %If our channel is closing somewhere we don't like, then we should try to use a channel_slash transaction to save our money.
    Dict3.
dict_check_slash(From, Dict, NewHeight, TheirNonce) ->
    case channel_manager:read(From) of
	error -> 
	    ok;
	{ok, CD} ->
	    SPK = channel_feeder:them(CD),
	    SS = channel_feeder:script_sig_them(CD), 
	    {_, CDNonce, _} = 
		spk:dict_run(fast, 
			SS,
			testnet_sign:data(SPK),
			NewHeight, 1, Dict),
	    if
		CDNonce > TheirNonce ->
                    {Trees, _, _} = tx_pool:data(),
		    Governance = trees:governance(Trees),
		    GovCost = governance:get_value(cs, Governance),
		    {Tx, _} = channel_slash_tx:make(keys:pubkey(), free_constants:tx_fee() + GovCost, keys:sign(SPK), SS, Trees),
		    Stx = keys:sign(Tx),
		    tx_pool_feeder:absorb(Stx);
		true -> ok
	    end
    end.
