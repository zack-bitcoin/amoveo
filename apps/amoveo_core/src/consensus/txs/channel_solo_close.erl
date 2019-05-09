-module(channel_solo_close).
-export([go/4, make/5, make_dict/4, from/1, id/1]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).
-include("../../records.hrl").
from(X) -> X#csc.from.
id(X) -> 
    SPK = X#csc.scriptpubkey,
    (testnet_sign:data(SPK))#spk.cid.
make_dict(From, Fee, ScriptPubkey, ScriptSig) ->
    true = is_list(ScriptSig),
    CID = (testnet_sign:data(ScriptPubkey))#spk.cid,
    <<_:256>> = CID,
    Acc = trees:get(accounts, From),
    #csc{from = From, nonce = Acc#acc.nonce+1, 
	 fee = Fee,
	 scriptpubkey = ScriptPubkey, 
	 scriptsig = ScriptSig}.
    
make(From, Fee, ScriptPubkey, ScriptSig, Trees) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    true = is_list(ScriptSig),
    CID = (testnet_sign:data(ScriptPubkey))#spk.cid,
    <<_:256>> = CID,
    {_, Acc, Proof1} = accounts:get(From, Accounts),
    {_, _Channel, Proofc} = channels:get(CID, Channels),
    
    Tx = #csc{from = From, nonce = Acc#acc.nonce+1, 
	      fee = Fee,
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    From = Tx#csc.from, 
    SPK = Tx#csc.scriptpubkey,
    ScriptPubkey = testnet_sign:data(SPK),
    TimeGas = governance:dict_get_value(time_gas, Dict),
    SpaceGas = governance:dict_get_value(space_gas, Dict),
    true = ScriptPubkey#spk.time_gas < TimeGas,
    true = ScriptPubkey#spk.space_gas < SpaceGas,
    CID = (testnet_sign:data(SPK))#spk.cid,
    OldChannel = channels:dict_get(CID, Dict),
    0 = channels:closed(OldChannel),
    0 = channels:amount(OldChannel),
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    true = spk:verify_sig(SPK, Acc1, Acc2),
    %true = testnet_sign:verify(SPK),
    Acc1 = ScriptPubkey#spk.acc1,
    %Acc2 = ScriptPubkey#spk.acc2,
    SS = Tx#csc.scriptsig,
    CB1OC = channels:bal1(OldChannel),
    CB2OC = channels:bal2(OldChannel),
    {Amount0, NewCNonce, Delay} = spk:dict_run(fast, SS, ScriptPubkey, NewHeight, 0, Dict),
    F15 = forks:get(15),
    Amount = if
                 NewHeight > F15 -> min(CB1OC, max(-CB2OC, Amount0));
                 true -> Amount0
             end,
    %false = Amount == 0,
    CNOC = channels:nonce(OldChannel),
    %io:fwrite("closing channel 0\n"),
    %io:fwrite(integer_to_list(NewCNonce)),
    %io:fwrite(" : new nonce \n"),
    %io:fwrite(integer_to_list(CNOC)),
    %io:fwrite(" : old nonce\n"),
    Dict2 = if
		NewCNonce > CNOC ->
		    %io:fwrite("closing channel 1\n"),
		    NewChannel = channels:dict_update(CID, Dict, NewCNonce, 0, 0, Amount, Delay, NewHeight, false),
		    CB1NC = channels:bal1(NewChannel),
		    CB2NC = channels:bal2(NewChannel),
                    %io:fwrite("ch1, ch2, amount (from 1 to 2)\n"),
                    %io:fwrite(packer:pack([CB1NC, CB2NC, Amount])),
                    %io:fwrite("\n"),
		    if 
			((-1 < (CB1NC-Amount)) and 
			 (-1 < (CB2NC+Amount))) ->
			    %io:fwrite("closing channel 2\n"),
			    channels:dict_write(NewChannel, Dict);
			true -> Dict
		    end;
		true -> Dict
	    end,

    Nonce = if
		NonceCheck -> Tx#csc.nonce;
		true -> none
	    end,
    Facc = accounts:dict_update(From, Dict, -Tx#csc.fee, Nonce),
    Dict3 = accounts:dict_write(Facc, Dict2),
    spawn(fun() -> dict_check_slash(From, Dict3, NewHeight, NewCNonce) end), 
   %If our channel is closing somewhere we don't like, then we should try to use a channel_slash transaction to save our money.
    Dict3.
dict_check_slash(From, Dict, NewHeight, TheirNonce) ->
    case channel_manager:read(From) of
	error -> 
	    ok;
	{ok, CD} ->
	    SPK = CD#cd.them,
	    SS = CD#cd.ssthem, 
	    {_, CDNonce, _} = 
		spk:dict_run(fast, 
			SS,
			testnet_sign:data(SPK),
			NewHeight, 1, Dict),
	    if
		CDNonce > TheirNonce ->
                    wait_block(NewHeight, SPK, SS);
		true -> ok
	    end
    end.
wait_block(X, SPK, SS) ->
    Y = api:height(),
    case Y of
        X -> slash_it(SPK, SS);
        _ -> 
            timer:sleep(500),
            wait_block(X, SPK, SS)
    end.
slash_it(SPK, SS) ->
    GovCost = trees:get(governance, cs),
    {ok, TxFee} = application:get_env(amoveo_core, tx_fee),
    Tx = channel_slash_tx:make_dict(keys:pubkey(), TxFee + GovCost, keys:sign(SPK), SS),
    Stx = keys:sign(Tx),
    tx_pool_feeder:absorb(Stx).
