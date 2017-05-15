-module(channel_solo_close).
-export([doit/3, make/5, scriptpubkey/1, next_ss/6]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).

scriptpubkey(X) -> X#csc.scriptpubkey.

make(From, Fee, ScriptPubkey, ScriptSig, Trees) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    %true = is_list(ScriptSig),
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    %io:fwrite("in channel solo close make CID is "),
    %io:fwrite(integer_to_list(CID)),
    %io:fwrite("\n"),
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
    true = testnet_sign:verify(SPK, Accounts),
    Acc1 = channels:acc1(OldChannel),
    Acc2 = channels:acc2(OldChannel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channels:entropy(OldChannel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    SS = Tx#csc.scriptsig,
    {Amount, NewCNonce, Shares, Delay} = spk:run(fast, SS, ScriptPubkey, NewHeight, 0, Trees),
    %false = Amount == 0,
    true = NewCNonce > channels:nonce(OldChannel),
    %SharesRoot = shares:root_hash(shares:write_many(Shares, 0)),
    NewChannel = channels:update(From, CID, Trees, NewCNonce, 0, 0, Amount, Delay, NewHeight, false, Shares),

    true = (-1 < (channels:bal1(NewChannel)-Amount)),
    true = (-1 < (channels:bal2(NewChannel)+Amount)),

    NewChannels = channels:write(NewChannel, Channels),
    Facc = accounts:update(From, Trees, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = accounts:write(Accounts, Facc),
    Trees2 = trees:update_channels(Trees, NewChannels),
    Trees3 = trees:update_accounts(Trees2, NewAccounts),
    spawn(fun() -> check_slash(From, Acc1, Acc2, SS, SPK, Trees3, NewCNonce) end), %If our channel is closing somewhere we don't like, then we need to use a channel_slash transaction to stop them and save our money.
    Trees3.

check_slash(From, Acc1, Acc2, TheirSS, SSPK, Trees, TheirNonce) ->
    %if our partner is trying to close our channel without us, and we have a ScriptSig that can close the channel at a higher nonce, then we should make a channel_slash_tx to do that.
    %From = MyID,
    SPK = testnet_sign:data(SSPK),
    case next_ss(From, TheirSS, SPK, Acc1, Acc2, Trees) of
	ok -> ok;
	{_, Nonce, SSM, _OurSecret}  -> 
	    true = Nonce > TheirNonce,
	    timer:sleep(40000),%we need to wait enough time to finish loading the current block before we make this tx
    %Depending
	    {Trees2,_,_} = tx_pool:data(),
	    Accounts2 = trees:accounts(Trees2),
	    MyID = keys:id(),
	    {Tx, _} = channel_slash_tx:make(MyID, free_constants:tx_fee(), SSPK, [SSM], Trees2),
	    Stx = keys:sign(Tx, Accounts2),
	    tx_pool_feeder:absorb(Stx)
	    %easy:sync()
    end.
next_ss(From, TheirSS, SPK, Acc1, Acc2, Trees) ->
    %this is customized for dice.
    case channel_manager:read(From) of
	{ok, CD} ->
	    next_ss2(From, TheirSS, SPK, Acc1, Acc2, Trees, CD);
	_ -> ok
    end.
next_ss2(From, TheirSS, SPK, Acc1, Acc2, Trees, CD) ->
    OurSS1 = channel_feeder:script_sig_them(CD),%this is not a typo. this is OurSS which can be used for the highest nonced SPK they signed before this SPK we are currently looking at.
    OurSS = case OurSS1 of%this trick only works because we limit it to one bet per channel.
		[] -> channel_feeder:script_sig_me(CD);
		X -> X
	    end,
    Slash = 0,%this flag tells whether it is a channel-slash transaction, or a solo-close transaction.
    %SPK = testnet_sign:data(channel_feeder:them(CD)),
    Height = block:height(block:read(top:doit())),
    NewHeight = Height + 1,
    %State = chalang:new_state(0, Height, Slash, 0, Accounts, Channels),
    State = spk:chalang_state(Height, Slash, Trees),
    {Amount1, Nonce1, _} = spk:run(safe, OurSS, SPK, NewHeight, Slash, Trees),
    [_|[OurSecret|_]] = free_constants:vm(hd(OurSS), State),
    Out1 = {Amount1, Nonce1, OurSS, OurSecret},
    Height = block:height(block:read(top:doit())),
    case free_constants:vm(hd(TheirSS), State) of
	%This is how we extract their secret from their scriptSig.
	[_] -> Out1;
	[_|[TheirSecret|_]] ->
	    MyID = keys:id(),
	    {From, S1, S2} = 
		case MyID of
		    Acc1 -> 
			{Acc2, OurSecret, TheirSecret};
		    Acc2 -> 
			{Acc1, TheirSecret, OurSecret};
		    true -> Acc1 = Acc2
		end,
	    S1size = size(S1),
	    S2size = size(S2),
	    SSF = <<2, S1size:32, S1/binary, 2, S2size:32, S2/binary, 0, 3:32>>,
	    
	    {Amount2, Nonce2, _} = spk:run(safe, [SSF], SPK, NewHeight, Slash, Trees),
	    %io:fwrite("channel solo close nonce "),
	    %io:fwrite(integer_to_list(Nonce2)),
	    %io:fwrite("\n"),
	    NonceM = max(Nonce1, Nonce2),
	    case NonceM of
		Nonce1 -> Out1;
		Nonce2 -> {Amount2, Nonce2, SSF, OurSecret};
		_ -> Nonce1 = NonceM
	    end
    end.
