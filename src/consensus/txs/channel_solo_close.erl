-module(channel_solo_close).
-export([doit/3, make/6, scriptpubkey/1, next_ss/7]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).

scriptpubkey(X) -> X#csc.scriptpubkey.

make(From, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    %true = is_list(ScriptSig),
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    %io:fwrite("in channel solo close make CID is "),
    %io:fwrite(integer_to_list(CID)),
    %io:fwrite("\n"),
    {_, Acc, Proof1} = account:get(From, Accounts),
    {_, _Channel, Proofc} = channel:get(CID, Channels),
    
    Tx = #csc{from = From, nonce = account:nonce(Acc)+1, 
	      fee = Fee,
	      scriptpubkey = ScriptPubkey, 
	      scriptsig = ScriptSig},
    {Tx, [Proof1, Proofc]}.

doit(Tx, Trees, NewHeight) ->
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#csc.from, 
    SPK = Tx#csc.scriptpubkey,
    CID = spk:cid(testnet_sign:data(SPK)),
    {_, OldChannel, _} = channel:get(CID, Channels),
    0 = channel:amount(OldChannel),
    true = testnet_sign:verify(SPK, Accounts),
    ScriptPubkey = testnet_sign:data(SPK),
    Acc1 = channel:acc1(OldChannel),
    Acc2 = channel:acc2(OldChannel),
    Acc1 = spk:acc1(ScriptPubkey),
    Acc2 = spk:acc2(ScriptPubkey),
    true = channel:entropy(OldChannel) == spk:entropy(ScriptPubkey),
    %NewCNonce = spk:nonce(ScriptPubkey),
    SS = Tx#csc.scriptsig,
    {Amount, NewCNonce, Shares} = spk:run(fast, SS, ScriptPubkey, NewHeight, 0, Accounts, Channels),
    false = Amount == 0,
    SR = spk:slash_reward(ScriptPubkey),
    true = NewCNonce > channel:nonce(OldChannel),
    %SharesRoot = shares:root_hash(shares:write_many(Shares, 0)),
    NewChannel = channel:update(From, CID, Channels, NewCNonce, 0, 0, Amount, spk:delay(ScriptPubkey), NewHeight, false, Shares),

    true = (-1 < (channel:bal1(NewChannel)-SR-Amount)),
    true = (-1 < (channel:bal2(NewChannel)-SR+Amount)),

    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    spawn(fun() -> check_slash(From, Acc1, Acc2, SS, SPK, NewAccounts, NewChannels, NewCNonce) end), %If our channel is closing somewhere we don't like, then we need to use a channel_slash transaction to stop them and save our money.
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).

check_slash(From, Acc1, Acc2, TheirSS, SSPK, Accounts, Channels, TheirNonce) ->
    %if our partner is trying to close our channel without us, and we have a ScriptSig that can close the channel at a higher nonce, then we should make a channel_slash_tx to do that.
    %From = MyID,
    SPK = testnet_sign:data(SSPK),
    {_, Nonce, SSM, _OurSecret} = next_ss(From, TheirSS, SPK, Acc1, Acc2, Accounts, Channels),
    true = Nonce > TheirNonce,
    timer:sleep(40000),%we need to wait enough time to finish loading the current block before we make this tx
    %Depending
    {Accounts,Channels,_,_} = tx_pool:data(),
    MyID = keys:id(),
    {Tx, _} = channel_slash_tx:make(MyID, free_constants:tx_fee(), SSPK, [SSM], Accounts, Channels),
    Stx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(Stx),
    easy:sync().
next_ss(From, TheirSS, SPK, Acc1, Acc2, Accounts, Channels) ->
    %this is customized for dice.
    {ok, CD} = channel_manager:read(From),
    %io:fwrite("in next_ss. CD is "),
    %io:fwrite(packer:pack(CD)),
    %io:fwrite("\n"),
    OurSS1 = channel_feeder:script_sig_them(CD),%this is not a typo. this is OurSS which can be used for the highest nonced SPK they signed before this SPK we are currently looking at.
    OurSS = case OurSS1 of%this trick only works because we limit it to one bet per channel.
		[] -> channel_feeder:script_sig_me(CD);
		X -> X
	    end,
    Slash = 0,%this flag tells whether it is a channel-slash transaction, or a solo-close transaction.
    %SPK = testnet_sign:data(channel_feeder:them(CD)),
    Height = block:height(block:read(top:doit())),
    NewHeight = Height + 1,
    State = chalang:new_state(0, Height, Slash, 0, Accounts, Channels),
    {Amount1, Nonce1, _} = spk:run(safe, OurSS, SPK, NewHeight, Slash, Accounts, Channels),
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
	    
	    {Amount2, Nonce2, _} = spk:run(safe, [SSF], SPK, NewHeight, Slash, Accounts, Channels),
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
