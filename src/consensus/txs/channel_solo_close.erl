-module(channel_solo_close).
-export([doit/4, make/6, scriptpubkey/1, next_ss/7]).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).

scriptpubkey(X) -> X#csc.scriptpubkey.

make(From, Fee, ScriptPubkey, ScriptSig, Accounts, Channels) ->
    %true = is_list(ScriptSig),
    CID = spk:cid(testnet_sign:data(ScriptPubkey)),
    {_, Acc, Proof1} = account:get(From, Accounts),
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
    {_, OldChannel, _} = channel:get(CID, Channels),
    Channel = channel:update(CID, Channels, none, channel:rent(OldChannel), 0,0,0, channel:delay(OldChannel), NewHeight),
    true = testnet_sign:verify(SPK, Accounts),
    ScriptPubkey = testnet_sign:data(SPK),
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
    SS = Tx#csc.scriptsig,
    io:fwrite("channel solo close "),
    io:fwrite(packer:pack(SS)),
    io:fwrite("\n"),
    {Amount, NewCNonce} = spk:run(fast, SS, ScriptPubkey, NewHeight, 0, Accounts, Channels),

    true = NewCNonce > channel:nonce(Channel),
    NewChannel = channel:update(CID, Channels, NewCNonce, 0, -(Amount), Amount, Mode, spk:delay(ScriptPubkey), NewHeight),
    NewChannels = channel:write(NewChannel, Channels),
    Facc = account:update(From, Accounts, -Tx#csc.fee, Tx#csc.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    %<<TheirSecret/binary, _>> = SS,
    spawn(fun() -> check_slash(From, Acc1, Acc2, SS, SPK, NewAccounts, NewChannels, NewCNonce) end), %If our channel is closing somewhere we don't like, then we need to use a channel_slash transaction to stop them and save our money.
    {NewChannels, NewAccounts}.

check_slash(From, Acc1, Acc2, TheirSS, SSPK, Accounts, Channels, TheirNonce) ->
    %if our partner is trying to close our channel without us, and we have a ScriptSig that can close the channel at a higher nonce, then we should make a channel_slash_tx to do that.
    %From = MyID,
    SPK = testnet_sign:data(SSPK),
    {_, Nonce, SSM, _OurSecret} = next_ss(From, TheirSS, SPK, Acc1, Acc2, Accounts, Channels),
    io:fwrite("their nonce is "),
    io:fwrite(integer_to_list(TheirNonce)),
    io:fwrite("\n"),
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
    {Amount1, Nonce1} = spk:run(safe, OurSS, SPK, NewHeight, Slash, Accounts, Channels),
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
	    
	    {Amount2, Nonce2} = spk:run(safe, [SSF], SPK, NewHeight, Slash, Accounts, Channels),
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
