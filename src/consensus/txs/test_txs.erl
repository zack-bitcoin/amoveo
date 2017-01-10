-module(test_txs).
-export([test/0]).
 
test() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    S = success,
    S = test1(),
    S = test2(),
    S = test3(),
    S = test4(),
    S = test5(),
    S = test6(),
    S.
absorb(Tx) -> 
    tx_pool_feeder:absorb(Tx),
    timer:sleep(100).
test1() ->
    io:fwrite(" create_account tx\n"),
    %create account, spend, delete account
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 100000000, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {Accounts3, _, _, _} = tx_pool:data(),

    {Ctx3, _} = delete_account_tx:make(1, 2, Fee, Accounts3),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, 2, Accounts3),
    absorb(Stx3),
    {_Accounts4, _, _, Txs} = tx_pool:data(),

    Block = block:make(PH, Txs, 1),%1 is the master pub
    MBlock = block:mine(Block, 1000000000),
    block:check2(MBlock),
    success.
    
    
test2() ->
    io:fwrite(" repo tx\n"),
    %repo_tx
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,_NewPub,_NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 0, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = repo_tx:make(2, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {_Accounts3, _, _, Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 100000000),%1 is the master pub
    block:check2(Block),
    success.
    
    
   
test3() ->
    io:fwrite(" new channel tx\n"),
    %new channel, grow channel, channel team close
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    io:fwrite("account2 is "),
	   io:fwrite(integer_to_list(Accounts2)),
    io:fwrite("\n"),
    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 100, 200, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),

    {Ctx3, _} = grow_channel_tx:make(CID, Accounts3, Channels, 22, 33, 0, Fee),
    Stx3 = keys:sign(Ctx3, Accounts3),
    SStx3 = testnet_sign:sign_tx(Stx3, NewPub, NewPriv, ID2, Accounts3),
    absorb(SStx3),
    {Accounts4,Channels2,_,_} = tx_pool:data(),

    {Ctx4, _} = channel_team_close_tx:make(CID, Accounts4, Channels2, 0, Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv, ID2, Accounts4),
    absorb(SStx4),
    {_,_,_,Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 1000000000),%1 is the master pub
	   block:check2(Block),
    success.
    
test4() -> 
    %channel repo
    io:fwrite(" channel repo tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    CID = 5,
    Entropy = 432, 

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 0, 0, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),

    {Ctx4, _} = channel_repo_tx:make(1, CID, Fee, Accounts3,Channels),
    Stx4 = keys:sign(Ctx4, Accounts3),
    absorb(Stx4),
    {_,_,_,Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success.
    
test5() -> 
    %channel solo close, channel timeout
    io:fwrite("channel solo close tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 100, 200, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),
    
    Code = compiler_chalang:doit(<<"int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Code], 10000, 10000, Delay, ChannelNonce, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Accounts3, Channels), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    {Accounts4, Channels2, _, _Txs} = tx_pool:data(),

    {Ctx4, _} = channel_timeout_tx:make(1,Accounts4,Channels2,CID,Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {_, _, _, Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 100000000),%1 is the master pub
    block:check2(Block),
    success.


test6() -> 
    %channel slash
    io:fwrite("channel slash tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 100, 200, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),
    
    Code = compiler_chalang:doit(<<"int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Code], 10000, 10000, Delay, ChannelNonce, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Accounts3, Channels), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    {Accounts4, Channels2, _, _Txs} = tx_pool:data(),

    ScriptSig2 = compiler_chalang:doit(<<" int 2 ">>),
    {Ctx4, _} = channel_slash_tx:make(2,Fee,SignedScriptPubKey,[ScriptSig2],Accounts4,Channels2),
    Stx4 = testnet_sign:sign_tx(Ctx4, NewPub, NewPriv, ID2, Accounts4),
    %Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {_, _, _, Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success.
