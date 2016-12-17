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
    S.
test1() ->
    %create account, spend, delete account
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 100000000, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    tx_pool_feeder:absorb(Stx2),
    {Accounts3, _, _, _} = tx_pool:data(),

    {Ctx3, _} = delete_account_tx:make(1, 2, Fee, Accounts3),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, 2, Accounts3),
    tx_pool_feeder:absorb(Stx3),
    {_Accounts4, _, _, Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
    
test2() ->
    %repo_tx
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Accounts = block:accounts(BP),
    {NewAddr,_NewPub,_NewPriv} = testnet_sign:hard_new_key(),

    Fee = 10,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 0, Fee, 1, 2, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _, _, _} = tx_pool:data(),

    {Ctx2, _} = repo_tx:make(2, Fee, 1, Accounts2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    tx_pool_feeder:absorb(Stx2),
    {_Accounts3, _, _, Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
    
    
test3() ->
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
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 100, 200, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    tx_pool_feeder:absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),

    {Ctx3, _} = grow_channel_tx:make(CID, Accounts3, Channels, 22, 33, 0, Fee),
    Stx3 = keys:sign(Ctx3, Accounts3),
    SStx3 = testnet_sign:sign_tx(Stx3, NewPub, NewPriv, ID2, Accounts3),
    tx_pool_feeder:absorb(SStx3),
    {Accounts4,Channels2,_,_} = tx_pool:data(),

    {Ctx4, _} = channel_team_close_tx:make(CID, Accounts4, Channels2, 0, Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv, ID2, Accounts4),
    tx_pool_feeder:absorb(SStx4),
    {_,_,_,Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
test4() -> 
    %channel repo
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
    tx_pool_feeder:absorb(Stx),
    {Accounts2, _Channels, _, _} = tx_pool:data(),

    CID = 5,
    Entropy = 432, 

    {Ctx2, _} = new_channel_tx:make(CID, Accounts2, 1, ID2, 0, 0, 0, Entropy, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    tx_pool_feeder:absorb(SStx2),
    {Accounts3, Channels, _, _} = tx_pool:data(),

    {Ctx4, _} = channel_repo_tx:make(1, CID, Fee, Accounts3,Channels),
    Stx4 = keys:sign(Ctx4, Accounts3),
    tx_pool_feeder:absorb(Stx4),
    {_,_,_,Txs} = tx_pool:data(),

    {block_plus, Block, _, _} = block:make(PH, Txs, 1),%1 is the master pub
    block:check(Block),
    success.
    
    
