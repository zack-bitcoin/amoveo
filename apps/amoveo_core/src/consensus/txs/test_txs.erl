-module(test_txs).
-export([test/0, test/1, contracts/0, jobs/0, 
         mine_blocks/1, absorb/1, restart_chain/0, 
         test_gt/1]).

-include("../../records.hrl").

jobs() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),
    S = success,
    S = test(69),%reading and writing jobs to the verkle tree.
    S = test(70),%creating jobs, buying them, adjusting them, adjusting the salary, 
    S.

contracts() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),
    S = success,
    S = test(36),%shareable contracts, spending subcurrency.
    S = test(37),%resolving a contract into a different contract. matrix X vector simplification
    S = test(38),%simplification by matrix X matrix.
    S = test(39),%like test(38), but this time it is in a subcurrency. Also tests pushing money through the entire process.
    S = test(40),%swapping
    S = test(41),%swapping in a multi-tx
    S = test(43),%2 of 2 state channel
    S = test(44),%when someone buys a contract, they should already have an offer to sell it, if they win. so they can automatically withdraw to veo or whatever their prefered currency is when they win, even if they are offline. test(44) goes through this process.
    S = test(45),%starts as a state channel, gets converted to a binary derivative. we post the oracle on-chain and use it to enforce the outcome of the binary derivative contract, and the correct person withdraws their winnings.
    S = test(46),%flash loans inside a multi-tx.
    S = test(47),%scalar derivative with on-chain oracle enforcement.
    S = test(48),%on-chain market maker
    S = test(49),%on-chain market maker in a multi-tx.
    S = test(50),%multiple on-chain market maker that feed into each other.
    S = test(51),%market liquidity tx
    S = test(52),%multi-tx flash minting to pay the tx fee
    S = test(53),%market_swap_tx re-publish 
    %S = test(54),%market_liquitity_tx, none left to withdraw test.
    S = test(55),%swap_tx2 and trade_cancel_tx tests
    S = test(56),%swap_tx2 without partial matching.
    S = test(57),%trade_cancel_tx when the trade id doesn't yet exist.
    S = test(58),%hard update 46 test
    %S = test(59),%make a bid to buy veo
    %S = test(60),%make a bid to buy veo and the bitcoin deposit address is not provided in time.
    %S = test(61),%make a bid to buy veo and the btc is not provided in time.
    %S = test(62),%withdraw someone's money from an oracle for them.
    %S = test(63),%full contract process in one block.
    %S = test(64),

    S.
   
test_gt(N) -> 
    L = [1, 2, 11, 16, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 55, 56, 57, 58, 69, 70],
    L2 = lists:filter(fun(X) -> not(X < N) end, L),
    lists:map(fun(X) ->
                      success = test(X)
              end, L2),
    success.
    
    
test() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    S = success,
    S = test(1),%create account, spend, delete %S = test(2),%repo tx
    S = test(2),
%    S = test(3),%channel team close
%    S = test(4),%channel timeout
%    S = test(5),%account delete, channel timeout
%    S = test(6),%channel slash
%    S = test(8),%channel solo close - channel team close
%    S = test(9),%channel slash - channel team close
    %S = test(7),%existence
%    S = test(14),%financial options
%    S = test(12),%multiple bets in a single channel
%    S = test(15),%automatic channel slash
    %warning! after running test(11), we can no longer run other tests. because test(11) mines blocks, so tx_pool:dump can no longer undo transactions.
    %S = test(13),%testing governance
    S = test(11),%try out the oracle
    S = test(16),%try out the oracle further
    %S = test(17),%blocks filled with create account txs
    %S = test(28),%new channel tx2
    S = contracts(),
    S = jobs(),
    S.
absorb(Tx) -> 
    tx_pool_feeder:absorb(Tx, 1000).
block_trees(X) ->
    X#block.trees.
restart_chain() ->
    headers:dump(),
    block:initialize_chain(),
    tx_reserve:dump(),
    tx_pool:dump(),
    mine_blocks(1),
    ok.
test(1) ->
    io:fwrite(" create_account tx test \n"),
    %create account, spend, delete account
    restart_chain(),
    mine_blocks(10),
    BP = block:get_by_height(block:height()),
    PH = block:hash(BP),
    %Trees = block_trees(BP),
    %{NewPub,NewPriv} = signing:new_key(),
    {NewPub,NewPriv} = %signing:new_key(),
        {<<4,175,48,50,202,47,72,21,98,10,251,128,243,51,147,
           110,102,72,18,51,92,50,111,206,185,189,131,147,
           187,108,88,6,192,76,202,96,234,45,125,72,58,116,
           163,255,176,201,92,87,224,9,138,78,140,221,251,
           176,0,93,114,14,1,152,50,120,133>>,
         <<27,160,56,208,169,158,47,38,189,144,205,119,226,
           6,81,210,189,55,47,160,244,79,66,230,11,6,247,
           255,155,228,98,10>>},

    Fee = constants:initial_fee() + 20,
    %{Ctx, _} = create_account_tx:new(NewPub, 100000000, Fee, constants:master_pub(), Trees),
    Ctx = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    0 = many_txs(),
    absorb(Stx),
    1 = many_txs(),

    timer:sleep(500),


    %PB0 = potential_block:read(),
    %#block{trees = Trees0} = PB0,
    %Accs0 = trees:accounts(Trees0),
   
    %<<132,70,24,214,_:(8*28)>> = trees:root_hash(Trees0),
    %<<142,15,146,252,_:(8*28)>> = trie:root_hash(accounts, Accs0),
    %io:fwrite({trees:root_hash(Trees0), trie:root_hash(accounts, Accs0)}),


    Ctx2 = spend_tx:make_dict(NewPub, 10, Fee, constants:master_pub()),
    Stx2 = keys:sign(Ctx2),
    absorb(Stx2),
    2 = many_txs(),
    Ctx21 = spend_tx:make_dict(NewPub, 10, Fee, constants:master_pub()),
    Stx21 = keys:sign(Ctx21),
    absorb(Stx21),
    3 = many_txs(),
    %Ctx3 = delete_account_tx:make_dict(constants:master_pub(), NewPub, Fee),
    %Stx3 = signing:sign_tx(Ctx3, NewPub, NewPriv),
    %absorb(Stx3),
    Ctx4 = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    3 = many_txs(),
    potential_block:new(),
    potential_block:read(),
    %timer:sleep(2000),
    %1=2,
    %api:mine_block(),
    mine_blocks(1),


    F52 = forks:get(52),
    BH = block:height(),
    if
        BH < F52 ->

    %io:fwrite({accounts:all_accounts()}),
            PB0 = potential_block:read(),
            #block{trees = Trees0} = PB0,
            Accs0 = trees:accounts(Trees0),
            <<54,251,220,70,_:(28*8)>> = trie:root_hash(accounts, Accs0),
            <<152,56,71,123,_:(28*8)>> = trees:root_hash(Trees0);
        true -> ok
    end,

    success;
test(2) ->
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = -900000000,
    {Ctx, _} = create_account_tx:new(NewPub, 1, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    mine_blocks(1),
    io:fwrite(packer:pack(api:account(NewPub))),
    success;
 
    
test(4) -> 
    %channel solo close, channel timeout
    io:fwrite("channel solo close tx, channel timeout tx test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID0 = <<5:256>>,
    Delay = 0,
    
    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    mine_blocks(1),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    %io:fwrite("test_txs channel solo close\n"),
    %io:fwrite(packer:pack(Ctx3)),
    %io:fwrite("\n"),
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    %1=2,
    mine_blocks(1),
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    io:fwrite("about to absorb: "),
    io:fwrite(packer:pack(Ctx4)),
    io:fwrite("\n"),
    absorb(Stx4),
    mine_blocks(1),
    success;
test(5) -> 
    %channel solo close, channel timeout
    io:fwrite("account delete tx, channel solo close tx, channel timeout tx test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = signing:new_key(),
    
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    {Ctx, _Proof} = create_account_tx:new(NewPub, Amount, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID0 = <<5:256>>,
    Delay = 0,
    
    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    %Ctx25 = delete_account_tx:make_dict(keys:pubkey(), NewPub, Fee),
    %Stx25 = signing:sign_tx(Ctx25, NewPub, NewPriv),
    %absorb(Stx25),
    mine_blocks(1),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    mine_blocks(1),
    success;
test(unused) -> 
    %a smart contract that runs out of time or space gas. testing using an infinite loop.
% look at the result of `trees:get(channels, <<5:256>>).` to see how this changes the channel.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    {NewPub,NewPriv} = signing:new_key(),
    
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    potential_block:save(),
    mine_blocks(1),
    
    CID0 = <<5:256>>,

    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, 30, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    potential_block:new(),
    mine_blocks(1),

    %Code = compiler_chalang:doit(<<" nil : doit int4 5 swap cons dup dup recurse call ; doit call ">>),% this version runs out of space
    Code = compiler_chalang:doit(<<" nil : doit int4 5 swap cons recurse call ; doit call ">>),%this version runs out of time.
    %Code = compiler_chalang:doit(<<" drop int4 2  int4 2 int4 2 ">>), % this version does not run out of gas, for comparison.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 1000000, 1000000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    io:fwrite("\nbefore\n"),
    absorb(Stx3),
    io:fwrite("before 2\n"),
    potential_block:new(),
    mine_blocks(1),
    io:fwrite("before 3\n"),
    success;
test(6) -> 
    io:fwrite("channel slash tx test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    %potential_block:save(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    potential_block:save(),
    mine_blocks(1),
    mine_blocks(1),

    CID0 = <<5:256>>,

    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, 30, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    potential_block:new(),
    mine_blocks(1),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    potential_block:new(),
    mine_blocks(1),

    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 2 ">>), []),
    Ctx4 = channel_slash_tx:make_dict(NewPub,Fee,SignedScriptPubKey,[ScriptSig2]),
    Stx4 = signing:sign_tx(Ctx4, NewPub, NewPriv),
    absorb(Stx4),
    potential_block:new(),
    mine_blocks(1),

    ScriptSig3 = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 3 ">>), []),
    Ctx5 = channel_slash_tx:make_dict(constants:master_pub(),Fee,SignedScriptPubKey,[ScriptSig3]),
    Stx5 = keys:sign(Ctx5),
    absorb(Stx5),

    Ctx6 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx6 = keys:sign(Ctx6),
    absorb(Stx6),
    empty = trees:get(channels, <<1:256>>),
    potential_block:new(),

    mine_blocks(1),
    success;
test(8) ->
    io:fwrite(" channel solo close, and channel team close tx test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID0 = <<5:256>>,

    Delay = 10,
    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, Delay, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    mine_blocks(1),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Bet = spk:new_bet(Code, Code, 50),
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(),Fee, SignedScriptPubKey, [ScriptSig]),
    %{Ctx3, _} = grow_channel_tx:make(CID, Trees3, 22, 33, Fee),
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),

    Ctx4 = channel_team_close_tx2:make_dict(CID, 0, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = signing:sign_tx(Stx4, NewPub, NewPriv),
    absorb(SStx4),
    mine_blocks(1),
    success;
test(9) ->
    io:fwrite(" channel slash tx, and channel team close tx test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID0 = <<5:256>>,

    Delay = 10,
    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, Delay, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Bet = spk:new_bet(Code, Code, 50),
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(),Fee, SignedScriptPubKey, [ScriptSig]),
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    potential_block:new(),
    mine_blocks(1),
    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 2 ">>), []),
    Ctx35 = channel_slash_tx:make_dict(keys:pubkey(), Fee, SignedScriptPubKey, [ScriptSig2]),
    Stx35 = keys:sign(Ctx35),
    absorb(Stx35),

    Ctx4 = channel_team_close_tx2:make_dict(CID, 0, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = signing:sign_tx(Stx4, NewPub, NewPriv),
    absorb(SStx4),
    potential_block:new(),
    mine_blocks(1),
    success;

test(7) ->
    %existence tx
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    io:fwrite("existence test \n"),
    S = <<"test data">>,
    %tx_pool:dump(),
    %potential_block:new(),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    %Accounts = trees:accounts(Trees),
    Data = hash:doit(S),
    Fee = constants:initial_fee() + 20,
    Tx = existence_tx:make_dict(constants:master_pub(), Fee, Data),
    Stx = keys:sign(Tx),
    absorb(Stx),
    C = trees:get(existence, Data),
    Data = existence:hash(C),
    potential_block:new(),
    mine_blocks(1),
    success;
test(11) ->
    io:fwrite("testing an oracle \n"),
    io:fwrite("test 11 0\n"),
    %testing the oracle
    %launch an oracle with oracle_new
    Question = <<>>,
    MP = constants:master_pub(),
    %<<OID:80>> = crypto:strong_rand_bytes(10),
    %OID = crypto:strong_rand_bytes(32),
    Fee = constants:initial_fee() + 20,
    restart_chain(),
    mine_blocks(4),
    io:fwrite("test 11 1\n"),
    {Pub,Priv} = signing:new_key(),
    Amount = 1000000000,
    %%BP = block:get_by_height(block:height()),
    %Trees = block_trees(BP),
    Ctx0 = create_account_tx:make_dict(Pub, Amount, Fee, constants:master_pub()),
    Stx0 = keys:sign(Ctx0),
    0 = many_txs(),
    absorb(Stx0),
    1 = many_txs(),
    tx_reserve:dump(),
    mine_blocks(1),
    io:fwrite("test 11 2\n"),


    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, block:height() + 1, 0, 0), %Fee, question, start, id gov, govamount 
    OID = oracle_new_tx:id(Tx),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    potential_block:new(),
    mine_blocks(5),
    true = 3 == (trees:get(oracles, OID))#oracle.type,
    io:fwrite("test 11 3\n"),
    %make some bets in the oracle with oracle_bet
    %Tx20 = oracle_bet_tx:make_dict(Pub, Fee, OID, 2, 100000000), 
    Tx20 = oracle_bet_tx:make_dict(Pub, Fee, OID, 2, 50000000), 
    Stx20 = signing:sign_tx(Tx20, Pub, Priv),
    io:fwrite("try pack/unpack\n"),
    Stx20 = packer:unpack(packer:pack(Stx20)),
    io:fwrite("succeed pack/unpack\n"),
    absorb(Stx20),
    1 = many_txs(),
    io:fwrite("tx absorbed, next mining a block\n"),
    mine_blocks(1),
    io:fwrite("block mined\n"),
    true = 2 == (trees:get(oracles, OID))#oracle.type,
    io:fwrite("test 11 4\n"),

    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Bal1 = api:balance(),

    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL+1 + 100000000), %only fails in the multi-tx.


    %Stx9 = keys:sign(Tx2),
    %absorb(Stx9),
    %1 = many_txs(),
    %mine_blocks(1),

    %close the oracle with oracle_close
    Tx3 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    

    %Stx8 = keys:sign(Tx3),
    %absorb(Stx8),
    %1 = many_txs(),
    %mine_blocks(1),
    %success = this_point,
    


    Tx7 = multi_tx:make_dict(MP, [Tx2, Tx3], Fee*2),
    %Tx7 = multi_tx:make_dict(MP, [Tx2], Fee*2),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    

    %If you look up an oracle from the dictionary, it will always point to 0 for the orders. You need to query seperately to get the orders out, or you can transform the dict into a trie. After the transformation, you can look up orders how it is commented out below.

    %Trees4 = (tx_pool:get())#tx_pool.trees,
    %get your spare money out with oracle_unmatched
    %Oracles = trees:oracles(Trees4),
    %{_, Oracle2, _} = oracles:get(OID, Oracles),
    Oracle = trees:get(oracles, OID),
    %Orders = Oracle#oracle.orders,
    %{OrderID, _} = orders:head_get(Orders),%This only works because there is exactly 1 order in the order book.
    Tx4 = oracle_unmatched_tx:make_dict(constants:master_pub(), Fee, OID),

    
    %Stx8 = keys:sign(Tx4),
    %absorb(Stx8),
    %1 = many_txs(),
    %mine_blocks(1),


    %get your winnings with oracle_shares
    Tx5 = oracle_winnings_tx:make_dict(constants:master_pub(), Fee, OID),%pays 0.36


    %Stx9 = keys:sign(Tx5),
    %absorb(Stx9),
    %1 = many_txs(),
    %mine_blocks(1),
    %success = this_point,


    Tx6 = multi_tx:make_dict(MP, [Tx4, Tx5], Fee*3),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),

    Bal2 = api:balance(),
    io:fwrite("balance change"),
    io:fwrite(packer:pack([Bal1, Bal2])),
    io:fwrite("\n"),
    success;
test(16) ->
    io:fwrite("testing an oracle with more bets\n"),
    %testing the oracle
    %launch an oracle with oracle_new
    {Pub1,Priv1} = signing:new_key(),
    {Pub2,Priv2} = signing:new_key(),
    Question = <<>>,
    %OID = <<1:256>>,
    Fee = constants:initial_fee() + 20,
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    Amount = 1000000000,
    Ctx_1 = create_account_tx:make_dict(Pub1, Amount, Fee, constants:master_pub()),
    Stx_1 = keys:sign(Ctx_1),
    absorb(Stx_1),
    1 = many_txs(),
    potential_block:new(),
    
    Ctx_2 = create_account_tx:make_dict(Pub2, Amount, Fee, constants:master_pub()),
    Stx_2 = keys:sign(Ctx_2),
    absorb(Stx_2),
    2 = many_txs(),
    potential_block:new(),

    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, block:height() + 1, 0, 0),
    OID = oracle_new_tx:id(Tx),
    Stx = keys:sign(Tx),
    absorb(Stx),
    3 = many_txs(),
    %1=2,
    potential_block:new(),
    mine_blocks(1),
    mine_blocks(1),
    mine_blocks(1),
    mine_blocks(1),
    mine_blocks(1),
    %make some bets in the oracle with oracle_bet
    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL), 
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    Tx21 = oracle_bet_tx:make_dict(Pub1, Fee, OID, 1, OIL*2), 
    Stx21 = signing:sign_tx(Tx21, Pub1, Priv1),
    absorb(Stx21),
    1 = many_txs(),
    mine_blocks(1),
    Tx22 = oracle_bet_tx:make_dict(Pub2, Fee, OID, 2, OIL), 
    Stx22 = signing:sign_tx(Tx22, Pub2, Priv2),
    absorb(Stx22),
    1 = many_txs(),
    mine_blocks(1),
    %close the oracle with oracle_close
    Tx3 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    Tx41 = oracle_unmatched_tx:make_dict(Pub1, Fee, OID),
    Stx41 = signing:sign_tx(Tx41, Pub1, Priv1),
    absorb(Stx41),
    1 = many_txs(),
    mine_blocks(1),
    Tx5 = oracle_winnings_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    Tx51 = oracle_winnings_tx:make_dict(Pub1, Fee, OID),
    Stx51 = signing:sign_tx(Tx51, Pub1, Priv1),
    absorb(Stx51),
    1 = many_txs(),
    mine_blocks(1),
    Tx52 = oracle_winnings_tx:make_dict(Pub2, Fee, OID),
    Stx52 = signing:sign_tx(Tx52, Pub2, Priv2),
    absorb(Stx52),
    1 = many_txs(),

    mine_blocks(1),
    success;
test(12) ->
    io:fwrite("multiple bets in a single channel test \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    {NewPub,NewPriv} = signing:new_key(),
    
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID0 = <<5:256>>,
    Delay = 0,
    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    mine_blocks(1),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Code2 = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    Bet2 = spk:new_bet(Code2, Code2, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet, Bet2], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 2 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig, ScriptSig2]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    Height4 = (tx_pool:get())#tx_pool.height,
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    mine_blocks(1),
    success;
test(13) ->
    %testing the governance
    %launch an oracle with oracle_new, close it on state "bad", 
    io:fwrite("test governance \n"),
    Question = <<>>,
    Fee = constants:initial_fee() + 20,
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(2),
    %OID2 = <<1:256>>,
    Tx3 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), 1, 5),
    OID2 = oracle_new_tx:id(Tx3),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),

    MOT_gov = trees:get(governance, minimum_oracle_time),
    MOT = governance:value(MOT_gov),
    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    potential_block:new(),
    mine_blocks(1+MOT),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID2, 1, OIL * 3), 
    BR1_gov = trees:get(governance, block_reward),
    BR1 = governance:value(BR1_gov),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    potential_block:new(),
    mine_blocks(1+MOT),
    GovVal1 = governance:value(
                trees:get(governance, 1)),

    Tx5 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID2),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    potential_block:new(),
    mine_blocks(1),
    GovVal2 = governance:value(
                trees:get(governance, 1)),
    io:fwrite(packer:pack({GovVal2, GovVal1})),
    io:fwrite("\n"),
    true = GovVal2 > GovVal1,

    %OID3 = <<2:256>>,
    BR2 = goverance:value(
            trees:get(governance, block_reward)),
    Tx7 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), 1, 5),
    OID3 = oracle_new_tx:id(Tx7),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    potential_block:new(),
    mine_blocks(1),

    Tx8 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID3, 1, OIL * 2), 
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    potential_block:new(),
    mine_blocks(1+MOT),

    Tx9 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID3),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),

    BR3 = governance:value(
            trees:get(governance, block_reward)),
    true = BR1 < BR2,
    true = BR2 < BR3,
    mine_blocks(1),
    success;
test(14) -> 
    %options
    io:fwrite("options derivatives enforcement test\n"),
    restart_chain(),
%    headers:dump(),
%    block:initialize_chain(),
%    tx_pool:dump(),
    mine_blocks(2),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = signing:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID0 = <<5:256>>,

    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, 10, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int4 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    potential_block:new(),
    mine_blocks(1),

    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 2 ">>), []),
    Ctx4 = channel_slash_tx:make_dict(NewPub,Fee,SignedScriptPubKey,[ScriptSig2]),
    Stx4 = signing:sign_tx(Ctx4, NewPub, NewPriv),
    absorb(Stx4),

    Ctx6 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx6 = keys:sign(Ctx6),
    absorb(Stx6),
    BP2 = block:get_by_height(0),
    PH = block:hash(BP2),
    potential_block:new(),
    mine_blocks(1),
    success;

test(15) ->
    %If your partner tries closing at a low-nonced channel state, your node needs to automatically create a channel_slash to stop them.
    io:fwrite("channel slash automatic test\n"),
    restart_chain(),
%    headers:dump(),
%    block:initialize_chain(),
%    tx_pool:dump(),
    mine_blocks(1),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = signing:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    1 = many_txs(),

    CID0 = <<5:256>>,

    Ctx2 = new_channel_tx:make_dict(CID0, constants:master_pub(), NewPub, 100, 200, 10, Fee),
    CID = new_channel_tx:salted_id(Ctx2),
    Stx2 = keys:sign(Ctx2),
    SStx2 = signing:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    2 = many_txs(),
    Code = compiler_chalang:doit(<<"drop int4 50">>),
    Secret = spk:new_ss(compiler_chalang:doit(<<" int4 0 int4 2 ">>), []),
    %secrets:add(Code, Secret),
    
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    SPK = spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay),
    TheySPK = signing:sign_tx(SPK, NewPub, NewPriv),
    CD = channel_feeder:new_cd(SPK, TheySPK, [Secret], [Secret], CID, 1000),
    channel_manager:write(NewPub, CD),
    ScriptPubKey = keys:sign(SPK),
    SignedScriptPubKey = signing:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int4 5 int4 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(NewPub, Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = signing:sign_tx(Ctx3, NewPub, NewPriv),
    absorb(Stx3),
    3 = many_txs(),
    %1=2,
    potential_block:new(),
    mine_blocks(1),
    Txs2 = (tx_pool:get())#tx_pool.txs,
    %io:fwrite("~s", [packer:pack({slash_exists, Txs2})]),
    true = slash_exists(Txs2),%check that the channel_slash transaction exists in the tx_pool.
    1 = many_txs(),
    %Block = block:mine(block:make(PH, Txs2, 1), 10000000000),%1 is the master pub
    %block:check2(Block),
    success;
test(17) ->
    test({17, 1});
test({17, N}) ->
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    %fill blocks completely with create_account txs.
    create_accounts(636, N),%fits 636 at the start.
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    %mine_blocks(1),
    potential_block:new(),
    block:mine(100000),
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    success;
test(18) ->
    test18(10000);
test(19) ->
    {NewPub,_NewPriv} = signing:new_key(<<10000:256>>),
    Fee = constants:initial_fee() + 20,
    Ctx = create_account_tx:make_dict(NewPub, 1, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    potential_block:new(),
    block:mine(100000),
    block:mine(100000),
    PerBlock = 650,
    %PerBlock = 10,
    %PerBlock = 1,
    Rounds = 20,
    spend_lots(Rounds, PerBlock, PerBlock, NewPub);
test(20) ->
    {NewPub,NewPriv} = signing:new_key(<<10000:256>>),
    Fee = constants:initial_fee() + 20,
    Ctx = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    Question = <<>>,
    %OID = <<1000:256>>,
    Tx2 = oracle_new_tx:make_dict(NewPub, Fee, Question, 1 + block:height(), 0, 0),
    OID = oracle_new_tx:id(Tx2),
    Stx2 = signing:sign_tx(Tx2, NewPub, NewPriv),
    absorb(Stx2),
    potential_block:new(),
    block:mine(100000),
    success;
test(21) ->
    io:fwrite("basic multi-tx"),
    H = block:height(),
    if
        H < 12 -> mine_blocks(13 - H);
        true -> ok
    end,
    Pub = keys:pubkey(),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = 10*(constants:initial_fee() + 20),
    Tx1 = create_account_tx:make_dict(NewPub, 2, 0, Pub),
    Tx2 = spend_tx:make_dict(NewPub, 1, 0, Pub),
    Txs = [Tx1, Tx2],
    Tx = multi_tx:make_dict(Pub, Txs, Fee),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),
    success;

%set forks:get(10) to 6 for these tests.
test(24) -> test24(0);
%in test 24 the oracle closes before the fork, and that works correctly. but trying to collect oracle winnings or oracle unmatched fails.
test(25) -> test24(1);
%in test 25 the fork happens before the oracle closes.the oracle bet is before the fork, and it gets included, but the oracle cannot be closed.
test(26) -> test24(2);
%in test 26 the fork happens before the oracle bet, so the oracle bet and after does not happen, but the oracle_new does happen.
test(27) -> test24(3);
%in test 27 the fork happens before oracle new, so the oracle is entirely after the fork, so the entire oracle works.

test(22) ->
    %api bad signature failure test
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = constants:initial_fee() + 20,
    {Ctx, _} = create_account_tx:new(NewPub, 100000000, Fee, constants:master_pub(), Trees),
    Stx0 = keys:sign(Ctx),
    Ctx2 = setelement(6, Ctx, 1),
    Stx = setelement(2, Stx0, Ctx2),
    Out2 = tx_pool_feeder:absorb(Stx),
    case Out2 of
	ok -> 1=2;
	_ -> ok
    end,
    Out = ext_handler:doit({txs, [Stx]}),
    case Out of
	{ok, <<_:256>>} -> 1=2;
	_ -> ok
    end,
    test(23);
test(23) ->
    %api insufficient balance failure test
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = constants:initial_fee() + 20,
    {Ctx, _} = create_account_tx:new(NewPub, 10000000000000, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    Out2 = tx_pool_feeder:absorb(Stx),
    case Out2 of
	ok -> 1=2;
	_ -> ok
    end,
    Out = ext_handler:doit({txs, [Stx]}),
    case Out of
	{ok, <<_:256>>} -> 1=2;
	_ -> ok
    end,
    success;
test(28) ->    
    io:fwrite(" new channel tx2\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = signing:new_key(),
    Fee = constants:initial_fee() + 20,
    Amount = 5000000000,
    {Ctx, _Proof} = create_account_tx:new(NewPub, Amount, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),
    CID0 = <<5:256>>,

    Delay = 0,

    LimitOrderTime = 10,
    CID = new_channel_tx:salted_id(CID0, NewPub),
    SPK = spk:new(NewPub, 1, CID, [], 0,0,2,Delay),
    Offer = signing:sign_tx(new_channel_tx2:make_offer(CID0, NewPub, LimitOrderTime, 100, 200, Delay, 5000, SPK), NewPub, NewPriv),
    Ctx2 = new_channel_tx2:make_dict(keys:pubkey(), Offer, Fee, SPK),
    SPKSig2 = element(5, Ctx2),
    NewData = spk:hash(SPK),
    Sig = signing:sign(NewData, NewPriv),
    SSPK2 = {signed, SPK, {2, Sig}, {2, SPKSig2}},
    true = spk:verify_sig(SSPK2, NewPub, keys:pubkey()),
    SStx2 = keys:sign(Ctx2),
    absorb(SStx2),
    absorb(SStx2),
    1 = many_txs(),
    true = (1 == length(element(2, tx_pool:get()))),
    tx_pool:dump(),
    true = (0 == length(element(2, tx_pool:get()))),
    %here we are testing the ability to cancel a channel offer
    SpendCancel = spend_tx:make_dict(keys:pubkey(), 1, Fee, NewPub),
    SSpendCancel = signing:sign_tx(SpendCancel, NewPub, NewPriv),
    absorb(SSpendCancel),
    1 = many_txs(),
    %true = (1 == length(element(2, tx_pool:get()))),
    absorb(SStx2),
    1 = many_txs(),
    %true = (1 == length(element(2, tx_pool:get()))),
    tx_pool:dump(),
    absorb(SStx2),
    1 = many_txs(),
    mine_blocks(1),
    absorb(SStx2),
    0 = many_txs(),

    Ctx4 = channel_solo_close:make_dict(constants:master_pub(), Fee, SSPK2, []),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    Ctx5 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx5 = keys:sign(Ctx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    
    FC = trees:get(channels, CID),
    true = FC#channel.closed == 1,

    success;
test(35) ->
    %timer test
    Sig = keys:raw_sign(<<>>),
    Times = 1000,
    T1 = erlang:now(),
    test35(<<>>, Sig, keys:pubkey(), Times),
    T2 = erlang:now(),
    timer:now_diff(T2, T1);
test(36) ->
    io:fwrite("test 36\n"),
    %shareable contract test
    %tests creating a shareable contract, resolving it, and building a shareable contract priced in a subcurrency from the first contract. 
    %tests spending subcurrency.
    %tests binary resolution of a contract.
    restart_chain(),
%    headers:dump(),
%    block:initialize_chain(),
%    tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*100,

    %creating a shareable contract with subcurrencies.
    %Code = compiler_chalang:doit(<<"binary 32 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE= int 0 int 1" >>),
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    %Code = <<3,1,3,0,2,32,>>,
    CH = hash:doit(Code),
    Many = 3,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 10000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
   
    %spending one of the subcurrency types
    Amount2 = 2000,
    {NewPub,NewPriv} = signing:new_key(),
    Tx3 = sub_spend_tx:make_dict(NewPub, Amount2, Fee, CID, 1, MP),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),

    %using the contract in reverse. combining the different types back into the source currency.
    Tx4 = contract_use_tx:make_dict(MP, CID, -5000, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    %a potential resolution of the contract.
    Tx5 = contract_evidence_tx:make_dict(MP, Code, CID, <<>>, [], Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    %resolve the contract because the delay timer has finished.
    Tx6 = contract_timeout_tx2:make_dict(MP, CID, Fee),


    %temp test
    %Stx6 = keys:sign(Tx6),
    %absorb(Stx6),
    %1 = many_txs(),


    %withdrawing from a resolved contract
    SubAcc1 = sub_accounts:make_key(MP, CID, 3),
    Tx7 = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, [<<0:32>>,<<0:32>>,<<-1:32>>]),
   
    %temp test
    %Stx7 = keys:sign(Tx7),
    %absorb(Stx7),
    %2 = many_txs(),
    %mine_blocks(1),
    %1=2,
    
      


 
    Txs7 = [Tx6, Tx7],
    Tx71 = multi_tx:make_dict(MP, Txs7, Fee*2),
    Stx71 = keys:sign(Tx71),
    absorb(Stx71),

    1 = many_txs(),
    mine_blocks(1),

    %contract priced in a subcurrency.
    %Code2 = compiler_chalang:doit(<<"int 0 binary 32 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE= int 0 int 1" >>),
    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
int 0 [ int 0, int 4294967295]\
int 0 int 1" >>),
    CH2 = hash:doit(Code2),
    Tx8 = contract_new_tx:make_dict(MP, CH2, 2, Fee),
    Stx8 = keys:sign(Tx8),
    CID2 = contracts:make_id(CH2, 2,<<0:256>>,0),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),

    %now the child currency, built off the 3rd subtype.
    Tx9 = contract_new_tx:make_dict(MP, CH, Many, CID2, 3, Fee),
    Stx9 = keys:sign(Tx9),
    CID3 = contracts:make_id(CH, 3,CID2,3),
    absorb(Stx9),
    1 = many_txs(),
    mine_blocks(1),
    
    %potential resolutions
    Tx10 = contract_evidence_tx:make_dict(MP, Code2, CID2, <<>>, [], Fee),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    1 = many_txs(),
    %mine_blocks(1),

    Tx11 = contract_evidence_tx:make_dict(MP, Code, CID3, <<>>, [], Fee),
    Stx11 = keys:sign(Tx11),
    absorb(Stx11),
    2 = many_txs(),
    mine_blocks(1),
    
    %resolve the contract because the delay timer has finished.
    Tx12 = contract_timeout_tx2:make_dict(MP, CID2, Fee),
    Stx12 = keys:sign(Tx12),
    absorb(Stx12),
    1 = many_txs(),
    Tx13 = contract_timeout_tx2:make_dict(MP, CID3, Fee),
    Stx13 = keys:sign(Tx13),
    absorb(Stx13),
    2 = many_txs(),
    mine_blocks(1),

    success;

test(37) ->
    io:fwrite("test 37\n"),
    %tests resolving a contract into a different contract.
    %tests simplification by matrix X vector.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*100,
    %Max = round(math:pow(2, 32)) - 1, 4294967295
    %HalfMax = Max div 2, 2147483647
    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 2147483648 ,\
  int 2147483647 ]\
  int 0 int 1" >>),%splits the money 50-50
    CH2 = hash:doit(Code2),

    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 0 , int 4294967295 ] ,\
  [ int 4294967295 , int 0 ] ,\
  [ int 0 , int 0 ] ]\
binary 32 ",  
               (base64:encode(CH2))/binary, 
               " int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3, 
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 10000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
    
    %potential resolution of first contract
    Tx3 = contract_evidence_tx:make_dict(MP, Code, CID, <<>>, [], Fee),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),

    %timeout first contract.
    %we need to rebuild the merkle tree so that we can make the proofs we need.
    Type = 1,
    Full = <<4294967295:32>>,
    Empty = <<0:32>>,
    Matrix = %same matrix from inside the forth code.
        [[Empty, Full],
         [Full, Empty],
         [Empty, Empty]],
    CID2 = contracts:make_id(CH2, 2,<<0:256>>,0),

    Proofs = contract_evidence_tx:make_proof1(Matrix),

    Tx4 = contract_timeout_tx2:make_dict(MP, CID, Fee, Proofs, CH2, [Empty, Full]),
    io:fwrite("tx 4\n"),
    io:fwrite(packer:pack(Tx4)),
    io:fwrite("\n"),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),


    %withdraw one kind of winnings from first contract into the second
    SubAcc1 = sub_accounts:make_key(MP, CID, Type),
    Tx7 = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, [Empty, Full], Proofs),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),


    %potential resolution of second contract
    %CID2 = contracts:make_id(CH2, 2,<<0:256>>,0),
    Tx8 = contract_evidence_tx:make_dict(MP, Code2, CID2, <<>>, [], Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    


    %timeout second
    Tx9 = contract_timeout_tx2:make_dict(MP, CID2, Fee),



    %withdraw to veo
    PayoutVector = %same as payout vector defined in Forth.
        [<<2147483648:32>>, 
         <<2147483647:32>>],
    SubAcc1_2 = sub_accounts:make_key(MP, CID2, 2),
    Tx10 = contract_winnings_tx:make_dict(MP, SubAcc1_2, CID2, Fee, PayoutVector),

    %simplify by matrix multiplication
    %so that both contracts use a payout vector, allowing anyone holding any of the subcurrencies to withdraw directly to veo.
    Tx11 = contract_simplify_tx:make_dict(MP, CID, CID2, 0, Matrix, PayoutVector, Fee), 

    %withdraw the second kind of subcurrency directly to veo.
    PayoutVector2 = contract_simplify_tx:apply_matrix2vector(Matrix, PayoutVector),
    SubAcc2 = sub_accounts:make_key(MP, CID, 2),
    Tx12 = contract_winnings_tx:make_dict(MP, SubAcc2, CID, Fee, PayoutVector2),

    Txs13 = [Tx9, Tx10, Tx11, Tx12],
    Tx13 = multi_tx:make_dict(MP, Txs13, Fee),
    Stx13 = keys:sign(Tx13),
    absorb(Stx13),
    1 = many_txs(),
    mine_blocks(1),
    
    success;
test(38) ->
    io:fwrite("test 38\n"),
    %tests simplification by matrix X matrix
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*100,
    Code3 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 2147483648 ,\
  int 2147483647 ]\
  int 0 int 1" >>),%splits the money 50-50
    CH3 = hash:doit(Code3),
    Half0 = <<2147483647:32>>,
    Half1 = <<2147483648:32>>,
    Zero = <<0:32>>,
    Full = <<4294967295:32>>,
    PayoutVector = [Half1, Half0],

    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 0 , int 0 ] ,\
  [ int 2147483647 , int 0 ] ,\
  [ int 2147483648 , int 4294967295 ] ]\
binary 32 ",
(base64:encode(CH3))/binary,
"  int 0 int 1" >>),
    CH2 = hash:doit(Code2),
    Matrix2 = [[Zero, Zero],
               [Half0, Zero],
               [Half1, Full]],
    

    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 2147483648 , int 0 , int 0 ] ,\
  [ int 2147483647 , int 2147483647 , int 0 ] ,\
  [ int 0 , int 2147483648 , int 4294967295 ] ]\
binary 32 ",
(base64:encode(CH2))/binary,
"  int 0 int 1" >>),
    CH = hash:doit(Code),
    Matrix = [[Half1, Zero, Zero],
              [Half0, Half0, Zero],
              [Zero, Half1, Full]],

    Tx1 = contract_new_tx:make_dict(MP, CH, 3, Fee),
    CID = contracts:make_id(CH, 3,<<0:256>>,0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    
    Tx2 = contract_new_tx:make_dict(MP, CH2, 3, Fee),
    CID2 = contracts:make_id(CH2, 3,<<0:256>>,0),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
    
    Tx3 = contract_new_tx:make_dict(MP, CH3, 2, Fee),
    CID3 = contracts:make_id(CH3, 2,<<0:256>>,0),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    
    %settled contract 1 and 2.
    Tx4 = contract_evidence_tx:make_dict(MP, Code, CID, <<>>, [], Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx5 = contract_evidence_tx:make_dict(MP, Code2, CID2, <<>>, [], Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Proof = contract_evidence_tx:make_proof1(Matrix),
    Tx6 = contract_timeout_tx2:make_dict(MP, CID, Fee, Proof, CH2, lists:nth(1, Matrix)),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),

    Proof2 = contract_evidence_tx:make_proof1(Matrix2),
    Tx7 = contract_timeout_tx2:make_dict(MP, CID2, Fee, Proof2, CH3, lists:nth(1, Matrix2)),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),

    %do the simplification from 1 to 2.
    Tx8 = contract_simplify_tx:make_dict(MP, CID, CID2, CID3, Matrix, Matrix2, Fee), 
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),
   
    %settle contract 3
    Tx9 = contract_evidence_tx:make_dict(MP, Code3, CID3, <<>>, [], Fee),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx10 = contract_timeout_tx2:make_dict(MP, CID3, Fee),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    1 = many_txs(),
    mine_blocks(1),

    %do the simplification from 1 to 3
    Matrix3 = contract_simplify_tx:apply_matrix2matrix(Matrix, Matrix2),
    Tx11 = contract_simplify_tx:make_dict(MP, CID, CID3, 0, Matrix3, PayoutVector, Fee), 
    Stx11 = keys:sign(Tx11),
    absorb(Stx11),
    1 = many_txs(),
    mine_blocks(1),
  
    %do the simplification from 2 to 3
    Tx12 = contract_simplify_tx:make_dict(MP, CID2, CID3, 0, Matrix2, PayoutVector, Fee), 
    Stx12 = keys:sign(Tx12),
    absorb(Stx12),
    1 = many_txs(),
    mine_blocks(1),
    

    success;
test(39) ->
    io:fwrite("test 39\n"),
    %tests simplification by matrix X matrix, but in a subcurrency.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*100,
    Code0 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 3294967295 ,\
  int 1000000000 ]\
 int 0 int 1
">>),%give all the money to #1
    CH0 = hash:doit(Code0),
    Half0 = <<2147483647:32>>,
    Half1 = <<2147483648:32>>,
    Zero = <<0:32>>,
    Full = <<4294967295:32>>,
    PayoutVector0 = [<<3294967295:32>>, 
                     <<1000000000:32>>],
    

    %creating the layer-1 subcurrency
    Tx0 = contract_new_tx:make_dict(MP, CH0, 2, Fee),
    CID0 = contracts:make_id(CH0, 2,<<0:256>>,0),
    Stx0 = keys:sign(Tx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    %buying layer-1 subcurrency
    Amount01 = 100000000,%1 veo
    Tx01 = contract_use_tx:make_dict(MP, CID0, Amount01, Fee),
    Stx01 = keys:sign(Tx01),
    absorb(Stx01),
    1 = many_txs(),
    mine_blocks(1),
    %I now have 1 veo of type 1 subcurrency, and 1 veo of type 2 subcurrency. All the contracts in this example will be using my type 1 subcurrency, and I will withdraw the type 2 directly to veo at the end.


    Code3 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 2147483648 ,\
  int 2147483647 ]\
  int 0 int 1" >>),%splits the money 50-50
    CH3 = hash:doit(Code3),
    PayoutVector = [Half1, Half0],

    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 0 , int 0 ] ,\
  [ int 2147483647 , int 0 ] ,\
  [ int 2147483648 , int 4294967295 ] ]\
binary 32 ",
(base64:encode(CH3))/binary,
"  int 0 int 1" >>),
    CH2 = hash:doit(Code2),
    Matrix2 = [[Zero, Zero],
               [Half0, Zero],
               [Half1, Full]],
    

    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 2147483648 , int 0 , int 0 ] ,\
  [ int 2147483647 , int 2147483647 , int 0 ] ,\
  [ int 0 , int 2147483648 , int 4294967295 ] ]\
binary 32 ",
(base64:encode(CH2))/binary,
"  int 0 int 1" >>),
    CH = hash:doit(Code),
    Matrix = [[Half1, Zero, Zero],
              [Half0, Half0, Zero],
              [Zero, Half1, Full]],

    %creating the first layer-2 contract
    Tx1 = contract_new_tx:make_dict(MP, CH, 3, CID0, 1, Fee),
    CID = contracts:make_id(CH, 3,CID0,1),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %using some of my type 1 subcurrency from the layer-1 contract, I am able to participate in the first layer-2 contract

    Tx1_1 = contract_use_tx:make_dict(MP, CID, Amount01, Fee),
    Stx1_1 = keys:sign(Tx1_1),
    absorb(Stx1_1),
    1 = many_txs(),
    mine_blocks(1),
    %now I own 1 veo of each of the 3 types defined by the first layer 2 contract.
    %I also still own 1 veo of layer-1 type 2.
   
    %creating the second layer-2 contract
    Tx2 = contract_new_tx:make_dict(MP, CH2, 3, CID0, 1, Fee),
    CID2 = contracts:make_id(CH2, 3,CID0,1),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
   
    %creating the third layer-2 contract
    Tx3 = contract_new_tx:make_dict(MP, CH3, 2, CID0, 1, Fee),
    CID3 = contracts:make_id(CH3, 2,CID0,1),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    
    %settled contract 1 and 2.
    Tx4 = contract_evidence_tx:make_dict(MP, Code, CID, <<>>, [], Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx5 = contract_evidence_tx:make_dict(MP, Code2, CID2, <<>>, [], Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Proof = contract_evidence_tx:make_proof1(Matrix),
    Tx6 = contract_timeout_tx2:make_dict(MP, CID, Fee, Proof, CH2, lists:nth(1, Matrix)),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),

    %since contract 1 is resolved, but 2 is not, we can withdraw some of our veo to 2.

    Proof6_0 = contract_evidence_tx:make_proof1(Matrix),
    SubAcc = sub_accounts:make_key(MP, CID, 1), 
    Tx6_0 = contract_winnings_tx:make_dict(MP, SubAcc, CID, Fee, lists:nth(1, Matrix), Proof6_0),
    Stx6_0 = keys:sign(Tx6_0),
    absorb(Stx6_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    %I own:
    %1 veo of layer 1 type 2
    %1 veo of layer 2 #1 types 2 and 3.
    %0.5 veo of layer 2 #2, type 1
    


    Proof2 = contract_evidence_tx:make_proof1(Matrix2),
    Tx7 = contract_timeout_tx2:make_dict(MP, CID2, Fee, Proof2, CH3, lists:nth(1, Matrix2)),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %at this point 1 and 2 are resolved, we need to do a simplification. so it should be impossible for me to convert my tokens in 1 to 2.
    Proof7_0 = contract_evidence_tx:make_proof1(Matrix),
    SubAcc7_0 = sub_accounts:make_key(MP, CID, 2), 
    Tx7_0 = contract_winnings_tx:make_dict(MP, SubAcc7_0, CID, Fee, lists:nth(2, Matrix), Proof7_0),
    Stx7_0 = keys:sign(Tx7_0),
    absorb(Stx7_0),
    0 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    

    %do the simplification from 1 to 2.
    Tx8 = contract_simplify_tx:make_dict(MP, CID, CID2, CID3, Matrix, Matrix2, Fee), 
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %verify that contracts #1 and #2 are closed, but #3 is not.
    
    1 = contracts:closed(trees:get(contracts, CID)),
    1 = contracts:closed(trees:get(contracts, CID2)),
    0 = contracts:closed(trees:get(contracts, CID3)),

    %now that 1 and 2 are simplified, I should be able to move my tokens from 1 directly to 3.
    Matrix8_0 = contract_simplify_tx:apply_matrix2matrix(Matrix, Matrix2),
    Proof8_0 = contract_evidence_tx:make_proof(2, Matrix8_0),
    SubAcc8_0 = sub_accounts:make_key(MP, CID, 2), 
    %moves 1 veo of layer 1 types 2, to 1/4 veo in layer 2 contract #3 type 1.
    Tx8_0 = contract_winnings_tx:make_dict(MP, SubAcc8_0, CID, Fee, lists:nth(2, Matrix8_0), Proof8_0),
    Stx8_0 = keys:sign(Tx8_0),
    absorb(Stx8_0),
    1 = many_txs(),
    mine_blocks(2),
    0 = many_txs(),

    %I own:
    %1 of layer2 #1 type 3
    %0.5 of layer 2 #2, type 1
    %0.25 of layer 2 #3, type 1
    
    %verify that I have money in contract 3
    SubAdd8_1 = sub_accounts:make_key(MP, CID3, 1),
    SubAcc8_1 = trees:get(sub_accounts, SubAdd8_1),
    true = is_tuple(SubAcc8_1),
    
   
    %settle contract 3
    Tx9 = contract_evidence_tx:make_dict(MP, Code3, CID3, <<>>, [], Fee),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),
    mine_blocks(2),
    0 = many_txs(),

    Tx10 = contract_timeout_tx2:make_dict(MP, CID3, Fee),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %it should now be possible to withdraw from 3.
    SubAcc10_0 = sub_accounts:make_key(MP, CID3, 1),
    Tx10_0 = contract_winnings_tx:make_dict(MP, SubAcc10_0, CID3, Fee, PayoutVector),
    Stx10_0 = keys:sign(Tx10_0),
    absorb(Stx10_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    %this has converted 0.25 of layer 2 #3 type 1 into 0.125 veo.

    %I own:
    %1  of layer2 #1 type 3
    %0.5  of layer 2 #2, type 1

    %verify that I no longer have money in contract 3
    SubAdd8_1 = sub_accounts:make_key(MP, CID3, 1),
    case trees:get(sub_accounts, SubAdd8_1) of
        empty -> ok;
        SA -> 
            if
                (SA#sub_acc.balance == 0) -> ok; %the verkle tree cannot delete things.
                true -> io:fwrite({SA#sub_acc.balance, balance, SA})
            end
    end,

    %do the simplification from 1 to 3
    Matrix3 = contract_simplify_tx:apply_matrix2matrix(Matrix, Matrix2),
    Tx11 = contract_simplify_tx:make_dict(MP, CID, CID3, 0, Matrix3, PayoutVector, Fee), 
    Stx11 = keys:sign(Tx11),
    absorb(Stx11),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
  
    %do the simplification from 2 to 3
    Tx12 = contract_simplify_tx:make_dict(MP, CID2, CID3, 0, Matrix2, PayoutVector, Fee), 
    Stx12 = keys:sign(Tx12),
    absorb(Stx12),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %resolve contract0
    Tx13 = contract_evidence_tx:make_dict(MP, Code0, CID0, <<>>, [], Fee),
    Stx13 = keys:sign(Tx13),
    absorb(Stx13),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx14 = contract_timeout_tx2:make_dict(MP, CID0, Fee),
    Stx14 = keys:sign(Tx14),
    absorb(Stx14),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %withdraw cid0 type 2
    SubAcc15 = sub_accounts:make_key(MP, CID0, 2),
    Tx15 = contract_winnings_tx:make_dict(MP, SubAcc15, CID0, Fee, PayoutVector0),
    Stx15 = keys:sign(Tx15),
    absorb(Stx15),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    
    %withdraw cid2 type type 1
    PayoutVector16 = contract_simplify_tx:apply_matrix2vector(Matrix2, PayoutVector),
    SubAcc16 = sub_accounts:make_key(MP, CID2, 1),
    Tx16 = contract_winnings_tx:make_dict(MP, SubAcc16, CID2, Fee, PayoutVector16),
    Stx16 = keys:sign(Tx16),
    absorb(Stx16),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %withdraw cid type 3
    PayoutVector17 = contract_simplify_tx:apply_matrix2vector(Matrix, PayoutVector16),
    SubAcc17 = sub_accounts:make_key(MP, CID, 3),
    Tx17 = contract_winnings_tx:make_dict(MP, SubAcc17, CID, Fee, PayoutVector17),
    Stx17 = keys:sign(Tx17),
    absorb(Stx17),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    success;
test(40) ->
    io:fwrite("test 40\n"),
    %test swapping
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    %make a contract and buy some subcurrency.
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3, 
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    io:fwrite("test 40 contract new\n"),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),


    %buy some subcurrency.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    io:fwrite("test 40 contract use\n"),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
   
    %create an account
    {NewPub,NewPriv} = signing:new_key(),
    Tx3 = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx3 = keys:sign(Tx3),
    io:fwrite("test 40 create account\n"),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),


    0 = many_txs(),
    %swap some subcurrency for the new account's veo.

    SO = swap_tx2:make_offer(MP, 0, 1000, CID, 1, 50000000, <<0:256>>, 0, 80000000, 1, Fee),
    SSO = keys:sign(SO),
    Tx4 = swap_tx2:make_dict(NewPub, SSO, 1, Fee*2),
    Stx4 = signing:sign_tx(Tx4, NewPub, NewPriv),
    io:fwrite("test 40 before absorb 2\n"),
    absorb(Stx4),
    1 = many_txs(),
    io:fwrite("test 40 before mine\n"),
    mine_blocks(1),

    success;
test(41) ->
    io:fwrite("test 41\n"),
    %test swapping in multi-tx
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(12),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {NewPub,NewPriv} = signing:new_key(),
    StartBalance = 1000000000,
    Tx1 = create_account_tx:make_dict(NewPub, StartBalance, Fee, constants:master_pub()),%send them 10 veo
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),

    Tx1_0 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    Stx1_0 = keys:sign(Tx1_0),
    absorb(Stx1_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Zero = <<0:32>>,
    Full = <<-1:32>>,

    OneVeo = 100000000,
    NewCID = contracts:make_id(CH, 2, <<0:256>>, 0),
    PBO = swap_tx2:make_offer(MP, 0, 1000, <<0:256>>, 0, OneVeo, NewCID, 1, OneVeo, 1, Fee),
    SPBO = keys:sign(PBO),
    Swap2 = swap_tx2:make_dict(NewPub, SPBO, 1, Fee),
    Use2 = contract_use_tx:make_dict(NewPub, NewCID, OneVeo, Fee),%buy one veo of a full set
    Txs2 = [Swap2, Use2],
    Tx2 = multi_tx:make_dict(NewPub, Txs2, Fee*2),
    Stx2 = signing:sign_tx(Tx2, NewPub, NewPriv),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    Tx3 = contract_evidence_tx:make_dict(MP, Code, NewCID, <<>>, [], Fee),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    

    Tx4 = contract_timeout_tx2:make_dict(MP, NewCID, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    
    
    SubAcc2 = sub_accounts:make_key(NewPub, NewCID, 2),
    Tx5 = contract_winnings_tx:make_dict(NewPub, SubAcc2, NewCID, Fee, [<<0:32>>, <<-1:32>>]),
    Stx5 = signing:sign_tx(Tx5, NewPub, NewPriv),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),

    %verify that the new account won the bet.
    AccF = trees:get(accounts, NewPub),
    true = AccF#acc.balance > (StartBalance + OneVeo - (Fee * 20)),

    success;

test(43) ->
    io:fwrite("test 43, 2 of 2 state channel\n"),
    %2 of 2 state channel
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %headers:dump(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {NewPub,NewPriv} = signing:new_key(),
    StartBalance = 1000000000,
    Tx1 = create_account_tx:make_dict(NewPub, StartBalance, Fee, constants:master_pub()),%send them 10 veo
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %finalizes as acc1 owning everything
    %delay 0, nonce 1
    _Code1 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
def \
  [ int 4294967295, int 0]\
  int 0 int 1 ; \
" >>),
    %F2 = hd(vm(Code1)),

    %finalizes as acc2 owning everything
    %delay 0, nonce 2
    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
def \
  [ int 0, int 4294967295 ]\
  int 0 int 2 ; \
" >>),
    F1 = hd(vm(Code2)),

    
%expects sig1 sig2 functionid
%if both signatures are valid for this functionid, then it calls the function.
% this is a 2 of 2 multisig that allows for arbitrary updates, it is a statechannel smart contract.
    Code = compiler_chalang:doit(
             <<" binary 65 ",
(base64:encode(NewPub))/binary, 
" binary 65 ",
(base64:encode(MP))/binary, 
" Acc1 ! Acc2 ! \
drop F ! \
F @ Acc2 @ verify_sig swap \
F @ Acc1 @ verify_sig and \
if \
   F @ call \
else fail then ">>),

    CH = hash:doit(Code),

    Tx1_0 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    Stx1_0 = keys:sign(Tx1_0),
    absorb(Stx1_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Zero = <<0:32>>,
    Full = <<-1:32>>,

    OneVeo = 100000000,
    NewCID = contracts:make_id(CH, 2, <<0:256>>, 0),
    PBO = swap_tx2:make_offer(MP, 0, 1000, <<0:256>>, 0, OneVeo, NewCID, 1, OneVeo, 1, Fee),
    SPBO = keys:sign(PBO),
    Swap2 = swap_tx2:make_dict(NewPub, SPBO, 1, Fee),
    Use2 = contract_use_tx:make_dict(NewPub, NewCID, OneVeo, Fee),

    Txs2 = [Swap2, Use2],
    Tx2 = multi_tx:make_dict(NewPub, Txs2, Fee*2),
    Stx2 = signing:sign_tx(Tx2, NewPub, NewPriv),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %F1 is the outcome where Acc2 wins everything.
    %if they both sign over F1, then it can occur.
    Sig1 = keys:raw_sign(F1),
    Sig2 = signing:sign(F1, NewPriv),
    %sig1 sig2 functionid
    EvidenceString = 
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
binary ", (integer_to_binary(size(Sig1)))/binary, " ",
               (base64:encode(Sig1))/binary, " binary ", 
               (integer_to_binary(size(Sig2)))/binary, 
               " ", (base64:encode(Sig2))/binary, 
               " def \
  [ int 0, int 4294967295 ]\
  int 0 int 2 ; \
">>,
    %the evidence is made up of both signatures, the function id, and the definition of the function.
    Evidence = compiler_chalang:doit(EvidenceString),
    Tx3 = contract_evidence_tx:make_dict(MP, Code, NewCID, Evidence, [], Fee),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    

    Tx4 = contract_timeout_tx2:make_dict(MP, NewCID, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    
    
    SubAcc2 = sub_accounts:make_key(NewPub, NewCID, 2),
    Tx5 = contract_winnings_tx:make_dict(NewPub, SubAcc2, NewCID, Fee, [<<0:32>>, <<-1:32>>]),
    Stx5 = signing:sign_tx(Tx5, NewPub, NewPriv),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),

    %verify that the new account won the bet.
    AccF = trees:get(accounts, NewPub),
    true = AccF#acc.balance > (StartBalance + OneVeo - (Fee * 20)),
    
    success;


test(44) ->
    io:fwrite("test 44\n"),
%Someone who buys a contract, they should simultaniously make an offer to sell it for 99% of it's maximum value.
    %acc2 starts with veo. they make a bet in a sports game denominated in veo. when the game ends, they want their winnings to automatically switch to being veo

    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {NewPub,NewPriv} = signing:new_key(),
    StartBalance = 1000000000,
    Tx1 = create_account_tx:make_dict(NewPub, StartBalance, Fee, constants:master_pub()),%send them 10 veo
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %First account 1 makes an offer for a new contract.

    Zero = <<0:32>>,
    Full = <<-1:32>>,
    OneVeo = 100000000,
    %2 way contract, all goes to acc2.
    Code1 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
def \
  [ int 0, int 4294967295]\
  int 0 int 1 ; \
" >>),
    CH = hash:doit(Code1),

    Tx1_0 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    Stx1_0 = keys:sign(Tx1_0),
    absorb(Stx1_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    NewCID = contracts:make_id(CH, 2, <<0:256>>, 0),
    PBO = swap_tx2:make_offer(MP, 0, 1000, <<0:256>>, 0, OneVeo, NewCID, 1, 2 * OneVeo, 1, Fee),
    SPBO = keys:sign(PBO),

    %account 2 makes an offer to sell their winnings from this contract before they join it.
    SO = swap_tx2:make_offer(NewPub, 0, 1000, NewCID, 2, OneVeo * 2, <<0:256>>, 0, 199900000, 1, 2),%Fee),
    SSO = signing:sign_tx(SO, NewPub, NewPriv),
    
    %account 2 joins the contract
    Swap2 = swap_tx2:make_dict(NewPub, SPBO, 1, Fee),
    Use2 = contract_use_tx:make_dict(NewPub, NewCID, OneVeo * 2, Fee),
    Txs2 = [Swap2, Use2],
    Tx2 = multi_tx:make_dict(NewPub, Txs2, Fee*2),
    Stx2 = signing:sign_tx(Tx2, NewPub, NewPriv),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %account1 takes the opportunity to let acc2 cash out.
    Tx3 = swap_tx2:make_dict(MP, SSO, 1, Fee),
    io:fwrite("test txs 44\n"),
    io:fwrite(packer:pack(Tx3)),
    io:fwrite("\n"),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    
    %verify that the new account won the bet.
    AccF = trees:get(accounts, NewPub),
    true = AccF#acc.balance > (StartBalance + OneVeo - (Fee * 20)),
    
    success;
test(45) ->
    io:fwrite("test 45\n"),
    %binary derivative in the new channel, using an oracle to enforce the outcome.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo*10, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),


    Question = <<"1=1">>,
    %Tx = oracle_new_tx:make_dict(MP, Fee, Question, block:height() + 1, 0, 0), %Fee, question, start, id gov, govamount %here
    Tx = oracle_new_tx:make_dict(MP, Fee, Question, 5, 0, 0), %Fee, question, start, id gov, govamount 
    OID = oracle_new_tx:id(Tx),
    io:fwrite("test 45 oid is \n"),
    io:fwrite(packer:pack(OID)),
    io:fwrite("\n"),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),


    %state channel contract
    ChannelCode = compiler_chalang:doit(
             <<" Proof ! binary 65 ",
               (base64:encode(Pub))/binary, 
               " binary 65 ",
               (base64:encode(MP))/binary, 
               " Acc1 ! Acc2 ! F ! \
F @ Acc2 @ verify_sig swap \
F @ Acc1 @ verify_sig and \
if \
   Proof @ F @ call \
else fail then ">>),
    CH = hash:doit(ChannelCode),

    Tx1_0 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    Stx1_0 = keys:sign(Tx1_0),
    absorb(Stx1_0),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Zero = <<0:32>>,
    Full = <<-1:32>>,
    Half1 = <<2147483648:32>>,
    Half0 = <<2147483647:32>>,

    ChannelCID = contracts:make_id(CH, 2, <<0:256>>, 0),
    PBO = swap_tx2:make_offer(MP, 0, 100, <<0:256>>, 0, OneVeo, ChannelCID, 1, OneVeo * 2, 1, Fee),
    
    SPBO = keys:sign(PBO),
    
    Swap2 = swap_tx2:make_dict(Pub, SPBO, 1, Fee),
    Use2 = contract_use_tx:make_dict(Pub, ChannelCID, OneVeo*2, Fee),
    Txs2 = [Swap2, Use2],
    Tx2 = multi_tx:make_dict(Pub, Txs2, Fee*2),
    Stx2 = signing:sign_tx(Tx2, Pub, Priv),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),


%binary derivative contract based on oracle id OID.

    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, BinaryCodeStatic} = file:read_file(PrivDir ++ "/binary.fs"),
    io:fwrite("binary contract is "),
    io:fwrite(integer_to_list(size(compiler_chalang:doit(BinaryCodeStatic)))),
    io:fwrite("\n"),
    io:fwrite(base64:encode(compiler_chalang:doit(BinaryCodeStatic))),
    io:fwrite("\n"),
% AAAAAAF4gxSDFhSDFhSDFAAAAAAghwAAAAABeTpGRw1IFBQAAAAAAYcWFAIAAAADAAAAFoYAAAAAAzpGhAAAAAAAFoIAAAAAABaCAP////8WgogAAAAAAAAAAAPoRxQAAAAAATpGhAD/////FoIAAAAAABaCAAAAAAAWgogAAAAAAAAAAAPoRxQAAAAAAjpGhAAAAAAAFoIA/////xaCAAAAAAAWgogAAAAAAAAAAAPoRxQUhACAAAAAFoIAf////xaCAAAAAAAWgogAAAATiAAAAAAKSEhI
    BinaryCodeInner = <<" binary 32 ",
                        (base64:encode(OID))/binary, 
                        BinaryCodeStatic/binary
                      >>,
    BinaryCode = <<" def ",
                   BinaryCodeInner/binary,
                   " ; ">>,

    BinaryDerivative = compiler_chalang:doit(BinaryCode),
    BinaryHash = hd(vm(BinaryDerivative)),
    BinaryCID = contracts:make_id(BinaryHash, 3, <<0:256>>, 0),
    io:fwrite("test 45 binary cid \n"),
    io:fwrite(packer:pack(BinaryCID)),
    io:fwrite("\n"),

    %this is the thing we sign over to convert the state channel into a binary bet.
    ToBinary = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
def \
[ [ int 0 , int 4294967295 , int 2147483648 ] , \
  [ int 4294967295, int 0 , int 2147483647 ] ] \
binary 32 ",
(base64:encode(BinaryHash))/binary,
" int 0 int 2000 \
;">>,
    Matrix = [[Zero, Full, Half1],%acc1 gets all type2
              [Full,Zero, Half0]],%acc2 gets all type1
    ToBinaryBytes = compiler_chalang:doit(ToBinary),
    ToBinaryHash = hd(vm(ToBinaryBytes)),
    
    Sig1 = keys:raw_sign(ToBinaryHash),
    Sig2 = signing:sign(ToBinaryHash, Priv),
    EvidenceString = 
             <<"\
 binary ", (integer_to_binary(size(Sig1)))/binary, " ",
               (base64:encode(Sig1))/binary, " binary ", 
               (integer_to_binary(size(Sig2)))/binary, 
               " ", (base64:encode(Sig2))/binary, 
               ToBinary/binary,
             " ">>,
    Evidence = compiler_chalang:doit(EvidenceString),
    %Tx3 = contract_evidence_tx:make_dict(MP, ChannelCode, ChannelCID, Evidence, [{oracles, OID}], Fee),
    Tx3 = contract_evidence_tx:make_dict(MP, ChannelCode, ChannelCID, Evidence, [], Fee),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),

    
    Proof = contract_evidence_tx:make_proof1(Matrix),
    Tx4 = contract_timeout_tx2:make_dict(MP, ChannelCID, Fee, Proof, BinaryHash, lists:nth(1, Matrix)),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    %TODO withdraw into the binary derivative.
    SubAcc2 = sub_accounts:make_key(Pub, ChannelCID, 2),
    Proof2 = contract_evidence_tx:make_proof(2, Matrix),
    Tx5 = contract_winnings_tx:make_dict(Pub, SubAcc2, ChannelCID, Fee, hd(tl(Matrix)), Proof2),
    Stx5 = signing:sign_tx(Tx5, Pub, Priv),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),

    %verify the existence of the coins withdrawn into the binary contract.
    BinSub1 = sub_accounts:make_key(Pub, BinaryCID, 1),
    SA5 = trees:get(sub_accounts, BinSub1),
    true = SA5#sub_acc.balance == (2 * OneVeo),

    %Pub now has an active bet on outcome 1 of the oracle. They can swap shares of this bet as a subcurrency.


    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx6 = oracle_bet_tx:make_dict(MP, Fee, OID, 1, OIL+1 + (10*OneVeo)), 
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    mine_blocks(1),


    Tx7 = oracle_close_tx:make_dict(MP,Fee, OID),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    mine_blocks(1),

    %resolve the binary contract.
    Tx8 = contract_evidence_tx:make_dict(MP, compiler_chalang:doit(BinaryCodeInner), BinaryCID, <<>>, [{oracles, OID}], Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),

    Tx9 = contract_timeout_tx2:make_dict(MP, BinaryCID, Fee),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %Pub should withdraw their 2 veo.
    SubAcc3 = sub_accounts:make_key(Pub, BinaryCID, 1),
    Tx10 = contract_winnings_tx:make_dict(Pub, SubAcc3, BinaryCID, Fee, [Full, Zero, Zero]),
    Stx10 = signing:sign_tx(Tx10, Pub, Priv),
    absorb(Stx10),
    1 = many_txs(),
    mine_blocks(1),

    %verify that pub won the bet
    
    AccF = trees:get(accounts, Pub),
    true = AccF#acc.balance > ((OneVeo * 11) - (20 * Fee)),

    success;
test(46) ->
    io:fwrite("test 46\n"),
    %tests flash loans.
    %2 users own opposite sides of the contract.
    %the winner offers to sell for 99% of it's value
    %the lose should be able to get their 1% out, even if they can't afford to buy the 99%.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(10),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 4294967295 , int 0 ]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3, 
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
   
    %spending one of the subcurrency types
    Amount2 = 100000000,
    {NewPub,NewPriv} = signing:new_key(),
    Ctx4 = create_account_tx:make_dict(NewPub, 1, Fee, constants:master_pub()),
    SCtx4 = keys:sign(Ctx4),
    absorb(SCtx4),
    1 = many_txs(),

    Tx3 = sub_spend_tx:make_dict(NewPub, Amount2, Fee, CID, 1, MP),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    2 = many_txs(),
    mine_blocks(1),

    %at this point account2 loses the  bet. so account 1 offers to sell their winnings.
    SO = swap_tx2:make_offer(MP, 0, 1000, CID, 2, 100000000, <<0:256>>, 0, 99000000, 1, Fee),
    SSO = keys:sign(SO),
    SO3 = swap_tx2:make_offer(MP, 0, 1000, CID, 3, 100000000, <<0:256>>, 0, 1, 1, Fee),
    SSO3 = keys:sign(SO3),
    
    %account 2 simultaniously buys account 1's winnings, combines it with the losing shares, and withdraws the source currency. this is a combination of a swap tx with a contract_use tx.

    SwapTx = swap_tx2:make_dict(NewPub, SSO, 1, Fee*2),%sends type 2.
    SwapTx3 = swap_tx2:make_dict(NewPub, SSO3, 1, Fee),%sends type 3.
    UseTx = contract_use_tx:make_dict(NewPub, CID, -100000000, Fee),%sells all 3 as a complete set.
    Txs = [SwapTx, SwapTx3, UseTx],
    Tx4 = multi_tx:make_dict(NewPub, Txs, Fee*2),
    Stx4 = signing:sign_tx(Tx4, NewPub, NewPriv),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    Stx5 = signing:sign_tx(SwapTx, NewPub, NewPriv),
    absorb(Stx5),
    0 = many_txs(),
    mine_blocks(1),
    
    success;
test(47) ->
    io:fwrite("test 47\n"),
    %scalar derivative in the new channel, using an oracle to enforce the outcome.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    %evidence includes the resulting price.
    %embed this price into the oracle question text. 
    %make sure the oracle returns type 1, otherwise fail.

    %inputs when creating the contract: 
    % *text description of number being measured.
    % *a block height for the oracle reporting to start.
    Measured = <<"the price of bitcoin in USD on October 31, noon, CMT time, according to coin market cap">>,
    MaxPrice = 30000,
    StartHeight = 5,
    
    % *range of values that can be measure

    %evidence when resolving:
    % *the resulting number.
    Max = 4294967295,
    OracleTextPart = <<"MaxPrice = ", (integer_to_binary(MaxPrice))/binary, "; MaxVal = 4294967295; B = ", (Measured)/binary, " from $0 to $MaxPrice; min(MaxVal, (B * MaxVal / MaxPrice)) is ">>,
    %oracle id calculation
    %S = <<Start:32,Gov:32,GA:32,QH/binary>>,
    %hash:doit(S).
    Settings = <<
%" int1 5 \
" swap binary ", (integer_to_binary(size(OracleTextPart)))/binary, 
          " ", 
          (base64:encode(OracleTextPart))/binary, " ">>,
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, ScalarCodeStatic} = file:read_file(PrivDir ++ "/scalar.fs"),

    ScalarContractBytes = compiler_chalang:doit(ScalarCodeStatic),
    SettingsBytes = compiler_chalang:doit(Settings),
    ContractBytes = <<SettingsBytes/binary, ScalarContractBytes/binary>>,
    CH = hash:doit(ContractBytes),

    CID = contracts:make_id(CH, 2, <<0:256>>, 0),
    
    NewTx = contract_new_tx:make_dict(MP, CH, 2, 0),
    UseTx = contract_use_tx:make_dict(MP, CID, OneVeo, 0, 2, <<0:256>>, 0),
    SpendTx = sub_spend_tx:make_dict(Pub, OneVeo, 0, CID, 1, MP),

    Txs = [NewTx, UseTx, SpendTx],
    Tx2 = multi_tx:make_dict(MP, Txs, Fee*length(Txs)),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    Max = 4294967295,
    Third = Max div 3,
    Q = <<OracleTextPart/binary, 
          (integer_to_binary(Third))/binary
        >>, 
    OracleNewTx = oracle_new_tx:make_dict(MP, Fee*2, Q, StartHeight, 0, 0),

    
    %Stx22 = keys:sign(OracleNewTx),
    %absorb(Stx22),
    %1 = many_txs(),
    %mine_blocks(1),

    OID = oracle_new_tx:id(OracleNewTx),
    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    OracleBetTx = oracle_bet_tx:make_dict(MP, Fee*2, OID, 1, OIL+1),

    %Stx23 = keys:sign(OracleBetTx),
    %absorb(Stx23),
    %1 = many_txs(),
    %mine_blocks(1),
    %success = to_here,
    
    Txs3 = [OracleNewTx, OracleBetTx],
    Tx3 = multi_tx:make_dict(MP, Txs3, Fee*length(Txs3)),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),%here
    mine_blocks(1),

    Tx4 = oracle_close_tx:make_dict(MP, Fee, OID),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    Tx5 = contract_evidence_tx:make_dict(MP, ContractBytes, CID, compiler_chalang:doit(<<
" int 4294967295 int1 3 / int1 5 ">>), 
[{oracles, OID}], Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
  
    Tx6 = contract_timeout_tx2:make_dict(MP, CID, Fee),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),

    SubAcc2 = sub_accounts:make_key(Pub, CID, 1),
    Tx7 = contract_winnings_tx:make_dict(Pub, SubAcc2, CID, Fee, [<<Third:32>>, <<(Max - Third):32>>]),
    Stx7 = signing:sign_tx(Tx7, Pub, Priv),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),

    AccF = trees:get(accounts, Pub),
    true = AccF#acc.balance > ((OneVeo) + (OneVeo div 3) - (10 * Fee)),

    success;
test(48) ->
    %market txs
    io:fwrite("test 48 \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(10),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    Forth = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
[ int 4294967295, int 0 ] \
int 0 int 1000 \
">>,
    Contract = compiler_chalang:doit(Forth), 
    CH = hash:doit(Contract),
    Tx1 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    CID = contracts:make_id(CH, 2,<<0:256>>,0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),

    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    Tx3 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 3, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),%here. it says we are trying to edit inaccessible state.
   
    io:fwrite("test txs mid is \n"),
    io:fwrite(base64:encode(MID)),
    io:fwrite("\n"),
    Tx4 = market_liquidity_tx:make_dict(MP, MID, Amount div 4, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    
    Tx5 = market_swap_tx:make_dict(MP, MID, Amount div 10, Amount div 30, 1, Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),

    success;
test(49) ->
    %market txs in a multi-tx
    io:fwrite("test 49 \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    
    Forth = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
[ int 4294967295, int 0 ] \
int 0 int 1000 \
">>,
    Contract = compiler_chalang:doit(Forth), 
    CH = hash:doit(Contract),
    Tx1 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    CID = contracts:make_id(CH, 2,<<0:256>>,0),

    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee, 2, <<0:256>>, 0),

    Tx3 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 3, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),

    Tx4 = market_liquidity_tx:make_dict(MP, MID, Amount div 4, Fee, CID, 1, <<0:256>>, 0),

    Tx5 = market_swap_tx:make_dict(MP, MID, Amount div 10, Amount div 30, 1, Fee, <<0:256>>, 0, CID, 1),
    Txs = [Tx1, Tx2, Tx3], 
    Txs2 = [Tx4, Tx5],
    Tx = multi_tx:make_dict(MP, Txs, Fee*3),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),
    
    MTx = multi_tx:make_dict(MP, Txs2, Fee*2),
    SMtx = keys:sign(MTx),
    absorb(SMtx),
    1 = many_txs(),
    mine_blocks(1),
    success;
test(50) ->
    %this is to set up the blockchain state to try out the uniswap tool from javascript.
    % We want to there to be many paths between the 2 currencies being swapped, and the optimal solution to involve buying a mixture of different paths.
    io:fwrite("test 50 \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    Forth = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
[ int 4294967295, int 0 ] \
int 0 int 1000 \
">>,
    Forth2 = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
[ int 4294967294, int 1 ] \
int 0 int 1000 \
">>,
    Contract = compiler_chalang:doit(Forth), 
    Contract2 = compiler_chalang:doit(Forth2), 
    CH = hash:doit(Contract),
    CH2 = hash:doit(Contract2),
    Tx1 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    Tx2 = contract_new_tx:make_dict(MP, CH2, 2, Fee),
    CID = contracts:make_id(CH, 2,<<0:256>>,0),
    CID2 = contracts:make_id(CH2, 2,<<0:256>>,0),

    OneVeo = 100000000,
    Amount = OneVeo,
    Tx3 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, CID, 2, Amount div 2, Fee),
    MID = markets:make_id(CID, 1, CID, 2),
    Tx4 = market_new_tx:make_dict(MP, CID2, 1, Amount div 2, <<0:256>>, 0, Amount div 3, Fee),
    MID2 = markets:make_id(CID2, 1, <<0:256>>, 0),

    %{Pub,Priv} = signing:new_key(),
    Pub = base64:decode("BLPyEIdHuulZoDWuBJgCK4xnSoTHkirqsgH3phF9LWu9b+Gv1PTOg3CgtDsF4OMMKNtwZL4UKVcJR7DeHY/XTsU="),
    Tx5 = create_account_tx:make_dict(Pub, OneVeo * 10, Fee*5, MP),
    Tx6 = contract_use_tx:make_dict(MP, CID, Amount*2, Fee, 2, <<0:256>>, 0),
    Tx7 = contract_use_tx:make_dict(MP, CID2, Amount, Fee, 2, <<0:256>>, 0),
    Tx8 = sub_spend_tx:make_dict(Pub, Amount, Fee, CID, 1, MP),

    Txs = [Tx1, Tx2, Tx3, Tx4, Tx5, Tx6, Tx7, Tx8,
           market_new_tx:make_dict(MP, CID, 1, Amount div 4, CID2, 1, Amount div 4, 0),
           market_new_tx:make_dict(MP, CID, 1, Amount div 4, CID2, 2, Amount div 4, 0)
],
    Txm = multi_tx:make_dict(MP, Txs, Fee*20),
    Stx = keys:sign(Txm),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    success;
test(51) ->
    %market liquidity test
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    
    Forth = <<" macro [ nil ; \
macro , swap cons ; \
macro ] swap cons reverse ; \
[ int 4294967295, int 0 ] \
int 0 int 1000 \
">>,
    Contract = compiler_chalang:doit(Forth), 
    CH = hash:doit(Contract),
    Tx1 = contract_new_tx:make_dict(MP, CH, 2, Fee),
    CID = contracts:make_id(CH, 2,<<0:256>>,0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),

    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    Tx3 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 3, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),

    Tx4 = market_liquidity_tx:make_dict(MP, MID, Amount div 8, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    
    absorb(Stx4),
    0 = many_txs(),
    
    success;
test(52) ->
    %multi-tx, using a flash loan to pay the tx fee.
    io:fwrite("test 52\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    %creating a shareable contract with subcurrencies.
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %spending one of the subcurrency types
    Amount2 = 30000000,
    {NewPub,NewPriv} = signing:new_key(),
    Tx3 = sub_spend_tx:make_dict(NewPub, Amount2, Fee, CID, 1, MP),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),

    %creating the account.
    Tx32 = create_account_tx:make_dict(NewPub, 1, Fee, MP),
    Stx32 = keys:sign(Tx32),
    absorb(Stx32),
    1 = many_txs(),
    mine_blocks(1),

    %creating a market
    Tx4 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 2, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    Tx5_0 = market_swap_tx:make_dict(NewPub, MID, Amount2, Amount2 div 30, 2, Fee),
    Tx5 = multi_tx:make_dict(NewPub, [Tx5_0], Fee),
    io:fwrite(packer:pack(Tx5)),
    io:fwrite("\n"),
    Stx5 = signing:sign_tx(Tx5, NewPub, NewPriv),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    
    success;
test(53) ->
    %market_swap_tx re-publish

    io:fwrite("test 53\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    %creating a shareable contract with subcurrencies.
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %creating a market
    Tx4 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 2, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    Tx5 = market_swap_tx:make_dict(MP, MID, 100, 1, 1, Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    absorb(Stx5),
    0 = many_txs(),
    %mine_blocks(1),

    success;
test(54) ->
    %io:fwrite("market liquidity, none left to withdraw.\n")
    io:fwrite("test 54\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,

    %creating a shareable contract with subcurrencies.
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buying some subcurrencies from the new contract.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %creating a market
    Tx4 = market_new_tx:make_dict(MP, CID, 1, Amount div 2, <<0:256>>, 0, Amount div 2, Fee),
    MID = markets:make_id(CID, 1, <<0:256>>, 0),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    %withdraw all liquidity
    Tx5 = market_liquidity_tx:make_dict(MP, MID, -(Amount div 2), Fee),%fails because your liquidity shares balance needs to be bigger than zero.
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    0 = many_txs(),
    mine_blocks(1),

    %add more liquidity
    Tx6 = market_liquidity_tx:make_dict(MP, MID, Amount div 4, Fee),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),

    success;
test(55) ->
    io:fwrite("test swap_tx2 and trade_cancel_tx\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),

    %make a contract 
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3, 
    Fee = constants:initial_fee() + 20,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buy some subcurrency.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %create an account
    {NewPub,NewPriv} = signing:new_key(),
    Tx3 = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    SO = swap_tx2:make_offer(MP, 0, 1000, CID, 1, 10000000, <<0:256>>, 0, 10000000, 10000, Fee),
    #swap_offer2{
                 salt = Salt
                } = SO,
    TID = swap_tx:trade_id_maker(MP, Salt),
    SSO = keys:sign(SO),
    Tx4 = swap_tx2:make_dict(NewPub, SSO, 3456, Fee*2),
    %matching 34.56% of the limit order.
    Stx4 = signing:sign_tx(Tx4, NewPub, NewPriv),
    empty = trees:get(trades, TID),
    100000000 = (trees:get(accounts, NewPub))#acc.balance,
    SubAcc1 = sub_accounts:make_key(MP, CID, 1),
    SubAcc21 = sub_accounts:make_key(NewPub, CID, 1),
    100000000 = (trees:get(sub_accounts, SubAcc1))#sub_acc.balance,
    empty = (trees:get(sub_accounts, SubAcc21)),

    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    absorb(Stx4),
    0 = many_txs(),%testing replay protection



    Trade1 = trees:get(trades, TID),
    3457 = Trade1#trade.height,
    96241724 = (trees:get(accounts, NewPub))#acc.balance,
    96544000 = (trees:get(sub_accounts, SubAcc1))#sub_acc.balance,
    3456000 = (trees:get(sub_accounts, SubAcc21))#sub_acc.balance,

    0 = many_txs(),

    Nonce0 = 3500,
    Tx5 = trade_cancel_tx:make_dict(
            MP, Nonce0, Fee, Salt),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    absorb(Stx5),%testing that the same tx can't be re-published.
    0 = many_txs(),
    
    Trade2 = trees:get(trades, TID),
    Nonce0 = Trade2#trade.height,

    %next testing in a multi-tx
    
    MatchMany2 = 400,
    Swap2 = swap_tx2:make_dict(NewPub, SSO, MatchMany2, Fee*2),
    Txs2 = [Swap2],
    Tx6 = multi_tx:make_dict(NewPub, Txs2, Fee*2),
    Stx6 = signing:sign_tx(Tx6, NewPub, NewPriv),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Trade3 = trees:get(trades, TID),
    Nonce1 = Nonce0 + MatchMany2,
    Nonce1 = Trade3#trade.height,

    Nonce2 = Nonce1+100,
    TradeCancel2 = 
        trade_cancel_tx:make_dict(
          MP, Nonce2, Fee, Salt),
    Txs3 = [TradeCancel2],
    Tx7 = multi_tx:make_dict(MP, Txs3, Fee*2),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    
    Trade4 = trees:get(trades, TID),
    Nonce2 = Trade4#trade.height,


    %test swap_tx2 in a flash minting tx.
    
    SO2 = swap_tx2:make_offer(MP, 0, 1000, <<0:256>>, 0, 10000, CID, 2, 10000, 10000, Fee),
    #swap_offer2{
                 salt = Salt2
                } = SO2,
    TID2 = swap_tx:trade_id_maker(MP, Salt2),
    SSO2 = keys:sign(SO2),
    SubAcc2 = sub_accounts:make_key(NewPub, CID, 2),
    empty = (trees:get(sub_accounts, SubAcc2)),%here
    Swap3 = swap_tx2:make_dict(NewPub, SSO2, 10000, Fee*2),
    CU = contract_use_tx:make_dict(NewPub, CID, 10000, Fee*2),
    Tx8 = multi_tx:make_dict(NewPub, [Swap3, CU], Fee*2),
    Stx8 = signing:sign_tx(Tx8, NewPub, NewPriv),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    success;
test(56) ->
    io:fwrite("test swap_tx2, a limit order that cannot be partially matched. test 56\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),

    %make a contract 
    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CH = hash:doit(Code),
    Many = 3, 
    Fee = constants:initial_fee() + 20,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),

    %buy some subcurrency.
    Amount = 100000000,
    Tx2 = contract_use_tx:make_dict(MP, CID, Amount, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),

    %create an account
    {NewPub,NewPriv} = signing:new_key(),
    Tx3 = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    SO = swap_tx2:make_offer(MP, 0, 1000, CID, 1, 10000000, <<0:256>>, 0, 10000000, 1, Fee),
    #swap_offer2{
                 salt = Salt
                } = SO,
    TID = swap_tx:trade_id_maker(MP, Salt),
    SSO = keys:sign(SO),
    Tx4 = swap_tx2:make_dict(NewPub, SSO, 1, Fee*2),
    %good
    %["swap_tx2","BGYtkmMAHPnboJmbZjU0R2YTeuq0ib6Veict4xoJidmXbDqOwkOUG6gcZ82LyaNMsFN84dGDUNRR61/dcfaxOzs=",1,302276,["signed",["swap_offer2","BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",0,1000,"/ssoJN6X/1R3Suo94kk+csiAAZbNmPk/9oq/VCJW5tc=",1,10000000,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",0,10000000,"zQ/JbDXv+FzJQuv1PMIgFiQFREyWEVetxbMzU5X4CHo=",1,1],"MEUCIQDlDU0aYHxbQjZGBTxW0E+7h4VKl0v2y8IkJjDKt5PnrgIgYDGOnvMZB5rOwyE4KrsnbABV4SLGL4XP97jmBUyPidw=",[-6]],1],
    Stx4 = signing:sign_tx(Tx4, NewPub, NewPriv),
    io:fwrite(packer:pack(Stx4)),
    io:fwrite("/n"),
    SubAcc1 = sub_accounts:make_key(MP, CID, 1),
    SubAcc2 = sub_accounts:make_key(NewPub, CID, 1),

    empty = trees:get(trades, TID),
    100000000 = (trees:get(accounts, NewPub))#acc.balance,
    100000000 = (trees:get(sub_accounts, SubAcc1))#sub_acc.balance,
    empty = (trees:get(sub_accounts, SubAcc2)),

    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),

    2 = (trees:get(trades, TID))#trade.height,
    89697724 = (trees:get(accounts, NewPub))#acc.balance,
    90000000 = (trees:get(sub_accounts, SubAcc1))#sub_acc.balance,
    10000000 = (trees:get(sub_accounts, SubAcc2))#sub_acc.balance,

    0 = many_txs(),

    success;
test(57) ->
    io:fwrite("test trade_cancel_tx when the trade id doesn't yet exist, test 57 \n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),

    Nonce0 = 3500,
    Fee = constants:initial_fee() + 20,
    Salt = <<0:256>>,
    Tx = trade_cancel_tx:make_dict(
           MP, Nonce0, Fee, Salt),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    success;
test(58) ->
    io:fwrite("test the hard update 46. It fixed the problem where if the oracle has exactly 2 unmatched orders, it would incorrectly resolve as 'bad question'\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Fee = constants:initial_fee() + 20,
    {Pub,Priv} = signing:new_key(),
    Tx1 = create_account_tx:make_dict(Pub, 100000000, Fee, MP),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Question = <<>>,
    Tx2 = oracle_new_tx:make_dict(Pub, Fee, Question, block:height() + 1, 0, 0), %Fee, question, start, id gov, govamount 
    OID = oracle_new_tx:id(Tx2),
    Stx2 = signing:sign_tx(Tx2, Pub, Priv),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),%here
    0 = many_txs(),
    Tx3 = oracle_bet_tx:make_dict(Pub, Fee, OID, 1, 2100000), 
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    Tx4 = oracle_bet_tx:make_dict(MP, Fee, OID, 2, 2000000), 
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    Tx5 = oracle_bet_tx:make_dict(MP, Fee, OID, 1, 5000000), 
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    Tx6 = oracle_close_tx:make_dict(MP ,Fee, OID),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    mine_blocks(5),
    Oracle = trees:get(oracles, OID),
    1 = Oracle#oracle.type,
    1 = Oracle#oracle.result,
    success;
test(59) ->
    io:fwrite("make a bid to buy veo\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(2),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo*2, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    Salt = crypto:strong_rand_bytes(32),
    TID = swap_tx:trade_id_maker(MP, Salt),
    Receipt = receipts:new(TID, Pub, 1),
    ReceiptID = receipts:id(Receipt),

    %prove_address_timeout 
    %oracle_start_height
    %blockchain
    %amount
    %ticker
    %date
    %trade id
    OracleStartHeight = 5,
    ReusableSettings = 
        <<" int4 ", 
          (integer_to_binary(OracleStartHeight))/binary, 
          " .\" bitcoin\" .\" 1\" .\" BTC\" .\" Jan 1 2021\" ">>,
    Settings = <<" int 100 ", 
                 ReusableSettings/binary,
                 " int 1 binary 32 ", 
                 (base64:encode(TID))/binary>>,
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, CodeStatic} = file:read_file(PrivDir ++ "/buy_veo.fs"),
    {ok, CodeStatic2} = file:read_file(PrivDir ++ "/buy_veo_part2.fs"),

    Gas = 100000,
    HashStatic2 = base64:encode(hd(chalang:stack(chalang:test(compiler_chalang:doit(<<CodeStatic2/binary, " part2 ">>), Gas, Gas, Gas, Gas, [])))), %contract hash of the static part.

    HashStatic2def = 
          <<" macro part2 binary 32 ", 
            HashStatic2/binary,
          " ; ">>,

    StaticBytes = compiler_chalang:doit(<<HashStatic2def/binary, CodeStatic/binary>>),
    io:fwrite("static bytes \n"),
    io:fwrite(base64:encode(StaticBytes)),
    io:fwrite("\n"),
    SettingsBytes = compiler_chalang:doit(Settings),
    ContractBytes = <<SettingsBytes/binary, StaticBytes/binary>>,
    io:fwrite("contract bytes \n"),
    io:fwrite(base64:encode(ContractBytes)),
    io:fwrite("\n"),
    io:fwrite("length 1\n"),
    io:fwrite(integer_to_list(size(ContractBytes))),
    io:fwrite("\n"),
    %io:fwrite(disassembler:doit(ContractBytes)),
    %io:fwrite("\n"),
    CH = hash:doit(ContractBytes),
    CID = contracts:make_id(CH, 2, <<0:256>>, 0),
    NewTx = contract_new_tx:make_dict(Pub, CH, 2, 0),

    SO = swap_tx2:make_offer(
           MP, 0, 1000, 
           <<0:256>>, 0, round(OneVeo / 20), 
           CID, 2, round(OneVeo * 21 / 20), 
           1, Fee, Salt),
    SSO = keys:sign(SO),
    SwapTx = swap_tx2:make_dict(Pub, SSO, 1, Fee*2),
    UseTx = contract_use_tx:make_dict(Pub, CID, round(1.1*OneVeo), 0, 2, <<0:256>>, 0),
    Txs2 = [NewTx, SwapTx, UseTx],
    Tx3 = multi_tx:make_dict(Pub, Txs2, Fee*length(Txs2)),
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    %Pub accepts the limit order, which creates the swap receipt.
    absorb(Stx3),
    %1 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    %Pub provides evidence of their bitcoin address, the contract transforms into a new contract.
    BitcoinAddress = <<"bitcoin_address">>,
    Sig = sign:sign(BitcoinAddress, Priv),
    Evidence = compiler_chalang:doit(<<" binary ", (integer_to_binary(size(Sig)))/binary, " ", (base64:encode(Sig))/binary, " .\" ", BitcoinAddress/binary, "\" ">>),
    io:fwrite("length evidence\n"),
    io:fwrite(integer_to_list(size(Evidence))),
    io:fwrite("\n"),
    Tx4 = contract_evidence_tx:make_dict(Pub, ContractBytes, CID, Evidence, [{receipts, ReceiptID}], Fee),
    Stx4 = signing:sign_tx(Tx4, Pub, Priv),
    absorb(Stx4),
    io:fwrite("evidence txs \n"),
    io:fwrite(packer:pack([Tx3, Tx4])),
    io:fwrite("\n"),
    2 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Full = <<4294967295:32>>,
    Empty = <<0:32>>,
    Matrix = %same matrix from inside the forth code.
        [[Full, Empty],
         [Empty, Full]],
    Proofs = contract_evidence_tx:make_proof1(Matrix),


    Contract2 = <<ReusableSettings/binary,
                  " .\" ", BitcoinAddress/binary, "\" binary 32 ",
                  HashStatic2/binary,
                  " call "
                >>,
    Contract2Bytes = compiler_chalang:doit(Contract2),
    io:fwrite("length 2\n"),
    io:fwrite(integer_to_list(size(Contract2Bytes))),
    io:fwrite("\n"),
    CH2 = hash:doit(Contract2Bytes),

    CID2 = contracts:make_id(CH2, 2, <<0:256>>, 0),

    Tx5 = contract_timeout_tx2:make_dict(MP, CID, Fee, Proofs, CH2, hd(Matrix), CID2),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    3 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %GetOracleCode = <<ReusableSettings/binary, CodeStatic2/binary, " .\" ", BitcoinAddress/binary, "\" Address ! Date ! Ticker ! Amount ! Blockchain ! drop Date @ Ticker @ Amount @ Address @ Blockchain @ oracle_builder ">>,
    GetOracleCode = <<ReusableSettings/binary, CodeStatic2/binary, " .\" ", BitcoinAddress/binary, "\" Address ! Date ! Ticker ! Amount ! Blockchain ! drop Blockchain @ Address @ Amount @ Ticker @ Date @ oracle_builder ">>,
    Question = hd(chalang:stack(chalang:test(compiler_chalang:doit(GetOracleCode), Gas, Gas, Gas, Gas, []))),
    Question2 = <<"The bitcoin address bitcoin_address is a valid address for that blockchain and has received more than or equal to 1 of BTC before Jan 1 2021">>,
    Question = Question2,%error here.
    Tx6 = oracle_new_tx:make_dict(MP, Fee, Question, OracleStartHeight, 0, 0), %Fee, question, start, id gov, govamount
    OID = oracle_new_tx:id(Tx6),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    
    Tx7 = oracle_bet_tx:make_dict(MP, Fee, OID, 1, 3000000), 
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx8 = oracle_close_tx:make_dict(MP,Fee, OID),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Evidence2 = compiler_chalang:doit(CodeStatic2),
    io:fwrite("length evidence2\n"),
    io:fwrite(integer_to_list(size(Evidence2))),
    io:fwrite("\n"),

    Tx9 = contract_evidence_tx:make_dict(MP, Contract2Bytes, CID2, Evidence2, [{oracles, OID}], Fee),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),

    Tx10 = contract_timeout_tx2:make_dict(MP, CID2, Fee),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    2 = many_txs(),

    Tx11 = contract_simplify_tx:make_dict(MP, CID, CID2, 0, Matrix, [Empty, Full], Fee),
    Stx11 = keys:sign(Tx11),
    absorb(Stx11),
    3 = many_txs(),

    Bal1 = element(2, trees:get(accounts, MP)),

    SubAcc1 = sub_accounts:make_key(MP, CID, 2),
    Tx12 = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, [Empty, Full]),
    Stx12 = keys:sign(Tx12),
    absorb(Stx12),
    4 = many_txs(),

    Bal2 = element(2, trees:get(accounts, MP)),
    true = (Bal2 - Bal1) > (OneVeo * 0.9),

    success;
test(60) ->
    io:fwrite("make a bid to buy veo, and they don't provide a deposit address\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(2),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo*2, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    Salt = crypto:strong_rand_bytes(32),
    TID = swap_tx:trade_id_maker(MP, Salt),
    Receipt = receipts:new(TID, Pub, 1),
    ReceiptID = receipts:id(Receipt),

    %prove_address_timeout 
    %oracle_start_height
    %blockchain
    %amount
    %ticker
    %date
    %trade id
    OracleStartHeight = 5,
    ReusableSettings = 
        <<" int4 ", 
          (integer_to_binary(OracleStartHeight))/binary, 
          " .\" bitcoin\" .\" 1\" .\" BTC\" .\" Jan 1 2021\" ">>,
    Settings = <<" int 3 ", 
                 ReusableSettings/binary,
                 " int 1 binary 32 ", 
                 (base64:encode(TID))/binary>>,
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, CodeStatic} = file:read_file(PrivDir ++ "/buy_veo.fs"),
    {ok, CodeStatic2} = file:read_file(PrivDir ++ "/buy_veo_part2.fs"),

    Gas = 100000,
    HashStatic2 = base64:encode(hd(chalang:stack(chalang:test(compiler_chalang:doit(<<CodeStatic2/binary, " part2 ">>), Gas, Gas, Gas, Gas, [])))), %contract hash of the static part.

    HashStatic2def = 
          <<" macro part2 binary 32 ", 
            HashStatic2/binary,
          " ; ">>,

    StaticBytes = compiler_chalang:doit(<<HashStatic2def/binary, CodeStatic/binary>>),
    SettingsBytes = compiler_chalang:doit(Settings),
    ContractBytes = <<SettingsBytes/binary, StaticBytes/binary>>,
    CH = hash:doit(ContractBytes),
    CID = contracts:make_id(CH, 2, <<0:256>>, 0),
    NewTx = contract_new_tx:make_dict(MP, CH, 2, 0),
    SO = swap_tx2:make_offer(
           MP, 0, 1000, 
           <<0:256>>, 0, round(OneVeo / 20), 
           CID, 2, round(OneVeo * 21 / 20), 
           1, Fee, Salt),
    SSO = keys:sign(SO),
    SwapTx = swap_tx2:make_dict(Pub, SSO, 1, Fee*2),
    UseTx = contract_use_tx:make_dict(Pub, CID, round(1.1*OneVeo), 0, 2, <<0:256>>, 0),
    Txs2 = [NewTx, SwapTx, UseTx],
    Tx3 = multi_tx:make_dict(Pub, Txs2, Fee*length(Txs2)),
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    mine_blocks(5),%Pub runs out of time to choose their bitcoin deposit address.

    Evidence = <<>>,
    Tx4 = contract_evidence_tx:make_dict(MP, ContractBytes, CID, Evidence, [], Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx5 = contract_timeout_tx2:make_dict(MP, CID, Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Bal1 = element(2, trees:get(accounts, MP)),

    Full = <<4294967295:32>>,
    Empty = <<0:32>>,
    SubAcc1 = sub_accounts:make_key(MP, CID, 2),
    Tx6 = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, [Empty, Full]),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Bal2 = element(2, trees:get(accounts, MP)),

    true = (Bal2 - Bal1) > ((OneVeo * 0.9) + governance:value(trees:get(governance, block_reward))),

    success;

test(61) ->
    io:fwrite("make a bid to buy veo, and the bitcoin are not delivered\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    timer:sleep(20),
    mine_blocks(2),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo*2, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    Salt = crypto:strong_rand_bytes(32),
    TID = swap_tx:trade_id_maker(MP, Salt),
    Receipt = receipts:new(TID, Pub, 1),
    ReceiptID = receipts:id(Receipt),

    %prove_address_timeout 
    %oracle_start_height
    %blockchain
    %amount
    %ticker
    %date
    %trade id
    OracleStartHeight = 5,
    ReusableSettings = 
        <<" int4 ", 
          (integer_to_binary(OracleStartHeight))/binary, 
          " .\" bitcoin\" .\" 1\" .\" BTC\" .\" Jan 1 2021\" ">>,
    Settings = <<" int 100 ", 
                 ReusableSettings/binary,
                 " int 1 binary 32 ", 
                 (base64:encode(TID))/binary>>,
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, CodeStatic} = file:read_file(PrivDir ++ "/buy_veo.fs"),
    {ok, CodeStatic2} = file:read_file(PrivDir ++ "/buy_veo_part2.fs"),

    Gas = 100000,
    HashStatic2 = base64:encode(hd(chalang:stack(chalang:test(compiler_chalang:doit(<<CodeStatic2/binary, " part2 ">>), Gas, Gas, Gas, Gas, [])))), %contract hash of the static part.

    HashStatic2def = 
          <<" macro part2 binary 32 ", 
            HashStatic2/binary,
          " ; ">>,

    StaticBytes = compiler_chalang:doit(<<HashStatic2def/binary, CodeStatic/binary>>),
    io:fwrite("static bytes \n"),
    io:fwrite(base64:encode(StaticBytes)),
    io:fwrite("\n"),
    SettingsBytes = compiler_chalang:doit(Settings),
    ContractBytes = <<SettingsBytes/binary, StaticBytes/binary>>,
    io:fwrite("length 1\n"),
    io:fwrite(integer_to_list(size(ContractBytes))),
    io:fwrite("\n"),
    %io:fwrite(disassembler:doit(ContractBytes)),
    %io:fwrite("\n"),
    CH = hash:doit(ContractBytes),
    CID = contracts:make_id(CH, 2, <<0:256>>, 0),
    NewTx = contract_new_tx:make_dict(Pub, CH, 2, 0),

    SO = swap_tx2:make_offer(
           MP, 0, 1000, 
           <<0:256>>, 0, round(OneVeo / 20), 
           CID, 2, round(OneVeo * 21 / 20), 
           1, Fee, Salt),
    SSO = keys:sign(SO),
    SwapTx = swap_tx2:make_dict(Pub, SSO, 1, Fee*2),
    UseTx = contract_use_tx:make_dict(Pub, CID, round(1.1*OneVeo), 0, 2, <<0:256>>, 0),
    Txs2 = [NewTx, SwapTx, UseTx],
    Tx3 = multi_tx:make_dict(Pub, Txs2, Fee*length(Txs2)),
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    %Pub accepts the limit order, which creates the swap receipt.
    absorb(Stx3),
    1 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    %Pub provides evidence of their bitcoin address, the contract transforms into a new contract.
    BitcoinAddress = <<"bitcoin_address">>,
    Sig = sign:sign(BitcoinAddress, Priv),
    Evidence = compiler_chalang:doit(<<" binary ", (integer_to_binary(size(Sig)))/binary, " ", (base64:encode(Sig))/binary, " .\" ", BitcoinAddress/binary, "\" ">>),
    io:fwrite("length evidence\n"),
    io:fwrite(integer_to_list(size(Evidence))),
    io:fwrite("\n"),
    Tx4 = contract_evidence_tx:make_dict(Pub, ContractBytes, CID, Evidence, [{receipts, ReceiptID}], Fee),
    Stx4 = signing:sign_tx(Tx4, Pub, Priv),
    absorb(Stx4),
    2 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Full = <<4294967295:32>>,
    Empty = <<0:32>>,
    Matrix = %same matrix from inside the forth code.
        [[Full, Empty],
         [Empty, Full]],
    Proofs = contract_evidence_tx:make_proof1(Matrix),


    Contract2 = <<ReusableSettings/binary,
                  " .\" ", BitcoinAddress/binary, "\" binary 32 ",
                  HashStatic2/binary,
                  " call "
                >>,
    Contract2Bytes = compiler_chalang:doit(Contract2),
    io:fwrite("length 2\n"),
    io:fwrite(integer_to_list(size(Contract2Bytes))),
    io:fwrite("\n"),
    CH2 = hash:doit(Contract2Bytes),
    CID2 = contracts:make_id(CH2, 2, <<0:256>>, 0),
    Tx5 = contract_timeout_tx2:make_dict(MP, CID, Fee, Proofs, CH2, hd(Matrix), CID2),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    3 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    %GetOracleCode = <<ReusableSettings/binary, CodeStatic2/binary, " .\" ", BitcoinAddress/binary, "\" Address ! Date ! Ticker ! Amount ! Blockchain ! drop Date @ Ticker @ Amount @ Address @ Blockchain @ oracle_builder ">>,
    GetOracleCode = <<ReusableSettings/binary, CodeStatic2/binary, " .\" ", BitcoinAddress/binary, "\" Address ! Date ! Ticker ! Amount ! Blockchain ! drop Blockchain @ Address @ Amount @ Ticker @ Date @ oracle_builder ">>,
    Question = hd(chalang:stack(chalang:test(compiler_chalang:doit(GetOracleCode), Gas, Gas, Gas, Gas, []))),
    Question2 = <<"The bitcoin address bitcoin_address is a valid address for that blockchain and has received more than or equal to 1 of BTC before Jan 1 2021">>,
    io:fwrite("\n"),
    io:fwrite(Question),
    io:fwrite("\n"),
    io:fwrite(Question2),
    io:fwrite("\n"),
    Question = Question2,
    Tx6 = oracle_new_tx:make_dict(MP, Fee, Question, OracleStartHeight, 0, 0), %Fee, question, start, id gov, govamount
    OID = oracle_new_tx:id(Tx6),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    
    Tx7 = oracle_bet_tx:make_dict(MP, Fee, OID, 2, 3000000), 
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx8 = oracle_close_tx:make_dict(MP,Fee, OID),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Evidence2 = compiler_chalang:doit(CodeStatic2),
    io:fwrite("length evidence2\n"),
    io:fwrite(integer_to_list(size(Evidence2))),
    io:fwrite("\n"),

    Tx9 = contract_evidence_tx:make_dict(Pub, Contract2Bytes, CID2, Evidence2, [{oracles, OID}], Fee),
    Stx9 = signing:sign_tx(Tx9, Pub, Priv),
    absorb(Stx9),
    2 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Tx10 = contract_timeout_tx2:make_dict(Pub, CID2, Fee),
    Stx10 = signing:sign_tx(Tx10, Pub, Priv),
    absorb(Stx10),
    3 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Tx11 = contract_simplify_tx:make_dict(Pub, CID, CID2, 0, Matrix, [Full, Empty], Fee),
    Stx11 = signing:sign_tx(Tx11, Pub, Priv),
    absorb(Stx11),
    4 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Bal1 = element(2, trees:get(accounts, Pub)),

    SubAcc1 = sub_accounts:make_key(Pub, CID, 1),
    Tx12 = contract_winnings_tx:make_dict(Pub, SubAcc1, CID, Fee, [Full, Empty]),
    Stx12 = signing:sign_tx(Tx12, Pub, Priv),
    absorb(Stx12),
    5 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),

    Bal2 = element(2, trees:get(accounts, Pub)),
    true = (Bal2 - Bal1) > ((OneVeo * 0.9)),

    success;
test(62) -> 
    %withdraw someone's money from an oracle for them.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    MP = constants:master_pub(),
    {Pub,Priv} = signing:new_key(),
    Fee = constants:initial_fee() + 20,
    Amount = 1000000000,
    Ctx = create_account_tx:make_dict(Pub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    %potential_block:save(),
    mine_blocks(5),


    %make 2 oracles. 
    Question1 = <<>>,
    Tx1 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question1, block:height() + 1, 0, 0), 
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    Question2 = <<"a">>,
    Tx2 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question2, block:height() + 1, 0, 0), 
    OID1 = oracle_new_tx:id(Tx1),
    OID2 = oracle_new_tx:id(Tx2),

    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %pub should bet in both.
    BetAmount = 100000000,
    Tx3 = oracle_bet_tx:make_dict(Pub, Fee, OID1, 1, BetAmount*2), 
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    absorb(Stx3),
    1 = many_txs(),
    Tx4 = oracle_bet_tx:make_dict(Pub, Fee, OID2, 1, BetAmount), 
    Stx4 = signing:sign_tx(Tx4, Pub, Priv),
    absorb(Stx4),
    2 = many_txs(),
    %MP should bet in other direction in one.
    Tx5 = oracle_bet_tx:make_dict(MP, Fee, OID1, 2, BetAmount), 
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    3 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    %settle both oracles so Pub wins.
    Tx6 = oracle_close_tx:make_dict(MP, Fee, OID1),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),
    Tx7 = oracle_close_tx:make_dict(MP, Fee, OID2),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    2 = many_txs(),
    mine_blocks(1),

    %MP should withdraw pubs money for them.
    UnmatchedTx = oracle_unmatched_tx:make_dict(Pub, Fee, OID1),
    UnmatchedTx2 = oracle_unmatched_tx:make_dict(Pub, Fee, OID2),
    WinningsTx = oracle_winnings_tx:make_dict(Pub, Fee, OID1),
    Tx8 = multi_tx:make_dict(MP, [UnmatchedTx, UnmatchedTx2, WinningsTx], Fee*3),
    %Tx8 = multi_tx:make_dict(MP, [WinningsTx], Fee*2),
    %Stx8 = signing:sign_tx(WinningsTx, Pub, Priv),
    io:fwrite(packer:pack(Tx8)),
    io:fwrite("\n"),
    Stx8 = keys:sign(Tx8),
    Acc1 = trees:get(accounts, Pub),
    absorb(Stx8),
    Acc2 = trees:get(accounts, Pub),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),
    %io:fwrite(packer:pack(Acc1#acc.balance)),
    %io:fwrite("\n"),
    io:fwrite(packer:pack(Acc2#acc.balance - Acc1#acc.balance)),
    true = ((BetAmount*4) == (Acc2#acc.balance - Acc1#acc.balance)),
    io:fwrite("\n"),

    success;
test(63) ->
    io:fwrite("test 63\n"),
    io:fwrite("test that we can create a contract, give it evidence, finalize it, simplify it, and withdraw winnings, all in the same block. also in the same flash loan.\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*2,
    {Pub,Priv} = signing:new_key(),
    OneVeo = 100000000,
    Ctx0 = create_account_tx:make_dict(Pub, OneVeo*2, Fee*5, MP),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),

    Salt = crypto:strong_rand_bytes(32),
    TID = swap_tx:trade_id_maker(MP, Salt),
    Receipt = receipts:new(TID, Pub, 1),
    ReceiptID = receipts:id(Receipt),

    Gas = 100000,
    Code2 = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 4294967295 ] \
 int 0 int 1">>),
    CH2 = hash:doit(Code2),
    ContractBytes = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ [ int 4294967295 , int 0 ] ,\
  [ int 0 , int 4294967295 ] ]\
binary 32 ",
               (base64:encode(CH2))/binary, 
" int 0 int 1">>),
    CH = hash:doit(ContractBytes),
    CID = contracts:make_id(CH, 2, <<0:256>>, 0),
    NewTx = contract_new_tx:make_dict(Pub, CH, 2, 0),
    SO = swap_tx2:make_offer(
           MP, 0, 1000, 
           <<0:256>>, 0, round(OneVeo / 20), 
           CID, 2, round(OneVeo * 21 / 20), 
           1, Fee, Salt),
    SSO = keys:sign(SO),
    SwapTx = swap_tx2:make_dict(Pub, SSO, 1, Fee*2),
    UseTx = contract_use_tx:make_dict(Pub, CID, round(1.1*OneVeo), 0, 2, <<0:256>>, 0),
    Txs2 = [NewTx, SwapTx, UseTx],
    Tx3 = multi_tx:make_dict(Pub, Txs2, Fee*length(Txs2)),
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    absorb(Stx3),
    1 = many_txs(),

    Evidence = compiler_chalang:doit(<<>>),
    Tx4 = contract_evidence_tx:make_dict(MP, ContractBytes, CID, Evidence, [{receipts, ReceiptID}], Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    2 = many_txs(),

    Full = <<4294967295:32>>,
    Empty = <<0:32>>,
    Matrix = %same matrix from inside the forth code.
        [[Full, Empty],
         [Empty, Full]],
    CID2 = contracts:make_id(CH2, 2,<<0:256>>,0),
    Proofs = contract_evidence_tx:make_proof1(Matrix),
    TimeoutTx = contract_timeout_tx2:make_dict(MP, CID, Fee, Proofs, CH2, [Full, Empty], CID2),
    SubAcc1 = sub_accounts:make_key(MP, CID, 2),
    Proofs2 = contract_evidence_tx:make_proof(2, Matrix),
    Txs5 = [TimeoutTx],
    Tx5 = multi_tx:make_dict(MP, Txs5, Fee*length(Txs5)),
    Stx5 = keys:sign(Tx5),
    %Stx5 = signing:sign_tx(Tx5, Pub, Priv),
    absorb(Stx5),
    3 = many_txs(),

    Tx8 = contract_evidence_tx:make_dict(MP, Code2, CID2, <<>>, [], Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    4 = many_txs(),
    TimeoutTx2 = contract_timeout_tx2:make_dict(MP, CID2, Fee),
    PayoutVector = %same as payout vector defined in Forth.
        [Empty, Full],
    SubAcc1_2 = sub_accounts:make_key(MP, CID2, 2),
    %WinningsTx = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, [Empty, Full], Proofs2),
    %WinningsTx2 = contract_winnings_tx:make_dict(MP, SubAcc1_2, CID2, Fee, PayoutVector),
    SimplifyTx = contract_simplify_tx:make_dict(MP, CID, CID2, 0, Matrix, PayoutVector, Fee), 
    WinningsTx2 = contract_winnings_tx:make_dict(MP, SubAcc1, CID, Fee, PayoutVector),
    Txs9 = [TimeoutTx2, SimplifyTx, WinningsTx2],
    %TODO
    %when we include the winningstx here, the tx gets dropped.
    Tx9 = multi_tx:make_dict(MP, Txs9, Fee*length(Txs9)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(Tx9)),
    io:fwrite("\n"),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    5 = many_txs(),
    mine_blocks(1),
    success;
test(64) ->
    io:fwrite("test 64\n"),
    io:fwrite("testing that txs can be made quickly from the internal api"),
    R = range(1, 64),
    X = lists:map(fun(_) ->
                          {NewPub,_NewPriv} = 
                              signing:new_key(),
                          api:spend(NewPub, 101)
                  end, R),
    X;
test(65) ->
    io:fwrite("test 65\n"),
    io:fwrite("minimal oracle_bet determinism test\n"),
    Question = <<>>,
    MP = constants:master_pub(),
    Fee = constants:initial_fee() + 20,
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),

    {Pub,Priv} = %signing:new_key(),
{<<4,246,183,170,64,225,129,182,244,36,226,50,201,
   110,202,228,43,130,188,42,132,193,89,13,17,212,
   208,198,132,149,228,81,100,38,74,181,88,173,47,
   176,168,181,171,49,143,82,22,8,126,2,35,171,23,
   186,217,38,216,117,31,9,172,147,164,213,45>>,
 <<243,186,70,5,153,164,216,216,40,5,63,71,205,
   81,107,2,46,155,95,10,192,134,183,77,18,229,
   159,193,132,59,205,212>>},
    Amount = 1000000000,
    Ctx0 = create_account_tx:make_dict(Pub, Amount, Fee, constants:master_pub()),
    Stx0 = keys:sign(Ctx0),
    absorb(Stx0),
    1 = many_txs(),
    mine_blocks(1),


    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, block:height() + 1, 0, 0), %Fee, question, start, id gov, govamount 
    OID = oracle_new_tx:id(Tx),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(5),
    true = 3 == (trees:get(oracles, OID))#oracle.type,
    Tx20 = oracle_bet_tx:make_dict(Pub, Fee, OID, 2, 50000000), 
    Stx20 = signing:sign_tx(Tx20, Pub, Priv),
    absorb(Stx20),
    1 = many_txs(),
    mine_blocks(1),

    Block = block:top(),
    #block{trees_hash = TH} = Block,
    <<20, 64, 169, 157, _:(8*28)>> = TH,

    Tx3 = oracle_bet_tx:make_dict(MP, Fee, OID, 1, 100000000),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    #block{trees_hash = TH2, trees = Trees} = block:top(),
    <<218, 205, 113, 244, _:(8*28)>> = TH2,

    Unmatched = Trees#trees5.unmatched,
    %UA = lists:map(fun(A) -> unmatched:deserialize(leaf:value(A)) end, trie:get_all(Unmatched, unmatched)),
    %io:fwrite({UA}), %[{unmatched, <<4, 246,...>>, <<160, 142,...>>, 0, <<0,0,0,0...>>},{unmatched, <<4, 133, ...>>, <<160,142,...>>, 50000000, <<0:?>>}, {<<4, 133, 89,...>>, 1}


    success;
test(66) ->
    %testing out updating from a checkpoint. This way we don't need to do multi-node testing for so much.
    restart_chain(),
    CFG = tree:cfg(amoveo),
    ID = cfg_verkle:id(CFG),
    ID = amoveo,
    LeafID = ids_verkle:leaf(CFG),
    StemID = ids_verkle:stem(CFG),

    mine_blocks(6),

    %verify that some data exists in the consensus state.

    DS = dump:top(StemID),
    DL = dump:top(LeafID),
    true = (DS > 1),
    true = (DL > 1),

    Block = block:top(),
    Trees = Block#block.trees,
    true = (Trees > 1),

    %record to the hard drive
    tree:quick_save(ID),

    %delete the consensus state stuff from ram.
    dump:delete_all(LeafID),
    dump:delete_all(StemID),
    timer:sleep(100),

    %verify that the data was deleted (or top pointer decremented below it.)
    1 = dump:top(StemID),
    1 = dump:top(LeafID),
    
    %restore the consensus state from the hard drive version.
    tree:reload_ets(ID),
    timer:sleep(200),

    %verify that the data is back.
    DS2 = dump:top(StemID),
    DL2 = dump:top(LeafID),
    if
        (not (DS == (DS2 - 1))) ->
            io:fwrite({stem, DS, DS2});
        (not (DL == DL2)) ->
            io:fwrite({leaf, DL, DL2});
        true -> ok
    end,

    success;
test(68) ->
    %making and verifying verkle proofs. update 52.
    restart_chain(),
    mine_blocks(4),
    true = forks:get(52) < block:height(),
    TP = (block:top())#block.trees,
    {Proof, Leaves} = trees2:get_proof([{accounts, keys:pubkey()}], TP, fast),
    {true, _ProofTree} = trees2:verify_proof(Proof, Leaves, block:height()),
    success;
    
 
test(unused) ->
    io:fwrite("test stablecoin_new_tx\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(2),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Fee = constants:initial_fee() + 20,

    %create a new stablecoin.
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, PerpetualStablecoin} = file:read_file(PrivDir ++ "/perpetual_stablecoin.fs"),
    PerpetualStablecoinBytes
        = compiler_chalang:doit(
            PerpetualStablecoin),



    Code = compiler_chalang:doit(
             <<"macro [ nil ;\
macro , swap cons ;\
macro ] swap cons reverse ;\
[ int 0, int 0, int 4294967295]\
int 0 int 1" >>),
    CodeHash = hash:doit(Code),
    TDuration = 5,
    UDuration = 5, 
    Period = 10,
    Expiration = 9,
    UTrigger = 50000,%how overcollateralized to trigger the auction out of 1000000, one million. this is 5%
    CStep = 100000,%out of 1000000, one million. this is 10%
    Margin = 10000000,%Get * 10000000 / Spend
    Salt = crypto:strong_rand_bytes(3),
    SID = stablecoin_new_tx:id_maker(MP, Salt),
    Source = <<0:256>>,
    SourceType = 0,
    %CID = contracts:make_id(CodeHash, 2, Source, SourceType),
    Tx1 = stablecoin_new_tx:make_dict(
            MP, Salt, Source, SourceType, CodeHash,
            TDuration, UDuration, Period,
            Expiration, UTrigger, CStep,
            Margin, Fee),
    ByteCode = <<2, 6, (<<Margin:48>>)/binary, 0, (<<Expiration:32>>)/binary, 2, 32, CodeHash/binary, 113>>,
    CH = hash:doit(ByteCode),
    CID = contracts:make_id(CH, 2, Source, SourceType),

    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    mine_blocks(1),

    %check replay attack vulnerability.
    absorb(Stx1),
    0 = many_txs(),
    %check that the stablecoin object looks right.
    #stablecoin{
               id = SID,
               auction_mode = false,
               source = Source,
               amount = 0,
               code_hash = CodeHash,
               timeout = 8,
               max_bid_pubkey = <<0:520>>,
               max_bid_amount = 0,
               timelimit_auction_duration = 5,
               undercollateralization_auction_duration = 5,
               undercollateralization_price_trigger = 50000,
               collateralization_step = 100000,
               margin = 10000000,
               period = 10
              } = trees:get(stablecoins, SID),
    %check that it creates the finite contract correctly
    #contract{
               code = CH,
               many_types = 2,
               nonce = 0,
               last_modified = 0,
               delay = 0,
               closed = 0,
               result = <<0:256>>,
               source = <<0:256>>,
               source_type = 0,
               sink = <<0:256>>,
               volume = 0
             } = trees:get(contracts, CID),
    success;
test(unused) ->
    io:fwrite("test stablecoin_new_tx in a multi-tx"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(1),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Fee = constants:initial_fee() + 20,

    %make the tx.

    success;
test(unused) ->
    %stablecoin timelimit auction
    %create the stablecoin
    %buy stablecoins
    %check replay of the buy_stablecoins tx
    %trigger the timelimit auction.
    %check for replay
    %check that the stablecoin object updated correctly.

    %withdraw stablecoins into finite1
    %check for replay
    %check that you received the finite1 and lost stablecoins and that the highest bid was partially refunded and that the stablecoin contract updated correctly.

    %make a bid in the auction
    %check for replay
    %make a bid at a lower price, it should fail.

    %withdraw stablecoins into finite1
    %check for replay
    %check that you received the finite1 and lost stablecoins.


    %make a bid at a higher price.
    %check for replay
    %check the refund of the first bid worked.
    %check that the stablecoin object updated correctly.
    %end the auction with a different account.
    %check for replay
    %check that the finite2 contract was created, and was updated to show the correct amount of outstanding shares etc.
    %check that the stablecoin updated correctly.
    %check that the winning bid account received long-veo2 + finite1
    %check that the stablecoins are still spendable.
    sucess;
test(unused) ->
    %stablecoin undercollateralization auction
    %create the stablecoin
    %buy stablecoins
    %trigger the undercollateralization auction.
    %check for replay
    %check that the account received finite1 and lost veo.
    %check that the stablecoin object updated correctly.

    %withdraw stablecoins into finite1
    %check for replay
    %check that you received the finite1 and lost stablecoins.


    %make a bid in the auction
    %check for replay
    %make a bid at a lower price, it should fail.

    %withdraw stablecoins into source
    %check for replay
    %check that you received the source and lost stablecoins and that the highest bid was partially refunded and that the stablecoin contract updated correctly.

    %make a bid at a higher price.
    %check for replay
    %check the refund of the first bid worked.
    %check that the stablecoin object updated correctly.
    %end the auction with a different account.
    %check for replay
    %check that the finite2 contract was created, and was updated to show the correct amount of outstanding shares etc.
    %check that the stablecoin updated correctly.
    %check that the winning bid account received long-veo2
    %check that the stablecoins are still spendable.
    sucess;
test(67) -> 
    io:fwrite("test 66\n"),
    io:fwrite("testing determinism when contract evidence tx fails.\n"),

    true = forks:get(52) > 6,

    %seems like receipts wasn't deterministic after the update for fork 52, this test is to see.

    %make a contract. provide invalid evidence. check the root hash.
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Fee = constants:initial_fee()*100,
    Code = compiler_chalang:doit(
             <<"fail \
">>),
    CH = hash:doit(Code),
    Many = 2,
    Tx = contract_new_tx:make_dict(MP, CH, Many, Fee),
    CID = contracts:make_id(CH, Many,<<0:256>>,0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    1 = many_txs(),
    mine_blocks(1),
    
    Tx2 = contract_evidence_tx:make_dict(MP, Code, CID, <<>>, [], Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),
    mine_blocks(1),
    
    RootHash = trees:root_hash(
                 (block:top())#block.trees),
    %io:fwrite({(block:top())#block.trees}),
                            %{{trees5,45,1,1,1,
                            %  473,1,1,1,5,1,1,1,
                            %  1}},
    <<191,217,58,101,_:(8*28)>> = RootHash,
    success;

test(empty) ->
    io:fwrite("test 55\n"),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    MP = constants:master_pub(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),

    success;
test(many_accounts) ->
    N = 39,
    All = make_many(N, 0, []),
    lists:map(fun(_) ->
                      timer:sleep(1000),
                      {P, _} = signing:new_key(),
                      spawn(fun() ->
                                    api:spend(P, 10)
                            end)
              end, All),
    
    success;
test(69) ->
    %inserting and reading from the jobs part of the verkle tree.
    {Pub,_NewPriv} = signing:new_key(),
    %Pub = trees2:compress_pub(NewPub),
    ID = <<255:8, 0:248>>,
    Job = #job{id = ID, worker = <<0:264>>, boss = <<0:264>>, value = 1, salary = 1, balance = 1, time = 1},

    SJ = trees2:serialize(Job),
    SL = dump:put(SJ, jobs_dump),
    SJ = dump:get(SL, jobs_dump),
    


    Acc = #acc{pubkey = Pub, balance = 1, nonce = 1, 
               bets = 0, bets_hash = <<0:256>>},
    Loc = 1,
%    {_, Leaves1} = trees2:get_proof([{jobs, ID}, {accounts, Pub}], 
%                                    Loc, fast),
    Loc2 = trees2:store_things([Job, Acc], Loc),

    [{{jobs,ID},#job{}}] = trees2:get([{jobs, ID}], Loc2),
    success;
test(70) ->
    io:fwrite("job create and job salary receive tx types."),
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(4),
    MP = constants:master_pub(),
    Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    Fee = constants:initial_fee()*100,
    N64 = 18446744073709551616,
    Salary = N64 div (50000), %about 100% annually.
    Tx1 = job_create_tx:make_dict(Pub, 10000000, Salary, 100000000, Fee),
    ID = job_create_tx:id(Tx1),
    
    Stx1 = keys:sign(Tx1),
    %io:fwrite({Stx1}),
    absorb(Stx1),
    1 = many_txs(),

    mine_blocks(4),
    0 = many_txs(),

    Tx2 = job_receive_salary_tx:make_dict(ID, Fee),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    {NewPub,NewPriv} = signing:new_key(),

    Ctx = create_account_tx:make_dict(
            NewPub, 1000000000, Fee, constants:master_pub()),
    Sctx = keys:sign(Ctx),
    absorb(Sctx),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),


    Tx3 = job_buy_tx:make_dict(NewPub, ID, 50000000, Fee),
    Stx3 = signing:sign_tx(Tx3, NewPub, NewPriv),
    absorb(Stx3),
    1 = many_txs(),

    Tx4 = job_adjust_tx:make_dict(ID, 20000000, 70000000, Fee),
    Stx4 = signing:sign_tx(Tx4, NewPub, NewPriv),
    absorb(Stx4),
    2 = many_txs(),
    
    %getting a raise.
    Tx5 = job_team_adjust_tx:make_dict(ID, Salary * 3 div 2, 30000000,
                                  70000000, Fee),
    Stx5 = keys:sign(Tx5),
    SStx5 = signing:sign_tx(Stx5, NewPub, NewPriv),
    %absorb(Stx5),
    %2 = many_txs(),
    absorb(SStx5),
    3 = many_txs(),


    success;
test(71) ->
    io:fwrite("read and write futarchy stuff to database."),

    {Pub,_NewPriv} = signing:new_key(),

    Salt = <<0:256>>,
    FID = futarchy:make_id(Pub, Salt, block:height()),
    F = #futarchy{decision_oid = <<0:256>>, goal_oid = <<0:256>>, batch_period = 1, creator = Pub, fid = FID},
    
    FU0 = #futarchy_unmatched{owner = Pub, futarchy_id = FID, decision = 0, goal = 0, revert_amount = 1234567, limit_price = 555444, ahead = <<0:256>>, behind = <<0:256>>},
    FU = futarchy_unmatched:make_id(FU0, 0),
    FUID = FU#futarchy_unmatched.id,

    Salt2 = <<2:256>>,
%    FMID = futarchy_matched:make_id(Pub, Salt2),
    %FMID = hash:doit(<<FID/binary, Pubkey/binary, 1:32>>),
    FMID = <<1:256>>,
    FM = #futarchy_matched{owner = Pub, futarchy_id = FID, decision = 0, revert_amount = 1234567, win_amount = 10101010, id = FMID, goal = 0},

    SF = trees2:serialize(F),
    SFU = trees2:serialize(FU),
    SFM = trees2:serialize(FM),


    SFP = dump:put(SF, futarchy_dump),
    SFUP = dump:put(SFU, futarchy_unmatched_dump),
    SFMP = dump:put(SFM, futarchy_matched_dump),
    SF = dump:get(SFP, futarchy_dump),
    SFU = dump:get(SFUP, futarchy_unmatched_dump),
    SFM = dump:get(SFMP, futarchy_matched_dump),


    Loc2 = trees2:store_things([F, FU, FM], 1),

    X = trees2:get([{futarchy, FID}, {futarchy_unmatched, FUID}, {futarchy_matched, FMID}], Loc2),
    %io:fwrite({X}),

    success;
test(72) ->
    %testing the futarchy tx types.
    %add unmatched bets to the order book, then use futarchy_unmatched to get your money out again.

    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    mine_blocks(6),
    MP = constants:master_pub(),
    %Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    Pub = keys:pubkey(),
    Fee = constants:initial_fee()*2,

    Tx1 = oracle_new_tx:make_dict(Pub, Fee, <<"Decision">>, block:height()-1, 0, 0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    DOID = oracle_new_tx:id(Tx1),
    Tx2 = oracle_new_tx:make_dict(Pub, Fee, <<"Goal">>, block:height() + 1, 0, 0),
    GOID = oracle_new_tx:id(Tx2),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),
    mine_blocks(1),

    VEO = 100000000,

    TrueLiquidity =  2 * VEO,
    FalseLiquidity = 1 * VEO,
    Period = 1,
    Salt2 = <<22:256>>,
    Tx3 = futarchy_new_tx:make_dict(
            Pub, DOID, GOID, Period, 
            TrueLiquidity, FalseLiquidity, Fee, 
            block:height()),
    Tx3Nonce = Tx3#futarchy_new_tx.nonce,
    FID = futarchy:make_id(Pub, <<Tx3Nonce:256>>, block:height()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    %mine_blocks(1),
    %0 = many_txs(),
    
    Decision = 1,
    Goal = 1,
    OtherDecision = 0,
    OtherGoal = 0,
    LimitPrice = round(math:pow(2, 14)),
    FutarchyHash1 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {UID1, _, Tx4} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, Goal, 1*VEO,
                    LimitPrice, FutarchyHash1, Fee),
    %io:fwrite(Tx4),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    2 = many_txs(),

    %io:fwrite({trees:get(futarchy, FID)}),
    %1 = 2,

    FutarchyHash2 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {UID2, _, Tx5} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, Goal, 1*VEO,
                    LimitPrice-5, FutarchyHash2, Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    3 = many_txs(),
    
    FutarchyHash3 = (trees:get(futarchy, FID))#futarchy.root_hash,
%    FutarchyHash3 = ok,
    {UID3, _, Tx6} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, Goal, 1*VEO,
                    LimitPrice-8, FutarchyHash3, Fee),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    4 = many_txs(),

    FutarchyHash4 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {UID4, _, Tx7} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, OtherGoal, 
                    round(2.5*VEO), LimitPrice-3, FutarchyHash4, Fee),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    5 = many_txs(),

    FutarchyHash5 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {UID5, _, Tx8} = futarchy_bet_tx:make_dict(
                    Pub, FID, OtherDecision, Goal, 
                    round(VEO), LimitPrice-1, FutarchyHash5, Fee),
    Stx8 = keys:sign(Tx8),
    io:fwrite("absorb tx 8\n"),
    absorb(Stx8),
    6 = many_txs(),

    %mine_blocks(1),
    %0 = many_txs(),

    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx9 = oracle_bet_tx:make_dict(MP, Fee, DOID, 1, OIL+1),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    7 = many_txs(),
    mine_blocks(1),
    %0 = many_txs(),

    Tx10 = oracle_close_tx:make_dict(constants:master_pub(),Fee, DOID),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    1 = many_txs(),
    mine_blocks(2),
    %0 = many_txs(),

    Tx11 = futarchy_resolve_tx:make_dict(
             MP, FID, DOID, Fee),
    Stx11 = keys:sign(Tx11),
    absorb(Stx11),
    1 = many_txs(),

    potential_block:new(),
    block:height(),
    %1=2,
    %do potential_block:new().

    mine_blocks(1),
    0 = many_txs(),

    UIDs = [UID1, UID2, UID3, UID4, UID5],

    Unmatcheds = lists:map(
                   fun(U) ->
                           Tx12 = futarchy_unmatched_tx:make_dict(
                                    Pub, U, Fee),
                           Stx12 = keys:sign(Tx12),
                           io:fwrite("absorb\n"),
                           absorb(Stx12)
                           %trees:get(futarchy_unmatched,U)
                   end, UIDs),
    %io:fwrite(Unmatcheds),
    5 = many_txs(),



    %withdraw unmatched/matched
%    Tx12 = futarchy_unmatched_tx:make_dict(MP, UID, Fee),
%    Stx12 = keys:sign(Tx12),
%    absorb(Stx12),
%    1 = many_txs(),
    
%    Tx13 = futarchy_matched_tx:make_dict(MP, MID, Reverted, Fee),
%    Stx13 = keys:sign(Tx13),
%    absorb(Stx13),
%    2 = many_txs(),


    
    success;
test(73) ->
    %testing futarchy_bet_tx

    % fully matched, ran out of liquidity in the lmsr step. 

    restart_chain(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    Fee = constants:initial_fee()*2,
    Tx1 = oracle_new_tx:make_dict(Pub, Fee, <<"Decision">>, block:height() + 1, 0, 0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    DOID = oracle_new_tx:id(Tx1),
    Tx2 = oracle_new_tx:make_dict(Pub, Fee, <<"Goal">>, block:height() - 1, 0, 0),
    GOID = oracle_new_tx:id(Tx2),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),
    mine_blocks(1),

    VEO = 100000000,

    TrueLiquidity =  20 * VEO,
    FalseLiquidity = 10 * VEO,
    Period = 1,
    Salt2 = <<22:256>>,
    Tx3 = futarchy_new_tx:make_dict(
            Pub, DOID, GOID, Period, 
            TrueLiquidity, FalseLiquidity, Fee, 
            block:height()),
    Tx3Nonce = Tx3#futarchy_new_tx.nonce,
    FID = futarchy:make_id(Pub, <<Tx3Nonce:256>>, block:height()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Decision = 1,
    Goal = 1,
    OtherDecision = 0,
    OtherGoal = 0,
    LimitPrice = round(math:pow(2, 32)-math:pow(2, 16)),
    FutarchyHash1 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {_UID1, MID1, Tx4} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, Goal, VEO,
                    LimitPrice, FutarchyHash1, Fee),
    %io:fwrite(Tx4),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx5 = oracle_bet_tx:make_dict(MP, Fee, DOID, 1, OIL+1),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    Tx6 = oracle_close_tx:make_dict(MP,Fee, DOID),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    Tx7 = futarchy_resolve_tx:make_dict(
             MP, FID, DOID, Fee),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx8 = futarchy_matched_tx:make_dict(Pub, MID1, 0, Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    
    mine_blocks(1),

    %todo
    %resolve the futarchy, 
    %futarchy_to_binary_tx, to convert it to a subcurrency, 
    %get the veo out.

    success;
test(74) ->
    %testing futarchy_bet_tx
    % fully matched, ran out of liquidity while matching a trade.
    restart_chain(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    Fee = constants:initial_fee()*2,
    Tx1 = oracle_new_tx:make_dict(Pub, Fee, <<"Decision">>, block:height() + 1, 0, 0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    DOID = oracle_new_tx:id(Tx1),
    Tx2 = oracle_new_tx:make_dict(Pub, Fee, <<"Goal">>, block:height() - 1, 0, 0),
    GOID = oracle_new_tx:id(Tx2),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),
    mine_blocks(1),

    VEO = 100000000,

    TrueLiquidity =  2 * VEO,
    FalseLiquidity = 1 * VEO,
    Period = 1,
    Salt2 = <<22:256>>,
    Tx3 = futarchy_new_tx:make_dict(
            Pub, DOID, GOID, Period, 
            TrueLiquidity, FalseLiquidity, Fee, 
            block:height()),
    Tx3Nonce = Tx3#futarchy_new_tx.nonce,
    FID = futarchy:make_id(Pub, <<Tx3Nonce:256>>, block:height()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Decision = 1,
    Goal = 1,
    OtherDecision = 0,
    OtherGoal = 0,

    LimitPrice = round(math:pow(2, 31)*1.01),
    FutarchyHash1 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {UID1, _MID1, Tx4} = futarchy_bet_tx:make_dict(
                           Pub, FID, Decision, Goal, VEO * 2,
                           LimitPrice, FutarchyHash1, Fee),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),

    FutarchyHash2 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {_UID2, MID2, Tx5} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, OtherGoal, VEO,
                    LimitPrice, FutarchyHash2, Fee),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    2 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),
    %1=2,

    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx6 = oracle_bet_tx:make_dict(MP, Fee, DOID, 1, OIL+1),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),

    Tx7 = oracle_close_tx:make_dict(constants:master_pub(),Fee, DOID),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    2 = many_txs(),

    Tx8 = futarchy_resolve_tx:make_dict(
             MP, FID, DOID, Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    3 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx9 = futarchy_unmatched_tx:make_dict(MP, UID1, Fee),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    1 = many_txs(),

    Tx10 = futarchy_matched_tx:make_dict(Pub, MID2, 0, Fee),
    Stx10 = keys:sign(Tx10),
    absorb(Stx10),
    2 = many_txs(),
    
    mine_blocks(1),
    0 = many_txs(),

    ok;
test(75) ->
    %testing futarchy_bet_tx
    % price went out of range during the lmsr step, so partially unmatched
    restart_chain(),
    mine_blocks(6),
    MP = constants:master_pub(),
    Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    Fee = constants:initial_fee()*2,
    Tx1 = oracle_new_tx:make_dict(Pub, Fee, <<"Decision">>, block:height() + 1, 0, 0),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),
    DOID = oracle_new_tx:id(Tx1),
    Tx2 = oracle_new_tx:make_dict(Pub, Fee, <<"Goal">>, block:height() - 1, 0, 0),
    GOID = oracle_new_tx:id(Tx2),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),
    mine_blocks(1),

    VEO = 100000000,

    TrueLiquidity =  2 * VEO,
    FalseLiquidity = 1 * VEO,
    Period = 1,
    Salt2 = <<22:256>>,
    Tx3 = futarchy_new_tx:make_dict(
            Pub, DOID, GOID, Period, 
            TrueLiquidity, FalseLiquidity, Fee, 
            block:height()),
    Tx3Nonce = Tx3#futarchy_new_tx.nonce,
    FID = futarchy:make_id(Pub, <<Tx3Nonce:256>>, block:height()),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Decision = 1,
    Goal = 1,
    OtherDecision = 0,
    OtherGoal = 0,
    %LimitPrice = round(math:pow(2, 32)-math:pow(2, 16)),
    LimitPrice = round(math:pow(2, 16)*0.6),
    FutarchyHash1 = (trees:get(futarchy, FID))#futarchy.root_hash,
    {_UID1, MID1, Tx4} = futarchy_bet_tx:make_dict(
                    Pub, FID, Decision, Goal, VEO*20,
                    LimitPrice, FutarchyHash1, Fee),
    %io:fwrite(Tx4),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx5 = oracle_bet_tx:make_dict(MP, Fee, DOID, 1, OIL+1),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    Tx6 = oracle_close_tx:make_dict(MP,Fee, DOID),
    Stx6 = keys:sign(Tx6),
    absorb(Stx6),
    1 = many_txs(),

    mine_blocks(1),
    0 = many_txs(),

    Tx7 = futarchy_resolve_tx:make_dict(
             MP, FID, DOID, Fee),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    1 = many_txs(),
    mine_blocks(1),
    0 = many_txs(),

    Tx8 = futarchy_matched_tx:make_dict(Pub, MID1, 0, Fee),
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    1 = many_txs(),
    
    mine_blocks(1),

    %todo
    %resolve the futarchy, 
    %futarchy_to_binary_tx, to convert it to a subcurrency, 
    %get the veo out.

    success;


    
test(80) ->
    %testing the tx_reserve.
    %TODO

    restart_chain(),
    mine_blocks(4),
    Me = keys:pubkey(),
    MP = constants:master_pub(),
    %Pub = base64:decode(<<"BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=">>),
    {Pub,_Priv} = signing:new_key(),
    Fee = constants:initial_fee()*2,

%    Tx1 = spend_tx:make_dict(Pub, 10, Fee, Me),
    Tx1 = create_account_tx:make_dict(Pub, 10, Fee, Me),
    Stx1 = keys:sign(Tx1),
    absorb(Stx1),
    1 = many_txs(),

    Tx2 = spend_tx:make_dict(Pub, 10, Fee, Me),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    2 = many_txs(),

    Tx3 = spend_tx:make_dict(Pub, 10, Fee, Me),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    3 = many_txs(),


%absorb async works.
    tx_pool:dump(),
    absorb(Stx1),
    1 = many_txs(),
    absorb(Stx2),

    tx_pool_feeder:absorb_async([Stx1, Stx2, Stx3]),
    timer:sleep(100),
    3 = many_txs(),

%absorb works.
    tx_pool:dump(),
    absorb(Stx1),
    1 = many_txs(),
    absorb(Stx2),

    tx_pool_feeder:absorb_async([Stx1, Stx2, Stx3]),
    timer:sleep(100),
    3 = many_txs(),

%reserve works.
    tx_pool:dump(),
    absorb(Stx1),
    1 = many_txs(),
    absorb(Stx2),

    tx_reserve:restore(),
    timer:sleep(100),
    potential_block:new(),
    3 = many_txs(),


%mine block doesn't drop txs
    tx_pool:dump(),
    absorb(Stx1),
    1 = many_txs(),
    absorb(Stx2),
    2 = many_txs(),

    mine_block_no_tx_reserve_dump(),

%    potential_block:new(),
%    api:mine_block(),
%    timer:sleep(100),
    0 = many_txs(),
    tx_reserve:restore(),
    timer:sleep(3000),
%    io:fwrite({tx_pool:get(), tx_reserve:all()}),
    potential_block:new(),
    1 = many_txs(),


    
    %lets confirm that tx_pool_feeder:absorb_async fails. after an invalid tx, it drops the following txs.

    %then, lets check that tx_pool_feeder:absorb succeeds in that situation.

    %finally, lets make sure that tx_reserve:restore() works for this situation as well.


    success;

test(futarchy) ->
    S = success,
    S = test(71),
    S = test(72),
    S = test(73),
    S.


    


make_many(I, _, R) when I < 1 ->
    R;
make_many(N, X, R) ->
    make_many(N-1, X, [X|R]).


test35(_, _, _, 0) -> ok;
test35(D, S, P, N) ->
    %true = signing:verify_sig(D, S, P),
    file:read_file("../../../../../../temp2.txt"),
    test35(D, S, P, N-1).
    
test18(0) -> success;
test18(N) ->
    test({17, N}),
    test18(N-1).
spend_lots(0, _, _, _) -> ok;
spend_lots(N, 0, M, P) -> 
    potential_block:new(),
    block:mine(100000),
    %%%timer:sleep(300),
    spend_lots(N-1, M, M, P);
spend_lots(N, M, L, P) ->
    io:fwrite("spend "),
    io:fwrite(integer_to_list(M)),
    io:fwrite("\n"),
    Fee = constants:initial_fee() + 20,
    Ctx = spend_tx:make_dict(P, 1, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    %%%%timer:sleep(30),
    spend_lots(N, M-1, L, P).
create_accounts(0, Salt) -> ok;
create_accounts(N, Salt) ->
    io:fwrite("create account "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    M = N + (1000*Salt),
    {NewPub,_NewPriv} = signing:new_key(<<M:256>>),
    Fee = constants:initial_fee() + 20,
    Ctx = create_account_tx:make_dict(NewPub, 1, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    create_accounts(N-1, Salt).
slash_exists([]) -> false;
slash_exists([Tx|T]) ->
    is_slash(Tx) or slash_exists(T).
is_slash(STx) ->
    Tx = signing:data(STx),
    channel_slash_tx:is_tx(Tx).
	     
mine_blocks(Many) when Many < 1 -> 
    headers:top(),
    %tx_pool:dump(),
    potential_block:new(),%crashes here.
    potential_block:read(),
    TP = tx_pool:get(),
    %timer:sleep(10),
    %Txs = TP#tx_pool.txs,
    %Height = TP#tx_pool.height,
    %PB = block:get_by_height(Height),
    %Hash = block:hash(PB),
    %{ok, Top} = headers:read(Hash),
    ok;
mine_blocks(Many) ->
    %only works if you set the difficulty very low.
    tx_reserve:dump(),
    mine_block_no_tx_reserve_dump(),
    tx_pool:dump(),
    mine_blocks(Many-1).
mine_block_no_tx_reserve_dump() ->
    headers:top(),
    potential_block:read(),
    TP = tx_pool:get(),
    Txs = TP#tx_pool.txs,
    Height = TP#tx_pool.height,
    PB = block:get_by_height(Height),
    Hash = block:hash(PB),
    {ok, Top} = headers:read(Hash),
    Block = block:make(Top, Txs, block_trees(PB), keys:pubkey()),
    block:mine(Block, 10000),
    wait_till_next_block(Height, 100).
    
wait_till_mineable(N, 0) ->
    io:fwrite({N, "failed to create a potential block"}),
    1=2,
    ok;
wait_till_mineable(Height, N) ->
    PB = block:get_by_height(Height),
    case PB of
        empty ->
            timer:sleep(50),
            wait_till_mineable(Height, N-1);
        _ -> ok
    end.
wait_till_next_block(_Height, 0) ->
    io:fwrite("failed to mine a block"),
    1=2,
    ok;
wait_till_next_block(Height, N) ->
    %H2 = block:height(),
    TP = tx_pool:get(),
    H2 = TP#tx_pool.height,
    if
        H2 > Height -> wait_till_mineable(H2, 200),
                       timer:sleep(300);
        true ->
            timer:sleep(50),
            wait_till_next_block(Height, N-1)
    end.

test24(I) ->
    %set forks:get(10) to 6 for this test.

    %test how oracles fail if the oracle occurs during different stages of their evolution.
    Question = <<>>,
    %OID = crypto:strong_rand_bytes(32),
    Fee = constants:initial_fee() + 20,
    restart_chain(),
    %headers:dump(),
    %block:initialize_chain(),
    %%%timer:sleep(150),
    %tx_pool:dump(),
    %%%timer:sleep(150),
    mine_blocks(3),
    %%%timer:sleep(150),
    if
	I > 0 -> mine_blocks(I);
		 %%%timer:sleep(100);
	true -> ok
    end,
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, block:height() + 1, 0, 0), %Fee, question, start, id gov, govamount
    OID = oracle_new_tx:id(Tx),
    Stx = keys:sign(Tx),
    absorb(Stx),
    %%%timer:sleep(150),
    potential_block:new(),
    %%%timer:sleep(150),
    mine_blocks(1),
    
    OIL_gov = trees:get(governance, oracle_initial_liquidity),
    OIL = governance:value(OIL_gov),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL+1), 
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    %%%timer:sleep(150),
    mine_blocks(1),

    Tx3 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    %%%timer:sleep(100),
    mine_blocks(1),

    Oracle = trees:get(oracles, OID),
    Tx4 = oracle_unmatched_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    %%%timer:sleep(100),
    Tx5 = oracle_winnings_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    %%%timer:sleep(100),
    mine_blocks(1),
    success.

many_txs() ->
    potential_block:read(),
    X = length(element(2, tx_pool:get())),
    %%%timer:sleep(10),
    X.
    

vm(Code) ->
    ExampleData = chalang:data_maker(1000000,1000000,1000,1000,<<>>,<<>>,chalang:new_state(0,0,0),32,2,false),
    Data2 = chalang:run5(Code, ExampleData),
    %io:fwrite("test txs vm "),
    %io:fwrite(packer:pack(Data2)),
    %io:fwrite("\n"),
    chalang:stack(Data2).
    
range(N, N) -> [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

make_pairs_from(Keys) ->
    Types = lists:map(fun(X) -> element(1, X) end,
                      Keys),
    Types2 = lists:map(fun(X) ->
                               case X of
                                   acc -> accounts;
                                   contracts -> contracts;
                                   sub_accounts -> sub_accounts;
                                   futarchy -> futarchy;
                                   oracle -> oracles
                               end
                       end, Types),
    true = (length(Types2) == length(Keys)),
    Keys2 = to_keys(Types2, Keys).
    %Keys2 = trees2:to_keys(Keys),
%TKs = make_pairs(Types2, Keys2).

to_keys([], []) -> [];
to_keys([accounts|T], [#acc{pubkey = P}|K]) -> 
    [{accounts, P}|to_keys(T, K)];
to_keys([Type|T], [X|K]) -> 
    [{Type, trees2:key(X)}|to_keys(T, K)].
    

make_pairs([], []) ->
    [];
make_pairs([H|T], [A|B]) ->
    [{H, A}|make_pairs(T, B)].

%tree_size(Type, [{Type, S}|_]) -> S;
%tree_size(Type, [{_, _}|T]) ->
%    tree_size(Type, T).

make_leaves(L) ->
    trees2:cs2v(L).

remove_repeat_leaves([], _) -> [];
remove_repeat_leaves([L|T], D) -> 
%    K = L#leaf.key,
    K = element(2, L),
    D2 = dict:store(K, true, D),
    Rest = remove_repeat_leaves(T, D2),
    case dict:find(K, D) of
        {ok, _} -> Rest;
        _ -> [L|Rest]
    end.


%    CFG = tree:cfg(amoveo),
%    make_leaves2(CFG, L).
%make_leaves2(_, []) -> [];
%make_leaves2(CFG, [{Type, Key}|T]) -> 
    %an empty slot.
%    make_leaves2(CFG, T);
%make_leaves2(CFG, [L|T]) -> 
    %[Key] = trees2:to_keys([L]),
%    <<_:256>> = Key,
%    Z = <<0:64>>,
%    N2 = <<0:(32*8)>>,
%    R = leaf_verkle:new(Key, <<0:(32*8)>>, <<0:64>>, CFG),
    %[R|make_leaves2(CFG, T)].

remove_empties([]) -> [];
remove_empties([{_, _}|T]) -> remove_empties(T);
remove_empties([H|T]) -> 
    [H|remove_empties(T)].
