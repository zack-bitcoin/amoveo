-module(test_txs).
-export([test/0, test/1, mine_blocks/1, absorb/1]).
 
-include("../../records.hrl").
test() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    S = success,
    S = test(1),%create account, spend, delete %S = test(2),%repo tx
    S = test(3),%channel team close
    S = test(4),%channel timeout
    S = test(5),%account delete, channel timeout
    S = test(6),%channel slash
    S = test(8),%channel solo close - channel team close
    S = test(9),%channel slash - channel team close
    S = test(7),%existence
    S = test(14),%financial options
    S = test(12),%multiple bets in a single channel
    S = test(15),%automatic channel slash
    %warning! after running test(11), we can no longer run other tests. because test(11) mines blocks, so tx_pool:dump can no longer undo transactions.
    S = test(13),%testing governance
    S = test(11),%try out the oracle
    S = test(16),%try out the oracle further
    %S = test(17),%blocks filled with create account txs
    timer:sleep(300),
    S.
absorb(Tx) -> 
    %tx_pool_feeder:absorb_unsafe(Tx).
    tx_pool_feeder:absorb(Tx).
    %timer:sleep(400).
block_trees(X) ->
    X#block.trees.
test(1) ->
    io:fwrite(" create_account tx test \n"),
    %create account, spend, delete account
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height_in_chain(0, headers:top()),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    {Ctx, _} = create_account_tx:new(NewPub, 100000000, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    Ctx2 = spend_tx:make_dict(NewPub, 10, Fee, constants:master_pub()),
    Stx2 = keys:sign(Ctx2),
    absorb(Stx2),
    Ctx21 = spend_tx:make_dict(NewPub, 10, Fee, constants:master_pub()),
    Stx21 = keys:sign(Ctx21),
    absorb(Stx21),
    Ctx3 = delete_account_tx:make_dict(constants:master_pub(), NewPub, Fee),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv),
    absorb(Stx3),
    Ctx4 = create_account_tx:make_dict(NewPub, 100000000, Fee, constants:master_pub()),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),

    Txs = lists:reverse((tx_pool:get())#tx_pool.txs),
    BP2 = block:get_by_height(0),
    PH = block:hash(BP2),

    Block = block:make(block:block_to_header(BP2), Txs, Trees, constants:master_pub()),%1 is the master pub
    MBlock = block:mine2(Block, 1),
    Header = block:block_to_header(MBlock),
    headers:absorb([Header]),
    {true, _} = block:check(MBlock),
    success;
    
test(3) ->
    io:fwrite(" new channel tx, grow channel tx, and channel team close tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    {Ctx, _Proof} = create_account_tx:new(NewPub, Amount, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    timer:sleep(100),
    CID = 5,

    Delay = 30,
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),

    Ctx4 = channel_team_close_tx:make_dict(CID, 0, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv),
    absorb(SStx4),
    Txs = lists:reverse((tx_pool:get())#tx_pool.txs),

    Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
    
test(4) -> 
    %channel solo close, channel timeout
    io:fwrite("channel solo close tx, channel timeout tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = testnet_sign:new_key(),
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID = 5,
    Delay = 0,
    
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    %mine_blocks(1),
    timer:sleep(500),
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    {_, _, Txs} = tx_pool:data(),
    Header0 = block:block_to_header(BP),
    Block0 = block:make(Header0, Txs, Trees, constants:master_pub()),
    Block = block:mine2(Block0, 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(5) -> 
    %channel solo close, channel timeout
    io:fwrite("account delete tx, channel solo close tx, channel timeout tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = testnet_sign:new_key(),
    
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    {Ctx, _Proof} = create_account_tx:new(NewPub, Amount, Fee, constants:master_pub(), Trees),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID = 5,
    Delay = 0,
    
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    Ctx25 = delete_account_tx:make_dict(keys:pubkey(), NewPub, Fee),
    Stx25 = testnet_sign:sign_tx(Ctx25, NewPub, NewPriv),
    absorb(Stx25),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    %mine_blocks(1),
    timer:sleep(500),
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    Txs = lists:reverse((tx_pool:get())#tx_pool.txs),

    Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(6) -> 
    io:fwrite("channel slash tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID = 5,

    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, 10, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    mine_blocks(1),
    timer:sleep(50),

    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int 0 int 2 ">>), []),
    Ctx4 = channel_slash_tx:make_dict(NewPub,Fee,SignedScriptPubKey,[ScriptSig2]),
    Stx4 = testnet_sign:sign_tx(Ctx4, NewPub, NewPriv),
    absorb(Stx4),
    mine_blocks(1),
    timer:sleep(50),

    ScriptSig3 = spk:new_ss(compiler_chalang:doit(<<" int 0 int 3 ">>), []),
    Ctx5 = channel_slash_tx:make_dict(constants:master_pub(),Fee,SignedScriptPubKey,[ScriptSig3]),
    Stx5 = keys:sign(Ctx5),
    absorb(Stx5),

    Ctx6 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx6 = keys:sign(Ctx6),
    absorb(Stx6),
    empty = trees:dict_tree_get(channels, 1),

    mine_blocks(1),
    %Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    %Header = block:block_to_header(Block),
    %headers:absorb([Header]),
    %{true, _} = block:check(Block),
    success;
test(8) ->
    io:fwrite(" channel solo close, and channel team close tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    timer:sleep(100),

    CID = 5,

    Delay = 10,
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Bet = spk:new_bet(Code, Code, 50),
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(),Fee, SignedScriptPubKey, [ScriptSig]),
    %{Ctx3, _} = grow_channel_tx:make(CID, Trees3, 22, 33, Fee),
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),

    Ctx4 = channel_team_close_tx:make_dict(CID, 0, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv),
    absorb(SStx4),
    {_,_,Txs} = tx_pool:data(),

    Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(9) ->
    io:fwrite(" channel slash tx, and channel team close tx test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    timer:sleep(100),

    CID = 5,

    Delay = 10,
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Bet = spk:new_bet(Code, Code, 50),
    ChannelNonce = 0,
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(),Fee, SignedScriptPubKey, [ScriptSig]),
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    mine_blocks(1),
    timer:sleep(50),
    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int 0 int 2 ">>), []),
    Ctx35 = channel_slash_tx:make_dict(keys:pubkey(), Fee, SignedScriptPubKey, [ScriptSig2]),
    Stx35 = keys:sign(Ctx35),
    absorb(Stx35),

    Ctx4 = channel_team_close_tx:make_dict(CID, 0, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv),
    absorb(SStx4),
    mine_blocks(1),
    %{_,_,Txs} = tx_pool:data(),
    %Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    %Header = block:block_to_header(Block),
    %headers:absorb([Header]),
    %{true, _} = block:check(Block),
    success;

test(7) ->
    %existence tx
    headers:dump(),
    block:initialize_chain(),
    io:fwrite("existence test \n"),
    S = <<"test data">>,
    tx_pool:dump(),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    %Accounts = trees:accounts(Trees),
    Data = hash:doit(S),
    Fee = constants:initial_fee() + 20,
    Tx = existence_tx:make_dict(constants:master_pub(), Fee, Data),
    Stx = keys:sign(Tx),
    absorb(Stx),
    C = trees:dict_tree_get(existence, Data),
    Data = existence:hash(C),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Txs = lists:reverse((tx_pool:get())#tx_pool.txs),
    Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(11) ->
    io:fwrite("testing an oracle \n"),
    %testing the oracle
    %launch an oracle with oracle_new
    Question = <<>>,
    <<OID:80>> = crypto:strong_rand_bytes(10),
    Fee = constants:initial_fee() + 20,
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1, OID, 0, 0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    timer:sleep(150),
    mine_blocks(5),
    timer:sleep(150),
    %make some bets in the oracle with oracle_bet

    OIL = trees:dict_tree_get(governance, oracle_initial_liquidity),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL+1), 
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),

    %mine_blocks(1),
    timer:sleep(150),
    %Trees3 = (tx_pool:get())#tx_pool.trees,
    %close the oracle with oracle_close
    Tx3 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    %mine_blocks(1),
    timer:sleep(100),

    %If you look up an oracle from the dictionary, it will always point to 0 for the orders. You need to query seperately to get the orders out, or you can transform the dict into a trie. After the transformation, you can look up orders how it is commented out below.

    %Trees4 = (tx_pool:get())#tx_pool.trees,
    %get your spare money out with oracle_unmatched
    %Oracles = trees:oracles(Trees4),
    %{_, Oracle2, _} = oracles:get(OID, Oracles),
    Oracle = trees:dict_tree_get(oracles, OID),
    %Orders = Oracle#oracle.orders,
    %{OrderID, _} = orders:head_get(Orders),%This only works because there is exactly 1 order in the order book.
    Tx4 = oracle_unmatched_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx4 = keys:sign(Tx4),
    absorb(Stx4),
    %mine_blocks(1),
    timer:sleep(100),

    %get your winnings with oracle_shares
    Tx5 = oracle_winnings_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    %mine_blocks(1),
    timer:sleep(100),
    {_,Height6,Txs} = tx_pool:data(),
    BP = block:get_by_height(Height6),
    Block = block:mine2(block:make(block:block_to_header(BP), Txs, block_trees(BP), constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(16) ->
    io:fwrite("testing an oracle with more bets\n"),
    %testing the oracle
    %launch an oracle with oracle_new
    {Pub1,Priv1} = testnet_sign:new_key(),
    {Pub2,Priv2} = testnet_sign:new_key(),
    Question = <<>>,
    OID = 1,
    Fee = constants:initial_fee() + 20,
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    Amount = 1000000000,
    Ctx_1 = create_account_tx:make_dict(Pub1, Amount, Fee, constants:master_pub()),
    Stx_1 = keys:sign(Ctx_1),
    absorb(Stx_1),
    
    Ctx_2 = create_account_tx:make_dict(Pub2, Amount, Fee, constants:master_pub()),
    Stx_2 = keys:sign(Ctx_2),
    absorb(Stx_2),

    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1, OID, 0, 0),
    Stx = keys:sign(Tx),
    absorb(Stx),
    timer:sleep(150),
    mine_blocks(5),
    timer:sleep(150),
    %make some bets in the oracle with oracle_bet
    OIL = trees:dict_tree_get(governance, oracle_initial_liquidity),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL), 
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),

    Tx21 = oracle_bet_tx:make_dict(Pub1, Fee, OID, 1, OIL*2), 
    Stx21 = testnet_sign:sign_tx(Tx21, Pub1, Priv1),
    absorb(Stx21),
    Tx22 = oracle_bet_tx:make_dict(Pub2, Fee, OID, 2, OIL), 
    Stx22 = testnet_sign:sign_tx(Tx22, Pub2, Priv2),
    absorb(Stx22),
    %mine_blocks(1),
    timer:sleep(150),
    %close the oracle with oracle_close
    Tx3 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    Tx41 = oracle_unmatched_tx:make_dict(Pub1, Fee, OID),
    Stx41 = testnet_sign:sign_tx(Tx41, Pub1, Priv1),
    absorb(Stx41),
    Tx5 = oracle_winnings_tx:make_dict(constants:master_pub(), Fee, OID),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    Tx51 = oracle_winnings_tx:make_dict(Pub1, Fee, OID),
    Stx51 = testnet_sign:sign_tx(Tx51, Pub1, Priv1),
    absorb(Stx51),
    Tx52 = oracle_winnings_tx:make_dict(Pub2, Fee, OID),
    Stx52 = testnet_sign:sign_tx(Tx52, Pub2, Priv2),
    absorb(Stx52),

    timer:sleep(100),
    {_,Height6,Txs} = tx_pool:data(),
    BP = block:get_by_height(Height6),
    Block = block:mine2(block:make(block:block_to_header(BP), Txs, block_trees(BP), constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(12) ->
    io:fwrite("multiple bets in a single channel test \n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    {NewPub,NewPriv} = testnet_sign:new_key(),
    
    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),
    
    CID = 5,
    Delay = 0,
    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Code2 = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    Bet2 = spk:new_bet(Code2, Code2, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet, Bet2], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int 0 int 2 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig, ScriptSig2]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    timer:sleep(500),
    Height4 = (tx_pool:get())#tx_pool.height,
    Ctx4 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx4 = keys:sign(Ctx4),
    absorb(Stx4),
    BP = block:get_by_height(Height4),
    PH = block:hash(BP),
    {_,_,Txs} = tx_pool:data(),
    Block = block:mine2(block:make(block:block_to_header(BP), Txs, block_trees(BP), constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),
    success;
test(13) ->
    %testing the governance
    %launch an oracle with oracle_new, close it on state "bad", 
    io:fwrite("test governance \n"),
    Question = <<>>,
    Fee = constants:initial_fee() + 20,
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    OID2 = 1,
    Tx3 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1, OID2, 1, 5),
    Stx3 = keys:sign(Tx3),
    absorb(Stx3),
    timer:sleep(100),

    MOT = trees:dict_tree_get(governance, minimum_oracle_time),
    OIL = trees:dict_tree_get(governance, oracle_initial_liquidity),
    mine_blocks(1+MOT),
    timer:sleep(200),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID2, 1, OIL * 2), 
    BR1 = trees:dict_tree_get(governance, block_reward),
    Stx2 = keys:sign(Tx2),
    absorb(Stx2),
    mine_blocks(1+MOT),
    timer:sleep(100),

    Tx5 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID2),
    Stx5 = keys:sign(Tx5),
    absorb(Stx5),
    timer:sleep(50),
    mine_blocks(1),

    OID3 = 2,
    BR2 = trees:dict_tree_get(governance, block_reward),
    Tx7 = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1, OID3, 1, 5),
    Stx7 = keys:sign(Tx7),
    absorb(Stx7),
    mine_blocks(1),
    timer:sleep(50),

    Tx8 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID3, 1, OIL * 2), 
    Stx8 = keys:sign(Tx8),
    absorb(Stx8),
    mine_blocks(1+MOT),
    timer:sleep(100),

    Tx9 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID3),
    Stx9 = keys:sign(Tx9),
    absorb(Stx9),
    timer:sleep(50),

    BR3 = trees:dict_tree_get(governance, block_reward),
    true = BR1 < BR2,
    true = BR2 < BR3,
    {_,H,Txs} = tx_pool:data(),
    BP = block:get_by_height(H),
    Block = block:mine2(block:make(block:block_to_header(BP), Txs, block_trees(BP), constants:master_pub()), 10),
    Header = block:block_to_header(Block),
    headers:absorb([Header]),
    {true, _} = block:check(Block),

    success;
test(14) -> 
    %options
    io:fwrite("options derivatives enforcement test\n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID = 5,

    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, 10, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    
    Code = compiler_chalang:doit(<<"drop int 50">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    ScriptPubKey = keys:sign(spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay)),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 0 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(constants:master_pub(), Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = keys:sign(Ctx3),
    absorb(Stx3),
    mine_blocks(1),
    timer:sleep(50),

    ScriptSig2 = spk:new_ss(compiler_chalang:doit(<<" int 0 int 2 ">>), []),
    Ctx4 = channel_slash_tx:make_dict(NewPub,Fee,SignedScriptPubKey,[ScriptSig2]),
    Stx4 = testnet_sign:sign_tx(Ctx4, NewPub, NewPriv),
    absorb(Stx4),

    Ctx6 = channel_timeout_tx:make_dict(constants:master_pub(),CID,Fee),
    Stx6 = keys:sign(Ctx6),
    absorb(Stx6),
    BP2 = block:get_by_height(0),
    PH = block:hash(BP2),
    mine_blocks(1),

    %{_,_,Txs} = tx_pool:data(),
    %Block = block:mine2(block:make(block:block_to_header(BP), Txs, Trees, constants:master_pub()), 10),
    %Header = block:block_to_header(Block),
    %headers:absorb([Header]),
    %{true, _} = block:check(Block),
    success;

test(15) ->
    %If your partner tries closing at a low-nonced channel state, your node needs to automatically create a channel_slash to stop them.
    io:fwrite("channel slash automatic test\n"),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    BP = block:get_by_height(0),
    PH = block:hash(BP),
    Trees = block_trees(BP),
    Accounts = trees:accounts(Trees),
    {NewPub,NewPriv} = testnet_sign:new_key(),

    Fee = constants:initial_fee() + 20,
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx = keys:sign(Ctx),
    absorb(Stx),

    CID = 5,

    Ctx2 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 100, 200, 10, Fee),
    Stx2 = keys:sign(Ctx2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv), 
    absorb(SStx2),
    Code = compiler_chalang:doit(<<"drop int 50">>),
    Secret = spk:new_ss(compiler_chalang:doit(<<" int 0 int 2 ">>), []),
    %secrets:add(Code, Secret),
    %timer:sleep(100),
    
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, Code, 50),
    SPK = spk:new(constants:master_pub(), NewPub, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay),
    TheySPK = testnet_sign:sign_tx(SPK, NewPub, NewPriv),
    CD = channel_feeder:new_cd(SPK, TheySPK, [Secret], [Secret], CID, 1000),
    channel_manager:write(NewPub, CD),
    timer:sleep(100),
    ScriptPubKey = keys:sign(SPK),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv), 
    ScriptSig = spk:new_ss(compiler_chalang:doit(<<" int 5 int 1 ">>), []),
    Ctx3 = channel_solo_close:make_dict(NewPub, Fee, SignedScriptPubKey, [ScriptSig]), 
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv),
    absorb(Stx3),
    timer:sleep(100),
    mine_blocks(1),
    timer:sleep(50),
    Txs2 = (tx_pool:get())#tx_pool.txs,
    %io:fwrite("~s", [packer:pack({slash_exists, Txs2})]),
    %timer:sleep(2000),
    true = slash_exists(Txs2),%check that the channel_slash transaction exists in the tx_pool.
    %Block = block:mine(block:make(PH, Txs2, 1), 10000000000),%1 is the master pub
    %block:check2(Block),
    success;
test(17) ->
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    %fill blocks completely with create_account txs.
    create_accounts(650),%fits 636 at the start.
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    mine_blocks(1),
    io:fwrite(packer:pack(now())),
    io:fwrite("\n"),
    timer:sleep(300),
    success;
test(18) ->
    test18(100).
test18(0) -> success;
test18(N) ->
    test(17),
    test18(N-1).
create_accounts(0) -> ok;
create_accounts(N) ->
    io:fwrite("create account "),
    io:fwrite(integer_to_list(N)),
    %io:fwrite("\n"),
    %io:fwrite(packer:pack([1, now()])),%200
    %io:fwrite("\n"),
    {NewPub,_NewPriv} = testnet_sign:new_key(),
    Fee = constants:initial_fee() + 20,
    %io:fwrite(packer:pack([2, now()])),%2000 or 20 000
    %io:fwrite("\n"),
    Trees = (tx_pool:get())#tx_pool.trees,
    %io:fwrite(packer:pack([3, now()])),%1300
    %io:fwrite("\n"),
    {Ctx, _} = create_account_tx:new(NewPub, 1, Fee, constants:master_pub(), Trees),
    %io:fwrite(packer:pack([4, now()])),%750
    %io:fwrite("\n"),
    Stx = keys:sign(Ctx),
    %io:fwrite(packer:pack([5, now()])),%18000
    %io:fwrite("\n"),
    absorb(Stx),
    %io:fwrite(packer:pack([6, now()])),%45 000 or 1 000 000
    %io:fwrite("\n"),
    create_accounts(N-1).
slash_exists([]) -> false;
slash_exists([Tx|T]) ->
    is_slash(Tx) or slash_exists(T).
is_slash(STx) ->
    Tx = testnet_sign:data(STx),
    channel_slash_tx:is_tx(Tx).
	     
mine_blocks(Many) when Many < 1 -> ok;
mine_blocks(Many) ->
    %only works if you set the difficulty very low.
    {_, Height, Txs} = tx_pool:data(),
    PB = block:get_by_height(Height),
    Hash = block:hash(PB),
    {ok, Top} = headers:read(Hash),
    Block = block:make(Top, Txs, block_trees(PB), keys:pubkey()),
    block:mine(Block, 10),
    timer:sleep(1000),
    mine_blocks(Many-1).
