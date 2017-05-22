-module(test_txs).
-export([test/0, test/1, mine_blocks/1, absorb/1]).
 
test() ->
    unlocked = keys:status(),
    Pub = constants:master_pub(),
    Pub = keys:pubkey(),

    S = success,
    S = test(1),%create account, spend, delete
    S = test(2),%repo tx
    S = test(3),%channel team close, channel grow
    S = test(4),%channel repo
    S = test(5),%channel timeout
    S = test(6),%channel slash
    S = test(7),%existence
    S = test(8),%spend shares with spend
    S = test(9),%spend shares with team_close
    S = test(10),%spend shares with timeout
    S = test(14),%financial options
    S = test(15),%automatic channel slash
    S = test(11),%try out the oracle
    %warning! after running test(11), we can no longer run other tests. because test(11) mines blocks, so tx_pool:dump can no longer undo transactions.
    S = test(12),%multiple bets in a single channel
    S = test(13),%testing governance
    timer:sleep(300),
    S.
absorb(Tx) -> 
    tx_pool_feeder:absorb(Tx),
    timer:sleep(400).
test(1) ->
    io:fwrite(" create_account tx\n"),
    %create account, spend, delete account
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 100000000, Fee, 1, 2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2,  _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Trees2, []),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    {Ctx3, _} = delete_account_tx:make(1, 2, Fee, Trees3),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, 2, Accounts3),
    absorb(Stx3),
    {_, _, Txs} = tx_pool:data(),
    BP = block:genesis(),
    PH = block:hash(BP),

    Block = block:make(PH, Txs, 1),%1 is the master pub
    MBlock = block:mine(Block, 1000000),
    block:check2(MBlock),
    success;
    
test(2) ->
    io:fwrite(" repo tx\n"),
    %repo_tx
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,_NewPub,_NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, 0, Fee, 1, 2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    {Ctx2, _} = repo_tx:make(2, Fee, 1, Trees2),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {_, _, Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 1000000),%1 is the master pub
    block:check2(Block),
    success;
test(3) ->
    io:fwrite(" new channel tx\n"),
    %new channel, grow channel, channel team close
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    timer:sleep(100),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    CID = 5,
    Entropy = 432,

    Delay = 30,
    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 100, 200, Entropy, Delay, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),

    {Ctx3, _} = grow_channel_tx:make(CID, Trees3, 22, 33, Fee),
    Stx3 = keys:sign(Ctx3, Accounts3),
    SStx3 = testnet_sign:sign_tx(Stx3, NewPub, NewPriv, ID2, Accounts3),
    absorb(SStx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),

    {Ctx4, _} = channel_team_close_tx:make(CID, Trees4, 0, [], Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv, ID2, Accounts4),
    absorb(SStx4),
    {_,_,Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 1000000000),%1 is the master pub
	   block:check2(Block),
    success;
    
test(4) -> 
    %channel repo
    io:fwrite(" channel repo tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    CID = 5,
    Entropy = 432, 
    Delay = 0,

    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 0, 0, Entropy, Delay, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),

    {Ctx4, _} = channel_repo_tx:make(1, CID, Fee, Trees3),
    Stx4 = keys:sign(Ctx4, Accounts3),
    absorb(Stx4),
    {_,_,Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success;
    
test(5) -> 
    %channel solo close, channel timeout
    io:fwrite("channel solo close tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    
    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    
    CID = 5,
    Entropy = 432,
    Delay = 0,
    
    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 10000, 20000, Entropy, Delay, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    
    Code = compiler_chalang:doit(<<"int 50 nil">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 0 int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Trees3), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    %mine_blocks(1),
    timer:sleep(500),
    {Trees4, _, _Txs} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    {Ctx4, _} = channel_timeout_tx:make(1,Trees4,CID,[],Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {_, _, Txs} = tx_pool:data(),

    Block = block:mine(block:make(PH, Txs, 1), 100000000),%1 is the master pub
    block:check2(Block),
    success;
test(6) -> 
    %channel slash
    io:fwrite("\nchannel slash tx\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 100, 200, Entropy, 10, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    
    Code = compiler_chalang:doit(<<"int 50 nil">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 0 int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Trees3), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),

    ScriptSig2 = compiler_chalang:doit(<<" int 0 int 2 ">>),
    {Ctx4, _} = channel_slash_tx:make(2,Fee,SignedScriptPubKey,[ScriptSig2],Trees4),
    Stx4 = testnet_sign:sign_tx(Ctx4, NewPub, NewPriv, ID2, Accounts4),
    %Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {Trees5, _, _} = tx_pool:data(),
    Accounts5 = trees:accounts(Trees5),

    ScriptSig3 = compiler_chalang:doit(<<" int 0 int 3 ">>),
    {Ctx5, _} = channel_slash_tx:make(1,Fee,SignedScriptPubKey,[ScriptSig3],Trees5),
    Stx5 = keys:sign(Ctx5, Accounts5),
    %Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx5),
    {Trees6, _, _Txs2} = tx_pool:data(),
    Accounts6 = trees:accounts(Trees6),

    {Ctx6, _} = channel_timeout_tx:make(1,Trees6,CID,[],Fee),
    Stx6 = keys:sign(Ctx6, Accounts6),
    absorb(Stx6),
    {Trees7, _, Txs} = tx_pool:data(),
    Channels7 = trees:channels(Trees7),
    {_, empty, _} = channels:get(1, Channels7),

    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success;

test(7) ->
    %existence tx
    io:fwrite("\nexistence test \n"),
    S = <<"test data">>,
    ID = keys:id(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    C = existence:new(testnet_hasher:doit(S)),
    {Tx, _} = existence_tx:make(ID, 1000, C, Trees),
    Stx = keys:sign(Tx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    ETree = trees:existence(Trees2),
    {_, C, _} = existence:get(existence:hash(C), ETree),
    BP = block:genesis(),
    PH = block:hash(BP),
    {_, _, Txs} = tx_pool:data(),
    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success;
test(8) ->
    %spend shares
    io:fwrite("\nspend shares test\n"),
    tx_pool:dump(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    Fee = 20,
    {Ctx, _} = create_account_tx:make(NewAddr, 1000000000, Fee, 1, 2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    timer:sleep(200),
    Shares = [
	      shares:new(100, 500, 0),
	      shares:new(101, 500, 0),
	      shares:new(110, 500, 0)
	     ],
    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Trees2, Shares),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    timer:sleep(150),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    {_, A1, _} = accounts:get(1, Accounts3),
    {_, A2, _} = accounts:get(2, Accounts3),
    S1 = accounts:shares(A1), 
    S2 = accounts:shares(A2),
    {shares:get(100, S1),
     shares:get(100, S2),
     shares:get(101, S1),
     shares:get(101, S2),
     shares:get(110, S1),
     shares:get(110, S2)},
    {Ctx3, _} = spend_tx:make(1, 10, Fee, 2, Trees3, Shares),
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, 2, Accounts3), 
    absorb(Stx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    {_, A3, _} = accounts:get(1, Accounts4),
    {_, A4, _} = accounts:get(2, Accounts4),
    S3 = accounts:shares(A3),
    S4 = accounts:shares(A4),
    {_, empty, _} = shares:get(100, S3),
    {_, empty, _} = shares:get(101, S3),
    {_, empty, _} = shares:get(110, S3),
    {_, empty, _} = shares:get(100, S4),
    {_, empty, _} = shares:get(101, S4),
    {_, empty, _} = shares:get(110, S4),
    BP = block:genesis(),
    PH = block:hash(BP),
    {_, _, Txs} = tx_pool:data(),
    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    %success;
    success;
test(9) ->
   %spend shares with channel. 
    io:fwrite("spend shares with channel\n"),
    tx_pool:dump(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    Fee = 20,
    {Ctx, _} = create_account_tx:make(NewAddr, 1000000000, Fee, 1, 2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    timer:sleep(200),
    Shares = [
	      shares:new(100, 500, 0),
	      shares:new(101, 500, 0),
	      shares:new(110, 500, 0)
	     ],
    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Trees2, Shares),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {Trees3, _, _} = tx_pool:data(),
    %make a channel and unspend all the shares.
    Accounts3 = trees:accounts(Trees3),
    %Channels = trees:channels(Trees3),
    CID = 5,
    Entropy = 432,
    Delay = 30,
    {Ctx3, _} = new_channel_tx:make(CID, Trees3, 2, 1, 10000, 200000, Entropy, Delay, Fee),
    Stx3 = keys:sign(Ctx3, Accounts3),
    SStx3 = testnet_sign:sign_tx(Stx3, NewPub, NewPriv, 2, Accounts3), 
    absorb(SStx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    {Ctx4, _} = channel_team_close_tx:make(CID, Trees4, 0, Shares, Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv, 2, Accounts4),
    absorb(SStx4),
    timer:sleep(300),
    {Trees5, _, _} = tx_pool:data(),
    Accounts5 = trees:accounts(Trees5),
    {_, A3, _} = accounts:get(1, Accounts5),
    {_, A4, _} = accounts:get(2, Accounts5),
    S3 = accounts:shares(A3),
    S4 = accounts:shares(A4),
    {_, empty, _} = shares:get(100, S3),
    {_, empty, _} = shares:get(101, S3),
    {_, empty, _} = shares:get(110, S3),
    {_, empty, _} = shares:get(100, S4),
    {_, empty, _} = shares:get(101, S4),
    {_, empty, _} = shares:get(110, S4),
    BP = block:genesis(),
    PH = block:hash(BP),
    {_, _, Txs} = tx_pool:data(),
    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    timer:sleep(1000),
    success;
test(10) ->
   %spend shares with channel, with solo_close
    io:fwrite("spend_shares with channel solo_close"),
    tx_pool:dump(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    Fee = 20,
    {Ctx, _} = create_account_tx:make(NewAddr, 1000000000, Fee, 1, 2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    timer:sleep(200),
    Shares = [
	      shares:new(100, 500, 0),
	      shares:new(101, 500, 0),
	      shares:new(110, 500, 0)
	     ],
    {Ctx2, _} = spend_tx:make(2, 10, Fee, 1, Trees2, Shares),
    Stx2 = keys:sign(Ctx2, Accounts2),
    absorb(Stx2),
    {Trees3, _, _} = tx_pool:data(),
    %make a channel and unspend all the shares.
    Accounts3 = trees:accounts(Trees3),
    %Channels = trees:channels(Trees3),
    CID = 5,
    Entropy = 432,
    Delay = 0,
    {Ctx3, _} = new_channel_tx:make(CID, Trees3, 2, 1, 10000, 20000, Entropy, Delay, Fee),
    Stx3 = keys:sign(Ctx3, Accounts3),
    SStx3 = testnet_sign:sign_tx(Stx3, NewPub, NewPriv, 2, Accounts3), 
    absorb(SStx3),

    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    SC = shares:to_code(Shares),
    %io:fwrite(SC),
    Code = compiler_chalang:doit(
	     <<<<"int 50 ">>/binary,%channel nonce is 1, sends 50.
	       SC/binary>>),
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    ScriptPubKey = keys:sign(spk:new(2, 1, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay, Entropy), Accounts4),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, 2, Accounts4), 
    ScriptSig = compiler_chalang:doit(<<" int 0 int 1 ">>),
    {Ctx4, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Trees4), 
    Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {Trees5, _, _} = tx_pool:data(),
    Accounts5 = trees:accounts(Trees5),
    %Channels3 = trees:channels(Trees5),
    {Ctx5, _} = channel_timeout_tx:make(1,Trees5,CID,Shares,Fee),
    %io:fwrite(packer:pack(Ctx5)),
    Stx5 = keys:sign(Ctx5, Accounts5),
    absorb(Stx5),

    {Trees6, _, _} = tx_pool:data(),
    Accounts6 = trees:accounts(Trees6),
    {_, A3, _} = accounts:get(1, Accounts6),
    {_, A4, _} = accounts:get(2, Accounts6),
    S3 = accounts:shares(A3),
    S4 = accounts:shares(A4),
    {_, empty, _} = shares:get(100, S3),
    {_, empty, _} = shares:get(101, S3),
    {_, empty, _} = shares:get(110, S3),
    {_, empty, _} = shares:get(100, S4),
    {_, empty, _} = shares:get(101, S4),
    {_, empty, _} = shares:get(110, S4),
    {_, _, Txs} = tx_pool:data(),
    BP = block:genesis(),
    PH = block:hash(BP),
    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    timer:sleep(1000),
    success;
test(11) ->
    io:fwrite("testing an oracle\n"),
    %testing the oracle
    %launch an oracle with oracle_new
    Question = <<>>,
    OID = 1,
    Fee = 20,
    tx_pool:dump(),
    {Trees,_,_Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = oracle_new_tx:make(1, Fee, Question, 1, OID, constants:initial_difficulty(), 0, 0, 0, Trees),
    Stx = keys:sign(Tx, Accounts),
    absorb(Stx),
    timer:sleep(150),
    mine_blocks(2),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    %make some bets in the oracle with oracle_bet
    Governance2 = trees:governance(Trees2),
    OIL = governance:get_value(oracle_initial_liquidity, Governance2),
    {Tx2, _} = oracle_bet_tx:make(1, Fee, OID, true, OIL, Trees2), 
    Stx2 = keys:sign(Tx2, Accounts2),
    absorb(Stx2),
    %timer:sleep(100),

    mine_blocks(10),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    %close the oracle with oracle_close
    {Tx3, _} = oracle_close_tx:make(1,Fee, OID, Trees3),
    Stx3 = keys:sign(Tx3, Accounts3),
    absorb(Stx3),
    timer:sleep(100),

    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    %get your spare money out with oracle_unmatched
    Oracles = trees:oracles(Trees4),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    Orders = oracles:orders(Oracle),
    {OrderID, _} = orders:head_get(Orders),%This only works because there is exactly 1 order in the order book.
    {Tx4, _} = oracle_unmatched_tx:make(1, Fee, OID, OrderID, Trees4),
    Stx4 = keys:sign(Tx4, Accounts4),
    absorb(Stx4),

    {Trees5, _, _} = tx_pool:data(),
    Accounts5 = trees:accounts(Trees5),
    %get your shares out with oracle_shares
    {Tx5, _}=oracle_shares_tx:make(1, Fee, OID, Trees5),
    Stx5 = keys:sign(Tx5, Accounts5),
    absorb(Stx5),
    {_,_,Txs} = tx_pool:data(),
    Block = block:mine(block:make(top:doit(), Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success;
test(12) ->
    %multiple bets in a single channel
    io:fwrite("multiple bets in a single channel\n"),
    tx_pool:dump(),
    {Trees, _, _Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    
    Fee = 20,
    Amount = 1000000,
    ID2 = 50,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    
    CID = 5,
    Entropy = 432,
    Delay = 0,
    
    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 10000, 20000, Entropy, Delay, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    
    Code = compiler_chalang:doit(<<"int 50 nil">>),%channel nonce is 1, sends 50.
    Code2 = compiler_chalang:doit(<<"int 50 nil">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    Bet2 = spk:new_bet(Code2, 50, []),
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Bet, Bet2], 10000, 10000, ChannelNonce+1, Delay, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 0 int 1 ">>),
    ScriptSig2 = compiler_chalang:doit(<<" int 0 int 2 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig, ScriptSig2], Trees3), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    timer:sleep(500),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    {Ctx4, _} = channel_timeout_tx:make(1,Trees4,CID,[],Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    BP = block:genesis(),
    PH = block:hash(BP),
    {_,_,Txs} = tx_pool:data(),
    Block = block:mine(block:make(PH, Txs, 1), 100000000),%1 is the master pub
    block:check2(Block),
    success;
test(13) ->
    %testing the governance
    %launch an oracle with oracle_new, close it on state "bad", 
    io:fwrite("test governance\n"),
    Question = <<>>,
    OID = 6,
    Fee = 20,
    tx_pool:dump(),
    Diff = constants:initial_difficulty(),
    {Trees,_,_Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = oracle_new_tx:make(1, Fee, Question, 1, OID, Diff, 0, 0, 0, Trees),
    Stx = keys:sign(Tx, Accounts),
    absorb(Stx),

    mine_blocks(8),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    %close the oracle with oracle_close
    {Tx2, _} = oracle_close_tx:make(1,Fee, OID, Trees2),
    Stx2 = keys:sign(Tx2, Accounts2),
    absorb(Stx2),
    OID2 = 2,
    {Trees3,_,_} = tx_pool:data(),
    %OraclesEE = trees:oracles(Trees3),
    %{_, Oracle4, _} = oracles:get(OID, OraclesEE),
    Accounts3 = trees:accounts(Trees3),
    {Tx3, _} = oracle_new_tx:make(1, Fee, Question, 1, OID2, Diff, OID, 1, 5, Trees3),
    Stx3 = keys:sign(Tx3, Accounts3),
    absorb(Stx3),

    Question2 = <<"1+1=2">>,
    OID3 = 3,
    {Trees4,_,_} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    {Tx4, _} = oracle_new_tx:make(1, Fee, Question2, 1, OID3, Diff div 2, OID, 0, 0, Trees4),
    Stx4 = keys:sign(Tx4, Accounts4),
    absorb(Stx4),

    {_,_,Txs} = tx_pool:data(),
    Block = block:mine(block:make(top:doit(), Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),

    success;
test(14) -> 
    %options
    io:fwrite("options derivatives enforcement\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 100, 200, Entropy, 10, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    
    Code = compiler_chalang:doit(<<"int 50 nil">>),%channel nonce is 1, sends 50.
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    ScriptPubKey = keys:sign(spk:new(1, ID2, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay, Entropy), Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 0 int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(1, Fee, SignedScriptPubKey, [ScriptSig], Trees3), 
    Stx3 = keys:sign(Ctx3, Accounts3),
    absorb(Stx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),

    ScriptSig2 = compiler_chalang:doit(<<" int 0 int 2 ">>),
    {Ctx4, _} = channel_slash_tx:make(2,Fee,SignedScriptPubKey,[ScriptSig2],Trees4),
    Stx4 = testnet_sign:sign_tx(Ctx4, NewPub, NewPriv, ID2, Accounts4),
    %Stx4 = keys:sign(Ctx4, Accounts4),
    absorb(Stx4),
    {Trees5, _, _} = tx_pool:data(),
    Accounts5 = trees:accounts(Trees5),

    {Ctx5, _} = grow_channel_tx:make(CID, Trees5, 22, 33, Fee),
    Stx5 = keys:sign(Ctx5, Accounts5),
    SStx5 = testnet_sign:sign_tx(Stx5, NewPub, NewPriv, ID2, Accounts5),
    absorb(SStx5),

    {Trees6, _, _Txs2} = tx_pool:data(),
    Accounts6 = trees:accounts(Trees6),

    {Ctx6, _} = channel_timeout_tx:make(1,Trees6,CID,[],Fee),
    Stx6 = keys:sign(Ctx6, Accounts6),
    absorb(Stx6),
    BP = block:genesis(),
    PH = block:hash(BP),

    {_,_,Txs} = tx_pool:data(),
    Block = block:mine(block:make(PH, Txs, 1), 10000000000),%1 is the master pub
    block:check2(Block),
    success;

test(15) ->
    %If your partner tries closing at a low-nonced channel state, your node needs to automatically create a channel_slash to stop them.
    io:fwrite("channel slash automatic\n"),
    BP = block:genesis(),
    PH = block:hash(BP),
    tx_pool:dump(),
    Trees = block:trees(BP),
    Accounts = trees:accounts(Trees),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),

    Fee = 20,
    Amount = 1000000,
    ID2 = 2,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees),
    Stx = keys:sign(Ctx, Accounts),
    absorb(Stx),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),

    CID = 5,
    Entropy = 432,

    {Ctx2, _} = new_channel_tx:make(CID, Trees2, 1, ID2, 100, 200, Entropy, 10, Fee),
    Stx2 = keys:sign(Ctx2, Accounts2),
    SStx2 = testnet_sign:sign_tx(Stx2, NewPub, NewPriv, ID2, Accounts2), 
    absorb(SStx2),
    Code = compiler_chalang:doit(<<"int 50 nil">>),%sends 50.
    Secret = compiler_chalang:doit(<<" int 0 int 2 ">>),
    %secrets:add(Code, Secret),
    %timer:sleep(100),
    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    
    Delay = 0,
    ChannelNonce = 0,
    Bet = spk:new_bet(Code, 50, []),
    SPK = spk:new(1, ID2, CID, [Bet], 10000, 10000, ChannelNonce+1, Delay, Entropy),
    TheySPK = testnet_sign:sign_tx(SPK, NewPub, NewPriv, ID2, Accounts3),
    CD = channel_feeder:new_cd(SPK, TheySPK, [Secret], [Secret], Entropy, CID),
    channel_manager:write(ID2, CD),
    timer:sleep(100),
    ScriptPubKey = keys:sign(SPK, Accounts3),
    SignedScriptPubKey = testnet_sign:sign_tx(ScriptPubKey, NewPub, NewPriv, ID2, Accounts3), 
    ScriptSig = compiler_chalang:doit(<<" int 5 int 1 ">>),
    {Ctx3, _} = channel_solo_close:make(ID2, Fee, SignedScriptPubKey, [ScriptSig], Trees3), 
    Stx3 = testnet_sign:sign_tx(Ctx3, NewPub, NewPriv, ID2, Accounts3),
    absorb(Stx3),
    {_, _, Txs2} = tx_pool:data(),
    true = slash_exists(Txs2),%check that the channel_slash transaction exists in the tx_pool.
    %Block = block:mine(block:make(PH, Txs2, 1), 10000000000),%1 is the master pub
    %block:check2(Block),
    success.
slash_exists([]) -> false;
slash_exists([Tx|T]) ->
    is_slash(Tx) or slash_exists(T).
is_slash(STx) ->
    Tx = testnet_sign:data(STx),
    channel_slash_tx:is_tx(Tx).
	     
mine_blocks(Many) when Many < 1 -> ok;
mine_blocks(Many) ->
    %only works if you set the difficulty very low.
    block:mine_blocks(1, 100000),
    timer:sleep(200),
    mine_blocks(Many-1).
