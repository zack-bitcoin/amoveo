-module(market).
-export([test/0, test2/0]).

test() ->
    Question = <<>>,
    OID = 3,
    Fee = 20,
    Entropy = 555,
    tx_pool:dump(),
    {Trees,_,_Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = oracle_new_tx:make(1, Fee, Question, 1, OID, constants:initial_difficulty(), 0, 0, 0, Trees),
    Stx = keys:sign(Tx, Accounts),
    test_txs:absorb(Stx),
    Fee = 20,
    Entropy = 555,
    timer:sleep(200),
    test_txs:mine_blocks(1),
    timer:sleep(1000),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    %make some bets in the oracle with oracle_bet
    Governance2 = trees:governance(Trees2),
    OIL = governance:get_value(oracle_initial_liquidity, Governance2),
    {Tx2, _} = oracle_bet_tx:make(1, Fee, OID, true, OIL*2, Trees2), 
    Stx2 = keys:sign(Tx2, Accounts2),
    test_txs:absorb(Stx2),

    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    ID2 = 50,
    Amount = 1000000,
    {Ctx, _Proof} = create_account_tx:make(NewAddr, Amount, Fee, 1, ID2, Trees3),
    Stx3 = keys:sign(Ctx, Accounts3),
    test_txs:absorb(Stx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    
    CID = 5,
    Delay = 0,
    
    {Ctx4, _} = new_channel_tx:make(CID, Trees4, 1, ID2, 10000, 20000, Entropy, Delay, Fee),
    Stx4 = keys:sign(Ctx4, Accounts4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv, ID2, Accounts4), 
    test_txs:absorb(SStx4),
    timer:sleep(400),
    test2(). 

test2() ->
    Entropy = 555,
    OID = 3,
    {Trees5, _, _} = tx_pool:data(),
    %Accounts5 = trees:accounts(Trees5),
    Code0 = <<" macro bet_amount int 10000 ; ">>,%set to 10000 to bet on true, 0 to bet on false.
    {ok, Code} = file:read_file("src/oracle_bet.fs"),%creates macro "oracle_bet" which is used in market.fs
    %MinPrice is in the range 0 to 10000,
    % it is the limit of how much you are willing to pay the server for the derivative. You will pay this much or less.
    % Pubkey is the pubkey of the market manager.
    MarketID = 405,
    Code2 = " \
macro bet oracle_bet ; \
macro Expires int " ++ integer_to_list(1000) ++ " ;\
macro MinPrice int " ++ integer_to_list(6000) ++ " ;\
macro MarketID int " ++ integer_to_list(MarketID) ++ " ;\
macro Pubkey binary " ++ integer_to_list(size(base64:decode(keys:pubkey()))) ++ " " ++ binary_to_list(keys:pubkey()) ++ " ;\
macro Period int " ++ integer_to_list(101) ++ " ;\
",
    {ok, Code3} = file:read_file("src/market.fs"),
    Compiled = compiler_chalang:doit(<<Code0/binary, Code/binary, (list_to_binary(Code2))/binary, Code3/binary>>),
    Bet = spk:new_bet(Compiled, 100, [{oracles, OID}]),
    SPK = spk:new(1, 2, 1, [Bet], 10000, 10000, 1, 0, Entropy),
						%ScriptPubKey = testnet_sign:sign_tx(keys:sign(SPK, Accounts5), NewPub, NewPriv, ID2, Accounts5),
						%we need to try running it in all 4 ways of market, and all 4 ways of oracle_bet.
    Price = 5500,
    Height = 300,
    PD = <<Height:32, Price:16, MarketID:16>>,
    Signature = keys:raw_sign(PD),
    true = sign:verify_sig(PD, Signature, keys:pubkey()),
    Sig1 = base64:decode(Signature),
    SPD = <<PD/binary, Sig1/binary>>,
    PriceDeclare = binary_to_list(base64:encode(SPD)),
    SS1a = "binary "++ integer_to_list(size(SPD))++ 
" " ++ PriceDeclare ++ " int 1",
    SS1 = compiler_chalang:doit(list_to_binary(SS1a)),
    %First we check that if we try closing the bet early, it has a delay that lasts at least till Expires, which we can set far enough in the future that we can be confident that the oracle will be settled.
    %amount, newnonce, shares, delay
    {50,1000001,[],999} = %the bet amount was 100, so if the oracle is canceled the money is split 50-50.
	spk:run(fast, [SS1], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker has disappeared and stopped publishing prices
    SS2a = " int 0 ",
    SS2 = compiler_chalang:doit(list_to_binary(SS2a)),
    %amount, newnonce, shares, delay
    {0, 999901, [],101} = 
	spk:run(fast, [SS2], SPK, 1, 0, Trees5),
    
    %Next try closing it as if the market maker tries to stop us from closing the bet early, because he is still publishing data.
    SS3a = " binary " ++ integer_to_list(size(SPD)) ++ " " ++ binary_to_list(base64:encode(SPD)) ++ " int 3 ",
    SS3 = compiler_chalang:doit(list_to_binary(SS3a)),
    %amount, newnonce, shares, delay
    {50, 999952, [], 999} = %the nonce is bigger than no_publish, by half a period. So the market maker can always stop a no_publish by publishing a new price declaration and using it in a channel_slash transaction.
	%The delay is until the contract expires. Once the oracle tells us a result we can do a channel slash to update to the outcome of our bet. So "amount" doesn't matter. It will eventually be replaced by the outcome of the bet.
	spk:run(fast, [SS3], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker cheated by publishing 2 different prices too near to each other in time.
    PD2 = <<(Height + 1):32, (Price - 1):16, MarketID:16>>,
    Signature2 = keys:raw_sign(PD2),
    true = sign:verify_sig(PD2, Signature2, keys:pubkey()),
    Sig2 = base64:decode(Signature2),
    SPD2 = <<PD2/binary, Sig2/binary>>,
    PriceDeclare2 = binary_to_list(base64:encode(SPD2)),
    SS4a = 
	" binary " ++ integer_to_list(size(SPD)) ++ " " ++ PriceDeclare ++ 
	" binary " ++ integer_to_list(size(SPD2)) ++ " " ++ PriceDeclare2 ++
	" int 2 ",
    SS4 = compiler_chalang:doit(list_to_binary(SS4a)),
    %amount, newnonce, shares, delay
    {0,2000001,[],0} = 
	%The nonce is super high, and the delay is zero, because if the market maker is cheating he should be punished immediately.
	%Amount is 0 because none of the money goes to the market maker.
       spk:run(fast, [SS4], SPK, 1, 0, Trees5),

    Fee = 20,
    %Accounts5 = trees:accounts(Trees5),
    %%make some bets in the oracle with oracle_bet
    %Governance5 = trees:governance(Trees5),
    %OIL = governance:get_value(oracle_initial_liquidity, Governance5),
    %{Tx5, _} = oracle_bet_tx:make(1, Fee, OID, true, OIL, Trees5), 
    %Stx5 = keys:sign(Tx5, Accounts5),
    %test_txs:absorb(Stx5),

    test_txs:mine_blocks(1),
    timer:sleep(1000),
    {Trees6, _, _} = tx_pool:data(),
    Accounts6 = trees:accounts(Trees6),
    %close the oracle with oracle_close
    {Tx6, _} = oracle_close_tx:make(keys:id(),Fee, OID, Trees6),
    Stx6 = keys:sign(Tx6, Accounts6),
    test_txs:absorb(Stx6),
    timer:sleep(1000),
    %amount, newnonce, shares, delay
    %Now that the bet is settled the delay is only zero so that we can get our money out as fast as possible.
    {100,1000003,[],0} = spk:run(fast, [SS1], SPK, 1, 0, Trees6),
    success.
    
    
    
    

