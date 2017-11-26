-module(market).
-export([price_declaration_maker/4, market_smart_contract/9,
	 settle/3,no_publish/1,evidence/2,
	 contradictory_prices/3, market_smart_contract_key/5,
	 unmatched/1,
	 test/0, test3/0]).

market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID) -> %contracts that can be arbitraged against each other have the same result.
    {market, 1, MarketID, Expires, Pubkey, Period, OID}.
market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Code0 = case Direction of %set to 10000 to bet on true, 0 to bet on false.
		1 -> <<" : bet_amount int 10000 ; : flip ; : check_size flip call > not ; ">>; %this is for when the customer bets on true.
		2 -> <<" : bet_amount int 0 ; : flip int 10000 swap - ; : check_size flip call < not ; ">> % maybe should be 10000 - MaxPrice0
	    end,
    {ok, Code} = file:read_file(BetLocation),%creates macro "bet" which is used in market.fs
    %MaxPrice is in the range 0 to 10000,
    % it is the limit of how much you are willing to pay the server for the derivative. You will pay this much or less.
    % Pubkey is the pubkey of the market manager.
    true = size(Pubkey) == constants:pubkey_size(),
    Code2 = " \
: Expires int " ++ integer_to_list(Expires) ++ " ;\
: MaxPrice int " ++ integer_to_list(MaxPrice) ++ " ;\
: MarketID int " ++ integer_to_list(MarketID) ++ " ;\
: Pubkey binary " ++ integer_to_list(size(Pubkey)) ++ " " ++ binary_to_list(base64:encode(Pubkey)) ++ " ;\
: Period int " ++ integer_to_list(Period) ++ " ;\
",
    PrivDir = code:priv_dir(ae_core),
    {ok, Code3} = file:read_file(PrivDir ++ "/market.fs"),
    FullCode = <<Code0/binary, (list_to_binary(Code2))/binary, Code/binary, Code3/binary>>,
    %io:fwrite(FullCode),
    Compiled = compiler_chalang:doit(FullCode),
    CodeKey = market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID),
    %ToProve = [{oracles, OID}],
    spk:new_bet(Compiled, CodeKey, Amount, {Direction, MaxPrice}).
unmatched(OID) ->
    SS = " int 4 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS)), [{oracles, OID}]).
settle(SPD, OID, Price) ->
    %If the oracle comes to a decision, this is how you get your money out.
    PriceDeclare = binary_to_list(base64:encode(SPD)),
    SS1a = "binary "++ integer_to_list(size(SPD))++ 
" " ++ PriceDeclare ++ " int 1",
    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(SS1a)), [{oracles, OID}]),
    spk:set_ss_meta(SS, Price).
no_publish(OID) ->
    %If the market maker fails in his duty to publish a price, this is how you withdraw your funds from the market early.
    SS2a = " int 0 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS2a)), [{oracles, OID}]).
evidence(SPD, OID) ->
    %If users try withdrawing funds while the market maker is still publishing prices, this is how he stops them from taking their money out early and robbing the market maker.
    SS3a = " binary " ++ integer_to_list(size(SPD)) ++ " " ++ binary_to_list(base64:encode(SPD)) ++ " int 3 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS3a)), [{oracles, OID}]).
contradictory_prices(SPD, SPD2, OID) ->
    %If the market maker publishes two prices too close to the same time, then this is how you can withdraw your funds from the market early.
    PriceDeclare1 = binary_to_list(base64:encode(SPD)),
    PriceDeclare2 = binary_to_list(base64:encode(SPD2)),
    SS4a = 
	" binary " ++ integer_to_list(size(SPD)) ++ " " ++ PriceDeclare1 ++ 
	" binary " ++ integer_to_list(size(SPD2)) ++ " " ++ PriceDeclare2 ++
	" int 2 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS4a)), [{oracles, OID}]).
price_declaration_maker(Height, Price, PortionMatched, MarketID) ->
    PD = <<Height:32, Price:16, PortionMatched:16, MarketID:16>>,
    Signature = keys:raw_sign(PD),
    Sig1 = base64:decode(Signature),
    <<PD/binary, Sig1/binary>>.
    %<<PD/binary, Signature/binary>>.


test() ->
    Question = <<>>,
    OID = 3,
    Fee = 20,
    Entropy = 555,
    tx_pool:dump(),
    headers:dump(),
    %block:initialize_chain(),
    {Trees,_,_Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = oracle_new_tx:make(constants:master_pub(), Fee, Question, 1, OID, 0, 0, Trees),
    Stx = keys:sign(Tx),
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
    {Tx2, _} = oracle_bet_tx:make(constants:master_pub(), Fee, OID, 1, OIL*2, Trees2), 
    Stx2 = keys:sign(Tx2),
    test_txs:absorb(Stx2),

    {Trees3, _, _} = tx_pool:data(),
    Accounts3 = trees:accounts(Trees3),
    {NewPub,NewPriv} = testnet_sign:new_key(),
    Amount = 1000000,
    {Ctx, _Proof} = create_account_tx:new(NewPub, Amount, Fee, constants:master_pub(), Trees3),
    Stx3 = keys:sign(Ctx),
    test_txs:absorb(Stx3),
    {Trees4, _, _} = tx_pool:data(),
    Accounts4 = trees:accounts(Trees4),
    
    CID = 5,
    Delay = 0,
    
    {Ctx4, _} = new_channel_tx:make(CID, Trees4, constants:master_pub(), NewPub, 10000, 20000, Entropy, Delay, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv), 
    test_txs:absorb(SStx4),
    timer:sleep(400),
    test2(NewPub). 

test2(NewPub) ->
    Entropy = 555,
    OID = 3,
    {Trees5, _, _} = tx_pool:data(),
    %Accounts5 = trees:accounts(Trees5),
    MarketID = 405,
    PrivDir = code:priv_dir(ae_core),
    Location = constants:oracle_bet(),
    Bet = market_smart_contract(Location, MarketID,1, 1000, 4000, keys:pubkey(),101,100,OID),
    SPK = spk:new(constants:master_pub(), NewPub, 1, [Bet], 10000, 10000, 1, 0, Entropy),
						%ScriptPubKey = testnet_sign:sign_tx(keys:sign(SPK), NewPub, NewPriv, ID2, Accounts5),
						%we need to try running it in all 4 ways of market, and all 4 ways of oracle_bet.
    Price = 3500,
    Height = 1,
    SPD = price_declaration_maker(Height, Price, 5000, MarketID),
    SS1 = settle(SPD, OID, Price),
    %First we check that if we try closing the bet early, it has a delay that lasts at least till Expires, which we can set far enough in the future that we can be confident that the oracle will be settled.
    %amount, newnonce, delay
    {60,1000002,999} = %the bet amount was 100, so if the oracle is canceled the money is split 50-50.
	spk:run(fast, [SS1], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker has disappeared and stopped publishing prices
    SS2 = no_publish(OID),
    %amount, newnonce, shares, delay
    {0, 999901, 101} = 
	spk:run(fast, [SS2], SPK, 1, 0, Trees5),
    
    %Next try closing it as if the market maker tries to stop us from closing the bet early, because he is still publishing data.
    SS3 = evidence(SPD, OID),
    %amount, newnonce, shares, delay
    {60, 999952, 999} = %the nonce is bigger than no_publish, by half a period. So the market maker can always stop a no_publish by publishing a new price declaration and using it in a channel_slash transaction.
	%The delay is until the contract expires. Once the oracle tells us a result we can do a channel slash to update to the outcome of our bet. So "amount" doesn't matter. It will eventually be replaced by the outcome of the bet.
	spk:run(fast, [SS3], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker cheated by publishing 2 different prices too near to each other in time.
    SPD2 = price_declaration_maker(Height+1, Price-1, 5000, MarketID),
    SS4 = contradictory_prices(SPD, SPD2, OID),
    %amount, newnonce, shares, delay
    {0,2000001,0} = 
	%The nonce is super high, and the delay is zero, because if the market maker is publishing contradictory prices, he should be punished immediately.
	%Amount is 0 because none of the money goes to the market maker.
       spk:run(fast, [SS4], SPK, 1, 0, Trees5),

    Fee = 20,

    test_txs:mine_blocks(1),
    timer:sleep(1000),
    {Trees6, _, _} = tx_pool:data(),
    Accounts6 = trees:accounts(Trees6),
    %close the oracle with oracle_close
    {Tx6, _} = oracle_close_tx:make(constants:master_pub(),Fee, OID, Trees6),
    Stx6 = keys:sign(Tx6),
    test_txs:absorb(Stx6),
    timer:sleep(1000),
    %amount, newnonce, shares, delay
    %Now that the bet is settled the delay is only zero so that we can get our money out as fast as possible.
    %The server won the bet, and gets all 100.
    %amount, newnonce, shares, delay
    {100,1000004,0} = spk:run(fast, [SS1], SPK, 1, 0, Trees6),

    %Now we will try betting in the opposite direction.
    PrivDir = code:priv_dir(ae_core),
    Bet2 = market_smart_contract(Location, MarketID,2, 1000, 8000, keys:pubkey(),101,100,OID),
    SPK2 = spk:new(constants:master_pub(), NewPub, 1, [Bet2], 10000, 10000, 1, 0, Entropy),
    %Again, the delay is zero, so we can get our money out as fast as possible once they oracle is settled.
    %This time we won the bet, so we keep all 100.
    %amount, newnonce, shares, delay
    {0,1000004,0} = spk:run(fast, [SS1], SPK2, 1, 0, Trees6),

    %test a trade that gets only partly matched.
    SPD3 = price_declaration_maker(Height, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SS5 = settle(SPD3, OID, 3000),
    %amount, newnonce, shares, delay
    {100, 1000004, 0} = spk:run(fast, [SS5], SPK, 1, 0, Trees5),
    %The first 50 tokens were won by betting, the next 20 tokens were a refund from a bet at 2-3 odds.

    %test a trade that goes unmatched.
    %since it is unmatched, they each get their money back.
    %the nonce is medium, and delay is non-zero because if a price declaration is found, it could be used.
    SS6 = unmatched(OID), 
    %amount, newnonce, delay
    {60, 500001, 51} = spk:run(fast, [SS6], SPK, 1, 0, Trees5),
    success.
    
test3() ->    
    OID = 123,
    BetLocation = constants:oracle_bet(),
    Pubkey = keys:pubkey(),
    A = market_smart_contract(BetLocation, OID, 1, 124, 125, Pubkey, 126, 127, OID),
    B = market_smart_contract(BetLocation, OID+10, 1, 134, 135, <<0:520>>, 136, 137, OID+10),
    io:fwrite("direction 1 \n"),
    io:fwrite(packer:pack(element(2, A))),
    io:fwrite("\n"),
    io:fwrite("direction 2 \n"),
    io:fwrite(packer:pack(element(2, B))),
    io:fwrite("\n"),

    A2 = "00000000c80000000001781600000000003a461414000000007e00000f42405e32000000007e330000000000471400000000013a46141416000000000a8715170200000041047baa5fa10aa21bef7aa91c6f21a0136e3b8d0e4ed22d9828edbb631e7471777ef09fdd0954e7333f10594512f4be68196531e423edb1e78775d65cca098655a30a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a000000000487160000000002870200000002000016861600000000028702000000020000168616020000000200001686000000007b3a00000000003a460000000001790a0d4700000000017900000000013200000000017848141414140000000002781500000000037815000000007d0a365000000000003a460000000001790a0d470000000001790000000001320000000001784814141e5e365000000000003a460000000001790a0d4700000000017900000000013200000000017848141483148316148316148314000000002087140000000001871614020000000300000016860a0a0a00000000013a461414000000000000000000030000002710471400000000023a461414000000000000000000030000002710000000271033471400000000033a461414000000000000000000030000002710000000007d33471400000000003a461414000000000100000000010000002710000000007d33474848484818000000007c5e1936463347141400000000004834175e161e1600000f42403216141f000000000379000000007d3a461414000000000279340000002710350000002710000000007d33000000271000000000027933340000002710353247141448471400000000023a46141414000000000a8715170200000041047baa5fa10aa21bef7aa91c6f21a0136e3b8d0e4ed22d9828edbb631e7471777ef09fdd0954e7333f10594512f4be68196531e423edb1e78775d65cca098655a30a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a000000000487160000000002870200000002000016861600000000028702000000020000168616020000000200001686000000007b3a00000000003a460000000001790a0d4700000000017900000000013200000000017848141414140000000004781e1e000000000a8715170200000041047baa5fa10aa21bef7aa91c6f21a0136e3b8d0e4ed22d9828edbb631e7471777ef09fdd0954e7333f10594512f4be68196531e423edb1e78775d65cca098655a30a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a000000000487160000000002870200000002000016861600000000028702000000020000168616020000000200001686000000007b3a00000000003a460000000001790a0d470000000001790000000001320000000001784814141414000000000578161f1919364616144714481e19364614471614481f33000000007e0000000002353700000000003a460000000001790a0d470000000001790000000001320000000001784814141f3a50161416140000000004790000000005793a50161416145200000000003a460000000001790a0d47000000000179000000000132000000000178481414000000000000000f424000000f4240320000000000471400000000033a46141414000000000a8715170200000041047baa5fa10aa21bef7aa91c6f21a0136e3b8d0e4ed22d9828edbb631e7471777ef09fdd0954e7333f10594512f4be68196531e423edb1e78775d65cca098655a30a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a000000000487160000000002870200000002000016861600000000028702000000020000168616020000000200001686000000007b3a00000000003a460000000001790a0d470000000001790000000001320000000001784814141414141414000000007c5e3300000f42405e32000000007e000000000235330000002710000000007d33471400000000043a461414831483161483161483140000000020871400000000018716140200000003000000168600000000003a46000000007c000000007e325e3300000000643200000186a00000002710000000007d33470000000033000007a1200000002710000000007d3348471448484848480b",
    B2 = "00000000c80000000001781600000000003a461414000000008800000f42405e320000000088330000000000471400000000013a46141416000000000a871517020000004100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a00000000048716000000000287020000000200001686160000000002870200000002000016861602000000020000168600000000853a00000000003a460000000001790a0d470000000001790000000001320000000001784814141414000000000278150000000003781500000000870a365000000000003a460000000001790a0d470000000001790000000001320000000001784814141e5e365000000000003a460000000001790a0d4700000000017900000000013200000000017848141483148316148316148314000000002087140000000001871614020000000300000016860a0a0a00000000013a461414000000000000000000030000002710471400000000023a461414000000000000000000030000002710000000271033471400000000033a461414000000000000000000030000002710000000008733471400000000003a46141400000000010000000001000000271000000000873347484848481800000000865e1936463347141400000000004834175e161e1600000f42403216141f00000000037900000000873a461414000000000279340000002710350000002710000000008733000000271000000000027933340000002710353247141448471400000000023a46141414000000000a871517020000004100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a00000000048716000000000287020000000200001686160000000002870200000002000016861602000000020000168600000000853a00000000003a460000000001790a0d4700000000017900000000013200000000017848141414140000000004781e1e000000000a871517020000004100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a00000000048716000000000287020000000200001686160000000002870200000002000016861602000000020000168600000000853a00000000003a460000000001790a0d470000000001790000000001320000000001784814141414000000000578161f1919364616144714481e19364614471614481f3300000000880000000002353700000000003a460000000001790a0d470000000001790000000001320000000001784814141f3a50161416140000000004790000000005793a50161416145200000000003a460000000001790a0d47000000000179000000000132000000000178481414000000000000000f424000000f4240320000000000471400000000033a46141414000000000a871517020000004100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a290a00000000003a460000000001790a0d470000000001790000000001320000000001784814140a0a0a0a0a00000000048716000000000287020000000200001686160000000002870200000002000016861602000000020000168600000000853a00000000003a460000000001790a0d47000000000179000000000132000000000178481414141414141400000000865e3300000f42405e320000000088000000000235330000002710000000008733471400000000043a461414831483161483161483140000000020871400000000018716140200000003000000168600000000003a4600000000860000000088325e3300000000643200000186a00000002710000000008733470000000033000007a120000000271000000000873348471448484848480b",
    compare_test(A2, B2, 0),
    success.
compare_test("", _, _) ->
    ok;
compare_test([A|[A2|AT]], [B|[B2|BT]], N) ->
    if 
        ((A == B) and (A2 == B2)) -> ok;
        true ->  
            io:fwrite("mismatch at byte "),
            io:fwrite(integer_to_list(N)),
            io:fwrite("\n")
    end,
    compare_test(AT, BT, N+1).
    

