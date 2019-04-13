-module(market).
-export([price_declaration_maker/4, market_smart_contract/10,
	 settle/3,no_publish/1,evidence/2,
	 contradictory_prices/3, market_smart_contract_key/5,
	 unmatched/1,
	 test/0, test3/0]).
-include("../records.hrl").

market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID) -> %contracts that can be arbitraged against each other have the same result.
    {market, 1, MarketID, Expires, Pubkey, Period, OID}.
market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID, Height) ->
    <<_:256>> = MarketID,
    Code0 = case Direction of %set to 10000 to bet on true, 0 to bet on false.
		1 -> <<" int 10000 bet_amount ! macro flip int 0 swap + ; macro check_size flip > not ; ">>; %this is for when the customer bets on true.
		2 -> <<" int 0 bet_amount ! macro flip int 10000 swap - ; macro check_size flip < not ; ">> % maybe should be 10000 - MaxPrice0
	    end,
    {ok, Code} = file:read_file(BetLocation),%creates macro "bet" which is used in market.fs
    %MaxPrice is in the range 0 to 10000,
    % it is the limit of how much you are willing to pay the server for the derivative. You will pay this much or less.
    % Pubkey is the pubkey of the market manager.
    true = size(Pubkey) == constants:pubkey_size(),
    Code2 = " \
int " ++ integer_to_list(Height) ++ " Height ! \
int " ++ integer_to_list(Expires) ++ " Expires ! \
int " ++ integer_to_list(MaxPrice) ++ " MaxPrice ! \
binary 32 " ++ binary_to_list(base64:encode(MarketID)) ++ " MarketID ! \
int " ++ integer_to_list(Period) ++ " Period ! \
binary " ++ integer_to_list(size(Pubkey)) ++ " " ++ binary_to_list(base64:encode(Pubkey)) ++ " Pubkey ! \
",
    PrivDir = code:priv_dir(amoveo_core),
    {ok, Code3} = file:read_file(PrivDir ++ "/market.fs"),
    FullCode = <<Code0/binary, (list_to_binary(Code2))/binary, Code/binary, Code3/binary>>,
    %io:fwrite(FullCode),
    Compiled = compiler_chalang:doit(FullCode),
    io:fwrite("compiled code is \n"),
    io:fwrite(base64:encode(Compiled)),
    io:fwrite("\n"),
    CodeKey = market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID),
    %ToProve = [{oracles, OID}],
    A2 = Amount * (10000 + MaxPrice) div 10000,
    spk:new_bet(Compiled, CodeKey, A2, {Direction, MaxPrice}).
%    spk:new_bet(Compiled, CodeKey, Amount, {Direction, MaxPrice}).
unmatched(OID) ->
    SS = " int 4 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS)), [{oracles, OID}]).
settle(SPD, OID, Price) ->
    %If the oracle comes to a decision, this is how you get your money out.
    PriceDeclare = binary_to_list(base64:encode(SPD)),
    SS1a = "binary "++ integer_to_list(size(SPD))++ 
" " ++ PriceDeclare ++ " int 1",
    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(SS1a)), [{oracles, OID}]),
    SS#ss{meta = Price}.
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
    PD = <<Height:32, Price:16, PortionMatched:16, MarketID/binary>>,
    Signature = keys:raw_sign(PD),
    %Sig1 = base64:decode(Signature),
    %<<PD/binary, Sig1/binary>>.
    <<PD/binary, Signature/binary>>.


test() ->
    Question = <<>>,
    OID = <<3:256>>,
    Fee = 20 + constants:initial_fee(),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    test_txs:mine_blocks(2),
    timer:sleep(150),
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), OID, 0, 0),
    Stx = keys:sign(Tx),
    test_txs:absorb(Stx),
    timer:sleep(200),
    test_txs:mine_blocks(1),%was 1
    timer:sleep(1000),
    %make some bets in the oracle with oracle_bet
    OIL = trees:get(governance, oracle_initial_liquidity),
    Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL*2), 
    Stx2 = keys:sign(Tx2),
    test_txs:absorb(Stx2),

    {NewPub,NewPriv} = testnet_sign:new_key(),
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx3 = keys:sign(Ctx),
    test_txs:absorb(Stx3),
    
    CID = <<5:256>>,
    Delay = 0,
    
    Ctx4 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv), 
    test_txs:absorb(SStx4),
    timer:sleep(400),
    test2(NewPub). 

test2(NewPub) ->
    test_txs:mine_blocks(1),
    test_txs:mine_blocks(1),
    OID = <<3:256>>,
    Fee = 20 + constants:initial_fee(),
    Trees5 = (tx_pool:get())#tx_pool.block_trees,
    %Dict5 = (tx_pool:get())#tx_pool.dict,
    %MarketID = <<405:256>>,
    MarketID = OID,
    PrivDir = code:priv_dir(amoveo_core),
    Location = constants:oracle_bet(),
    Period = 3,
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Bet = market_smart_contract(Location, MarketID,1, 1000, 4000, keys:pubkey(),Period,100,OID, 0),
    SPK = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet], 10000, 10000, 1, 0),
						%ScriptPubKey = testnet_sign:sign_tx(keys:sign(SPK), NewPub, NewPriv, ID2, Accounts5),
						%we need to try running it in all 4 ways of market, and all 4 ways of oracle_bet.
    Price = 3500,
    Height = 1,
    %SPD = price_declaration_maker(Height+5, Price, 5000, MarketID),
    SPD = price_declaration_maker(5, Price, 5000, MarketID),
    SS1 = settle(SPD, OID, Price),
    %First we check that if we try closing the bet early, it has a delay that lasts at least till Expires, which we can set far enough in the future that we can be confident that the oracle will be settled.
    %amount, newnonce, delay
    %{55,1000001,999} = %the bet amount was 100, so if the oracle is canceled the money is split 50-50.
	%spk:run(fast, [SS1], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker has disappeared and stopped publishing prices
    SS2 = no_publish(OID),
    %amount, newnonce, delay
    {0, 2, Period} = 
	spk:run(fast, [SS2], SPK, 5, 0, Trees5),
	%spk:dict_run(fast, [SS2], SPK, 1, 0, Dict5),
    
    %Next try closing it as if the market maker tries to stop us from closing the bet early, because he is still publishing data.
    SS3 = evidence(SPD, OID),
    %amount, newnonce, delay
    {59, 3, 995} = %the nonce is bigger than no_publish, by half a period. So the market maker can always stop a no_publish by publishing a new price declaration and using it in a channel_slash transaction.
	%The delay is until the contract expires. Once the oracle tells us a result we can do a channel slash to update to the outcome of our bet. So "amount" doesn't matter. It will eventually be replaced by the outcome of the bet.
	spk:run(fast, [SS3], SPK, 5, 0, Trees5),


    %Next we try closing the bet as if the market maker cheated by publishing 2 different prices too near to each other in time.
    %SPD2 = price_declaration_maker(Height+1, Price-1, 5000, MarketID),
    SPD2 = price_declaration_maker(5, Price-1, 5000, MarketID),
    SS4 = contradictory_prices(SPD, SPD2, OID),
    %amount, newnonce, shares, delay
    {0,2000001,0} = 
	%The nonce is super high, and the delay is zero, because if the market maker is publishing contradictory prices, he should be punished immediately.
	%Amount is 0 because none of the money goes to the market maker.
       spk:run(fast, [SS4], SPK, 5, 0, Trees5),


    test_txs:mine_blocks(1),
    timer:sleep(1000),
    Trees60 = (tx_pool:get())#tx_pool.block_trees,
    %Dict60 = (tx_pool:get())#tx_pool.dict,
    %close the oracle with oracle_close
    Tx6 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    %{Tx6, _} = oracle_close_tx:make(constants:master_pub(),Fee, OID, Trees60),
    Stx6 = keys:sign(Tx6),
    test_txs:absorb(Stx6),
    test_txs:mine_blocks(1),
    timer:sleep(1000),
    %amount, newnonce, delay
    %Now that the bet is settled the delay is only zero so that we can get our money out as fast as possible.
    %The server won the bet, and gets all 100.
    %amount, newnonce, delay
    Trees61 = (tx_pool:get())#tx_pool.block_trees,
    {105,999,0} = spk:run(fast, [SS1], SPK, 5, 0, Trees61),%ss1 is a settle-type ss
    %{95,1000001,0} = spk:run(fast, [SS1], SPK, 1, 0, Trees61),%ss1 is a settle-type ss
    %{95,1000001,0} = spk:dict_run(fast, [SS1], SPK, 1, 0, Dict60),

    %Now we will try betting in the opposite direction.
    PrivDir = code:priv_dir(amoveo_core),
    Bet2 = market_smart_contract(Location, MarketID,2, 1000, 8000, keys:pubkey(),Period,100,OID, 0),
    SPK2 = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet2], 10000, 10000, 1, 0),
    %Again, the delay is zero, so we can get our money out as fast as possible once they oracle is settled.
    %This time we won the bet.
    %amount, newnonce, shares, delay
    {14,999,0} = spk:run(fast, [SS1], SPK2, 5, 0, Trees60),

    %test a trade that gets only partly matched.
    %SPD3 = price_declaration_maker(Height+5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SPD3 = price_declaration_maker(5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SS5 = settle(SPD3, OID, 3000),
    %amount, newnonce, shares, delay
    {109, 999, 0} = spk:run(fast, [SS5], SPK, 5, 0, Trees5),
    %The first 50 tokens were won by betting, the next 20 tokens were a refund from a bet at 2-3 odds.

    %test a trade that goes unmatched.
    %since it is unmatched, they each get their money back.
    %the nonce is medium, and delay is non-zero because if a price declaration is found, it could be used.
    SS6 = unmatched(OID), 
    %amount, newnonce, delay
    {59, 2, Period} = spk:run(fast, [SS6], SPK, 5, 0, Trees5),
    success.
test3() ->    
    %This makes the compiled smart contract in market.js
    OID = <<123:256>>,
    OID2 = <<-1:256>>,
    BetLocation = constants:oracle_bet(),
    Pubkey = keys:pubkey(),
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Direction = 2,
    A = market_smart_contract(BetLocation, OID, Direction, 124, 125, Pubkey, 126, 0, OID, 0),
    Max = 4294967295,
    B = market_smart_contract(BetLocation, OID2, Direction, Max, Max, <<0:520>>, Max, Max, Max, Max),
    A2 = element(2, A),
    B2 = element(2, B),
    compare_test(A2, B2, 0, <<>>),
    success.
compare_test(<<>>, _, _, Final) ->
    io:fwrite(base64:encode(binary_reverse(Final, <<>>))),
    io:fwrite("\n"),
    ok;
compare_test(<<A, AT/binary>>, <<B, BT/binary>>, N, Final) ->
    if
        (A == B) -> compare_test(AT, BT, N+1, <<A, Final/binary>>);
        true ->
            if
                Final == <<>> -> ok;
                true ->
                    io:fwrite(base64:encode(binary_reverse(Final, <<>>))),
                    io:fwrite("\n")
            end,
            io:fwrite("mismatch at byte "),
            io:fwrite(integer_to_list(N)),
            io:fwrite(", "),
            io:fwrite(integer_to_list(A)),
            io:fwrite(", "),
            io:fwrite(integer_to_list(B)),
            io:fwrite("\n"),
            compare_test(AT, BT, N+1, <<>>)
    end.
binary_reverse(<<>>, X) -> X;
binary_reverse(<<A, T/binary>>, X) ->
    binary_reverse(T, <<A, X/binary>>).
    

