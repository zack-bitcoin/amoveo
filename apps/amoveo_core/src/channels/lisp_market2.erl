-module(lisp_market2).
-export([price_declaration_maker/4, market_smart_contract/9,
	 settle/3,no_publish/1,evidence/2,
	 contradictory_prices/3, market_smart_contract_key/5,
	 unmatched/1,
	 test/0, test3/0]).
-include("../records.hrl").

market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID) -> %contracts that can be arbitraged against each other have the same result.
    {market, 1, MarketID, Expires, Pubkey, Period, OID}.
market_smart_contract(MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID, Height) ->
    <<_:256>> = MarketID,
    %{ok, Code} = file:read_file(BetLocation),%creates macro "bet" which is used in market.fs
    %MaxPrice is in the range 0 to 10000,
    % it is the limit of how much you are willing to pay the server for the derivative. You will pay this much or less.
    % Pubkey is the pubkey of the market manager.
    true = size(Pubkey) == constants:pubkey_size(),
    Code2 = " (()  \
" ++ integer_to_list(Direction) ++" \
" ++ integer_to_list(Height) ++ " \
" ++ integer_to_list(Expires) ++ " \
" ++ integer_to_list(MaxPrice) ++ " \
--" ++ binary_to_list(base64:encode(MarketID)) ++ " \
" ++ integer_to_list(Period) ++ " \
--" ++ binary_to_list(base64:encode(Pubkey)) ++ " ) \
",
    %PrivDir = code:priv_dir(amoveo_core),
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, Code3} = file:read_file(PrivDir ++ "/market2.scm"),
    FullCode = <<(list_to_binary(Code2))/binary, Code3/binary, <<"\n">>/binary >>,
    io:fwrite(FullCode),
    Compiled = compiler_lisp2:compile(FullCode, PrivDir ++"/", true),
    io:fwrite("compiled code is \n"),
    disassembler:doit(Compiled),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(size(Compiled))),%2080
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
    %OID = <<3:256>>,
    Fee = 20 + constants:initial_fee(),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    test_txs:mine_blocks(2),
    timer:sleep(150),
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), 0, 0),
    OID = oracle_new_tx:id(Tx),
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
    test2(NewPub, OID). 

test2(NewPub, OID) ->
    test_txs:mine_blocks(1),
    test_txs:mine_blocks(1),
    %OID = <<3:256>>,
    Gas = 40000,
    Fee = 20 + constants:initial_fee(),
    Trees5 = (tx_pool:get())#tx_pool.block_trees,
    %Dict5 = (tx_pool:get())#tx_pool.dict,
    %MarketID = <<405:256>>,
    MarketID = OID,
    %PrivDir = code:priv_dir(amoveo_core),
    Period = 3,
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Bet = market_smart_contract(MarketID,1, 1000, 4000, keys:pubkey(),Period,100,OID, 0),
    SPK = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet], Gas, Gas, 1, 0),
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
	%spk:dict_run(fast, [SS1], SPK, 1, 0, Trees5),

    %Next we try closing the bet as if the market maker has disappeared and stopped publishing prices
    SS2 = no_publish(OID),
    %amount, newnonce, delay
    {0, 2, Period} = 
	spk:dict_run(fast, [SS2], SPK, 5, 0, Trees5),
	%spk:dict_run(fast, [SS2], SPK, 1, 0, Dict5),
    
    {0, 1, 9999999} = spk:dict_run(fast, [SS1], SPK, 5, 0, Trees5),
    %Next try closing it as if the market maker tries to stop us from closing the bet early, because he is still publishing data.
    SS3 = evidence(SPD, OID),
    %amount, newnonce, delay
    {59, 3, 995} = %the nonce is bigger than no_publish, by half a period. So the market maker can always stop a no_publish by publishing a new price declaration and using it in a channel_slash transaction.
	%The delay is until the contract expires. Once the oracle tells us a result we can do a channel slash to update to the outcome of our bet. So "amount" doesn't matter. It will eventually be replaced by the outcome of the bet.
	spk:dict_run(fast, [SS3], SPK, 5, 0, Trees5),


    %Next we try closing the bet as if the market maker cheated by publishing 2 different prices too near to each other in time.
    %SPD2 = price_declaration_maker(Height+1, Price-1, 5000, MarketID),
    SPD2 = price_declaration_maker(5, Price-1, 5000, MarketID),
    SS4 = contradictory_prices(SPD, SPD2, OID),
    %amount, newnonce, shares, delay
    {0,2000001,0} = 
	%The nonce is super high, and the delay is zero, because if the market maker is publishing contradictory prices, he should be punished immediately.
	%Amount is 0 because none of the money goes to the market maker.
       spk:dict_run(fast, [SS4], SPK, 5, 0, Trees5),


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
    {105,999,0} = spk:dict_run(fast, [SS1], SPK, 5, 0, Trees61),%ss1 is a settle-type ss
    %{95,1000001,0} = spk:dict_run(fast, [SS1], SPK, 1, 0, Trees61),%ss1 is a settle-type ss
    %{95,1000001,0} = spk:dict_run(fast, [SS1], SPK, 1, 0, Dict60),

    %Now we will try betting in the opposite direction.
    PrivDir = code:priv_dir(amoveo_core),
    Bet2 = market_smart_contract(MarketID,2, 1000, 8000, keys:pubkey(),Period,100,OID, 0),
    SPK2 = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet2], Gas, Gas, 1, 0),
    %Again, the delay is zero, so we can get our money out as fast as possible once they oracle is settled.
    %This time we won the bet.
    %amount, newnonce, delay
    {14,999,0} = spk:dict_run(fast, [SS1], SPK2, 5, 0, Trees61),

    %test a trade that gets only partly matched.
    %SPD3 = price_declaration_maker(Height+5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SPD3 = price_declaration_maker(5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SS5 = settle(SPD3, OID, 3000),
    %amount, newnonce, shares, delay
    {109, 999, 0} = spk:dict_run(fast, [SS5], SPK, 5, 0, Trees61),
    %The first 50 tokens were won by betting, the next 20 tokens were a refund from a bet at 2-3 odds.

    %test a trade that goes unmatched.
    %since it is unmatched, they each get their money back.
    %the nonce is medium, and delay is non-zero because if a price declaration is found, it could be used.
    SS6 = unmatched(OID), 
    %amount, newnonce, delay
    {59, 2, Period} = spk:dict_run(fast, [SS6], SPK, 5, 0, Trees61),
    success.
test3() ->    
    %This makes the compiled smart contract in market.js
    OID = <<123:256>>,
    OID2 = <<-1:256>>,
    BetLocation = constants:oracle_bet(),
    Pubkey = keys:pubkey(),
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Direction = 2,
    A = market_smart_contract(OID, Direction, 124, 125, Pubkey, 126, 0, OID, 0),
    Max = 4294967295,
    B = market_smart_contract(OID2, Direction, Max, Max, <<0:520>>, Max, Max, Max, Max),
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
    
%to start with direction 1
%AAAAAAIA
%<<0,0,0,0,2,0>>

%direction 2
%AAAAAAEA
%<<0,0,0,0,1,0>>

%to load an integer
%AA==
%<<0>>

%to load first binary
%AgAAACA=
%<<2,0,0,0,32>>

%second binary
%AgAAAEE=
%<<2,0,0,0,65>>

%to end
%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
