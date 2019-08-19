-module(lisp_scalar).
-export([price_declaration_maker/4, market_smart_contract/13,
	 %settle/3,
         no_publish/1,evidence/2,
	 contradictory_prices/3, market_smart_contract_key/8,
	 unmatched/1, scalar_oracle_make/6,
	 test/0, test_contract/0, test3/0]).
-include("../records.hrl").

market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID, LowerLimit, UpperLimit, StartHeight) -> %contracts that can be arbitraged against each other have the same result.
    %true = LowerLimit < UpperLimit,
    %true = LowerLimit > -1,
    %true = UpperLimit < 1024,
    {market, 2, MarketID, Expires, Pubkey, Period, OID, LowerLimit, UpperLimit, StartHeight}.
market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID, Height, LowerLimit, UpperLimit, StartHeight) ->
    <<_:256>> = MarketID,
    true = size(Pubkey) == constants:pubkey_size(),
    Code = " (() \
" ++ integer_to_list(Direction) ++" \
" ++ integer_to_list(Height) ++ " \
" ++ integer_to_list(Expires) ++ " \
" ++ integer_to_list(MaxPrice) ++ " \
--" ++ binary_to_list(base64:encode(base64:encode(MarketID))) ++ " \
--" ++ binary_to_list(base64:encode(MarketID)) ++ " \
" ++ integer_to_list(Period) ++ " \
--" ++ binary_to_list(base64:encode(Pubkey)) ++ " ) \
" ++ integer_to_list(UpperLimit) ++ " \
" ++ integer_to_list(LowerLimit) ++ " \
" ++ integer_to_list(StartHeight) ++ " \
",
    %PrivDir = code:priv_dir(amoveo_core),
    PrivDir = "../../../../apps/amoveo_core/priv",
    {ok, Code3} = file:read_file(PrivDir ++ "/scalar_market.scm"),
    FullCode = <<(list_to_binary(Code))/binary, Code3/binary>>,
    %io:fwrite(FullCode),
    case compiler_lisp2:compile(FullCode, PrivDir++"/", true) of
        error -> error;
        Compiled ->
            %disassembler:doit(Compiled),
            io:fwrite("compiled code is \n"),
            %io:fwrite(base64:encode(Compiled)),
            %io:fwrite("\n"),
            io:fwrite(integer_to_list(size(Compiled))),%2080
            io:fwrite("\n"),
            CodeKey = market_smart_contract_key(MarketID, Expires, Pubkey, Period, OID, LowerLimit, UpperLimit, StartHeight),
    %ToProve = [{oracles, OID}],
    %A2 = Amount * (20000 - MaxPrice) div 10000,
            A2 = Amount * (10000 + MaxPrice) div 10000,
            spk:new_bet(Compiled, CodeKey, A2, {Direction, MaxPrice})
    end.
%    spk:new_bet(Compiled, CodeKey, Amount, {Direction, MaxPrice}).
unmatched(OID) ->
    SS = " int 4 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS)), [{oracles, OID}]).
settle_scalar_oracles(_, 0) -> [];
settle_scalar_oracles(OID, Many) ->
    H = {oracles, <<OID:256>>},
    [H|settle_scalar_oracles(OID+1, Many - 1)].
settle_scalar(SPD, OIDS, Price, Many) ->
    PriceDeclare = binary_to_list(base64:encode(SPD)),
    SS1a = "binary "++ integer_to_list(size(SPD))++ 
" " ++ PriceDeclare ++ " int 1",
    %OIDS = settle_scalar_oracles(OID, Many),
    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(SS1a)), OIDS),
    SS#ss{meta = Price}.
%settle(SPD, OID, Price) ->
    %If the oracle comes to a decision, this is how you get your money out.
%    PriceDeclare = binary_to_list(base64:encode(SPD)),
%    SS1a = "binary "++ integer_to_list(size(SPD))++ 
%" " ++ PriceDeclare ++ " int 1",
%    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(SS1a)), [{oracles, OID}]),
%    SS#ss{meta = Price}.
no_publish(OID) ->
    %If the market maker fails in his duty to publish a price, this is how you withdraw your funds from the market early.
    SS2a = " int 0 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS2a)), []).
evidence(SPD, OID) ->
    %If users try withdrawing funds while the market maker is still publishing prices, this is how he stops them from taking their money out early and robbing the market maker.
    SS3a = " binary " ++ integer_to_list(size(SPD)) ++ " " ++ binary_to_list(base64:encode(SPD)) ++ " int 3 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS3a)), []).
contradictory_prices(SPD, SPD2, OID) ->
    %If the market maker publishes two prices too close to the same time, then this is how you can withdraw your funds from the market early.
    PriceDeclare1 = binary_to_list(base64:encode(SPD)),
    PriceDeclare2 = binary_to_list(base64:encode(SPD2)),
    SS4a = 
	" binary " ++ integer_to_list(size(SPD)) ++ " " ++ PriceDeclare1 ++ 
	" binary " ++ integer_to_list(size(SPD2)) ++ " " ++ PriceDeclare2 ++
	" int 2 ",
    spk:new_ss(compiler_chalang:doit(list_to_binary(SS4a)), []).
price_declaration_maker(Height, Price, PortionMatched, MarketID) ->
    PD = <<Height:32, Price:16, PortionMatched:16, MarketID/binary>>,
    Signature = keys:raw_sign(PD),
    %Sig1 = base64:decode(Signature),
    %<<PD/binary, Sig1/binary>>.
    <<PD/binary, Signature/binary>>.

test_contract() ->
    %PrivDir = code:priv_dir(amoveo_core),
    %Location = PrivDir ++ "/scalar_oracle_bet.fs",
    %Location = constants:scalar_oracle_bet(),
    Location = "../../../../apps/amoveo_core/priv/scalar_oracle_bet.fs",
    {ok, Code} = file:read_file(Location),
    Code2 = " \
test \
",
    %can test out leverage by changing  UL and LL here.
    Code0 = " \
int 1023 UL ! \
int 0 LL ! \
int 10000 bet_amount ! \ % set to zero for bet on false.
",
    FullCode = <<(list_to_binary(Code0))/binary, Code/binary, (list_to_binary(Code2))/binary>>,
    %io:fwrite(FullCode),
    %io:fwrite("\n"),
    Compiled = compiler_chalang:doit(FullCode),
    Gas = 10000,
    chalang:test(Compiled, Gas, Gas, Gas, Gas, []).
scalar_oracle_make(SH, A, B, C, D, Limit) ->
    scalar_oracle_make(SH, A, B, C, D, 0, Limit).
scalar_oracle_make(_, _, _Fee, _Question, _, L, L) -> [];
scalar_oracle_make(StartHeight, Pubkey, Fee, Question, OID1, Many, Limit) ->
    %io:fwrite("SCALAR ORACLE MAKE\n"),
    Q1 = case Many of
             0 -> Question;
             _ ->
                 %QH = hash:doit(Question),
                 <<(list_to_binary("scalar "))/binary, 
                   (base64:encode(OID1))/binary, 
                   (list_to_binary(" bit number "))/binary, 
                   (list_to_binary (integer_to_list(Many)))/binary>>
         end,
    %Tx = oracle_new_tx:make_dict(Pubkey, Fee, Q1, 1 + block:height(), 0, 0),
    Tx = oracle_new_tx:make_dict(Pubkey, Fee, Q1, StartHeight, 0, 0),
    OID = oracle_new_tx:id(Tx),
    OIDR = case Many of
               0 -> OID;
               _ -> OID1
           end,
    %io:fwrite(packer:pack(Tx)),
    %io:fwrite("\n"),
    Stx = keys:sign(Tx),
    test_txs:absorb(Stx),
    [{oracles, OID}|scalar_oracle_make(StartHeight, Pubkey, Fee, Question, OIDR, Many + 1, Limit)].
scalar_bet_make(Pubkey, Fee, OIDL, Output, BetSize) -> 
    sbm(Pubkey, Fee, OIDL, Output, BetSize).
sbm(_, _, [], _, _) -> ok;
sbm(Pubkey, Fee, [{oracles, OID}|T], Output, BetSize) -> 
    %OIL = trees:get(governance, oracle_initial_liquidity),
    Bit = case (Output rem 2) of
	      0 -> 2;
	      1 -> 1
	      end,
    Tx2 = oracle_bet_tx:make_dict(Pubkey, Fee, OID, Bit, BetSize), 
    Stx2 = keys:sign(Tx2),
    test_txs:absorb(Stx2),
    timer:sleep(100),
    sbm(Pubkey, Fee, T, Output div 2, BetSize).
oracle_close_many(_Pubkey, _Fee, []) ->
    ok;
oracle_close_many(Pubkey, Fee, [{_, OID}|T]) ->
    Tx6 = oracle_close_tx:make_dict(Pubkey,Fee,OID),
    Stx6 = keys:sign(Tx6),
    test_txs:absorb(Stx6),
    oracle_close_many(Pubkey, Fee, T).

test() ->
    Question = <<>>,
    %OID = <<3:256>>,
    StartHeight = 6,
    Fee = 20 + constants:initial_fee(),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    test_txs:mine_blocks(2),
    timer:sleep(150),
    Many = 10,
    OIDL = scalar_oracle_make(StartHeight, constants:master_pub(), Fee, Question, -1, Many),
    %Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), OID, 0, 0),
    %Stx = keys:sign(Tx),
    %test_txs:absorb(Stx),
    timer:sleep(1000),
    test_txs:mine_blocks(1),%was 1
    timer:sleep(1000),
    %make some bets in the oracle with oracle_bet
    OIL = trees:get(governance, oracle_initial_liquidity),
    scalar_bet_make(constants:master_pub(), Fee, OIDL, 1023, OIL * 2), %512 is half way between all and nothing.
    %scalar_bet_make(constants:master_pub(), Fee, OID0, 0, Many, OIL * 2), %512 is half way between all and nothing.
    %1=2,

    %Tx2 = oracle_bet_tx:make_dict(constants:master_pub(), Fee, OID, 1, OIL*2), 
    %Stx2 = keys:sign(Tx2),
    %test_txs:absorb(Stx2),

    {NewPub,NewPriv} = testnet_sign:new_key(),
    Amount = 1000000,
    Ctx = create_account_tx:make_dict(NewPub, Amount, Fee, constants:master_pub()),
    Stx3 = keys:sign(Ctx),
    test_txs:absorb(Stx3),
    
    CID = <<5:256>>,
    Delay = 0,
    
    Ctx4 = new_channel_tx:make_dict(CID, constants:master_pub(), NewPub, 10000, 20000, Delay, Fee),
    %io:fwrite(packer:pack(Ctx4)),
    Stx4 = keys:sign(Ctx4),
    SStx4 = testnet_sign:sign_tx(Stx4, NewPub, NewPriv), 
    test_txs:absorb(SStx4),
    timer:sleep(400),
    test2(NewPub, Many, StartHeight, OIDL). 

test2(NewPub, Many, StartHeight, OIDL) ->
    %OID = <<3:256>>,
    %<<OIDN:256>> = OID,
   [{_, OID}|_] = OIDL,
    Fee = 20 + constants:initial_fee(),
    Trees5 = (tx_pool:get())#tx_pool.block_trees,
    %Dict5 = (tx_pool:get())#tx_pool.dict,
    %MarketID = <<405:256>>,
    MarketID = OID,
    LL = 0,
    UL = 1023,
    PrivDir = code:priv_dir(amoveo_core),
    %Location = constants:oracle_bet(),
    Location = "../../../../apps/amoveo_core/priv/scalar_oracle_bet.fs",
    Period = 3,
    Gas = 100000,
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Bet = market_smart_contract(Location, MarketID,1, 1000, 4000, keys:pubkey(),Period,100,OID, 0, LL, UL, StartHeight),
    SPK = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet], Gas, Gas, 1, 0),
						%ScriptPubKey = testnet_sign:sign_tx(keys:sign(SPK), NewPub, NewPriv, ID2, Accounts5),
						%we need to try running it in all 4 ways of market, and all 4 ways of oracle_bet.
    Price = 3500,
    _Height = 1,
    %SPD = price_declaration_maker(Height+5, Price, 5000, MarketID),
    SPD = price_declaration_maker(5, Price, 5000, MarketID),
    %SS1 = settle(SPD, OID, Price),
    SS1 = settle_scalar(SPD, OIDL, Price, Many),
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
    oracle_close_many(constants:master_pub(), Fee, OIDL),

    %Tx6 = oracle_close_tx:make_dict(constants:master_pub(),Fee, OID),
    %{Tx6, _} = oracle_close_tx:make(constants:master_pub(),Fee, OID, Trees60),
    %Stx6 = keys:sign(Tx6),
    %test_txs:absorb(Stx6),
    test_txs:mine_blocks(1),
    timer:sleep(1000),
    %amount, newnonce, shares, delay
    %Now that the bet is settled the delay is only zero so that we can get our money out as fast as possible.
    %The server won the bet, and gets all 100.
    %amount, newnonce, shares, delay
    Trees61 = (tx_pool:get())#tx_pool.block_trees,
    %{amount, nonce, delay}
    % if oracle amount is 0 {5,999,0} = spk:dict_run(fast, [SS1], SPK, 5, 0, Trees61),%ss1 is a settle-type ss
    %{45,999,0} = spk:dict_run(fast, [SS1], SPK, 5, 0, Trees61),%ss1 is a settle-type ss
    io:fwrite("scalar market ss 1: "),
    io:fwrite(packer:pack(SS1)),
%found a blockfound a blockscalar market ss 1: ["ss",
%"AgAAAHAAAAAFDawTiAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADMEYCIQCe4bS0HaB+/NHizg3XQ7xAuq2L0Xm73edGtlhmXIlHHQIhAMtyJSG3mRFbzDFZavAf09PTCY8omw7T2Ppvj8+XXtTKAAAAAAE="
%,[-6,["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAM="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAc="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAg="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAk="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAo="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAs="],["oracles","AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAw="]],3500]

    io:fwrite("\n"),
    {105,999,0} = spk:dict_run(fast, [SS1], SPK, 5, 0, Trees61),%ss1 is a settle-type ss
    % 5 is height.
    %{95,1000001,0} = spk:dict_run(fast, [SS1], SPK, 1, 0, Trees61),%ss1 is a settle-type ss
    %{95,1000001,0} = spk:dict_run(fast, [SS1], SPK, 1, 0, Dict60),

    %Now we will try betting in the opposite direction.
    PrivDir = code:priv_dir(amoveo_core),
    Bet2 = market_smart_contract(Location, MarketID,2, 1000, 8000, keys:pubkey(),Period,100,OID, 0, LL, UL, StartHeight),
    %willing to pay 8000, but it only cost 6500. so there should be a refund of 1500
    SPK2 = spk:new(constants:master_pub(), NewPub, <<1:256>>, [Bet2], Gas, Gas, 1, 0),
    %Again, the delay is zero, so we can get our money out as fast as possible once the oracle is settled.
    %This time we won the bet.
    %amount, newnonce, shares, delay
    % if oracle amount is 0 {15,999,0} = spk:dict_run(fast, [SS1], SPK2, 5, 0, Trees60),
    {14,999,0} = spk:dict_run(fast, [SS1], SPK2, 5, 0, Trees61),

    %test a trade that gets only partly matched.
    %SPD3 = price_declaration_maker(Height+5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    SPD3 = price_declaration_maker(5, 3000, 5000, MarketID),%5000 means it gets 50% matched.
    %SS5 = settle(SPD3, OID, 3000),
    SS5 = settle_scalar(SPD3, OIDL, 3000, Many),
    %amount, newnonce, shares, delay
    {109, 999, 0} = spk:dict_run(fast, [SS5], SPK, 5, 0, Trees61),
    %The first 50 tokens were won by betting, the next 20 tokens were a refund from a bet at 2-3 odds.

    %test a trade that goes unmatched.
    %since it is unmatched, they each get their money back.
    %the nonce is medium, and delay is non-zero because if a price declaration is found, it could be used.
    %SS6 = unmatched_scalar(OIDN, Many), 
    SS6 = unmatched(OID), 
    %amount, newnonce, delay
    {59, 2, Period} = spk:dict_run(fast, [SS6], SPK, 5, 0, Trees61),
    success.
test3() ->    
    %This makes the compiled smart contract in market.js
    OID = <<123:256>>,
    OID2 = <<-1:256>>,
    BetLocation = constants:scalar_oracle_bet(),
    Pubkey = keys:pubkey(),
    LL = 0,
    UL = 0,
    StartHeight = 2,
%market_smart_contract(BetLocation, MarketID, Direction, Expires, MaxPrice, Pubkey,Period,Amount, OID) ->
    Direction = 2,
    A = market_smart_contract(BetLocation, OID, Direction, 124, 125, Pubkey, 126, 0, OID, 0, LL, UL, StartHeight),
    Max = 4294967295,
    B = market_smart_contract(BetLocation, OID2, Direction, Max, Max, <<0:520>>, Max, Max, Max, Max, Max, Max, Max),
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
    

