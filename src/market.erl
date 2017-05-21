-module(market).
-export([test/0, test2/0]).

%we need a smart contract for the trust-free markets for financial derivatives.
%It acts as an order book.
%the market maker has to publish the price periodically, every trade is matched at the earliest price possible.
%If the market maker publishes a price early, he loses every bet. Delay = 0, nonce = big
%If the market maker publishes contradictory prices at the same time, he loses every bet. delay = 0, nonce = big
%if the market maker failes to publish by a certain point in time, then everyone can take all the money from the channels. delay = low, nonce = big. 

%The contract involves verifying the market maker's signature, and checking that the thing that was signed is correctly formatted.

%First we should write the contract for a bet based on the outcome of an oracle, then we should embed that contract inside the market contract.

%facts [[5, Key, SerializedOracle]]
%If the oracle is undecided, amount = 1/2 delay = long, nonce = small
%If the oracle is decided, amount = 1 or 0, delay = 0, nonce = big.
test() ->
    Question = <<>>,
    OID = 1,
    Fee = 20,
    Entropy = 555,
    tx_pool:dump(),
    {Trees,_,_Txs} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {Tx, _} = oracle_new_tx:make(1, Fee, Question, 1, OID, constants:initial_difficulty(), 0, 0, 0, Trees),
    Stx = keys:sign(Tx, Accounts),
    test_txs:absorb(Stx),
    OID = 1,
    Fee = 20,
    Entropy = 555,
    timer:sleep(150),
    test_txs:mine_blocks(2),
    {Trees2, _, _} = tx_pool:data(),
    Accounts2 = trees:accounts(Trees2),
    %make some bets in the oracle with oracle_bet
    Governance2 = trees:governance(Trees2),
    OIL = governance:get_value(oracle_initial_liquidity, Governance2),
    {Tx2, _} = oracle_bet_tx:make(1, Fee, OID, true, OIL, Trees2), 
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
    {Trees5, _, _} = tx_pool:data(),
    %Accounts5 = trees:accounts(Trees5),
    io:fwrite("\n\n\n"),
    {ok, Code} = file:read_file("src/market.fs"),
    %Code = <<" print car swap drop print print int 1 int 1 int 1 nil ">>,
    Compiled = compiler_chalang:doit(Code),
    Bet = spk:new_bet(Compiled, 100, [{oracles, 1}]),
    SPK = spk:new(1, 2, 1, [Bet], 10000, 10000, 1, 0, Entropy),
    %ScriptPubKey = testnet_sign:sign_tx(keys:sign(SPK, Accounts5), NewPub, NewPriv, ID2, Accounts5),
    spk:run(fast, [<<>>], SPK, 1, 0, Trees5),
    ok.
