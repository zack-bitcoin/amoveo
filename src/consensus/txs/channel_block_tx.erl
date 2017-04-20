%Channel blocks need to be rethought a little.
%We need to add a third signature, to lock in the secret_hashes. 
%The fee and oracle judgement should be signed by the third signature, but not the first 2.
%Channel_slasher needs to be upgraded so that you can slash someone, not just for publishing a low-nonced channel block, but also for failing to provide some evidence.
%SignedCB = #signed{data = channel_block, sig1 = signature, sig2 = signature, revealed = [evidence]}
%#signed{data = #signed_channel_block{channel_block = SignedCB, fee = 100}, sig1 = signature}.

-module(channel_block_tx).
-export([doit/7, origin_tx/3, channel/7, channel_block/5, channel_block/6, cc_losses/1, close_channel/4, id/1, delay/1, nonce/1, make_signed_cb/4, reveal_union/4, slash_bet/1, make_bet/2, acc1/1, acc2/1, amount/1, bets/1, fast/1, expiration/1, nlock/1, fee/1, add_bet/4, bet_code/1, bet_amount/1, bet_to/1, update/3, is_cb/1, channel_block_from_channel/7, replace_bet/3, test/0]).
-record(channel_block, {acc1 = 0, acc2 = 0, amount = 0, nonce = 0, bets = [], id = 0, fast = false, delay = 10, expiration = 0, nlock = 0, fee = 0}).
is_cb(CB) -> is_record(CB, channel_block).
acc1(CB) -> CB#channel_block.acc1.
acc2(CB) -> CB#channel_block.acc2.
amount(CB) -> CB#channel_block.amount.
bets(Ch) -> Ch#channel_block.bets.
id(Ch) -> Ch#channel_block.id.
fast(Ch) -> Ch#channel_block.fast.
expiration(Ch) -> Ch#channel_block.expiration.
nlock(Ch) -> Ch#channel_block.nlock.
fee(Ch) -> Ch#channel_block.fee.
nonce(X) -> X#channel_block.nonce.
delay(X) -> X#channel_block.delay.
-record(signed_cb, {acc = 0, nonce = 0, channel_block = #channel_block{}, fee = 0}).
-record(bet, {amount = 0, code = language:assemble([crash]), to = 1}).%code is like scriptpubkey from bitcoin.
bet_code(Bet) -> Bet#bet.code.
bet_amount(Bet) -> Bet#bet.amount.
bet_to(Bet) -> Bet#bet.to.
-record(tc, {acc1 = 0, acc2 = 0, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = false, fee = 0, id = -1, increment = 0}).
add_bet(CB, Amount, Code, To) ->
    0 = Amount rem 2,
    A = abs(Amount div 2),
    Bets = CB#channel_block.bets,
    NewBets = [#bet{amount = abs(A), code = Code, to = To}|Bets],
    replace_bet(CB, NewBets, To * A).
%replace_bet(CB, NewBets, (To * Amount) div 2).
replace_bet(CB, NewBets, Amount) ->
    update(CB, Amount, 0, NewBets, CB#channel_block.fast, CB#channel_block.delay, CB#channel_block.expiration, CB#channel_block.nlock, CB#channel_block.fee).
    
update(CB, Amount, Nonce) ->
    update(CB, Amount, Nonce, CB#channel_block.bets, CB#channel_block.fast, CB#channel_block.delay, CB#channel_block.expiration, CB#channel_block.nlock, CB#channel_block.fee).
update(CB, Amount, Nonce, NewBets, Fast, Delay, Expiration, Nlock, Fee) -> 
    BetAmount = bets_sum(CB#channel_block.bets),
    Channel = block_tree:channel(CB#channel_block.id),
    CB1C = channels:bal1(Channel),
    CB2C = channels:bal2(Channel),
    TCBA = CB#channel_block.amount,
    %StartAmount = CB1C + CB2C,

    true = ((CB1C - TCBA - BetAmount) > -1),
    true = ((CB2C + TCBA - BetAmount) > -1),
    
    Height = block_tree:height(),
    true = (CB#channel_block.expiration == 0) or (CB#channel_block.expiration > Height),    
    A = CB#channel_block.amount + Amount,

    #channel_block{acc1 = CB#channel_block.acc1, acc2 = CB#channel_block.acc2, amount = A, nonce = CB#channel_block.nonce + Nonce, bets = NewBets, id = CB#channel_block.id, fast = Fast, delay = Delay, expiration = Expiration, nlock = Nlock, fee = Fee}.
make_bet(Amount, Code) ->
    #bet{amount = Amount, code = Code}.
make_signed_cb(Acc, CB, Fee, Evidence) ->
    A = block_tree:account(Acc),
    Nonce = accounts:nonce(A),
    NewCB = #signed_cb{acc = Acc, nonce = Nonce + 1, channel_block = CB, fee = Fee},
    testnet_sign:set_revealed(testnet_sign:empty(NewCB), Evidence).
close_channel(Id, Amount, Nonce, Fee) ->
    Channel = block_tree:channel(Id),
    #channel_block{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), amount = Amount, nonce = Nonce, id = Id, fast = true, fee = Fee}.
cc_losses(Txs) -> cc_losses(Txs, 0).%filter out channel_block, channel_slash, and channel_close type txs. add up the amount of money in each such channel. 
cc_losses([], X) -> X;
cc_losses([SignedTx|T], X) -> 
    Tx = testnet_sign:data(SignedTx),
    case element(1, Tx) of
	signed_cb ->
	    SCBTx = testnet_sign:data(SignedTx),
	    STx = SCBTx#signed_cb.channel_block,
	    Tt = testnet_sign:data(STx),
	    Channel = block_tree:channel(Tt#channel_block.id, dict:new()),
	    SA = channels:bal1(Channel) + channels:bal2(Channel),
	    cc_losses(T, X+SA);
	channel_slash ->
	    cc_losses([channel_slash_tx:channel_block(Tx)|T], X);
	channel_close ->
	    Id = channel_close_tx:id(Tx),
	    Channel = block_tree:channel(Id, dict:new()),
	    SA = channels:bal1(Channel) + channels:bal2(Channel),
	    cc_losses(T, X+SA);
	_ -> cc_losses(T, X)
    end.
    
creator([], _) -> testnet_sign:empty(#tc{});
creator([SignedTx|T], Id) ->
    Tx = testnet_sign:data(SignedTx),
    Type = element(1, Tx),
    if
	Type == timeout ->
	    SSignedCB = channel_timeout_tx:channel_block(Tx),
	    CB = testnet_sign:data(SSignedCB),
	    %SCB = SignedCB#signed_cb.channel_block,
	    %CB = testnet_sign:data(SCB),
	    I = CB#channel_block.id,
	    if 
		I == Id -> SignedTx;
		true -> creator(T, Id)
	    end;
	true ->
	    creator(T, Id)
    end.
bets_sum(X) -> bets_sum(X, 0).
bets_sum([], X) -> X;
bets_sum([Tx|Txs], X) -> bets_sum(Txs, X+abs(Tx#bet.amount)).
channel_block(Id, Amount, Nonce, Delay, Fee) ->
channel_block(Id, Amount, Nonce, Delay, Fee, []).
channel_block(Id, Amount, Nonce, Delay, Fee, Bets) ->
    Channel = block_tree:channel(Id),
    channel_block_from_channel(Id, Channel, Amount, Nonce, Delay, Fee, Bets).
channel_block_from_channel(Id, Channel, Amount, Nonce, Delay, Fee, Bets) ->
    true = Delay < constants:max_reveal(),
    #channel_block{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), amount = Amount, nonce = Nonce, id = Id, fast = false, delay = Delay, fee = Fee, bets=Bets}.
origin_tx(BlockNumber, ParentKey, ID) ->
    OriginBlock = block_tree:read_int(BlockNumber, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    creator(OriginTxs, ID).
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    CB = testnet_sign:data(Tx#signed_cb.channel_block),
    true = CB#channel_block.fast,%If fast is false, then you have to use close_channel instead. 
    A = Tx#signed_cb.acc,
    Acc = block_tree:account(A, ParentKey, Accounts),
    NAcc = accounts:update(Acc, NewHeight, -Tx#signed_cb.fee, 0, 1, TotalCoins),
    NewAccounts = dict:store(A, NAcc, Accounts),
    Nonce = accounts:nonce(NAcc),
    Nonce = Tx#signed_cb.nonce,
    channel(Tx#signed_cb.channel_block, ParentKey, Channels, NewAccounts, TotalCoins, S, NewHeight).

channel(SignedCB, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    CB = testnet_sign:data(SignedCB),
    Acc1 = block_tree:account(CB#channel_block.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(CB#channel_block.acc2, ParentKey, Accounts),
    Channel = block_tree:channel(CB#channel_block.id, ParentKey, Channels),
    FChannel = channels:read_channel(CB#channel_block.id),
    AccN1 = CB#channel_block.acc1,
    AccN1 = channels:acc1(Channel),
    AccN2 = CB#channel_block.acc2,
    AccN2 = channels:acc2(Channel),
    CB1C = channels:bal1(Channel),
    CB2C = channels:bal2(Channel),
    StartAmount = CB1C + CB2C,
    BetAmount = bets_sum(CB#channel_block.bets),
    TCBA = CB#channel_block.amount,
    true = CB1C + TCBA - BetAmount > -1,
    true = CB2C - TCBA - BetAmount > -1,

    true = (CB#channel_block.expiration == 0) or (CB#channel_block.expiration > NewHeight),    
    true = (CB#channel_block.nlock < NewHeight),
    A1 = CB#channel_block.acc1,
    A2 = CB#channel_block.acc2,
    B1 = channels:acc1(FChannel),
    B2 = channels:acc2(FChannel),
    Type = channels:type(Channel),
    {D1, D2} = 
	if
	    not ((B1 == A1) and
		 (B2 == A2)) ->
		{0, 0};
	    <<"delegated_1">> == Type ->
		{StartAmount, 0};
	    Type == <<"delegated_2">> ->
		{0, StartAmount}
    end,
    {Win2, Win1, Loss} = bet_results(CB#channel_block.bets, testnet_sign:revealed(SignedCB), BetAmount),
    N1 = accounts:update(Acc1, NewHeight, channels:bal1(Channel) + CB#channel_block.amount + Win1 - Win2, -D1, 0, TotalCoins),
    N2 = accounts:update(Acc2, NewHeight, channels:bal2(Channel) - CB#channel_block.amount + Win2 - Win1, -D2, 0, TotalCoins),
    NewChannels = dict:store(CB#channel_block.id, channels:empty(),Channels),
    NewAccounts1 = dict:store(CB#channel_block.acc1, N1, Accounts),
    NewAccounts2 = dict:store(CB#channel_block.acc2, N2, NewAccounts1),
    {NewChannels, NewAccounts2, TotalCoins - Loss, S}.%remove money from totalcoins that was deleted in bets.

bet_results(Bets, Revealed, BetAmount) -> 
    bet_results(Bets, Revealed, BetAmount, {0,0,0}).
bet_results([], [], _, X) -> X;
bet_results(_, [], BetAmount, {Win1, Win2, Loss}) -> 
    {Win1, Win2, Loss+BetAmount - Win1 - Win2};
bet_results([B|Bets], [Y|Revealed], BetAmount, {Win1, Win2, Loss}) when not is_list(Y)-> 
    X = {Win1, Win2, Loss+B#bet.amount},
    bet_results(Bets, Revealed, BetAmount, X);
bet_results([B|Bets], [R|Revealed], BA, {Win1, Win2, Loss}) ->
    {_, X, Del} = language:run_script(R++B#bet.code, constants:gas_limit()),
    true = fractions:is_fraction(X),
    true = fractions:is_fraction(Del),
    Y = fractions:subtract(fractions:subtract(fractions:new(1, 1), X), Del),
    TooSmall1 = fractions:less_than(X, fractions:new(0, 1)),
    TooSmall2 = fractions:less_than(Del, fractions:new(0, 1)),
    TooSmall3 = fractions:less_than(Y, fractions:new(0, 1)),
    TooBig = fractions:less_than(fractions:new(1, 1), fractions:add(X, Del)),
    D = fractions:multiply_int(Del, B#bet.amount),
    More1 = fractions:multiply_int(X, B#bet.amount),
    More2 = fractions:multiply_int(Y, B#bet.amount),
    Out = if
	      ((TooSmall1 or TooSmall2) or (TooSmall3 or TooBig)) -> 
		  {Win1, Win2, Loss + B#bet.amount};
	      B#bet.to == -1 ->
		  {Win1 + More1, Win2 + More2, Loss + D};
	      true ->
		  {Win1 + More2, Win2 + More1, Loss + D}
	  end,
    bet_results(Bets, Revealed, BA, Out).
code(X) -> code(X#channel_block.bets, []).
code([], Out) -> lists:reverse(Out);
code([B|Bets], Out) -> code(Bets, [B#bet.code|Out]).
reveal_union(CB1, Revealed1, CB2, Revealed2) ->
    BN1 = bet_nonces(code(CB1), Revealed1),
    BN2 = bet_nonces(code(CB2), Revealed2),
    union_helper(BN1, BN2, Revealed1, Revealed2).
union_helper([],[],[],[]) -> [];
union_helper([B1|BN1], [B2|BN2], [R1|RN1], [_|RN2]) when B1>B2->
    [R1|union_helper(BN1, BN2, RN1, RN2)];
union_helper([_|BN1], [_|BN2], [_|RN1], [R2|RN2]) ->
    [R2|union_helper(BN1, BN2, RN1, RN2)].

bet_nonces(Bets, Revealed) -> bet_nonces(Bets, Revealed, []).
bet_nonces([], [], Out) -> Out;
bet_nonces([_|Bets], [], Out) -> bet_nonces(Bets, [], [0|Out]);
bet_nonces([_|Bets], [R|Reveal], Out) when not is_list(R) ->
    bet_nonces(Bets, Reveal, [0|Out]);
bet_nonces([B|Bets], [R|Reveal], Out) ->
    {X, _, _} = language:run_script(R++B, constants:gas_limit()),
    true = is_integer(X),
    %X = hd(language:run(R++B)),
    bet_nonces(Bets, Reveal, [X|Out]).
%slash(CB) -> slash(CB#channel_block.bets, []).
%slash([], Out) -> lists:reverse(Out);
%slash([C|CB], Out) -> slash(CB, [slash_bet(C)|Out]).
slash_bet(B) -> %replace every 34 in the code with true. 
    Bets = B#channel_block.bets,
    NewBets = slash_codes(Bets),
    #channel_block{acc1 = B#channel_block.acc1, acc2 = B#channel_block.acc2, amount = B#channel_block.amount, nonce = B#channel_block.nonce, bets = NewBets, id = B#channel_block.id, fast = B#channel_block.fast, delay = B#channel_block.delay, expiration = B#channel_block.expiration, nlock = B#channel_block.nlock, fee = B#channel_block.fee}.
slash_codes(X) -> slash_codes(X, []).
slash_codes([], Out) -> lists:reverse(Out);
slash_codes(X, Out) -> slash_codes([slash_code(X)|Out]).
slash_code(X) -> slash_code(X, []).
slash_code([], Out) -> lists:reverse(Out);
slash_code([Word|X], Out) -> slash_code(X, [slash_word(Word)|Out]).
slash_word(34) -> true;
slash_word(X) -> X.
    
test() ->    
    CB = #channel_block{},
    update(CB, 100, 0).
    
     
    
    
