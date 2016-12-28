-module(channel_manager_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, recieve/3,recieve_account/3,channel/1,recieve_locked_payment/4,spend_locked_payment/4,spend/2,new_channel/3,create_unlock_hash/2,spend_account/2,read_channel/1,unlock_hash/3,common/2]).
-record(f, {channel = [], unlock = []}).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({new_channel, ChId, Channel, Accounts}, _From, X) ->
    Ch = keys:sign(channel_block_tx:channel_block_from_channel(ChId, Channel, 0, 1, constants:max_reveal()-1, 0, []), Accounts),
    F = #f{channel = Ch, unlock = []},
    Out = channel_manager:store(ChId, F),
    {reply, Out, X};
handle_call({locked_payment, ChId, SignedChannel, Amount, SecretHash, Spend}, _From, X) ->
    NewCh = testnet_sign:data(SignedChannel),
    true = channel_block_tx:is_cb(NewCh),
    F = channel_manager:read(ChId),
    Ch = testnet_sign:data(F#f.channel),
    %NewAmount = channel_block_tx:amount(NewCh),
    %OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(NewCh),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    %Am = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,%error here.
    Ch2 = channel_block_tx:update(Ch, 0, N),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    ID = keys:id(),
    Bet = hd(channel_block_tx:bets(NewCh)),
    A = channel_block_tx:bet_amount(Bet),
    AA = A,
    AA = abs(Amount div 2),
    To1 = case ID of
	     Acc1 -> 1;
	     Acc2 -> -1
	 end,
    To = if
	     Spend -> -To1;
	     true -> To1
	 end,
    SecretHash = language:extract_sh(channel_block_tx:bet_code(Bet)),
    Script = language:hashlock(SecretHash),
    NewCha = channel_block_tx:add_bet(Ch2, 2*A, Script, To),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce and bet.
    NewCh = NewCha,
    NewF = #f{channel = SignedChannel, unlock = [[28]|F#f.unlock]},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(NewCh),
    {reply, Out, X};
handle_call({unlock_hash, ChId, Secret, SignedCh}, _From, X) ->
    {SignedCh2, N, _} = common(ChId, Secret),
    %BH = hash:doit(BetCode),
    NewCh = testnet_sign:data(SignedCh2),
    NewCh = testnet_sign:data(SignedCh),
    F = channel_manager:read(ChId),
    %NewUnlock = replace_n(N, Secret, F#f.unlock),
    NewUnlock = remove_n(N, F#f.unlock),
    %channel_block_tx:add,
    NewF = #f{channel = SignedCh2, unlock = NewUnlock},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(NewCh),
    {reply, Out, X};
handle_call({recieve, ID, MinAmount, ChId, SignedPayment}, _From, X) -> 
    Payment = testnet_sign:data(SignedPayment),
    F = channel_manager:read(ChId),
    Ch = testnet_sign:data(F#f.channel),
    NewAmount = channel_block_tx:amount(Payment),
    OldAmount = channel_block_tx:amount(Ch),
    NewN = channel_block_tx:nonce(Payment),
    OldN = channel_block_tx:nonce(Ch),
    Channel = block_tree:channel(ChId),
    A = NewAmount - OldAmount,
    N = NewN - OldN,
    true = N > 0,
    Payment2 = channel_block_tx:update(Ch, A, N),%this ensures that they didn't adjust anything else in the channel besides the amount and nonce.
    Payment = Payment2,
    %recieve. positive goes to 1.
    BTA1C = channels:acc1(Channel),
    BTA2C = channels:acc2(Channel),
    B = case ID of
        BTA1C -> A;
        BTA2C -> -A
    end,
    true = B > MinAmount - 1,
    NewF = #f{channel = SignedPayment, unlock = F#f.unlock},
    channel_manager:store(ChId, NewF),
    Out = keys:sign(Payment),
    {reply, Out, X};
handle_call(_, _From, X) -> {reply, X, X}.

recieve_account(Acc, MinAmount, SignedPayment) ->
    recieve(hd(channel_manager:id(Acc)), MinAmount, SignedPayment).
recieve(ChId, MinAmount, SignedPayment) ->
    %we need to verify that the other party signed it.
    ID = keys:id(),
    Pub = testnet_sign:pub(SignedPayment),
    PAddr = testnet_sign:pubkey2address(Pub),
    Payment = testnet_sign:data(SignedPayment),
    A1 = channel_block_tx:acc1(Payment),
    A2 = channel_block_tx:acc2(Payment),
    Acc1 = block_tree:account(A1),
    Acc2 = block_tree:account(A2),
    Addr1 = accounts:addr(Acc1),
    Addr2 = accounts:addr(Acc2),
    %Pub1 = accounts:pub(Acc1),
    %Pub2 = accounts:pub(Acc2),
    true = case PAddr of
	       Addr1 -> testnet_sign:verify_1(SignedPayment, PAddr);
	       Addr2 -> testnet_sign:verify_2(SignedPayment, PAddr)
	   end,
    %A1 -> testnet_sign:verify_2(SignedPayment, Pub2);
    %A2 -> testnet_sign:verify_1(SignedPayment, Pub1)
    %end,
    true = channel_block_tx:is_cb(Payment),
    gen_server:call(?MODULE, {recieve, ID, MinAmount, ChId, SignedPayment}).
channel(X) -> X#f.channel.
read_channel(Key) ->
    F = channel_manager:read(Key),
    testnet_sign:data(F#f.channel).
match_n(X, Bets) -> match_n(X, Bets, 0).
match_n(X, [Bet|Bets], N) ->
    Y = language:extract_sh(channel_block_tx:bet_code(Bet)),
    if
        X == Y -> N;
        true -> match_n(X, Bets, N+1)
    end.
remove_n(0, [_|T]) -> T;
remove_n(N, [H|T]) -> [H|remove_n(N-1, T)].
remove_bet(Hash, [H|T]) -> 
    A = hash:doit(channel_block_tx:bet_code(H)),
    if
	A == Hash -> T;
	true -> [H|remove_bet(Hash, T)]
    end.
	    
remove_nth(N, Bets) -> remove_nth(N, Bets, []).
remove_nth(0, [_|Bets], Out) -> lists:reverse(Out) ++ Bets;
remove_nth(N, [B|Bets], Out) -> remove_nth(N, Bets, [B|Out]).

common(ChId, {secret, Secret}) -> common(ChId, Secret);
common(ChId, Secret) ->
    SecretHash = hash:doit(Secret),
    OldCh = read_channel(ChId),
    Bets = channel_block_tx:bets(OldCh),
    N = match_n(SecretHash, Bets),%if the bets were numbered in order, N is the bet we are unlocking.
    Bet = nth(N, Bets),
    A = channel_block_tx:bet_amount(Bet),
    BetCode = channel_block_tx:bet_code(Bet),
    BetTo = channel_block_tx:bet_to(Bet),
    Amount = language:valid_secret(Secret, BetCode),
    NewBets = remove_nth(N, Bets),
    NewBets = remove_bet(hash:doit(BetCode), Bets),
    D = fractions:multiply_int(Amount, A)*BetTo,
    NewCh = channel_block_tx:replace_bet(OldCh, NewBets, D),
    NewNewCh = channel_block_tx:update(NewCh, 0, 1),
    true = channel_block_tx:nonce(OldCh) < channel_block_tx:nonce(NewNewCh),
    {keys:sign(NewNewCh), N, BetCode}.
create_unlock_hash(ChId, Secret) ->
    {SignedCh, _, _} = common(ChId, Secret),
    SignedCh.
nth(0, [X|_]) -> X;
nth(N, [_|T]) -> nth(N-1, T).
unlock_hash(ChId, Secret, SignedCh) ->
    gen_server:call(?MODULE, {unlock_hash, ChId, Secret, SignedCh}).
general_locked_payment(ChId, SignedChannel, Amount, SecretHash, Spend) ->
    gen_server:call(?MODULE, {locked_payment, ChId, SignedChannel, Amount, SecretHash, Spend}).
recieve_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, false).
spend_locked_payment(ChId, SignedChannel, Amount, SH) ->
    general_locked_payment(ChId, SignedChannel, Amount, SH, true),
    ok.
new_channel(ChId, Channel, Accounts) -> 
    gen_server:call(?MODULE, {new_channel, ChId, Channel, Accounts}).
spend_account(Acc, Amount) ->
    spend(hd(channel_manager:id(Acc)), Amount).
spend(ChId, Amount) ->
    Ch = read_channel(ChId),
    A1 = channel_block_tx:acc1(Ch),
    A2 = channel_block_tx:acc2(Ch),
    A = case keys:id() of
	    A1 -> -Amount;
	    A2 -> Amount
	end,
    keys:sign(channel_block_tx:update(Ch, A, 1)).

