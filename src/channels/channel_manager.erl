%this module needs to keep track of the highest-nonced transaction recieved in each channel.
%We need the ability to spend and receive money, and to spend and receive hashlocked money.

-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, new_hashlock/3,hashlock/5,read/1,delete/1,id/1,keys/0,read_channel/1,bet_amounts/1,test/0,store/2]).
-define(LOC, constants:channel_manager()).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
        X == "" -> 
            K = dict:new(),
            db:save(?LOC,K),
	    K;
        true -> X
    end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC,K),
    io:format("channel_manager died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, N, Ch}, X) -> 
    %It would be nice to add a rule that we can only increase the nonce.
    {noreply, dict:store(N, Ch, X)};
handle_cast({delete, N}, X) -> 
    {noreply, dict:erase(N, X)}.
handle_call({read, N}, _From, X) -> 
    Out = case dict:find(N, X) of
	      error -> <<"does not exist">>;
	      Z -> Z
	  end,
    {reply, Out, X};
handle_call(keys, _From, X) -> 
    {reply, dict:fetch_keys(X), X}.
keys() -> gen_server:call(?MODULE, keys).
id(Partner) -> id_helper(Partner, keys(), []).
id_helper(_, [], Out) -> Out;
id_helper(Partner, [Key|T], Out) ->
    Ch = read_channel(Key),
    Acc1 = channel_block_tx:acc1(Ch),
    Acc2 = channel_block_tx:acc2(Ch),
    NewOut = if
                 ((Partner == Acc1) or (Partner == Acc2)) -> [Key|Out];
                 true -> Out
	     end,
    id_helper(Partner, T, NewOut).
read_channel(Key) ->
    F = read(Key),
    testnet_sign:data(channel_manager_feeder:channel(F)).
store(ChId, F) -> 
    gen_server:cast(?MODULE, {store, ChId, F}).
is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T).

read(ChId) -> 
    K = keys(),
    case is_in(ChId, K) of
	true ->
	    {ok, Out} = gen_server:call(?MODULE, {read, ChId}),
	    Out;
	false -> 
	    empty
    end.
delete(ChId) -> gen_server:call(?MODULE, {delete, ChId}).
bet_amounts(CB) ->
    Bets = channel_block_tx:bets(CB),
    sum_amounts(Bets, 0).
sum_amounts([], X) -> X;
sum_amounts([H|T], X) -> sum_amounts(T, X + abs(channel_block_tx:bet_amount(H))).

new_hashlock(Partner, A, SecretHash) ->
    ChId = hd(channel_manager:id(Partner)),
    Channel = block_tree:channel(ChId),
    A1 = channels:acc1(Channel),
    A2 = channels:acc2(Channel),
    Amount = case keys:id() of
	A2 -> A;
	A1 -> -A
    end,
    %Amount = A,
    hashlock(ChId, Amount, SecretHash).
hashlock(ChId, Amount, SecretHash) ->
    Ch = read_channel(ChId),
    keys:sign(hashlock(ChId, Ch, Amount, SecretHash, keys:id())).
hashlock(ChId, Ch, Amount, SecretHash, Id) ->
    Ch2 = channel_block_tx:update(Ch, 0, 1),
    Channel = block_tree:channel(ChId),
    Acc1 = channels:acc1(Channel),
    Acc2 = channels:acc2(Channel),
    OtherAccount = case Id of
            Acc1 -> -1;
            Acc2 -> 1
        end,
    Script = language:hashlock(SecretHash),
    channel_block_tx:add_bet(Ch2, Amount, Script, OtherAccount).


test() ->    
    %{Pub, Priv} = testnet_sign:new_key(),
    %{Addr, Pub, Priv} = {<<"VNSU5QDMZCvWmAU">>, <<"BN9moEiPOf9+rkCf+r10w8JzuDKnLyafUS18dgoA1OIiNeycYyx7vgcTYPDApZtZ0bP5vLms3E1QJDVPMWlWO7s=">>, <<"AteceXn/9Epq1TQaLO27EPn9SyjvNzL1p2Ot8Rpen/g=">>},
    {Addr, Pub, Priv} = testnet_sign:hard_new_key(),
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub, 620000, 0))),
    Partner = 4,
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(Partner, 10, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    block_tree:buy_block(),
    P5 = accounts:addr(block_tree:account(5)),
    P4 = accounts:addr(block_tree:account(4)),
    P3 = accounts:addr(block_tree:account(3)),
    P2 = accounts:addr(block_tree:account(2)),
    P1 = accounts:addr(block_tree:account(1)),
    ID = case Addr of 
	P1 -> 1;
	P2 -> 2;
	P3 -> 3;
	P4 -> 4;
	P5 -> 5
    end,
    CreateTx1 = keys:sign(to_channel_tx:create_channel(ID, 110000, 10000, <<"delegated_1">>, 0)),
    SignedCreateTx1 = testnet_sign:sign_tx(CreateTx1, Pub, Priv, ID, tx_pool:accounts()),
    true = testnet_sign:verify(SignedCreateTx1, tx_pool:accounts()),
    tx_pool_feeder:absorb(SignedCreateTx1),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    block_tree:buy_block(),
    S = 24004,
    C = read(S),
    D = element(2, element(2, C)),
    D = {channel_block,0,Partner,0,1,[],S,false,259,0,0,0},
    %Example of spending money through a channel.
    A = 1000,
    Tx = channel_manager_feeder:spend(S, A - 1),%send Tx to partner
    Tx2 = testnet_sign:sign_tx(Tx, Pub, Priv, ID, tx_pool:accounts()),%partner runs recieve/3 with 2nd option set to 0, returns Tx2.
    channel_manager_feeder:recieve(S, -A, Tx2),

    %Example of spending to ID rather than ChId.
    B = 2000,
    Tx3 = channel_manager_feeder:spend_account(Partner, B - 1),
    Tx4 = testnet_sign:sign_tx(Tx3, Pub, Priv, ID, tx_pool:accounts()),
    channel_manager_feeder:recieve_account(Partner, -B, Tx4),

    %example of hashlocked transaction.
    SH = secrets:new(),
    Amount = -200,
    Tx5 = hashlock(S, Amount, SH),
    Tx6 = testnet_sign:sign_tx(Tx5, Pub, Priv, ID, tx_pool:accounts()),%partner runs recieve_locked_payment/3, and returns Tx6
    channel_manager_feeder:spend_locked_payment(S, Tx6, Amount, SH),%we absorb Tx6.
    Tx7 = channel_manager_feeder:create_unlock_hash(S, secrets:read(SH)),
    %BH = hash:doit(channel_block_tx:bet_code(hd(channel_block_tx:bets(testnet_sign:data(Tx6))))),
    Tx8 = testnet_sign:sign_tx(Tx7, Pub, Priv, ID, tx_pool:accounts()),%partner runs unlock_hash/3 and returns Tx8
    channel_manager_feeder:unlock_hash(S, secrets:read(SH), Tx8),
    E = element(2, element(2, read(S))),
    E = {channel_block,0,Partner,-3198,5,[],S,false,259,0,0,0},
    success.
			 
			 
