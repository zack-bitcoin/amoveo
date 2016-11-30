-module(mail).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, pop/2,pop_hashes/1,cost/2,register/2,send/3,status/0,test/0,register_cost/0,internal_send/3]).
-record(msg, {start, lasts, msg, size = 0, to}).
-record(d, {db = dict:new(), accs = 0, msgs = 0}).
init(ok) -> {ok, #d{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new, Acc}, X) -> 
    B = case dict:find(Acc, X#d.db) of
            error ->
                NewD = dict:store(dict:new(), Acc, X#d.db),
                #d{db = NewD, accs = X#d.accs, msgs = X#d.msgs};
            {ok, _} -> X
    end,
    {noreply, B};
handle_cast({send, To, Message, Seconds}, X) -> 
    Accs = X#d.accs,
    DB = X#d.db,
    Msg = #msg{msg = Message, start = erlang:monotonic_time(), lasts = Seconds, size = size(Message), to = To},%price is the rate at which this costs money?
    A = case dict:find(To, DB) of
            error -> dict:new();
            {ok, Val} -> Val
    end,
    NewD = dict:store(To,dict:store(hash:doit(Msg), Msg, A), DB),
    NewX = #d{db = NewD, accs = Accs, msgs = X#d.msgs + 1},
    {noreply, NewX}.
handle_call({pop_hashes, Acc}, _From, X) -> 
    Out = case dict:find(Acc, X#d.db) of
	      error -> <<"empty">>;
	      {ok, Msgs} -> dict:fetch_keys(Msgs)
	  end,
    {reply, Out, X};
handle_call({pop, Acc, Hash}, _From, X) -> 
    {Out, NewX} = 
        case dict:find(Acc, X#d.db) of
            error -> {empty, X};
            %{ok, []} -> {empty, X};
            {ok, Msgs} ->
		D = dict:store(Acc, dict:erase(Hash, Msgs), X#d.db),
		    %D = dict:store(Acc, tl(Val), X#d.db),
                NX = #d{db = D, accs = X#d.accs, msgs = X#d.msgs-1},
		case dict:find(Hash, Msgs) of
		    error -> {empty2, X};
		    Y -> {Y, NX}
		end
        end,
    {reply, Out, NewX};
handle_call(status, _From, X) -> {reply, X#d.db, X}.
%-define(POP, <<1,7,3,24,7,4,2>>).
%price(Accounts, Messages) -> 10000 + ((Accounts + Messages) * 100).
%pop() -> ?POP.
%pop_maker(To) ->
    %Acc = keys:id(),
%A = block_tree:account(To),
%Pub = accounts:pub(A),
%encryption:send_msg(nonce:server_get(To), Pub).
pop_hashes(Account) ->
    gen_server:call(?MODULE, {pop_hashes, Account}).
pop(Account, Hashe) ->
    X = gen_server:call(?MODULE, {pop, Account, Hashe}),
    case X of 
	empty -> <<"no more messages">>;
	{ok, Y} -> pop3(Account, Y)
    end.
pop3(From, M) ->
    S = M#msg.lasts,
    if
	S == unlock ->
	    {unlock, M#msg.msg};
	S == locked_payment ->
	    {locked_payment, M#msg.msg};
	is_integer(M#msg.lasts) ->
	    Msg = M#msg.msg,
	    T = ((erlang:monotonic_time() - M#msg.start) div 1000) + 2000000,%2 second fee automatically.
						%T = timer:now_diff(erlang:monotonic_time(), M#msg.start) + 2000000,%2 second fee automatically.
	    Cost = cost(size(Msg), M#msg.lasts),
	    R = (M#msg.lasts * 1000000),
	    Refund = ((R - T) * Cost) div R,
	    if
		Refund < 1 -> 
		    io:fwrite("you paid for seconds "),
	    io:fwrite(integer_to_list(M#msg.lasts)),
		    io:fwrite("\n"),
		    io:fwrite("you needed"),
		    io:fwrite(integer_to_list(T)),
		    io:fwrite("\n"),
		    {ok, 0};
		true -> 
						%nonce:customer_next(From),
		    {pop_response, Msg, channel_manager_feeder:spend_account(From, Refund)}
	    end;
	true ->
	    io:fwrite(M#msg.lasts)
    end.
cost(MsgSize, Time) -> 10000 * MsgSize * Time. %time in seconds
-define(REGISTER, 100000).
register_cost() -> ?REGISTER.
status() -> gen_server:call(?MODULE, status).
register(Payment, Acc) ->
    ChId = hd(channel_manager:id(Acc)),
    channel_manager_feeder:recieve(ChId, ?REGISTER, Payment),
    gen_server:cast(?MODULE, {new, Acc}).
send(To, Msg, Seconds) ->
    gen_server:cast(?MODULE, {send, To, Msg, Seconds}).
internal_send(To, Msg, Seconds) ->
    send(To, Msg, Seconds).
%delete_account(Acc, Sig) ->
%    Time = abs(timer:now_diff(erlang:monotonic_time(), M#msg.time) div 1000000),
%    true = Time < 10,%time must be within 10 seconds of now
    %Sig must be over Time appended with <<"delete account">>.
%    ok = gen_server:call(?MODULE, {del_acc, Acc}),
%    channel_manager:spend_acc(Acc, ?REGISTER div 10 * 9).

            
test() ->            
    {Addr, Pub, Priv} = testnet_sign:hard_new_key(),
    %{Addr, Pub, Priv} = {<<"UrNC5nqd7uERs6m">>, <<"BIDUw/Dagrmh3R4akgRfCwH1/EIoCIAKrfMwPAKSoCn0h2iyjUb/lq8ZrngfARRlAdOsNSVp1gW0DQ8Xp+fz910=">>, <<"P1h6YBH65iQy/4lzvNhhPecYJrvoMoqeX+IR912bIjM=">>},
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub, 620000, 0))),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(1, 10, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    block_tree:buy_block(),
    CreateTx1 = keys:sign(to_channel_tx:create_channel(3, 110000, 1000, <<"delegated_1">>, 0)),
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
    SignedCreateTx1 = testnet_sign:sign_tx(CreateTx1, Pub, Priv, ID, tx_pool:accounts()),
    tx_pool_feeder:absorb(SignedCreateTx1),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    block_tree:buy_block(),
    gen_server:cast(?MODULE, {new, 3}),
    Msg = <<"test">>,
    gen_server:cast(?MODULE, {send, 3, Msg, 0}),
    gen_server:cast(?MODULE, {send, 3, Msg, 0}),
    PH = pop_hashes(3),
    {ok, Out} = gen_server:call(?MODULE, {pop, 3, hd(PH)}),
    Msg = Out#msg.msg,
    success.
