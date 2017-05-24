-module(order_book).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/1,match/0,price/0,remove/3,reduce/4]).
%To make the smart contract simpler, all trades matched are all-or-nothing. So we need to be a little careful to make sure the market maker isn't holding risk.
%The market maker needs to refuse to remove some trades from the order book, if those trades are needed to cover his risk against trades that have already been matched.
%To keep track of how much exposure has been matched, the market maker needs to remember a number.
%We need to keep track of how much depth we have matched on one side, that way we can refuse to remove trades that are locked against money we need to cover commitments we already made in channels.
-record(ob, {exposure = 0, price = 5000, buys = [], sells = []}).
%Exposure to buys is positive.
-record(order, {acc, price, type, amount}). %type is buy/sell
init(ok) -> {ok, #ob{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Order}, X) -> 
    true = is_integer(Order#order.price),
    true = Order#order.price > -1,
    true = Order#order.price < 10001,
    X2 = case Order#order.type of
	buy -> X#ob{buys = add_trade(Order, X#ob.buys)};
	sell -> X#ob{sells = add_trade(Order, X#ob.sells)}
    end,
    {noreply, X2};
handle_cast(match, _X) -> 
    %crawl upwards accepting the same volume of trades on each side, until they no longer profitably arbitrage. The final price should only close orders that are fully matched.
    X2 = ok,
    {noreply, X2};
handle_cast({remove, AccountID, Type, Price}, X) -> 
    %remove this order fromt the book, if it exists.
    X2 = ok,
    {noreply, X2};
handle_cast({remove, AccountID, Type, Price}, X) -> 
    %reduce this order by this amount, if it exists.
    X2 = ok,
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(price, _From, X) -> 
    {reply, X#ob.price, X};
handle_call(exposure, _From, X) -> 
    {reply, X#ob.exposure, X};
handle_call(_, _From, X) -> {reply, X, X}.

add_trade(Order, []) -> [Order];
add_trade(Order, [H|Trades]) ->
    P1 = Order#order.price,
    P2 = H#order.price,
    if
	P1 > P2 -> [Order|[H|Trades]];
	true -> [H|add_trade(Order, Trades)]
    end.

add(Order) ->
    gen_server:cast(?MODULE, {add, Order}).
match() ->
    gen_server:cast(?MODULE, match).
remove(AccountID, Type, Price) ->
    gen_server:cast(?MODULE, {remove, AccountID, Type, Price}).
reduce(AccountID, Type, Price, Amount) ->
    gen_server:cast(?MODULE, {reduce, AccountID, Type, Price, Amount}).
price() ->
    gen_server:call(?MODULE, price).
exposure() ->
    gen_server:call(?MODULE, exposure).



