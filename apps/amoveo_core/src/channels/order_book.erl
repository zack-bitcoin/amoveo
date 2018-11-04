-module(order_book).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 add/2,match/1,match/0,match_all/1,
         price/1,remove/4,exposure/1, ob_type/1,
	 new_market/3, new_scalar_market/6, make_order/4, 
	 data/1, info/1,
	 expires/1, period/1, keys/0, dump/1, dump_all/0,
	 test/0]).
%The market maker needs to refuse to remove some trades from the order book, if those trades are needed to cover his risk against trades that have already been matched.
%To keep track of how much exposure has been matched, the market maker needs to remember a number.
%We need to keep track of how much depth we have matched on one side, that way we can refuse to remove trades that are locked against money we need to cover commitments we already made in channels.
-record(ob, {exposure = 0, price = 5000, buys = [], sells = [], ratio = 5000, expires, period, height = 0, data}).%this is the price of buys, sells is 1-this.
%Exposure to buys is positive.
-record(order, {acc = 0, price, type=buy, amount}). %type is buy/sell
-include("../records.hrl").
-define(LOC, constants:order_book()).
expires(OB) ->
    OB#ob.expires.
period(OB) ->
    OB#ob.period.
ob_type(OB) ->
    OB#ob.data.
make_order(Acc, Price, Type, Amount) ->
    #order{acc = Acc, price = Price, type = Type, amount = Amount}.
%lets make a dictionary to store order books. add, match, price, remove, and exposure all need one more input to specify which order book in the dictionary we are dealing with.
%init(ok) -> {ok, #ob{}}.
init(ok) -> 
    %io:fwrite("start order book \n"),
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    KA = if
	     X == "" ->
		 K = dict:new(),
		 db:save(?LOC, K),
		 K;
	     true -> X
	 end,
    {ok, KA}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:fwrite("order book died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new_market, OID, Expires, Period, Data}, X) ->
    error = dict:find(OID, X),
    OB = #ob{expires = Expires, 
	     period = Period,
	     data = Data},
    NewX = dict:store(OID, OB, X),
    db:save(?LOC, NewX),
    {noreply, NewX};
handle_cast(dump_all, _) -> 
    {noreply, dict:new()};
handle_cast({dump, OID}, X) -> 
    X2 = dict:erase(OID, X),
    db:save(?LOC, X2),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({remove, AccountID, Type, Price, OID}, _From, X) -> 
    %remove this order from the book, if it exists.
    case dict:find(OID, X) of
	error -> {reply, error, X};
	{ok, OB} ->
	    Trades = case Type of
			 buy -> OB#ob.buys;
			 sell -> OB#ob.sells
		     end,
	    T2 = remove_if_exists(AccountID, Price, Trades),
	    OB2 = case Type of
		      buy -> OB#ob{buys = T2};
		      sell -> OB#ob{sells = T2}
		  end,
	    X2 = dict:store(OID, OB2, X),
	    db:save(?LOC, X2),
	    {reply, ok, X2}
    end;
handle_call({add, Order, OID}, _From, X) -> 
    {ok, OB} = dict:find(OID, X),
    true = is_integer(Order#order.price),
    true = Order#order.price > -1,
    true = Order#order.price < 10001,
    OB2 = case Order#order.type of
	      1 -> OB#ob{buys = add_trade(Order, OB#ob.buys)};
	      2 -> OB#ob{sells = add_trade(Order, OB#ob.sells)}
	  end,
    X2 = dict:store(OID, OB2, X),
    db:save(?LOC, X2),
    {reply, ok, X2};
handle_call(keys, _From, X) -> 
    K = dict:fetch_keys(X),
    {reply, K, X};
handle_call({match, OID}, _From, X) -> 
    %crawl upwards accepting the same volume of trades on each side, until they no longer profitably arbitrage. The final price should only close orders that are fully matched.
    %update a bunch of channels with this new price declaration.
    %io:fwrite("match internal\n"),
    {ok, OB} = dict:find(OID, X),
    Height = (tx_pool:get())#tx_pool.height,
    B = (Height - OB#ob.height) >= (OB#ob.period * 3 div 4),
    %B = true,
    {Out, X2}  = 
        case B of
            true ->
                %io:fwrite("do match\n"),
                {OB2, PriceDeclaration, Accounts, MatchPrice} = match_internal(Height, OID, OB, []),
                case Accounts of
                    [] -> 
                        %io:fwrite("nothing to match\n"),
                        {ok, X};%if there is nothing to match, then don't match anything.
                    _ ->
                        OB3 = OB2#ob{height = Height},
                        X3 = dict:store(OID, OB3, X),
                        db:save(?LOC, X3),%maybe this should be line should be lower.
                        Expires = expires(OB3),
                        Period = period(OB3),

			io:fwrite("order book before codekey\n"),
			io:fwrite(packer:pack(OB)),
			io:fwrite("\n"),
			{CodeKey, SS} = 
			    case ob_type(OB) of
				{binary} -> 
				    CodeKey0 = market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID),
				    SS0 = market:settle(PriceDeclaration, OID, MatchPrice),
				    {CodeKey0, SS0};
				{scalar, UpperLimit, LowerLimit, _} ->
				    CodeKey1 = scalar_market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID, UpperLimit, LowerLimit),
				    SS1 = scalar_market:settle(PriceDeclaration, OID, MatchPrice),
				    {CodeKey1, SS1}
			    end,
                        %CodeKey = market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID),
                        %SS = market:settle(PriceDeclaration, OID, MatchPrice),
                        secrets:add(CodeKey, SS),
                        channel_feeder:bets_unlock(channel_manager:keys()),
                        {{PriceDeclaration, Accounts}, X3}
                end;
            false ->
                %io:fwrite("do not match\n"),
                {ok, X}
        end,
            
    %Accounts are the account ids of the channels that needs to be updated.
    db:save(?LOC, X2),
    {reply, Out, X2};
handle_call({data, OID}, _From, Y) ->
    X = dict:find(OID, Y),
    {reply, X, Y};
handle_call({info, OID}, _From, X) ->
    {ok, OB} = dict:find(OID, X),
    Y = [OB#ob.price, OB#ob.exposure, OB#ob.ratio, OB#ob.data],
    {reply, Y, X};
%price exposure ratio should be depreciated.
handle_call({price, OID}, _From, X) -> 
    {ok, OB} = dict:find(OID, X),
    {reply, OB#ob.price, X};
handle_call({exposure, OID}, _From, X) -> 
    {ok, OB} = dict:find(OID, X),
    {reply, OB#ob.exposure, X};
handle_call({ratio, OID}, _From, X) -> 
    {ok, OB} = dict:find(OID, X),
    {reply, OB#ob.ratio, X};
handle_call(_, _From, X) -> {reply, X, X}.
finished_matching(Height, OID, OB, Accounts) ->
    E = OB#ob.exposure,
    Ratio = OB#ob.ratio,
    Price = OB#ob.price,
    MarketID = OID,
    PriceDeclaration = market:price_declaration_maker(Height, Price, Ratio, MarketID),
    OB2 = OB#ob{exposure = E, height = Height},
    {OB2, PriceDeclaration, Accounts, Price}.
    
match_internal(Height, OID, OB, Accounts) ->
    %io:fwrite("match internal internal\n"),
    E = OB#ob.exposure,
    Buys = OB#ob.buys,
    Sells = OB#ob.sells,
    if
	((Buys == []) or
	(Sells == [])) -> 
            %io:fwrite("no trades left to match in match internal\n"),
	    finished_matching(Height, OID, OB, Accounts);
	true ->
	    [Buy|B] = Buys,
	    [Sell|S] = Sells,
	    BuyPrice = Buy#order.price,
	    SellPrice = Sell#order.price,
	    if
		(BuyPrice+SellPrice) < 10000 ->
                    %io:fwrite("finished match internal \n"),
		    finished_matching(Height, OID, OB, Accounts);
		true ->
                    %io:fwrite("matching a trade\n"),
		    match_internal3(Height, OID, OB, Accounts, [Buy|B], [Sell|S])
	    end
    end.
match_internal3(Height, OID, OB, Accounts, [Buy|B], [Sell|S]) ->
    E = OB#ob.exposure,
    X = E - Sell#order.amount,
    Y = E + Buy#order.amount,
    X2 = abs(X),
    Y2 = abs(Y),
    {X4, AID1, AID2} = 
	if
	    X2 > Y2 -> %match the buy;
		%io:fwrite("match buy \n"),
		Ratio = (10000 * abs(Y)) div 
		    Sell#order.amount,
		{OB#ob{exposure = Y, buys = B, ratio = Ratio, price = (10000 - Sell#order.price)},
		 Buy#order.acc,
		 Sell#order.acc};
	    true -> %match the sell
		%io:fwrite("match sell \n"),
		Ratio = (10000 * abs(X)) div 
		    Buy#order.amount,
		{OB#ob{exposure = X, sells = S, ratio = Ratio, price = Buy#order.price}, 
		 Buy#order.acc,
		 Sell#order.acc}
	end,
    match_internal(Height, OID, X4, [AID1|[AID2|Accounts]]).
remove_if_exists(_, _, []) -> [];
remove_if_exists(AID, Price, [X|T]) -> 
    AID2 = X#order.acc,
    Price2 = X#order.price,
    case {AID2, Price2} of
	{AID, Price} -> T;
	_ -> [X|remove_if_exists(AID, Price, T)]
    end.
    
add_trade(Order, []) -> [Order];
add_trade(Order, [H|Trades]) ->
    P1 = Order#order.price,
    P2 = H#order.price,
    if
	P1 > P2 -> [Order|[H|Trades]];
	true -> [H|add_trade(Order, Trades)]
    end.
keys() ->
    gen_server:call(?MODULE, keys).
data(OID) -> 
    gen_server:call(?MODULE, {data, OID}).
info(OID) -> 
    gen_server:call(?MODULE, {info, OID}).
    
add(Order, OID) ->
    <<_:256>> = OID,
    gen_server:call(?MODULE, {add, Order, OID}).
match(OID) ->
    <<_:256>> = OID,
    Oracle = trees:get(oracles, OID),
    Result = Oracle#oracle.result,
    case Result of
        0 -> gen_server:call(?MODULE, {match, OID});
        _ -> ok
    end.
match() ->
    Keys = keys(),
    match_all(Keys).
match_all([]) -> ok;
match_all([H|T]) -> 
    match(H),
    match_all(T).
                    
remove(AccountID, Type, Price, OID) ->
    gen_server:call(?MODULE, {remove, AccountID, Type, Price, OID}).
%reduce(AccountID, Type, Price, Amount) ->
%    gen_server:cast(?MODULE, {reduce, AccountID, Type, Price, Amount}).
price(OID) ->
    gen_server:call(?MODULE, {price, OID}).
exposure(OID) ->
    gen_server:call(?MODULE, {exposure, OID}).
ratio(OID) ->
    gen_server:call(?MODULE, {ratio, OID}).
dump_all() ->
    gen_server:cast(?MODULE, dump_all).
dump(OID) ->
    gen_server:cast(?MODULE, {dump, OID}).
new_market(OID, Expires, Period) ->
    new_market(OID, Expires, Period, {binary}).
new_scalar_market(OID, Expires, Period, LL, UL, Many) ->
    new_market(OID, Expires, Period, {scalar, UL, LL, Many}).
new_market(OID, Expires, Period, Data) ->
    gen_server:cast(?MODULE, {new_market, OID, Expires, Period, Data}).



test() ->
    %add(#order{price = 4000, amount = 1000, type = buy}),
    %add(#order{price = 5999, amount = 100, type = sell}),
    %add(#order{price = 6001, amount = 100, type = sell}),
    OID = <<1:256>>,
    new_market(OID, 0, 0),
    dump(OID),
    new_market(OID, 0, 0),
    add(#order{price = 4000, amount = 1000, type = 2, acc = 3}, OID),
    add(#order{price = 5999, amount = 100, type = 1, acc = 2}, OID),
    add(#order{price = 6001, amount = 100, type = 1, acc = 4}, OID),
    %{_, [4,3]} = match(OID),
    {_, [4,3]} = gen_server:call(?MODULE, {match, OID}),
    {6000, 100, 1000} = {price(OID), exposure(OID), ratio(OID)},
    %1000 means 1/10th because only 1/10th of the big bet got matched.
    dump(OID),
    new_market(OID, 0, 0),
    add(#order{price = 5000, amount = 100, type = 1}, OID),
    add(#order{price = 6000, amount = 100, type = 1}, OID),
    add(#order{price = 4500, amount = 100, type = 2}, OID),
    add(#order{price = 3500, amount = 100, type = 2}, OID),
    gen_server:call(?MODULE, {match, OID}),
    {6000, -100,10000} = {price(OID), exposure(OID), ratio(OID)},
    dump(OID),
    success.
    
    
    
