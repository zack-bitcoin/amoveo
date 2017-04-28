-module(oracle_bet_tx).
-export([test/0, doit/3, doit2/3, make/6]).
-record(oracle_bet, {from, %your account id.
		     nonce, 
		     fee, 
		     id, %id is the id of the oracle they want to participate in.
		     type, %either "true", "false" or "bad_question"
		     amount}).%how many shares do you want to buy?
%This is how you can participate in an existing oracle.
%The market is an order book with 3 types of shares: "true", "false", "bad_question"
%All trades are matched into the order book in pairs at even odds.
%So the order book only stores 1 kind of order at a time.
%If you want your order to be held in the order book, it needs to be bigger than a minimum size.
%There is a maximum number of orders that can be stored in the order book at a time.
%If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.
make(From, Fee, OID, Type, Amount, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = account:get(From, Accounts),
    Tx = #oracle_bet{
       from = From, 
       nonce = account:nonce(Acc) + 1,
       fee = Fee,
       id = OID,
       type = Type,
       amount = Amount
	   },
    {Tx, []}.
doit(Tx, Trees, NewHeight) ->
    From = Tx#oracle_bet.from,
    Accounts = trees:accounts(Trees),
    Facc = account:update(From, Trees, -Tx#oracle_bet.fee - Tx#oracle_bet.amount, Tx#oracle_bet.nonce, NewHeight),
    Accounts2 = account:write(Accounts, Facc),
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(Tx#oracle_bet.id, Oracles),
    io:fwrite("oracle is "),
    io:fwrite(packer:pack(Oracle)),
    io:fwrite("\n"),
    0 = oracles:result(Oracle),%check that the oracle isn't already closed.
    Trees2 = trees:update_accounts(Trees, Accounts2),

    doit2(Tx, Trees2, NewHeight).
doit2(Tx, Trees2, NewHeight) -> %doit is split into two pieces because when we close the oracle we want to insert one last bet.
    From = Tx#oracle_bet.from,
    Accounts2 = trees:accounts(Trees2),
    Oracles = trees:oracles(Trees2),
    {_, Oracle0, _} = oracles:get(Tx#oracle_bet.id, Oracles),
    Orders0 = oracles:orders(Oracle0),
    %{Head, _} = orders:head_get(Orders0),
    VolumeCheck = orders:significant_volume(Orders0, Trees2),
    Governance = trees:governance(Trees2),
    MOT = governance:get_value(minimum_oracle_time, Governance),

    Oracle = if
    %if the volume of trades it too low, then reset the done_timer to another week in the future.
		 VolumeCheck -> Oracle0;
		 true -> 
		     oracles:set_done_timer(Oracle0, NewHeight + MOT)
	     end,
    true = NewHeight > oracles:starts(Oracle),
    
    %take some money from them. 
    Orders = oracles:orders(Oracle),
    OracleType = oracles:type(Oracle),
    TxType = case Tx#oracle_bet.type of
		 true -> 1;
		 false -> 2;
		 bad -> 3
	     end,
    Amount = Tx#oracle_bet.amount,
    ID = orders:available_id(Orders),
    NewOrder = orders:new(ID, Tx#oracle_bet.from, Amount),
    if
	TxType == OracleType ->
	    ManyOrders = orders:many(Orders0),
	    OIL = governance:get_value(oracle_initial_liquidity, Governance),
	    Minimum = OIL * det_pow(2, max(1, ManyOrders)), 
	    true = Amount >= Minimum,
	    NewOrders = orders:add(NewOrder, Orders),
	    NewOracle = oracles:set_orders(Oracle, NewOrders),
	    NewOracles = oracles:write(NewOracle, Oracles),
	    trees:update_oracles(Trees2, NewOracles);
	true ->
	    {Matches1, Matches2, Next, NewOrders} =
		orders:match(NewOrder, Orders),
	    Oracle2 = oracles:set_orders(Oracle, NewOrders),
	    Accounts3 = give_bets_main(From, Matches1, TxType, Accounts2, oracles:id(Oracle2)),
	    Accounts4 = give_bets(Matches2, OracleType, Accounts3, oracles:id(Oracle2)),
	    Trees3 = trees:update_accounts(Trees2, Accounts4),
	    Oracle3 = case Next of
			   same -> 
			       Oracle2;
			   switch ->
			       Oracle4 = oracles:set_done_timer(Oracle2, NewHeight + MOT),
			       oracles:set_type(Oracle4, TxType)
		       end,
	    NewOracles = oracles:write(Oracle3, Oracles),
	    trees:update_oracles(Trees3, NewOracles)
    end.
det_pow(X, 1) -> X;
det_pow(Base, Ex) ->
    B = Ex rem 2,
    case B of
	0 -> det_pow(Base*Base, Ex div 2);
	1 -> Base * det_pow(Base, Ex - 1)
    end.
give_bets_main(Id, Orders, Type, Accounts, OID) ->
    %Id bought many orders of the same type. sum up all the amounts, and give him this many bets.
    %return the new accounts tree
    Amount = sum_order_amounts(Orders, 0),
    {_, Acc, _} = account:get(Id, Accounts),
    OldBets = account:bets(Acc),
    NewBets = oracle_bets:add_bet(OID, Type, 2*Amount, OldBets),
    Acc2 = account:update_bets(Acc, NewBets),
    account:write(Accounts, Acc2).
sum_order_amounts([], N) -> N;
sum_order_amounts([H|T], N) -> 
    A = orders:amount(H),
    sum_order_amounts(T, A+N).
give_bets([], _Type, Accounts, _OID) -> Accounts;
give_bets([Order|T], Type, Accounts, OID) ->
    ID = orders:aid(Order),
    {_, Acc, _} = account:get(ID, Accounts),
    OldBets = account:bets(Acc),
    NewBets = oracle_bets:add_bet(OID, Type, 2*orders:amount(Order), OldBets),
    Acc2 = account:update_bets(Acc, NewBets),
    Accounts2 = account:write(Accounts, Acc2),
    give_bets(T, Type, Accounts2, OID).
test() ->
    success.
