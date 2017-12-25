-module(oracle_bet_tx).
-export([go/3, go2/3, make/6, id/1, from/1, to_prove/2]).
-record(oracle_bet, {from, %your account id.
		     nonce, 
		     fee, 
		     id, %id is the id of the oracle they want to participate in.
		     type, %either "true", "false" or "bad_question"
		     amount
                     }).%how many shares do you want to buy?
%This is how you can participate in an existing oracle.
%The market is an order book with 3 types of shares: "true", "false", "bad_question"
%All trades are matched into the order book in pairs at even odds.
%So the order book only stores 1 kind of order at a time.
%If you want your order to be held in the order book, it needs to be bigger than a minimum size.
%There is a maximum number of orders that can be stored in the order book at a time.
%If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.
to_prove(OID, Trees) ->
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    io:fwrite("oracle bet tx oid oracles oracle "),
    io:fwrite(packer:pack([OID, Oracles, Oracle])),%1, 7, 6th is 9
    io:fwrite("\n"),
    Orders = oracles:orders(Oracle),
    orders:all(Orders).
    
from(X) -> X#oracle_bet.from.
id(X) -> X#oracle_bet.id.
make(From, Fee, OID, Type, Amount, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = accounts:get(From, Accounts),
    Tx = #oracle_bet{
       from = From, 
       nonce = accounts:nonce(Acc) + 1,
       fee = Fee,
       id = OID,
       type = Type,
       amount = Amount
	   },
    {Tx, []}.
merge_sort(L) ->
    M = to_singles(L),
    merge2(M).
to_singles([]) -> [];
to_singles([H|T]) -> [[H]|to_singles(T)].
merge2([]) -> [];
merge2([L]) -> L;
merge2(L) -> merge2(improve(L)).
improve([]) -> [];
improve([X]) -> [X];
improve([A|[B|T]]) -> [merge3(A, B)|improve(T)].
merge3([], X) -> X;
merge3(X, []) -> X;
merge3([A|Ta], [B|Tb]) when A > B -> [A|merge3(Ta, [B|Tb])];
merge3([A|Ta], [B|Tb]) -> [B|merge3([A|Ta], Tb)].
ids([]) -> [];
ids([X|T]) ->
    [orders:aid(orders:deserialize(leaf:value(X)))|ids(T)].
det_pow(X, 1) -> X;
det_pow(Base, Ex) ->
    B = Ex rem 2,
    case B of
	0 -> det_pow(Base*Base, Ex div 2);
	1 -> Base * det_pow(Base, Ex - 1)
    end.
dict_give_bets_main(Id, Orders, Type, Dict, OID) ->
    %Id bought many orders of the same type. sum up all the amounts, and give him this many bets.
    Amount = sum_order_amounts(Orders, 0),
    oracle_bets:dict_add_bet(Id, OID, Type, 2*Amount, Dict).
sum_order_amounts([], N) -> N;
sum_order_amounts([H|T], N) -> 
    A = orders:amount(H),
    sum_order_amounts(T, A+N).
give_bets([], _Type, Accounts, _OID) -> Accounts;
give_bets([Order|T], Type, Accounts, OID) ->
    ID = orders:aid(Order),
    {_, Acc, _} = accounts:get(ID, Accounts),
    OldBets = accounts:bets(Acc),
    NewBets = oracle_bets:add_bet(OID, Type, 2*orders:amount(Order), OldBets),
    Acc2 = accounts:update_bets(Acc, NewBets),
    Accounts2 = accounts:write(Acc2, Accounts),
    give_bets(T, Type, Accounts2, OID).
dict_give_bets([], _Type, Dict, _OID) -> Dict;
dict_give_bets([Order|T], Type, Dict, OID) ->
    ID = orders:aid(Order),
    Dict2 = oracle_bets:dict_add_bet(ID, OID, Type, 2*orders:amount(Order), Dict),
    dict_give_bets(T, Type, Dict2, OID).
go(Tx, Dict, NewHeight) ->
    From = Tx#oracle_bet.from,
    Facc = accounts:dict_update(From, Dict, -Tx#oracle_bet.fee - Tx#oracle_bet.amount, Tx#oracle_bet.nonce, NewHeight),
    Dict2 = accounts:dict_write(Facc, Dict),
    Oracle = oracles:dict_get(Tx#oracle_bet.id, Dict2),
    0 = oracles:result(Oracle),%check that the oracle isn't already closed.
    go2(Tx, Dict2, NewHeight).
go2(Tx, Dict, NewHeight) -> %doit is split into two pieces because when we close the oracle we want to insert one last bet.
    From = Tx#oracle_bet.from,
    OID = Tx#oracle_bet.id,
    Oracle0 = oracles:dict_get(OID, Dict),
    OIL = governance:dict_get_value(oracle_initial_liquidity, Dict),
    VolumeCheck = orders:dict_significant_volume(Dict, OID, OIL),
    MOT = governance:dict_get_value(minimum_oracle_time, Dict),
    Oracle = if
    %if the volume of trades it too low, then reset the done_timer to another week in the future.
		 VolumeCheck -> Oracle0;
		 true -> 
		     oracles:set_done_timer(Oracle0, NewHeight + MOT)
	     end,
    true = NewHeight > oracles:starts(Oracle),
    %take some money from them. 
    OracleType = oracles:type(Oracle),
    TxType = case Tx#oracle_bet.type of
		 1 -> 1;
		 2 -> 2;
		 3 -> 3
	     end,
    Amount = Tx#oracle_bet.amount,
    NewOrder = orders:new(Tx#oracle_bet.from, Amount),
    Out = 
        if
	TxType == OracleType ->
                ManyOrders = dict_orders_many(OID, Dict),
                Minimum = OIL * det_pow(2, max(1, ManyOrders)), 
                true = Amount >= Minimum,
                Dict2 = orders:dict_add(NewOrder, OID, Dict),
                oracles:dict_write(Oracle, Dict2);
	true ->
                {Matches1, Matches2, Next, Dict2} =
                    orders:dict_match(NewOrder, OID, Dict),
    %Match1 is orders that are still open.
    %Match2 is orders that are already closed. We need to pay them their winnings.
                Dict3 = dict_give_bets_main(From, Matches1, TxType, Dict2, oracles:id(Oracle)),%gives a single oracle bet to this person
                Dict4 = dict_give_bets(Matches2, OracleType, Dict3, oracles:id(Oracle)),%gives oracle_bets to each account that got matched
                Oracle3 = case Next of
                              same -> 
                                  Oracle;
                              switch ->
                                  Oracle4 = oracles:set_done_timer(Oracle, NewHeight + MOT),
                                  oracles:set_type(Oracle4, TxType)
                          end,
                oracles:dict_write(Oracle3, Dict4)
        end,
    Out.
dict_orders_many(OID, Dict) ->
    {_, Many} = orders:dict_head_get(Dict, OID),
    Many.
    
